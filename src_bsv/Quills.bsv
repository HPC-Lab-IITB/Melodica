//Coprocessor unit: Takes Instructions from CPU and finishes the task

package Quills;
//====================================================================================================
//Library imports
import PAClib :: * ;
import StmtFSM :: *;
import Vector::*;
import FIFOF        :: *;
import FIFO        :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import FloatingPoint :: *;
import BRAMCore         :: *;
import BRAM		:: *;

//=====================================================================================================
//Project imports
import PositCore_Types :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import PositCore_accel ::*;
import Quire_Adder	::*;
import Fused_Commons       :: *;
import Scheduler_types	::*;	


//=====================================================================================================
//Type definitions

typedef enum { RESET, DOT_V, DOT_P, RD_Q_P, GEMV_P, WR_Q, MM_P} Instr_Cmds deriving (Bits, Eq, FShow);
/* 
RESET	: clears all the quires in Melodicas and the quire adder
DOT_V	: computes dot product of vectors extracting values from instruction  -- not implemented
DOT_P	: computes dot product of vectors by reading from the pointers (left as partial products)
GEMV_P	: Computes Matrix*vector and partial products stay in respective quires of Melodicas
RD_Q_P	: Finishes quire addition of partial products and return posit value
WR_Q	: Writes the value in quire accumulator into TCM
*/


typedef enum {Stg_1, Stg_2, Stg_3, Stg_4, Stg_5, Stg_6, OUT} State deriving (Bits,Eq, FShow);

typedef Tuple7#(Instr_Cmds, Mem_addr, Vec_len, Vec_len, Mem_addr, Vec_len, Vec_len)   Instruction; 
/* Instruction decodes as:
Instr_Cmds(1)	: opcode like RST_Q, GEMV, GEMV_P
Mem_addr(2)		: starting addr of A in TCM_A  -- TCM_A populated in row major format
Vec_len(3)		: # of posits in a row of A (columns in A)
Vec_len(4)		: No. of rows in A
Mem_addr(5)		: starting addr of B in TCM_B  -- TCM_B populated in column major format
Vec_len(6)		: # of posits in a row of B (columns in B)
Vec_len(7)		: No. of rows in B
*/
//=============================================================================================================

interface Scheduler_IFC;
   interface Server #(Instruction, Bit#(PositWidth)) server_sched;
endinterface

(* synthesize *)
module mkSched (Scheduler_IFC);
Reg#(Bit#(32)) count <- mkReg(0);	
Reg#(Bit#(TCM_ADDR)) pointer_A <- mkRegU;
Reg#(Bit#(TCM_ADDR)) pointer_B <- mkRegU;
Reg#(Bit#(TCM_ADDR)) addr_A <- mkRegU;
Reg#(Bit#(TCM_ADDR)) addr_B <- mkRegU;
Reg#(Bit#(TCM_ADDR)) count_row_B <- mkReg(0);
Reg#(Bit#(32)) lines <- mkRegU;
//Reg#(Bit#(TCM_ADDR)) op_lines <- mkRegU;
Reg#(Bit#(32)) pointer_lines <- mkRegU;
Reg#(Bit#(32)) col_len_A <- mkRegU;
Reg#(Bit#(32)) row_len_B <- mkRegU;
Reg#(Bit#(32)) row_len_B_dummy <- mkRegU;
Reg#(Bit#(32)) col_len_A_dummy <- mkRegU;
Reg#(Bit#(32)) col_count <- mkReg(0);
Reg#(State) reg_status <- mkReg(Stg_1);
FloatingPoint::RoundMode round_mode = Rnd_Nearest_Even;

FIFO #(Bit#(PositWidth))  fifo_posit   <- mkSizedFIFO(512);	// For holding GEMV_P outputs

FIFOF #(Instruction) ffO_Instr <- mkSizedFIFOF(1);	//input fifo
//FIFOF #(Vector #(N_melodica, Bit#(QuireWidth))) ffO_result <- mkSizedFIFOF(1);

Vector#(N_melodica,PositCore_IFC) melodica <- replicateM(mkPositCore);	//Vector of Melodicas

Quire_Adder_IFC q_adder <- mkQuire_Adder;	//quire_accumulator

// BRAM config constants
Bool config_output_register_BRAM = False;    // i.e., no output register
Bool load_file_is_binary_BRAM = False;       // file to be loaded is in hex format

 BRAM_DUAL_PORT #(Addr, TCM_Word) tcm_A <- mkBRAMCore2Load (mem_size, config_output_register_BRAM, "tcm_mem.hex", load_file_is_binary_BRAM);

 BRAM_DUAL_PORT #(Addr, TCM_Word) tcm_B <- mkBRAMCore2Load (mem_size, config_output_register_BRAM, "tcm_mem.hex", load_file_is_binary_BRAM);

let a_fd = tcm_A.a;
let a_bd = tcm_A.b;
let b_fd = tcm_B.a;
let b_bd = tcm_B.b;

// to convert pipe interface to a server interface with a fifo
module [Module] mkPipe_to_Server
                #(Pipe #(ta, tb) pipe)
                (Server #(ta, tb))
        provisos (Bits #(ta, wd_ta));

   FIFOF #(ta)   fifof  <- mkSizedFIFOF(1);
   PipeOut #(tb) serverPipe <- pipe (f_FIFOF_to_PipeOut (fifof));

   return (interface Server;
              interface Put request  = toPut (fifof);
              interface Get response = toGet (serverPipe);
           endinterface);
endmodule: mkPipe_to_Server



//---------------------Pipeline functions (PAC lib)--------------------------------------------------------------

function Vector #(Vec_size, Bit#(PositWidth)) fn_unpack (Bit#(TCM_XLEN) a);
	return unpack(a);
endfunction

function Pipe #(Bit#(TCM_XLEN), Vector #(Vec_size, Bit#(PositWidth))) mkUnpack ();	//function converted into pipe
	let unpacker = mkFn_to_Pipe (fn_unpack);
	return unpacker;
endfunction

function Pipe #(Vector #(Vec_size, Bit#(PositWidth)), Vector #(N_melodica, Bit#(PositWidth))) mkFnl();
	let funnel = mkFunnel ();
	return funnel;
endfunction

/*function Pipe #(Vector#(N_melodica, Bit#(QuireWidth)), Vector#(1, Bit#(QuireWidth))) mkQFnl();
	let qfunnel = mkFunnel ();
	return qfunnel;
endfunction
*/
//---------------------------------------------------------------------------------------------------------

module [Module] mkStage (Server#(Bit#(TCM_XLEN),Vector #(N_melodica, Bit#(PositWidth))));	//pipe converted into module with server ifc
	let s <- mkPipe_to_Server(mkCompose(mkUnpack,mkFnl));					// input from TCM , output to Melodica
	return s;
endmodule


	Server#(Bit#(TCM_XLEN),Vector #(N_melodica, Bit#(PositWidth))) stage_A <- mkStage;
	Server#(Bit#(TCM_XLEN),Vector #(N_melodica, Bit#(PositWidth))) stage_B <- mkStage;

//=======================================================================================================
// Sequential statement for fetching from TCMs
Stmt mem_pointer = 	seq
						//-------------------GEMV-----------------------------------------------
						par
						if (tpl_1(ffO_Instr.first) == GEMV_P)
						seq
						while (col_len_A > 0) // reset B's pointer as many times as Rows in A
							seq 
							action
$display("in loop", lines);
							pointer_B <= addr_B;
							pointer_lines <= lines;
							col_len_A <= col_len_A - 1;
							endaction
							while (pointer_lines > 0) // lines is # lines in TCM to store one column of B (or) one row of A
								seq
									action
									a_fd.put(False, pointer_A, ?);
									b_fd.put(False, pointer_B, ?);
									$display("tcm request",$time);
									endaction

									action
									stage_A.request.put(a_fd.read);
									stage_B.request.put(b_fd.read);
									pointer_A <= pointer_A + 1;
									pointer_B <= pointer_B + 1;
									pointer_lines <= pointer_lines - 1;
$display("in loop response");
									endaction
								endseq
							endseq
						endseq
						endpar
					//-----------------------------DOT_P---------------------------------------------------------
						par
						if (tpl_1(ffO_Instr.first) == DOT_P)
						seq
							action
							pointer_B <= addr_B;
							pointer_lines <= lines;
							endaction
							while (pointer_lines > 0) // lines is # lines in TCM to store one column of B (or) one row of A
								seq
									action
									a_fd.put(False, pointer_A, ?);
									b_fd.put(False, pointer_B, ?);
									//$display("tcm request",$time);
									endaction

									action
									stage_A.request.put(a_fd.read);
									stage_B.request.put(b_fd.read);
									//$display("a_fd",a_fd.read);
									//$display("b_fd",b_fd.read);
									//$display("tcm response",$time);
									pointer_A <= pointer_A + 1;
									pointer_B <= pointer_B + 1;
									pointer_lines <= pointer_lines - 1;
									endaction
								endseq
						endseq
						endpar
					//-------------------------------MATRIX MULT----------------------------------------------------------------
						par
						if (tpl_1(ffO_Instr.first) == MM_P)
						seq
						while (row_len_B > 0)
							seq
							action 
							pointer_A <= addr_A;
$display("loop1", row_len_B);
//							pointer_B <= addr_B + lines*(row_len_B_dummy - row_len_B);
							row_len_B <= row_len_B - 1;
							count_row_B <= count_row_B + 1;
							col_len_A <= col_len_A_dummy;
							endaction
							while (col_len_A > 0) // reset B's pointer as many times as Rows in A
								seq 
								action
$display("loop2",col_len_A);
								pointer_B <= addr_B + truncate(lines)*(count_row_B);
								//pointer_B <= addr_B + lines*(count_row_B);
								pointer_lines <= lines;
								col_len_A <= col_len_A - 1;
								endaction
								while (pointer_lines > 0) // lines is # lines in TCM to store one column of B (or) one row of A
									seq
										action
										a_fd.put(False, pointer_A, ?);
										b_fd.put(False, pointer_B, ?);
										$display("tcm request",$time);
										endaction

										action
										stage_A.request.put(a_fd.read);
										stage_B.request.put(b_fd.read);
										pointer_A <= pointer_A + 1;
										pointer_B <= pointer_B + 1;
										pointer_lines <= pointer_lines - 1;
										$display("tcm response", pointer_lines);
										endaction
									endseq
								endseq
							endseq
						endseq
						endpar		
		endseq;
						
//end of Stmt mem_pointer definition------------------------------------------------------------------------------------------------


FSM pointerFSM <- mkFSM( mem_pointer);


(* mutually_exclusive = "init, fetch" *)

//--------------rule that resets all Melodicas-------------------------------------------
	rule rl_reset (reg_status == Stg_1 && tpl_1(ffO_Instr.first)==RESET);
		$display("IN RESET",$time);
		for  (Bit#(32) i =0; i < fromInteger(valueOf(N_melodica)); i=i+1 )
		begin
		melodica[i].server_core.request.put(tuple4(?,?,?,RST_Q));
		end
		q_adder.clear; // line to clear quire accumulator
		ffO_Instr.deq;		
	endrule

//---------------rule that reads the instruction----------------------------------------
	rule init (reg_status == Stg_1 && (tpl_1(ffO_Instr.first)==GEMV_P  || tpl_1(ffO_Instr.first)==DOT_P || tpl_1(ffO_Instr.first)==MM_P) );
		let ps = fromInteger(valueOf(PositWidth));
		addr_B <= tpl_5(ffO_Instr.first);
		addr_A <= tpl_2(ffO_Instr.first);
		pointer_A <= tpl_2(ffO_Instr.first);
		let row_len_A = tpl_3(ffO_Instr.first);
		col_len_A <= tpl_4(ffO_Instr.first);
		col_len_A_dummy <= tpl_4(ffO_Instr.first);
		pointer_B <= tpl_5(ffO_Instr.first);
		row_len_B <= tpl_6(ffO_Instr.first);
		row_len_B_dummy <= tpl_6(ffO_Instr.first);
		Bit#(32) tcm_len = fromInteger(valueOf(TCM_XLEN));
		lines <= (row_len_A*ps)/tcm_len; 
//		if (tpl_1(ffO_Instr.first) == GEMV_P) op_lines <= (row_len_A*tpl_4(ffO_Instr.first)*ps)/tcm_len; //no. of lines for all posits in Matrix A
//		if (tpl_1(ffO_Instr.first) == MM_P) op_m_lines <= (row_len_A*tpl_4(ffO_Instr.first)*ps*(tpl_6(ffO_Instr.first)))/tcm_len; //no. of lines for all posits in Matrix A
		reg_status <= Stg_2;
		$display("Stg_1");		
	endrule

//------------rule that starts fsm to read values from TCM-------------------------------
	rule fetch (reg_status == Stg_2 && (tpl_1(ffO_Instr.first)==GEMV_P  || tpl_1(ffO_Instr.first)==DOT_P || tpl_1(ffO_Instr.first)==MM_P));
		$display("Fetch",lines, col_len_A,$time);
		pointerFSM.start();
		//ffO_Instr.deq;
		reg_status <= Stg_3;
	endrule

//-----------rule that reads from pipe and dispatches to Melodica------------------------------------
	rule dispatch (reg_status == Stg_3 && (tpl_1(ffO_Instr.first)==GEMV_P  || tpl_1(ffO_Instr.first)==DOT_P || tpl_1(ffO_Instr.first)==MM_P));

		$display("IN DISPATCH",$time);
		let opcode = tpl_1(ffO_Instr.first);
		Vector#(N_melodica,Bit#(PositWidth)) zA <- stage_A.response.get();
		Vector#(N_melodica,Bit#(PositWidth)) zB <- stage_B.response.get();

	for  (Bit#(32) i =0; i < fromInteger(valueOf(N_melodica)); i=i+1 )
		begin
		melodica[i].server_core.request.put(tuple4(tagged P zA[i],tagged P zB[i],round_mode,FMA_P));
		end

		if (count == fromInteger(valueOf(Op_count))*lines - 1) begin
			if (tpl_1(ffO_Instr.first)==DOT_P) begin
				reg_status <= Stg_1; 
				$display(" finished one DOT_P");
				count <= 0;
				ffO_Instr.deq;
				end
			else if (tpl_1(ffO_Instr.first)==GEMV_P) begin
				reg_status <= Stg_4; // finished one DOT_P reading the posit value
				count <= 0;
				col_count <= col_count + 1;
				$display(" finished one DOT_P");
				end
			else if (tpl_1(ffO_Instr.first)==MM_P) begin
				reg_status <= Stg_4; // finished one DOT_P reading the posit value
				count <= 0;
				col_count <= col_count + 1;
				$display(" finished one DOT_P");
				end
		end  
		else 		count <= count + 1; 
	endrule

//--------------rule that 'gets' from Melodica after each FMA_P-------------------------------------------
	rule re_dispatch ((reg_status == Stg_1 && tpl_1(ffO_Instr.first)==RD_Q_P) || (reg_status == Stg_4 && (tpl_1(ffO_Instr.first)==GEMV_P || tpl_1(ffO_Instr.first)==MM_P)));
		for  (Bit#(32) i =0; i < fromInteger(valueOf(N_melodica)); i=i+1 )
		begin
		melodica[i].server_core.request.put(tuple4(?,?,round_mode,RD_Q));
		end
		reg_status <= Stg_5;
	endrule

//---------------rule to Read from quire------------------------------------------------------------------
	rule read_quire (reg_status == Stg_5);
		//$display("IN Q_FNL",$time);
		Vector #(N_melodica, PositCore_accel::Fpu_Rsp) result; // stores response from Melodicas
		for  (Integer i =0; i < fromInteger(valueOf(N_melodica)); i=i+1 )
		begin
		result[i] <- melodica[i].server_core.response.get();
		end
		//$display("result", fshow(result), $time);
		Vector #(N_melodica, Quire_Acc) q_result;
		for  (Integer b =0; b < fromInteger(valueOf(N_melodica)); b=b+1 )
		begin
		q_result[b] = tpl_1(result[b]).Q;	//vector that need to be sent to quire_funnel
		end
		//$display("q_result", fshow(q_result), $time);	
		q_adder.add.put(q_result);
		
		//stage_quire.request.put(result);
		//ffO_Instr.deq;
		reg_status <= Stg_6;
//		positCore_accel.server_core.request.put(tuple4(?,?,RD_Q,round_mode));
	endrule

//--------------------------------------------------------------------------------------------------------
	rule add_quire (reg_status == Stg_6);	//stays in this rule for N_melodica times
		$display("STG_6");		
		q_adder.read_req_adder;
		reg_status <= OUT;
	endrule

//----------------------------------------------------------------------------------------------------------------
	rule rl_out (reg_status == OUT);
		let pf <- q_adder.read_posit_adder.get();
		q_adder.clear;
		fifo_posit.enq(pf);
		$display("enqueue fifo_posit",pf, " ", $time);
		if (tpl_1(ffO_Instr.first)==RD_Q_P) begin
			reg_status <= Stg_1;
			ffO_Instr.deq;
		end
		else if (tpl_1(ffO_Instr.first)==GEMV_P) begin
			if (col_count == col_len_A) begin
				reg_status <= Stg_1;
				ffO_Instr.deq;
				col_count <= 0;
			end
			else reg_status <= Stg_3;
		end
		else if (tpl_1(ffO_Instr.first)==MM_P) begin
			if (col_count == col_len_A_dummy*row_len_B_dummy) begin
				reg_status <= Stg_1;
				ffO_Instr.deq;
				col_count <= 0;
				$display(" finished one MM_P", col_count);
			end
			else reg_status <= Stg_3;
		end
	endrule		
//-----------------------------------------------------------------------------------------------------------------

	rule quire_write ((reg_status == Stg_1)	&& (tpl_1(ffO_Instr.first)==WR_Q));	
		let q = q_adder.write_quire(); 	// add padding
		//Vector#(5, Bit#(128)) qf = unpack(pack(q));
		//a_bd.put(True, 5, qf[0]);
	endrule

interface server_sched = toGPServer (ffO_Instr,fifo_posit);
//

endmodule

endpackage: Quills	
		



