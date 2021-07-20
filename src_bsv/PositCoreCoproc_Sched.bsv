//FMA FDA PtoQ QtoP FtoP PtoF
package PositCoreCoproc_Sched;

// Library imports
import FIFOF        :: *;
import FIFO        :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import ConfigReg :: *;
import Vector :: * ;

// Project imports
import PositCore_Types :: *;
import Extracter :: *;
import Normalizer :: *;
import Extracter_Types :: *;
import Normalizer_Types :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Common_Fused_Op :: *;
import Adder_Types_fused_op_PC 	:: *;
import Adder_fused_op_PC	:: *;
import Multiplier_fma	:: *;
import Multiplier_Types_fma	:: *;
import FMA_PNE_Quire_PC :: *;					
import FDA_PNE_Quire_PC :: *;
import FtoP_PNE_PC :: *;
import PtoF_PNE_PC :: *;
import PtoF_Types::*;
//import PositToQuire_PNE_PC :: *;
//import QuireToPosit_PNE_PC :: *;
//`ifdef BASIC_OPS
//import Add_PNE_PC :: *;
//import Mul_PNE_PC :: *;
//import Div_PNE_PC :: *;
//`endif
import FloatingPoint :: *;
import Utils  :: *;

`ifdef QUILLS
import FPU_Types :: *;
`else
//----------------------------------------------------------------------------------------------------
// Type definitions
/*
typedef FloatingPoint#(11,52) FDouble;
typedef FloatingPoint#(8,23)  FSingle;

typedef union tagged {
   FDouble D;
   FSingle S;
   } FloatU deriving(Bits,Eq);
*/
typedef Tuple2#( FloatU, FloatingPoint::Exception ) FloatE;	// all info about output Floating point 

typedef Tuple2#( Maybe#(FloatE), Bit #(1) )   Fpu_Rsp_accel;		 // Server response which include valid_bit

typedef enum {RST_Q, FMA_P, FMS_P, RD_Q} PositCmds deriving (Bits, Eq, FShow);		//for 3 opcodes currently...FMS to be included

typedef Tuple4#(FloatU, FloatU, PositCmds, RoundMode) Posit_Req_accel;			// Instruction has 2 Floating points and one opcode	

typedef Tuple2 #(Outputs_md, PositCmds) Mult_Out;			// multiplier output & opcode

//----------------------------------------------------------------------------------------------------			

interface PositCore_IFC_accel;
   interface Server #(Posit_Req_accel, Bit#(QuireWidth)) server_core;		// req type: Posit_Req_accel, resp type: Fpu_Rsp_accel
endinterface


(* synthesize *)
module mkPositCore_accel_sched #(Bit #(4) verbosity) (PositCore_IFC_accel);
	Reg #(Bit#(QuireWidth))  rg_quire   <- mkReg(0);		// quire register
	Reg #(Bit#(1))  rg_quire_busy   <- mkReg(0);			// set to 1 when quire is being updated(adder/ subtracter active)	

//---------------------
	Reg #(Bit#(QuireWidth))  rg_quire_1   <- mkReg(0);		// quire register
	Reg #(Bit#(1))  rg_quire_busy_1   <- mkReg(0);			// set to 1 when quire is being updated(adder/ subtracter
//----------------------			
	Reg #(Bit#(5))  rg_queue[2]	<- mkCReg(2,0);				//Creg : to check all computations before RD_Q are done
	FMA_PNE_Quire       fma             <- mkFMA_PNE_Quire(rg_quire);	// not being used : split up mult + add		
	FDA_PNE_Quire       fda             <- mkFDA_PNE_Quire(rg_quire);		
	FtoP_PNE            ftop1           <- mkFtoP_PNE;		//ftop module 1					
	FtoP_PNE            ftop2           <- mkFtoP_PNE;		//ftop module 2
//	QuireToPosit_PNE    qtop            <- mkQuireToPosit_PNE(rg_quire);		//qtop module		
	PtoF_PNE            ptof            <- mkPtoF_PNE;		//ptof module	
	Multiplier_IFC	    multiplier	    <- mkMultiplier;	//two posit multiplier
	Adder_IFC		adder 			<- mkAdder(rg_quire);	//adder module

	Adder_IFC		adder_1 			<- mkAdder(rg_quire_1);	//adder module
	    
`ifdef BASIC_OPS
	Mul_PNE             mul            <- mkMul_PNE;
	Div_PNE             div            <- mkDiv_PNE;		
	Add_PNE             add            <- mkAdd_PNE;	
`endif

	Extracter_IFC  	 extracter1 <- mkExtracter;
	Extracter_IFC    extracter2 <- mkExtracter;
//	Extracter_IFC    extracter3 <- mkExtracter;
	Normalizer_IFC   normalizer1 <- mkNormalizer;
	Normalizer_IFC   normalizer2 <- mkNormalizer;
//	Normalizer_IFC   normalizer3 <- mkNormalizer;


        // Bypass FIFO as opcodes can be bypassed 
        // case effectively merging rules extract_in and rl_ftop
	FIFOF #(PositCmds) opcode_in <- mkSizedBypassFIFOF(16);
	FIFOF #(PositCmds) opcode_norm <- mkSizedBypassFIFOF(16);
	FIFOF #(PositCmds) opcode_add <- mkSizedBypassFIFOF(16);
    FIFOF #(Mult_Out) ff_mul_Out <- mkSizedBypassFIFOF(16);

	FIFO #(PositCmds) opcode_qtop <- mkFIFO1;
`ifdef NORM_EXT
	FIFO #(PositCmds) opcode_qtop_norm <- mkFIFO1;
	FIFO #(PositCmds) opcode_qtop_ext <- mkFIFO1;
`endif
	FIFO #(PositCmds) opcode_ptof <- mkFIFO1;


//	FIFO #(PositCmds) opcode_out <- mkFIFO;

	FIFO #(Posit_Req_accel) ffI <- mkSizedFIFO(16);
	FIFO #(Bit#(QuireWidth)) 	ffO <- mkSizedFIFO(16);	
/*
(* mutually_exclusive = "rl_norm, rl_qtop" *)		//rules for normalizer put
`ifdef NORM_EXT	
(* mutually_exclusive = "rl_ext, rl_qtop_norm" *)		//rules for normalizer get and extracter put		
(* mutually_exclusive = "rl_mult, rl_qtop_out" *)
`endif
*/
	//-----------------------------------------------------------------------------------------------
	// rule for quire reset //fires when opcode is rst and all earlier instructions are executed

	rule reset_quire(tpl_3(ffI.first) == RST_Q && rg_queue[1] == 5'b0 );
		rg_quire 	<= 0;
		rg_quire_busy   <= 1'b0;
		Maybe#(FloatE) out_ffO= tagged Invalid;
		Bit#(1) valid_bit = 0;
		//ffO.enq(tuple2(out_ffO,valid_bit));
		ffI.deq;
	endrule
	//-----------------------------------------------------------------------------------------------			
	// rule for mandatory float to posit conversion for FMA_P // Creg queue count ++ //	enq FFO Invalid

	rule rl_ftop(tpl_3(ffI.first) == FMA_P || tpl_3(ffI.first) == FMS_P);
		let in_posit1 = tpl_1(ffI.first).P;
	//$display("in rl_extract",$time);	
		extracter1.inoutifc.request.put (in_posit1);
		let in_posit2 = tpl_2(ffI.first).P;
		extracter2.inoutifc.request.put (in_posit2);				
		opcode_in.enq(tpl_3(ffI.first));					
		rg_queue[0] <= rg_queue[0] + 1;	
		//Maybe#(FloatE) out_ffO = tagged Invalid;
		//Bit#(1) valid_bit = 0;
		//ffO.enq(tuple2(out_ffO,valid_bit));	
		ffI.deq;
        endrule

	//-----------------------------------------------------------------------------------------------
 	// rule for connecting extracter module to multiplier module // enq opcode_out // deq opcode_ext
	rule rl_mult(opcode_in.first == FMA_P || opcode_in.first == FMS_P);
	//$display("in rl_mult",$time);
		let extOut1 <- extracter1.inoutifc.response.get();
	   	let extOut2 <- extracter2.inoutifc.response.get();
		multiplier.inoutifc.request.put (Inputs_md {
              sign1: extOut1.sign,
              nanflag1: 1'b0,
              zero_infinity_flag1: extOut1.zero_infinity_flag ,
              scale1 : extOut1.scale,
              frac1 : extOut1.frac,
              sign2: extOut2.sign,
              nanflag2: 1'b0,
              zero_infinity_flag2: extOut2.zero_infinity_flag ,
              scale2 : extOut2.scale,
              frac2 : extOut2.frac});
		opcode_add.enq(opcode_in.first);
		opcode_in.deq;
               
	endrule
	//-----------------------------------------------------------------------------------------------
	//rule for multiplier populating the bypass FIFO ff_mul_Out // enq opcode-quire
	rule rl_mul_Out(opcode_add.first == FMA_P || opcode_add.first == FMS_P);
		let opadd = opcode_add.first();
	//$display("in rl_mul_Out",$time);
		let mulOut <- multiplier.inoutifc.response.get();
		Outputs_md mul_Out = mulOut;
		PositCmds op_add = opadd;
		Mult_Out m_out = tuple2(mul_Out,op_add);		
		ff_mul_Out.enq(m_out);
		opcode_add.deq;
	endrule

	//-----------------------------------------------------------------------------------------------
(* mutually_exclusive = "rl_quire_finish,rl_rdq" *)
	// rue initiates add/subtract accordingly // rg_quire_busy is set 1
	rule rl_quire_compute(rg_quire_busy == 1'b0);
		if (tpl_2(ff_mul_Out.first) == FMA_P || tpl_2(ff_mul_Out.first) == FMS_P)
			begin
			adder.inoutifc.request.put(Inputs_a{q2 : tpl_1(ff_mul_Out.first)});
			rg_quire_busy <= 1'b1;
	$display("in rl_quire_compute",$time, tpl_1(ff_mul_Out.first));
			ff_mul_Out.deq;
			end
		if (verbosity > 1)
                   $display ("%0d: %m: rl_quire_compute: ", cur_cycle,"Quire value : ",rg_quire);

//		else if (tpl_2(ff_mul_Out.first) == FMS_P)
//			begin
//			subtracter.inoutifc.request.put(Inputs_a{q2 : tpl_1(ff_mul_Out.first)});
//			rg_quire_busy <= 1'b1;
//			end

	endrule



	//----------------------------------------------------------------------------------------------------
	//rule after completion of addition/ subtraction and accepting the next from ff_mul_Out
	rule rl_quire_finish(rg_quire_busy == 1'b1);
		//if(tpl_2(ff_mul_Out.first) == FMA_P || tpl_2(ff_mul_Out.first) == FMS_P)
		begin
			let addOut <- adder.inoutifc.response.get();
			rg_queue[0] <= rg_queue[0] - 1;
			rg_quire_busy <= 1'b0;
						
	$display("in rl_quire_finish",$time, " ",rg_quire);
//			Maybe#(FloatE) out_ffO = tagged Invalid;
//			ffO.enq(out_ffO);
		end
//		else if(tpl_2(ff_mul_Out.first) == FMS_P)	begin
//			let subOut <- subtracter.inoutifc.response.get();
//			rg_queue[0] <= rg_queue[0] - 1;
//			rg_quire_busy <= 1'b0;
//			Maybe#(FloatE) out_ffO = tagged Invalid;
//			ffO.enq(out_ffO);
//		end


	endrule
//----------------------------------------------------------------------------------------------------
	// checks if quire value is valid and then initiates qtop // enqueues opcode qtop // 
	rule rl_rdq((tpl_3(ffI.first) == RD_Q) && rg_quire_busy == 1'b0 && (rg_queue[1] == 5'b0));
		//let posit_req_1 = ffI.first();
		//let op = tpl_3(posit_req_1);
		//qtop.compute.request.put(?);
		//rg_quire_busy <= 1'b1;
		ffO.enq(rg_quire);
		ffI.deq;
		//opcode_qtop.enq(op);
		if (verbosity > 1)
                   $display ("%0d: %m: rl_rdq: ", cur_cycle);
	endrule
	//------------------------------------------------------------------------------------------------
/*
	//----------------------------------------------------------------------------------------------------
	// checks if quire value is valid and then initiates qtop // enqueues opcode qtop // 
	rule rl_rdq((tpl_3(ffI.first) == RD_Q) && rg_quire_busy == 1'b0 && (rg_queue[1] == 5'b0));
		let posit_req_1 = ffI.first();
		let op = tpl_3(posit_req_1);
		qtop.compute.request.put(?);
		rg_quire_busy <= 1'b1;
		ffI.deq;
		opcode_qtop.enq(op);
		if (verbosity > 1)
                   $display ("%0d: %m: rl_rdq: ", cur_cycle);
	endrule
	//------------------------------------------------------------------------------------------------
	// qtop --> norm //enq opcode_qtop_norm // deq opcode_qtop //
	rule rl_qtop(opcode_qtop.first == RD_Q);
		let out_pf <- qtop.compute.response.get();
`ifdef NORM_EXT
		normalizer1.inoutifc.request.put (out_pf);
		opcode_qtop_norm.enq(RD_Q);
`else 
		ptof.compute.request.put(Extracted_Posit{zero_infinity_flag : out_pf.zero_infinity_flag,
										 	sign : out_pf.sign,
											scale : unpack(out_pf.scale),		//mind this
										 	frac : out_pf.frac});
			
		opcode_ptof.enq(RD_Q);
		$display("in else ifdef");
`endif
		rg_quire_busy <= 1'b0;
		opcode_qtop.deq;

                if (verbosity > 1)
                   $display ("%0d: %m: rl_qtop: ", cur_cycle);
	endrule
    //------------------------------------------------------------------------------------------------------------
	// norm --> ext //enq opcode_ptof // deq opcode_qtop_norm //
`ifdef NORM_EXT
	rule rl_qtop_norm(opcode_qtop_norm.first == RD_Q);
		let out_pf_norm <- normalizer1.inoutifc.response.get();
		let out_p3 = out_pf_norm.out_posit;
		extracter1.inoutifc.request.put (Input_posit{posit_inp : out_p3});
		opcode_qtop_norm.deq;
		opcode_qtop_ext.enq(RD_Q);
                if (verbosity > 1)
                   $display ("%0d: %m: rl_qtop_norm: ", cur_cycle);
	endrule

	//--------------------------------------------------------------------------------------------------
	// ext --> ptof //enq opcode_qtop_norm // deq opcode_qtop //
	rule rl_qtop_out(opcode_qtop_ext.first == RD_Q);
		let extOut3 <- extracter1.inoutifc.response.get();
		ptof.compute.request.put(extOut3);
		opcode_qtop_ext.deq;
		opcode_ptof.enq(RD_Q);
		$display("In rule qtop out NORM_EXT");
	endrule
`endif
	//--------------------------------------------------------------------------------------------------
	// Float ouput is sent to ffO to respond to the core
 	rule rl_ptof_out(opcode_ptof.first == RD_Q); 
		let excep = FloatingPoint::Exception{invalid_op : False, divide_0: False, overflow: False, underflow: False, inexact : False};
		let out_pf <- ptof.compute.response.get();
		FSingle fs = FSingle{sign : unpack(msb(out_pf.float_out)), 
								 exp : (out_pf.float_out[valueOf(FloatExpoBegin):valueOf(FloatFracWidth)]), 
								 sfd : truncate(out_pf.float_out) };
		FloatU out_float = tagged S fs;
		FloatE out_E = tuple2(out_float,excep);
		Maybe#(FloatE) out_ffO = tagged Valid out_E;
		Bit#(1) valid_bit = 1;
		ffO.enq(tuple2(out_ffO, valid_bit));
		opcode_ptof.deq;
                if (verbosity > 1)
                   $display ("%0d: %m: rl_ptof_out: ", cur_cycle);
	endrule
	//----------------------------------------------------------------------------------------------------
*/
		

interface server_core = toGPServer (ffI,ffO);

endmodule
endpackage: PositCoreCoproc_Sched            

