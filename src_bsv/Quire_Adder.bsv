// Library imports
//fifo input, funnel first stage and add into accumulaor
package Quire_Adder;

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Vector::*;
import PAClib :: * ;

import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Quire	::*;
import Scheduler_types	::*;
import Fused_Commons ::*;
import Normalizer          :: *;
import Extracter     :: *;

interface Quire_Adder_IFC ;
  interface Put #(Vector#(N_melodica, Quire_Acc)) add;    // add all the quires
  method Action read_req_adder;                   // start quire read
  interface Get#(Bit#(PositWidth)) read_posit_adder;  // quire read response
  method Quire_Acc write_quire;		//write quire into TCM (allocated space)
  method Action clear; 	//clears Quire_accumulator
endinterface


function Pipe #(Vector#(N_melodica, Quire_Acc), Vector#(1, Quire_Acc)) mkQFnl();
//	let qfunnel = mkFunnel();
//	return qfunnel;
	return mkFunnel();
endfunction

typedef enum {START,REQ, RESP
} State_req deriving (Bits,Eq, FShow);

module [Module] mkQuire_funnel (Server#(Vector#(N_melodica, Quire_Acc), Vector#(1, Quire_Acc)));	//pipe converted into module with server ifc
	let s <- mkPipe_to_Server(mkQFnl);
	return s;
endmodule

(* synthesize *)
module mkQuire_Adder (Quire_Adder_IFC);
	Bit #(2) verbosity = 3;  
	Reg#(Bit#(TAdd#(TLog#(N_melodica),1))) i <- mkRegU;	//TLog
	Reg #(Bool)                      rg_adder_busy     <- mkReg (False);
	Reg #(State_req)                      rg_state     <- mkReg (START);
	Reg #(Bit#(PositWidth))                      rg_final_posit     <- mkRegU;
	FIFOF #(Vector#(N_melodica, Quire_Acc))  fifo_input_quire <- mkFIFOF1;
	Server#(Vector#(N_melodica, Quire_Acc), Vector#(1, Quire_Acc))  quire_stage          <- mkQuire_funnel;
	Quire_IFC                           quire_accumulator          <- mkQuire (verbosity);
	Server #(Prenorm_Posit, Norm_Posit) normalizer     <- mkNormalizer (verbosity);


	rule add_quire (rg_adder_busy) ;
		i <= i - 1;
		Vector#(1, Quire_Acc) z <- quire_stage.response.get;
		quire_accumulator.accumulate.put(z[0]);
		if (i == 1) rg_adder_busy <= False;		
	endrule

	rule read_posit (rg_state == REQ);
		 let o <- quire_accumulator.read_rsp.get();
		 normalizer.request.put (o); 
		 rg_state <= RESP;	
	endrule

	rule get_posit (rg_state == RESP);
		let out_pf <- normalizer.response.get ();
		rg_final_posit <= out_pf.posit;
	endrule     
		

  interface Put add;
      method Action put (Vector#(N_melodica, Quire_Acc) q_vec) if (!rg_adder_busy);
        rg_adder_busy <= True;
		quire_stage.request.put(q_vec);
		i <= fromInteger(valueOf(N_melodica));
      endmethod
   endinterface

	
  method Action read_req_adder if(!rg_adder_busy) ;
		quire_accumulator.read_req;
		rg_state <= REQ;
  endmethod

  interface Get read_posit_adder = toGet (rg_final_posit);

  method Quire_Acc write_quire if(!rg_adder_busy) ;
		let q = quire_accumulator.read_quire;
		return q;
  endmethod 

  method Action clear if(!rg_adder_busy) ;
	  let p = Posit_Extract {ziflag : ZERO};
      quire_accumulator.init.put(p);
  endmethod
		

endmodule

endpackage
