package Testbench;

import GetPut       :: *;
import ClientServer :: *;

import Quills ::*;
import Posit_Numeric_Types :: *;
import FloatingPoint :: *;
import Utils  :: *;

(* synthesize *)
module mkTestbench (Empty);

	Scheduler_IFC scheduler <- mkSched;
	Reg#(int) count <- mkReg(0);

	rule instr(count < 1);
		Instruction inst1 = tuple7(MM_P, 16'h0001,
					 32'h00000010,	//columns in A
					 32'h00000010,	//Rows in A
					 16'h0001,
					 32'h00000010, //columns in B
					 32'h00000010//Rows In B
						);

		scheduler.server_sched.request.put(inst1);
		count <= count + 1;
	$display("rule Insr",$time);		
	endrule
/*
	rule instr_1(count == 5);
		Instruction inst2 = tuple7(RD_Q_P, 16'h0001,
					 128'h00000000000000000000000000000008,	//columns in A
					 128'h00000000000000000000000000000001,	//Rows in A
					 16'h0001,
					 128'h00000000000000000000000000000001, //columns in B
					 128'h00000000000000000000000000000008	//Rows In B
						);	
		scheduler.server_sched.request.put(inst2);
		count <= count + 1;		

	endrule*/


endmodule

endpackage
