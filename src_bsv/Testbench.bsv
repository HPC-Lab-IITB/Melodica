package Testbench;

import GetPut       :: *;
import ClientServer :: *;

import PositCore_v2 ::*;
import Posit_Numeric_Types :: *;
import FloatingPoint :: *;
import Utils  :: *;

(* synthesize *)
module mkTestbench (Empty);
   PositCore_IFC pc <- mkPositCore;
   Reg #(Bit #(3)) rg_state <- mkReg (0);
   Bit #(3) num_fmas = 3;
   Reg #(Bit #(3)) rg_rsp <- mkReg (0);
   rule rl_nb_req (rg_state < num_fmas);
      FloatingPoint::RoundMode round_mode = Rnd_Nearest_Even;
      PositCmds opcodes = FMA_P;
      //FSingle f1 = fromReal(2.5);
      //FSingle f2 = fromReal(4);
      PositCore_v2::FloatU in1 = tagged P 32'h60006000;
      PositCore_v2::FloatU in2 = tagged P 32'h54005400;
      let inp_posit = tuple4(in1,in2,round_mode,opcodes);
      pc.server_core.request.put (inp_posit);
      rg_state <= rg_state + 1;
      $display("%0d: %m.rl_nb_req ", cur_cycle);
      $display("   in1 %h in2 %h opcode %b"
         , tpl_1(inp_posit).P, tpl_2(inp_posit).P, tpl_4(inp_posit));
      endrule

   rule rl_read_out (rg_state == num_fmas);
      PositCmds opcodes = FCVT_P_R;
      FloatingPoint::RoundMode round_mode = Rnd_Nearest_Even;
      Bit #(32) dummy = ?;
      PositCore_v2::FloatU in1 = tagged P dummy; 
      PositCore_v2::FloatU in2 = tagged P dummy; 
      let inp_posit = tuple4(in1,in2,round_mode,opcodes);
      pc.server_core.request.put (inp_posit);
      rg_state <= rg_state + 1;
      $display("%0d: %m.rl_read_out: ", cur_cycle);
      $display("   in1 %h in2 %h opcode %b"
         , tpl_1(inp_posit).P, tpl_2(inp_posit).P, tpl_4(inp_posit));
   endrule

   rule rl_rsp;
      let z <- pc.server_core.response.get ();
      rg_rsp <= rg_rsp + 1;
      $display("%0d: %m.rl_rsp: out %h exception %b",cur_cycle,tpl_1(z).P,tpl_2(z));

      if (rg_rsp == num_fmas) $finish;  // this is the second and last response
   endrule

endmodule

endpackage
