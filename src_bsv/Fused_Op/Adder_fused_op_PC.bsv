// Copyright (c) HPC Lab, Department of Electrical Engineering, IIT Bombay
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

package Adder_fused_op_PC;

// --------------------------------------------------------------
// This package defines:
//
//    mkAdder: 2-stage adder which adds into the quire
//    PIPELINED: FIFOs sized for continuous pipeline operation
// --------------------------------------------------------------

// Library imports
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Multiplier_Types_fma ::*;
import Common_Fused_Op :: *;

// Intermediate stage type definition
typedef struct {Int#(QuireWidth) sum_calc;
		Bit#(1) q2_truncated_frac_zero;
		Bit#(1) q2_truncated_frac_notzero;
		PositType q1_zero_infinity_flag;
		PositType q2_zero_infinity_flag;
		Bit#(1) q2_nan_flag;} Stage0_a deriving(Bits,FShow);

(* synthesize *)
module mkAdder #(Bit #(2) verbosity) (Server #(Quire_Acc, Bit #(0)));
   Reg #(QuireWidth)                rg_quire          <- mkRegU;
   Reg #(Bool)                      rg_quire_busy     <- mkReg (False);

   FIFOF #(Stage0_a)                fifo_stage0_reg   <- mkFIFOF1;
   FIFOF #(Bit#(0))                 fifo_output_reg   <- mkFIFOF1;

   // --------
   // Pipeline stages
   // Pipe stage -- rounding and special cases
   rule rounding_special_cases;
      let dIn = fifo_stage0_reg.first;  fifo_stage0_reg.deq;
      Bit#(1) flag_truncated_frac = (lsb(dIn.sum_calc) & dIn.q2_truncated_frac_zero) | dIn.q2_truncated_frac_notzero;
      let sign0 = msb(dIn.sum_calc);
      Bit#(2) truncated_frac = (flag_truncated_frac == 1'b0) ? 2'b00
                                                             : {sign0,flag_truncated_frac};
      Int#(QuireWidth) sum_calc = boundedPlus(dIn.sum_calc,signExtend(unpack(truncated_frac)));
      Bit#(QuireWidthMinus1) sum_calc_unsigned = truncate(pack(sum_calc));
      Bit#(1) all_bits_0 = ~reduceOr(sum_calc_unsigned);

      PositType zero_infinity_flag0 =   (((all_bits_0 & ~sign0) == 1'b1)
                                      && (dIn.q1_zero_infinity_flag == REGULAR)
                                      && (dIn.q2_zero_infinity_flag == REGULAR)) ? ZERO : REGULAR;
      let d = Quire_Fields {
         sign : sign0,
         //taking care of corner cases for nan flag 
         nan_flag : all_bits_0 & sign0 | dIn.q2_nan_flag | pack(dIn.q1_zero_infinity_flag == INF || dIn.q2_zero_infinity_flag == INF),
         //also include the case when fraction bit msb = 0
         zero_infinity_flag : zero_infinity_flag0,
         carry_int_frac : sum_calc_unsigned };
      fifo_output_reg.enq(?);

      if (d.nan_flag == 1'b1) rg_quire <= {1'b1,'0};
      else if(d.zero_infinity_flag == ZERO) rg_quire <= '0;
      else rg_quire <= {d.sign,d.carry_int_frac};
      rg_quire_busy <= False;
   endrule

   interface Put request;
      method Action put (Quire_Acc p) if (!rg_quire_busy);
         let dIn = p;

         // Quire operations cannot be pipeleined as there is a WAR dependency
         rg_quire_busy <= True;

         // signed sum of the values since the numbers are integer.fractions
         Int#(QuireWidth) sum_calc = boundedPlus(unpack(rg_quire),dIn.quire_md);

         // check for special cases
         let stage0_regf = Stage0_a {
            sum_calc : sum_calc,
            q2_truncated_frac_zero : dIn.truncated_frac_msb & dIn.truncated_frac_zero,
            q2_truncated_frac_notzero : dIn.truncated_frac_msb & ~(dIn.truncated_frac_zero),
            q1_zero_infinity_flag : rg_quire == '0 ? ZERO : REGULAR,
            q2_zero_infinity_flag : dIn.ziflag,
            q2_nan_flag : dIn.nan_flag};

         fifo_stage0_reg.enq(stage0_regf);

         if (verbosity > 1) begin
            $display ("%0d: %m: request: ", cur_cycle);
            $display ("   dIn.q1.sign %b dIn.q1.carry_int_frac %b",dIn.q1.sign,dIn.q1.carry_int_frac);
            $display ("   dIn.quire_md %b",dIn.quire_md);
         end
      endmethod
   endinterface
   interface Get response = toGet (fifo_output_reg);
endmodule

endpackage : Adder_fused_op_PC
