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

package Divider_fda;

// --------------------------------------------------------------
// This package defines:
//
// mkDivider: 2-stage posit Divider that uses an iterative integer
//            divide algorithm.
// --------------------------------------------------------------

import FIFOF               :: *;
import GetPut              :: *;
import ClientServer        :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import IntDivide_generic   :: *;
import Fused_Commons       :: *;
import Extracter           :: *;
import Utils               :: *;

typedef struct {
   Bool                    nan_flag;
   PositType               ziflag;
   Bit #(1)                sign;
   Int #(ScaleWidthPlus2)  scale;
} Stage0_d deriving(Bits,FShow);

module mkDivider #(Bit #(2) verbosity) (
   Server #(Tuple2 #(Posit_Extract, Posit_Extract), Quire_Acc)
);
   FIFOF #(Quire_Acc)               fifo_output_reg   <- mkFIFOF1;
   FIFOF #(Stage0_d)                fifo_stage0_reg   <- mkFIFOF1;

   // Integer divider
   IntDivide_IFC intDivide <- mkIntDivide (verbosity);

   // Identify zero or infinity cases
   function PositType fv_zi_check (PositType z_i1, PositType z_i2);
      // Output ZERO: ZERO/num, num/INF
      if ((z_i1 == ZERO && z_i2 != ZERO) || (z_i1 != INF && z_i2 == INF))
         return ZERO;

      // Output INF: INF/num, num/ZERO
      else if ((z_i1 == INF && z_i2 != INF) || (z_i1 != ZERO && z_i2 == ZERO))
         return INF;

      else return REGULAR;
   endfunction
   
   // --------
   // Pipeline stages
   // Calculate fraction and generate final output
   rule stage_1;
      let dIn = fifo_stage0_reg.first;  fifo_stage0_reg.deq;

      // Output of integer divider
      match {.quotient, .frac_msb, .frac_zero} <- intDivide.response.get();

      // place an extra 0 infront of quotient because of the way the multiplier is designed
      match {.int_frac0, .carry0, .frac_msb0, .frac_zero0} = calc_frac_int (
         {1'b0, quotient}, dIn.scale, frac_msb, frac_zero);

      // carry bit extended
      Bit #(CarryWidthQuire) carry = extend(carry0);

      // the Quire value is sign-extended
      Bit #(QuireWidth) quire = (dIn.sign == 1'b0) ? {dIn.sign, carry, int_frac0}
                                                   : {  dIn.sign
                                                      , twos_complement ({carry, int_frac0})
                                                     };

      // taking care of corner cases for zero infinity flag
      PositType ziflag0 = ((quire == 0) && (dIn.ziflag == REGULAR)) ? ZERO
                                                                    : dIn.ziflag;

      // The output quire. Also include the case when fraction bit msb = 0
      let quire_in = Quire_Acc {
         nan         : dIn.nan_flag,
         zi          : ziflag0,
         quire       : unpack(quire),                        
         frac_msb    : frac_msb0,
         frac_zero   : frac_zero0
      };

      fifo_output_reg.enq (quire_in);

      if (verbosity > 1) begin
         $display ("%0d: %m: stage_1: ", cur_cycle);
         $display ("int_frac0 %b carry0 %h",int_frac0,carry0);
         $display ("quire %b",quire);
      end
   endrule

   // --------
   // Interface
   interface Put request;
      method Action put (Tuple2 #(Posit_Extract, Posit_Extract) extracted_posits);
         match {.ep1, .ep2} = extracted_posits;

         // Check for zero and infinity special cases
         let ziflag = fv_zi_check (ep1.ziflag, ep2.ziflag);

         // the hidden bit of the numerator and divisor fractions
         Bit #(2) zero_flag = 2'b11;
         if      (ep1.ziflag == ZERO) zero_flag = 2'b01;
         else if (ep2.ziflag == ZERO) zero_flag = 2'b10;

         // sum the scales (here actually a difference)
         let scale0 = calculate_sum_scale (ep1.scale, -ep2.scale);

         // divide the fractions (integer division)
         intDivide.request.put (tuple2 (  {zero_flag[1], ep1.frac}
                                        , {zero_flag[0], ep2.frac}));

         let stage0_regf = Stage0_d {
            // corner cases for nan flag 
            nan_flag : fv_nan_check (
               ep1.ziflag, ep2.ziflag, False, False),

            // also include the case when fraction bit msb = 0
            ziflag : ziflag,
            sign : (ep1.sign ^ ep2.sign),
            scale : scale0
         };

         fifo_stage0_reg.enq (stage0_regf);

         if (verbosity > 1) begin
            $display ("%0d: %m: request: ", cur_cycle);
            $display ("   zero-infinity-flag %b", stage0_regf.ziflag);
            $display ("   sign %b", stage0_regf.sign);
            $display ("   scale %h", stage0_regf.scale);
         end
     
      endmethod
   endinterface
   interface Get response = toGet (fifo_output_reg);
endmodule

endpackage: Divider_fda


