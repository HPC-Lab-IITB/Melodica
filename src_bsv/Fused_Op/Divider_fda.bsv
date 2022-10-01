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
//            divide algorithm. Non-pipelined.
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

// Intermediate stage type definition
typedef struct {
   Bool                    nan;
   PositType               zi;
   Bit #(1)                sign;
   Int #(ScaleWidthPlus2)  scale;
} Div_Stg1_In deriving(Bits,FShow);

(* synthesize *)
module mkDivider #(Bit #(2) verbosity) (
   Server #(Tuple2 #(Posit_Extract, Posit_Extract), Quire_Acc)
);
   FIFOF #(Quire_Acc)               ff_to_quire   <- mkFIFOF1;
   FIFOF #(Div_Stg1_In)             ff_pipe_reg   <- mkFIFOF1;

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
   // Output of integer divider (fraction) and prepare the input to the quire
   rule stage_1;
      let dIn = ff_pipe_reg.first;  ff_pipe_reg.deq;

      // Output of integer divider
      match {.quotient, .frac_msb, .frac_zero} <- intDivide.response.get();

      // Get the carry-Int-Frac value from the scale and frac values
      // Place an extra 0 infront of quotient because of the way the
      // multiplier is designed
      match {  .qif
             , .lead_one
             , .truncated_frac_msb
             , .truncated_frac_zero} = fn_calc_frac_int_mul ( {1'b0, quotient}
                                                            , dIn.scale
                                                            , frac_msb
                                                            , frac_zero);

      // the value to be sent for accumulation has zero carry. So, it is
      // sufficient to convert qif to signed form. The zero carry can be added in
      // at the accumulator

      // consider adding trailing zeros, together they will fix the non-zero
      // bits in the qif. If we can do it right, the quire addition can be reduced
      // to three 32-bit adds
      let meta = Quire_Meta {
           nan         : dIn.nan
         , zi          : dIn.zi
      };

      let quire_in = Quire_Acc {
           sign        : dIn.sign
         , qif         : qif
         , meta        : meta
         , frac_msb    : truncated_frac_msb
         , frac_zero   : truncated_frac_zero
      };

      ff_to_quire.enq (quire_in);

      if (verbosity > 1) begin
         $display ("%0d: %m.stage_1: ", cur_cycle);
         if (verbosity > 2) begin
            $display ("   frac_msb  : %0b", quire_in.frac_msb);
            $display ("   frac_zero : %0b", quire_in.frac_zero);
            $display ("   meta      : ", fshow (meta));

            // Before printing quire add the carry by sign-extending if
            Bit #(SIntFracWidthQ) s_if = {
               dIn.sign, (dIn.sign == 1'b0) ? qif
                                            :twos_complement (qif)};
            Int #(QuireWidth) s_cif = signExtend (unpack (s_if));
            fa_print_quire (pack (s_cif));
         end
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

         let stage0_regf = Div_Stg1_In {
            // corner cases for nan flag 
            nan : fv_nan_check_div (
               ep1.ziflag, ep2.ziflag, False, False),

            // also include the case when fraction bit msb = 0
            zi   : ziflag,
            sign : (ep1.sign ^ ep2.sign),
            scale: scale0
         };

         ff_pipe_reg.enq (stage0_regf);

         if (verbosity > 1) begin
            $display ("%0d: %m.request: ", cur_cycle);
            if (verbosity > 2) 
               $display ("   ", fshow (stage0_regf));
         end
      endmethod
   endinterface
   interface Get response = toGet (ff_to_quire);
endmodule

endpackage: Divider_fda


