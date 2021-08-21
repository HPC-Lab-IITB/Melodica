// Copyright (c) Department of Electrical Engineering, IIT Bombay
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

package Multiplier_fma;

// --------------------------------------------------------------
// This package defines:
//
// mkMultiplier: 2-stage pipelined posit multiplier
// --------------------------------------------------------------

import FIFOF               :: *;
import GetPut              :: *;
import ClientServer        :: *;
import FShow               :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import Fused_Commons       :: *;
import Extracter           :: *;
import Utils               :: *;

// Intermediate stage type definition
typedef struct {
   Bool nan;
   PositType zi;
   Bit#(1) sign;
   Int#(ScaleWidthPlus2) scale;
   Bit#(FracWidthPlus1Mul2) frac;
} Mul_Stg1_In deriving(Bits, FShow);

(* synthesize *)
module mkMultiplier #(Bit #(2) verbosity) (
   Server #(Tuple2 #(Posit_Extract, Posit_Extract), Quire_Acc)
);

   // make a FIFO to store 
   FIFOF #(Quire_Acc)   ff_to_quire <- mkFIFOF1;
   FIFOF #(Mul_Stg1_In) ff_pipe_reg <- mkFIFOF1;

   // Identify zero or infinity cases depending only on the flag value of inputs
   function PositType fv_zi_check (PositType z_i1, PositType z_i2);
      if (z_i1 == ZERO || z_i2 == ZERO)
         // Either inputs are zero then output is zero
         return ZERO;
      else if (z_i1 == INF || z_i2 == INF)
         // one of the inputs is infinity then output is infinity
         return INF;
      else return REGULAR;
   endfunction

   // Calculate product of the fraction bits
   function Tuple2 #(Bit #(1), Bit #(FracWidthPlus1Mul2)) product_frac (
        Bit #(1) sgn1
      , Bit #(1) sgn2
      , Bit #(FracWidthPlus1) f1
      , Bit #(FracWidthPlus1) f2
   );
      // the product of the two fractions
      // size = sum of sizes of input fractions (+2 for the hidden bits)
      Bit #(FracWidthPlus1Mul2) frac_product;
                
      frac_product = extend(f1) * extend(f2);

      // Generate the sign -- exor of the two signs
      // if any of the input numbers are 0 then the msb of the fraction will be 0
      // In this case, the sign of number will be 0 as the product is 0
      return tuple2 (((sgn1 ^ sgn2) & msb (f1) & msb (f2)), frac_product); 
   endfunction
        
   // --------
   // Pipeline stages
   // stage_1: Prepare the input to the quire
   rule stage_1;
      let dIn = ff_pipe_reg.first; ff_pipe_reg.deq;

      // Get the carry-Int-Frac value from the scale and frac values
      match {  .qif
             , .lead_one
             , .truncated_frac_msb
             , .truncated_frac_zero} = fn_calc_frac_int_mul (  dIn.frac
                                                      , dIn.scale
                                                      , 1'b0
                                                      , 1'b1);

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
                                            : twos_complement (qif)};
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

         // Zero-Infinity Check
         let ziflag = fv_zi_check (
            ep1.ziflag, ep2.ziflag);

         // Hidden bits of the two fractions
         Bit #(2) zero_flag = 2'b11;
         if      (ep1.ziflag == ZERO) zero_flag = 2'b01;
         else if (ep2.ziflag == ZERO) zero_flag = 2'b10;

         // Scale calculation: sum the scales
         let scale0 = calculate_sum_scale (ep1.scale, ep2.scale);

         // Calculate product of fractions
         match {.sign0, .frac0} = product_frac (
              ep1.sign
            , ep2.sign
            , {zero_flag[1], ep1.frac}
            , {zero_flag[0], ep2.frac});

         // Next stage prepares the output
         let stage0_regf = Mul_Stg1_In {
              nan : fv_nan_check_mul (
                 ep1.ziflag
               , ep2.ziflag
               , False
               , False)
            , zi : ziflag   // indicates if fraction msb is zero
            , sign : sign0
            , scale : scale0
            , frac : frac0
         };

         ff_pipe_reg.enq(stage0_regf);

         if (verbosity > 1) begin
            $display ("%0d: %m.request: ", cur_cycle);
            if (verbosity > 2) 
               $display ("   ", fshow (stage0_regf));
         end
      endmethod
   endinterface
   interface Get response = toGet (ff_to_quire);
endmodule

endpackage: Multiplier_fma
