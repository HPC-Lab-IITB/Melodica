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
// mkMultiplier: 2-stage posit multiplier
// --------------------------------------------------------------

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Common_Fused_Op :: *;

// Intermediate stage type definition
typedef struct {
   Bool nan_flag;
   PositType ziflag;
   Bit#(1) sign;
   Int#(ScaleWidthPlus2) scale;
   Bit#(FracWidthPlus1Mul2) frac;
} Stage0_m deriving(Bits,FShow);

(* synthesize *)
module mkMultiplier #(Bit #(2) verbosity) (Server #(Posit_Extract, Quire_Acc));
   // make a FIFO to store 
   FIFOF #(Quire_Acc)     fifo_output_reg <- mkFIFOF1;
   FIFOF #(Stage0_m )  fifo_stage0_reg <- mkFIFOF1;

   // Identify NaN cases
   function Bool fv_nan_mul (
      PositType z_i1, PositType z_i2, Bool nan1, Bool nan2 );
      // NaN flag = 1 when one input is infinity and other zero, or either inputs
      // are NaNs
      if (  (z_i1 == INF && z_i2 == ZERO)
         || (z_i2 == INF && z_i1 == ZERO)
         || (nan1 || nan2))
         return True;
      else return False;
   endfunction

   // Identify zero or infinity cases depending only on the flag value of inputs
   function PositType fv_zi_check (PositType z_i1, PositType z_i2);
      if (z_i1 == ZERO && z_i2 == ZERO)
         // Both inputs are zero then output is zero
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
   // stage_1: Prepare the quire input
   rule stage_1;
      let dIn = fifo_stage0_reg.first; fifo_stage0_reg.deq;

      // Get the carry-Int-Frac value from the scale and frac values
      match {  .int_frac0
             , .carry0
             , .truncated_frac_msb
             , .truncated_frac_zero} = calc_frac_int (  dIn.frac
                                                      , dIn.scale
                                                      , 1'b0
                                                      , 1'b1);
      // carry bit extended
      Bit #(CarryWidthQuire) carry = extend(carry0);

      // the Quire value is signed extend
      Bit #(QuireWidth) signed_carry_int_frac = {
           dIn.sign
         , ((dIn.sign == 1'b0) ? {carry, int_frac0}
                               : twos_complement ({carry, int_frac0}))};

      // taking care of corner cases for zero infinity flag
      let ziflag = (   (signed_carry_int_frac == 0)
                    && (dIn.ziflag == REGULAR)) ? ZERO : dIn.ziflag;

      let output_regf = Quire_Acc {
         nan_flag             : dIn.nan_flag,
         ziflag               : zi_flag,
         quire_md             : unpack(signed_carry_int_frac),                        
         truncated_frac_msb   : truncated_frac_msb,
         truncated_frac_zero  : truncated_frac_zero
      };

      if (verbosity > 1) begin
         $display ("%0d: %m: stage_1: ", cur_cycle);
         $display ("   int_frac0 %b carry0 %b", int_frac0,carry0);
         $display ("   signed_carry_int_frac %b", signed_carry_int_frac);
      end
      fifo_output_reg.enq(output_regf);
   endrule

   interface Put request;
      method Action put (Tuple2 #(Posit_Extract, Posit_Extract) extracted_posits);
         match {.ep1, .ep2} = extracted_posits;

         // Zero-Infinity Check
         let ziflag = fv_zi_check (
            ep1.ziflag, ep2.ziflag);

         // Hidden bits of the two fractions
         if      (ep1.ziflag == ZERO) zero_flag = 2'b01;
         else if (ep2.ziflag == ZERO) zero_flag = 2'b10;
         else                         zero_flag = 2'b11;

         // Scale calculation: sum the scales
         let scale0 = calculate_sum_scale (ep1.scale, ep2.scale);

         // Calculate product of fractions
         match {.sign0, .frac0} = product_frac (
              ep1.sign
            , ep2.sign
            , {zero_flag[1], ep1.frac}
            , {zero_flag[0], ep2.frac});

         // Next stage prepares the output
         let stage0_regf = Stage0_m {
              nan_flag : fv_nan_mul (
                 ep1.ziflag
               , ep2.ziflag
               , False
               , False)
            , ziflag : ziflag   // indicates if fraction msb is zero
            , sign : sign0
            , scale : scale0
            , frac : frac0
         };

         fifo_stage0_reg.enq(stage0_regf);

         if (verbosity > 1) begin
            $display ("%0d: %m: request: ", cur_cycle);
            $display ("   zero-infinity-flag %b",stage0_regf.ziflag);
            $display ("   sign0 %b",sign0);
            $display ("   scale0 %b frac0 %b",scale0,frac0);
         end
      endmethod
   endinterface
   interface Get response = toGet (fifo_output_reg);
endmodule

endpackage: Multiplier_fma
