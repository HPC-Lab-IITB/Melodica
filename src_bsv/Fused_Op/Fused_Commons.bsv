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

package Fused_Commons;

// --------------------------------------------------------------
// This package defines:
//
// Common functions and definitions used by fused operators
// --------------------------------------------------------------

import BUtils :: *;
import Posit_User_Types :: *;
import Posit_Numeric_Types :: *;

typedef struct {Bit#(1) sign1;
                Bit#(1) nanflag1;
                PositType zero_infinity_flag1;
                Int#(ScaleWidthPlus1 ) scale1;
                Bit#(FracWidth ) frac1;
                Bit#(1) sign2;
                Bit#(1) nanflag2;
                PositType zero_infinity_flag2;
                Int#(ScaleWidthPlus1 ) scale2;
                Bit#(FracWidth ) frac2;} Inputs_md deriving(Bits,FShow);
//Input_posit is the data received from user
//Input_posit consists of zero flag, infinity flag, sign of posit, scale , fraction for 2 inputs

// Meta information about Quire
typedef struct {
   Bool        nan;
   PositType   zi;
   Bit #(LogQuireWidth) lead_one;
} Quire_Meta deriving (Bits, FShow);

instance DefaultValue #(Quire_Meta);
   defaultValue = Quire_Meta {
      nan   : False,
      zi    : ZERO,
      lead_one : 0
   };
endinstance
	
// Input for the accumulate operation in the quire
typedef struct {
   Int #(QuireWidth) quire;
   Quire_Meta        meta;
   Bit #(1)          frac_msb;
   Bit #(1)          frac_zero;
} Quire_Acc deriving(Bits,FShow);

//Output_posit is the data available at the end of second pipeline
//Output_posit consists of zero flag, infinity flag, sign of posit, scale value, fraction value

        //This function finds the sum of the scale bits since the scale value has 2^scale contribution in the product
        function Int#(ScaleWidthPlus2) calculate_sum_scale(Int#(ScaleWidthPlus1 ) s1,Int#(ScaleWidthPlus1)s2);
                        Int#(ScaleWidthPlus2) scale;
                        //Scale is calculated as the sum of the respective scale
                        scale = signExtend(s1)+signExtend(s2);
                        return scale;
        endfunction

// Get the carry-Int-Frac value from the scale and frac values
function Tuple4 #(
     Bit #(QuireWidthMinus1)
   , Bit #(LogQuireWidth)
   , Bit #(1)
   , Bit #(1)) calc_frac_int_alt (Bit #(FracWidthPlus1Mul2) f
                              , Int #(ScaleWidthPlus2) s
                              , Bit#(1) truncated_frac_msb_in
                              , Bit#(1) truncated_frac_zero_in
                             );

   // Incoming frac: iifffffff....
   // Take the incoming frac with the first two bits of int and place it msb first
   // into the frac part of the quire.
   // Proviso: sizeOf(FracWidthQ) > sizeOf (FracWidthPlus1Mul2)
   Bit #(FracWidthQ) q_f = zExtendLSB(f);
   Bit #(IntWidthQPlusFracWidthQ) q_if = extend (q_f);

   // The << converts the q_if into i.ffffff form from .iiffffff. The radix point
   // being the separator between i and f in q_if. However, the actual input was
   // ii.fffff... so this shift actually means that the scale has to increment by
   // 1 (despite the <<) 
   q_if = q_if << 1;

   // Increment the scale to account for alignment of the integer bit
   s = s + 1;

   // This is the number of zeros in cif before the leading one. For the scale = 0
   // case, it is carry-width + int-width - 1 (for the 1.xxxxx case)
   Bit #(LogQuireWidth) leading_one = fromInteger (
      quire_carry_width + quire_int_width - 1);

   // Positive scale. Shift radix point to the right or q_if to the left
   if (s >= 0) begin // strictly > is sufficient, but >= infers simpler logic
      Bit #(IntWidthQPlusFracWidthQ) shftamt = extend (pack (s));
      q_if = q_if << shftamt;
      leading_one = leading_one - extend (pack (s));
   end 

   else begin
      s = abs(s);
      Bit #(IntWidthQPlusFracWidthQ) shftamt = extend (pack (s));
      q_if = q_if >> shftamt;
      leading_one = leading_one + extend (pack (s));
   end

   // Include the carry, which is all zeros
   Bit #(QuireWidthMinus1) q_cif = zeroExtend (q_if);
   Bit #(1) truncated_frac_msb = truncated_frac_msb_in;
   Bit #(1) truncated_frac_zero = ~truncated_frac_msb_in & truncated_frac_zero_in;

   return tuple4(q_cif, leading_one, truncated_frac_msb, truncated_frac_zero);
 endfunction


// Get the carry-Int-Frac value from the scale and frac values
function Tuple4 #(
     Bit #(IntWidthQPlusFracWidthQ)
   , Bit #(CarryWidthQ)
   , Bit #(1)
   , Bit #(1)) calc_frac_int (  Bit #(t) f
                              , Int #(ScaleWidthPlus2) s
                              , Bit#(1) truncated_frac_msb_in
                              , Bit#(1) truncated_frac_zero_in
                             ) provisos (  Add #(a__, t, TAdd #(FracWidthQ,2))
                                         , Add #(b__, CarryWidthQ, t)
                                         , Add #(c__, t, IntWidthQPlusFracWidthQ)
                                        );
   let frac_width = valueOf (t) - 2;
   Bit #(IntWidthQPlusFracWidthQ) f_new = extend(f);

   // First two bits of fraction are integer bits. If scale = 0 we have to shift
   // fract left by FWQ-(FW*2 or (no_of_frac_bits_input - 2))

   // frac_shift = FWQ-(FW*2 or (no_of_frac_bits_input - 2)) + scale(signed sum)
   // if input scale is negative beyond and extent s.t fracshift < 0
   Int #(TAdd #(LogCarryWidthPlusIntWidthPlusFracWidthQ,1)) scale_neg_temp = abs(signExtend(s)) - fromInteger(valueOf(FracWidthQ));//scale_neg_temp = abs(s)-FWQ
   Int #(LogCarryWidthPlusIntWidthPlusFracWidthQ) scale_neg = truncate(scale_neg_temp + fromInteger(frac_width));//frac_shift = scale_neg = abs(s) - (FWQ-(FW*2 or (no_of_frac_bits_input - 2)))
   // if input scale is negative beyond and extent s.t fracshift > 0
   Int #(TAdd#(LogCarryWidthPlusIntWidthPlusFracWidthQ,1)) scale_pos = signExtend(s) + fromInteger(valueOf(FracWidthQ)-frac_width);// frac_shift = scale_pos = s + FWQ-(FW*2 or (no_of_frac_bits_input - 2))
   Bit #(1) truncated_frac_msb = truncated_frac_msb_in;
   Bit #(1) truncated_frac_zero = ~truncated_frac_msb_in & truncated_frac_zero_in;
   Bit #(CarryWidthQ) carry = '0;

   if(msb(s) == 1'b1 && scale_neg>0) begin
      f_new = f_new>>scale_neg;// if frac_shift < -(FWQ-(FW*2 or (no_of_frac_bits_input - 2))) the scale will be shifted right and we will lose frac bits since the maximum available shift = FWQ-(FW*2 or (no_of_frac_bits_input - 2))
      truncated_frac_msb = scale_neg>0 ? f[scale_neg-1] : 1'b0;//in the truncated bits see the msb
      Bit#(IntWidthQPlusFracWidthQ) mask1 = ~('1>>scale_neg-1);
      truncated_frac_zero = scale_neg>1 ? ((extend(f) & mask1) == 0 ? 1'b1 : 1'b0) :1'b1;////in the truncated bits see the leftover bits other than msb
   end else begin
      f_new = f_new<<scale_pos;// right shift to accomodate the scale
      if(scale_neg_temp+2>0)
         //carry = extend(f[valueOf(FracWidthMul2Plus1):valueOf(FracWidthMul2)]);
         carry = truncate(f>>(fromInteger(frac_width)-scale_neg_temp));
         // nuw we can have over flow from the integer bits if the scale is large
         //total shift = S_pos, carry starts at SWQ+FWQ, so spos>SWQ+FWQ gives condition for carry
      end        
   return tuple4(f_new,carry,truncated_frac_msb,truncated_frac_zero);
 endfunction

   // This function is used to identify nan cases for mul
   function Bool fv_nan_check_mul (
      PositType z_i1, PositType z_i2, Bool nan1, Bool nan2
   );
      // Output NaN:
      // MUL: INF*0, 0*INF, or either NaN
      if (   (z_i1 == INF && z_i2 == ZERO)
          || (z_i2 == INF && z_i1 == ZERO)
          || (nan1 || nan2)) return True;

      else return False;
   endfunction

   // This function is used to identify nan cases for divide
   function Bool fv_nan_check_div (
      PositType z_i1, PositType z_i2, Bool nan1, Bool nan2
   );
      // Output NaN:
      // DIV: INF/INF, INF/ZERO, or either NaN
      if (   (z_i1 == INF && z_i2 == ZERO)
          || (z_i2 == INF && z_i1 == INF)
          || (nan1 || nan2)) return True;

      else return False;
   endfunction

   function Action fa_print_quire (Bit #(QuireWidth) qval);
      action
      let q = qval;
      let sign = msb (q);
      Bit #(FracWidthQ) qfrac = qval[(quire_frac_width-1):0];
      qval = qval >> fromInteger (quire_frac_width);
      Bit #(IntWidthQ) qint  = qval[(quire_int_width-1):0];
      qval = qval >> fromInteger (quire_int_width);
      Bit #(CarryWidthQ) qcarry = qval[(quire_carry_width-1):0];

      $display ("   Quire fields:");
      $display ("      sign : %b", sign);
      $display ("      carry: %0h", qcarry);
      $display ("      int: %0h", qint);
      $display ("      frac: %0h", qfrac);
      endaction
   endfunction

endpackage
