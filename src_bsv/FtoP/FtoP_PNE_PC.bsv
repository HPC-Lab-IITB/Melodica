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

package FtoP_PNE_PC;

// -----------------------------------------------------------------
// This package defines:
//
//    mkFtoP_PNE: A float to posit converter
// -----------------------------------------------------------------

import FIFO                :: *;
import GetPut              :: *;
import ClientServer        :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import Normalizer          :: *;

function Bool fv_check_nan (Bit#(FloatExpWidth) exp, Bit#(FloatFracWidth) frac);
   return ((exp == '1) && (frac != 0));
endfunction

function PositType fv_check_ziflag (
     Bit#(FloatExpWidth) exp
   , Bit#(FloatFracWidth) frac
);
   if ((exp == '1) && (frac == 0)) return INF;
   else if ((exp == 0) && (frac == 0)) return ZERO;
   else return REGULAR;
endfunction

(* synthesize *)
module mkFtoP_PNE #(Bit #(2) verbosity) (Server #(Bit#(FloatWidth), Prenorm_Posit));

   FIFO #(Prenorm_Posit) ffO <- mkFIFO1;

   interface Put request;
      method Action put (Bit#(FloatWidth) p);
         // Extract sign, exponent and fraction bits
         Bit #(FloatExpWidth) expo_f_unsigned = (
            p [valueOf (FloatExpoBegin) : valueOf (FloatFracWidth)]);
         Int #(FloatExpWidthPlus1) expo_f = unpack({0, expo_f_unsigned});
         Bit #(FloatFracWidth) frac_f = truncate (p);
         Bit #(1) sign_f = msb(p);

         // Subtract bias from scale
         Int #(FloatExpWidthPlus1) floatBias_int = fromInteger (valueOf (FloatBias));
         Int #(FloatExpWidth) expo_minus_floatBias = truncate(expo_f-floatBias_int);

         // Shift scale and fraction
         match{.scale, .frac_change} = fv_calculate_scale_shift_fp (expo_minus_floatBias);
         match{.frac_fp, .frac_msb, .frac_zero} = fv_calculate_frac_fp (frac_f); 

         // Introduce the hidden bit to the fraction
         Bit #(FracWidthPlus1) frac = {1, frac_fp}; 

         // Is the truncated fraction zero
         //    (frac_change < 0) : fraction bits lost
         //    (frac_change > 0) : fraction is maximum as scale is maximum
         let is_frac_zero = (frac_change < 0) ? pack (unpack (frac[abs(frac_change):0]) == 0)
                                              : ((frac_change == 0) ? 1'b1
                                                                    : 1'b0);               

         Bit #(FracWidth) pn_frac = (frac_change < 0) ? truncate (frac >> abs(frac_change) + 1)
                                                      : ((frac_change == 0) ? truncate(frac)
                                                                            : '1);

         let pn_frac_msb = (frac_change < 0) ? frac [abs(frac_change)+1]
                                             : ((frac_change == 0) ? frac_msb
                                                                   : 1'b1);
         let pn_frac_zero = (~frac_msb & frac_zero & is_frac_zero);

         // Zero-Infinity special cases
         let ziflag  = fv_check_ziflag (expo_f_unsigned, frac_f);
         let nanflag = fv_check_nan    (expo_f_unsigned, frac_f);

         // Package up the pre-normalized posit
         ffO.enq (Prenorm_Posit {
              sign      : sign_f
            , zi        : ziflag
            , nan       : nanflag
            , scale     : pack (scale)
            , frac      : pn_frac
            , frac_msb  : pn_frac_msb
            , frac_zero : pn_frac_zero
         });
      endmethod
   endinterface
   interface Get response = toGet (ffO);
endmodule

endpackage
