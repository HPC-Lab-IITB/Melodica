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

package PtoF_PNE_PC;

// -----------------------------------------------------------------
// This package defines:
//
//    mkPtoF_PNE: A posit to float converter
//
// -----------------------------------------------------------------
import FIFO                :: *;
import GetPut              :: *;
import ClientServer        :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import Extracter           :: *;
import Utils               :: *;

// --------
// Local type definitions
typedef struct {
   Bit#(1)                       sign;
   PositType                     ziflag;
   Int#(FloatExpWidth)           scale;
   Bit#(FloatFracWidth)          frac;
   Int#(LogFloatFracWidthPlus1)  frac_change;
   Bit#(1)                       frac_msb;
   Bit#(1)                       frac_zero;
} Stage0_pf deriving(Bits,FShow);


typedef struct {
   Bit#(FloatWidth) float_out;
   PositType ziflag;
   Bool rounding;
} Float_Extract deriving(Bits,FShow);


// --------
(* synthesize *)
module mkPtoF_PNE #(Bit #(2) verbosity) (Server #(Posit_Extract, Float_Extract));
   FIFO #(Float_Extract) ffO <- mkFIFO;

   // Extract float from the adjusted posit
   function Action fa_extract_float (Stage0_pf dIn);
      action
      //add hidden bit
      Bit#(FloatFracWidthPlus1) frac = {1, dIn.frac};

      // is the truncated fraction bits zero?
      // if frac change < 0 then frac bits lost but if >0 then frac is maximum since scale is already maximum  
      let truncated_frac_zero = (dIn.frac_change < 0) ? (~dIn.frac_msb & pack(unpack(frac[abs(dIn.frac_change):0]) ==  0))
                                                      : ((dIn.frac_change == 0) ? dIn.frac_zero
                                                                                : 1'b0);
      // the truncated bits msb
      // if frac change < 0 then frac bits lost but if >0 then frac is maximum since scale is already maximum 
      let truncated_frac_msb = (dIn.frac_change < 0) ? frac [abs(dIn.frac_change)+1]
                                                     : ((dIn.frac_change == 0) ? dIn.frac_msb
                                                                               : 1'b1);
      Int#(FloatExpWidthPlus1) scale_f =signExtend(dIn.scale);
      Int#(FloatExpWidthPlus1) floatBias_int = fromInteger(valueOf(FloatBias));

      // calculate exponent after adding bias
      Bit#(FloatExpWidth) scale_plus_bias = truncate(pack(scale_f+floatBias_int));

      // shift fraction depending on frac change
      Bit#(FloatFracWidth) frac_f = (dIn.frac_change < 0) ? truncate(frac>>abs(dIn.frac_change)+1)
                                                          : ((dIn.frac_change == 0) ? truncate(frac)
                                                                                    : '1);

      // concatenate sign, exponent and fraction bits
      Bit#(FloatWidth) float_no= {dIn.sign, scale_plus_bias, frac_f};

      // round the number depending on fraction bits lost
      Bit#(1) add_round = (~(truncated_frac_zero) | lsb(frac_f)) & (truncated_frac_msb);
      Bit#(FloatFracWidth) frac_zero = 0;

      // Zero-infinity special cases
      float_no = (dIn.ziflag == ZERO) ? 0 
                                      : (dIn.ziflag == INF) ? {'1,frac_zero}
                                                            : (float_no+extend(add_round)) ;

      if (verbosity > 1) begin
         $display ("%0d: %m: request.fa_extract_float: ", cur_cycle);
         $display ("   scale_f %b scale_plus_bias %b frac_f %b"
            , scale_f, scale_plus_bias, frac_f);
         $display ("   float_no %b add_round %b ", float_no, add_round);
         $display ("   truncated_frac_zero %b truncated_frac_msb %b lsbfrac_f %b"
            , truncated_frac_zero, truncated_frac_msb, lsb(frac_f));
      end

      let output_regf = Float_Extract {
         // Output floating point number
         float_out   : float_no,
         // Zero infinity flag
         ziflag      : (dIn.ziflag == REGULAR) ? ((float_no == 0) ? ZERO
                                                                  : ((float_no == {'1,frac_zero}) ? INF 
                                                                                                  : REGULAR))
                                               : dIn.ziflag,
         //rounnding bit
         rounding    : unpack(add_round)
      };
      ffO.enq(output_regf);
      endaction
   endfunction
   
   interface Put request;
      method Action put (Posit_Extract p);
         // Calc scale and fraction shifts due to restrictions on scale sizes
         // Look at Posit_Numeric_Types.N.ES.bsv for the auto-generated fn
         match {.scale, .frac_change} = fv_calculate_scale_shift_pf (p.scale);
         match {.frac, .frac_msb, .frac_zero} = fv_calculate_frac_pf (p.frac);
         let stage0_regf = Stage0_pf {
            sign : p.sign ,
            ziflag : p.ziflag ,
            scale : scale,
            frac_change : frac_change,
            frac : frac,
            frac_msb : frac_msb,
            frac_zero :frac_zero};
         fa_extract_float (stage0_regf);
         if (verbosity > 1) begin
            $display ("%0d: %m: request: ", cur_cycle);
            $display ("   Inputs: sign %b scale %b frac %b"
               ,p.sign, p.scale, p.frac);
            $display ("   Adjusted: frac %b scale %b frac_change %b"
               ,frac, scale, frac_change);
         end
      endmethod
   endinterface
   interface Get response = toGet (ffO);
endmodule

endpackage
