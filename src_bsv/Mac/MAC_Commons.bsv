package MAC_Commons;

// This package contains functions used by the Adder and the Multiplier logic in the MAC. If
// these functions are used more often by other pipelines, these functions should move to a
// Melodica-level Common pacakge.

import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;

//This function is used to identify zer or infinity cases depending only on the flag value of inputs
(* noinline *)
function PositType fv_check_for_z_i(PositType z_i1, PositType z_i2);
   if (z_i1 == ZERO && z_i2 == ZERO)
      // if both inputs are zero then output is zero
      return ZERO;
   else if (z_i1 == INF || z_i2 == INF)
      // if one of the inputs is infinity then output is infinity
      return INF;
   else
      return REGULAR;
endfunction

//This function checks if the scale value has exceeded the limits max and min set due to the
//restricted availability of regime bits fraction bits will be shifted to take care of the
//scale value change due to it being bounded
//output : bounded scale value and the shift in frac bits
(* noinline *)
function Tuple2#(Int#(ScaleWidthPlus1), Int#(LogFracWidthPlus1)) fv_calculate_scale_shift(Int#(ScaleWidthPlus2) scale);
   Int#(ScaleWidthPlus1) maxB,minB,scale0;
   Int#(LogFracWidthPlus1) frac_change;
   //max scale value is defined here... have to saturate the scale value 
   // max value = (N-2)*(2^es) 
   // scale = regime*(2^es) + expo.... max value of regime = N-2(00...1)
   Bit #(ScaleWidthPlus1) max_scale = fromInteger (valueOf (PositWidth) - 2);
   max_scale = max_scale << fromInteger (valueOf (ExpWidth));

   maxB = unpack (max_scale);
   minB = -maxB;
   // maxB = fromInteger((valueOf(PositWidth) -2)*(2**(valueOf(ExpWidth))));
   //similarly calculate the min 
   // minB = -maxB;
   //frac_change gives the number of bits that are more or less than scale bounds so that we can shift the frac bits to not lose scale information 
   if (scale < signExtend(minB)) begin
      frac_change = truncate(boundedMinus(scale,signExtend(minB)));// find the change in scale to bind it 
      scale0 = minB;//bound scale
   end
   else if (scale> signExtend(maxB)) begin
      frac_change = truncate(boundedMinus(scale,signExtend(maxB)));// find the change in scale to bind it 
      scale0 = maxB;//bound scale
   end
   else begin
      frac_change = fromInteger(0);
      scale0 = truncate(scale);//no change
   end
return tuple2(scale0,frac_change);

endfunction
endpackage
