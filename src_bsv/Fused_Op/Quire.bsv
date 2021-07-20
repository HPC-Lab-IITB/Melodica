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

package Quire;

// --------------------------------------------------------------
// This package defines:
//
//    mkQuire: Implements the quire register:
//             addition, initialization and read-out
// --------------------------------------------------------------

// Library imports
import FIFO                :: *;
import GetPut              :: *;
import FShow               :: *;
import DefaultValue        :: *;
import BUtils              :: *; // for zExtendLSB and friends

import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import Fused_Commons       :: *;
import Extracter           :: *;
import Normalizer          :: *;
import Utils               :: *;

// --------
// Local Types
//
// Intermediate pipe stage for accumulation
typedef struct {
   Int #(QuireWidth) sum_calc;
   Bit #(1)          q2_truncated_frac_zero;
   Bit #(1)          q2_truncated_frac_notzero;
   PositType         q2_zi;
   Bool              q2_nan;
} Acc_Stg1_In deriving (Bits, FShow);

// --------
// Interface
//
interface Quire_IFC;
   interface Put #(Quire_Acc) accumulate;    // add a value into the quire
   interface Put #(Posit_Extract) init;      // initialize a valu in quire
   method Action read_req;                   // start quire read
   interface Get #(Prenorm_Posit) read_rsp;  // quire read response
endinterface

// --------
// Functions
//
// checks for nan 100..00
function Bool is_nan (Bit#(1) sign, Bool is_zero);
   return ((sign == 1'b1) && is_zero);
endfunction

// Get the int-frac value from the scale and frac values
// function Tuple4 #(Bit #(LogQuireWidth)
//                 , Bool
//                 , Bool
function Tuple2 #(Bit #(LogQuireWidth)
                , Bit#(IntWidthQPlusFracWidthQ)) fv_calc_frac_int (
     Bit #(FracWidth) f
   , Int #(ScaleWidthPlus1) s
);
   Bit #(IntWidthQPlusFracWidthQ) f_new = extend(f);
   Bit #(FracWidthQ) qf = zExtendLSB (f); 
   Bit #(IntWidthQ) qi = 1;

   // Compose the fixed-point quire
   Bit #(LogQuireWidth) msbzeros = 0;
   // This is the number of zeros in cif before the leading one. For the scale = 0
   // case, it is carry-width + int-width - 1 (for the 1.xxxxx case)
   Bit #(LogQuireWidth) leading_one = fromInteger (
      quire_carry_width + quire_int_width - 1);

   // Positive scale. Shift radix point to the right or {qi, qf} to the left
   Bit #(IntWidthQPlusFracWidthQ) qif = {qi, qf};
   if (s >= 0) begin // strictly > is sufficient, but >= infers simpler logic
      Bit #(IntWidthQPlusFracWidthQ) shftamt = extend (pack (s));
      qif = qif << shftamt;
      leading_one = leading_one - extend (pack (s));
   end

   // Negative scale. Shift radix point to the left or {qi, qf} to the right
   else begin
      s = abs(s);
      Bit #(IntWidthQPlusFracWidthQ) shftamt = extend (pack (s));
      qif = qif >> shftamt;
      leading_one = leading_one + extend (pack (s));
   end

   return tuple2 (leading_one, qif);
endfunction

// Checks if the scale value has exceeded the limits max and min set due to the
// restricted availability of regime bits fraction bits will be shifted to take
// care of the scale value change due to it being bounded output : bounded
// scale value and the shift in frac bits
function Int #(ScaleWidthPlus1) calculate_scale_shift (
     Int #(LogCarryWidthPlusIntWidthPlusFracWidthQPlus1) scale
   , Int #(ScaleWidthPlus1) maxB
   , Int #(ScaleWidthPlus1) minB
);
   Int#(ScaleWidthPlus1) scale0;
   // frac_change gives the number of bits that are more or less than scale bounds
   // so that we can shift the frac bits to not lose scale information 
   if (scale < signExtend(minB))       scale0 = minB; // min bound scale
   else if (scale > signExtend(maxB))  scale0 = maxB; // max bound scale
   else                                scale0 = truncate(scale);  //no change
   return scale0;
endfunction

// --------
(* synthesize *)
module mkQuire #(Bit #(2) verbosity) (Quire_IFC);
   Reg #(Bit #(QuireWidth))         rg_quire          <- mkReg (0);
   Reg #(Bool)                      rg_quire_busy     <- mkReg (False);
   Reg #(Quire_Meta)                rg_quire_meta     <- mkReg (defaultValue);

   // Inputs to Stage 1 of quire accumulation pipeline
   FIFO  #(Acc_Stg1_In)             acc_stg1_f        <- mkFIFO1;

   // Pre-normalized posit output from reading the quire
   FIFO  #(Prenorm_Posit)           posit_rsp_f       <- mkFIFO1;

   Int#(ScaleWidthPlus1) maxB, minB;

   // max scale value is defined here... have to saturate the scale value 
   // max value = (N-2)*(2^es) 
   // scale = regime*(2^es) + expo.... max value of regime = N-2(00...1)
   maxB = fromInteger((valueOf(PositWidth) -2)*(2**(valueOf(ExpWidth))));

   // similarly calculate the min 
   minB = -maxB;	

   // --------
   // Pipeline stages
   // Pipe stage -- rounding and special cases
   rule rounding_special_cases (rg_quire_busy);
      let dIn = acc_stg1_f.first;  acc_stg1_f.deq;
      Bit#(1) flag_truncated_frac = (lsb(dIn.sum_calc) & dIn.q2_truncated_frac_zero) | dIn.q2_truncated_frac_notzero;
      let sign = msb(dIn.sum_calc);
      Bit#(2) truncated_frac = (flag_truncated_frac == 1'b0) ? 2'b00
                                                             : {sign, flag_truncated_frac};
      Int#(QuireWidth) sum_calc = boundedPlus (dIn.sum_calc, signExtend(unpack(truncated_frac)));
      Bit#(QuireWidthMinus1) sum_calc_unsigned = truncate(pack(sum_calc));
      Bit#(1) all_bits_0 = ~reduceOr(sum_calc_unsigned);
      Bool sum_is_zero = (sum_calc_unsigned == 0);

      PositType zero_infinity_flag =  ((sum_is_zero && (sign == 1'b0))
                                    && (rg_quire_meta.zi == REGULAR)
                                    && (dIn.q2_zi == REGULAR)) ? ZERO : REGULAR;

      // Put together the meta information on the quire
      let meta = Quire_Meta {
         nan   : (   (is_nan (sign, sum_is_zero))
                  || (dIn.q2_nan)
                  || (rg_quire_meta.zi == INF)
                  || (dIn.q2_zi == INF)),
         zi    : zero_infinity_flag
      };

      // Write the quire register based on the fields
      if (meta.nan)              rg_quire <= {1'b1, '0};
      else if (meta.zi == ZERO)  rg_quire <= 0;
      else                       rg_quire <= {sign, sum_calc_unsigned};

      rg_quire_meta <= meta;
      rg_quire_busy <= False;
   endrule

   interface Put accumulate;
      method Action put (Quire_Acc q) if (!rg_quire_busy);
         // Quire operations cannot be pipelined as there is a WAR dependency
         rg_quire_busy <= True;

         // signed sum of the values since the numbers are integer.fractions
         Int#(QuireWidth) sum_calc = boundedPlus (unpack (rg_quire), q.quire);

         // check for special cases
         let acc_stg1_in = Acc_Stg1_In {
              sum_calc : sum_calc
            , q2_truncated_frac_zero : q.frac_msb & q.frac_zero
            , q2_truncated_frac_notzero : q.frac_msb & ~(q.frac_zero)
            , q2_zi : q.zi
            , q2_nan : q.nan
         };

         acc_stg1_f.enq (acc_stg1_in);

         if (verbosity > 1) begin
            $display ("%0d: %m: accumulate: ", cur_cycle);
            if (verbosity > 2) begin
               $display ("   q1 %b", rg_quire);
               $display ("   q2 %b", q.quire);
               $display ("   ", fshow (acc_stg1_in));
            end
         end
      endmethod
   endinterface

   interface Put init;
      method Action put (Posit_Extract p) if (!rg_quire_busy);
         // match {.lead_one, .qif} = fv_calc_frac_int ({1'b1, p.frac}, p.scale);
         match {.lead_one, .qif} = fv_calc_frac_int (p.frac, p.scale);

         // Sign extend the Quire value
         Bit #(CarryWidthQ) carry = 0;
         Bit#(QuireWidth) s_carry_int_frac = (p.sign == 1'b0) ? {p.sign, carry, qif}
                                                              : {p.sign, twos_complement ({carry, qif})};
         rg_quire <= s_carry_int_frac;

         // Fill in the meta details to qualify the rg_quire bits
         let qm = Quire_Meta {
              nan : (p.ziflag == INF)
            , zi  : p.ziflag
            , lead_one : lead_one
         };

         rg_quire_meta <= qm;

         if (verbosity > 1) begin
            $display ("%0d: %m: init: ", cur_cycle);
         end
      endmethod
   endinterface

   // Verify by running a p2q-q2p standalone testbench. 
   method Action read_req if (!rg_quire_busy);
      // initialize prenorm_posit with value for a zero (or NaR) quire
      let prenorm_posit = Prenorm_Posit {
           sign      : 0
         , nan       : rg_quire_meta.nan
         , zi        : rg_quire_meta.zi
         , scale     : 0
         , frac      : 0
         , frac_msb  : 1'b0
         , frac_zero : 1'b1
      };


      // Quire is a regular number. Look at the bit pattern in rg_quire.
      if ((rg_quire_meta.zi == REGULAR) && (!rg_quire_meta.nan)) begin
         // Depending on the sign of the quire, interpret the rest for the bits
         Bit #(QuireWidthMinus1) cif = rg_quire [valueOf(QuireWidthMinus2):0];
         let sign = msb (rg_quire);
         Bit #(QuireWidthMinus1) s_cif = (sign == 1'b0) ? cif
                                                        : twos_complement (cif);
         let msbZeros = rg_quire_meta.lead_one;

         // calculate scale
         Int #(LogCarryWidthPlusIntWidthPlusFracWidthQPlus1) scale_temp =
            boundedMinus (  fromInteger (valueof (CarryWidthPlusIntWidthQ))
                          , (unpack (extend (msbZeros))+1));

         let nan = rg_quire_meta.nan;
         let ziflag = rg_quire_meta.zi;
         let scale = calculate_scale_shift (scale_temp, maxB, minB);

         // Pre-normalized posit fields
         Bit#(FracWidthPlus1) frac_p;
         PositType zi = (   (s_cif == 0)
                         && (ziflag == REGULAR)) ? ZERO : ziflag;
         Bit#(1) frac_msb_p;
         Bit#(1) frac_zero_p; 

         // interpret the scale to calculate number of fraction bit shifts required 
         if(scale < maxB) begin
            UInt #(LogCarryWidthPlusIntWidthPlusFracWidthQPlus1) truncate_msbZeros = unpack(
               pack (fromInteger (valueof (CarryWidthPlusIntWidthQ)) - signExtend(scale) - 1));

            // shift to get the frac bits 
            let carry_int_frac_shifted =  (s_cif << truncate_msbZeros);

            // extract the frac bits from the shifted quire bits
            frac_p = carry_int_frac_shifted [valueOf (QuireWidthMinus2) : valueOf (QuireWidthMinus2MinusFracWidth)];

            // the following flags are for rounding
            // the msb of the rest of the quire 
            frac_msb_p = carry_int_frac_shifted [valueOf (QuireWidthMinus3MinusFracWidth)];

            // get the remaining bits by truncating it to see if they are all 0's
            Bit #(QuireWidthMinus3MinusFracWidth) truncate_carry_int_frac_shifted = truncate (carry_int_frac_shifted);
            frac_zero_p = (truncate_carry_int_frac_shifted == 0) ? 1'b1 : 1'b0;
         end

         // quire overflow
         else begin
            frac_p = '1;
            frac_msb_p = 1'b1;
            frac_zero_p = 1'b0;
         end

         // Prepare the Prenorm_Posit for the normalizer
         prenorm_posit.sign      = sign;
         prenorm_posit.scale     = pack(scale);
         prenorm_posit.frac      = msb(frac_p) == 0 ? truncate (frac_p>>1)
                                                    : truncate (frac_p);
         prenorm_posit.frac_msb  = (zi == ZERO) ? 1'b0 : frac_msb_p;
         prenorm_posit.frac_zero = (zi == ZERO) ? 1'b1 : frac_zero_p;
      end

      posit_rsp_f.enq (prenorm_posit);
      if (verbosity > 1) begin
         $display ("%0d: %m: read_req: ", cur_cycle);
         if (verbosity > 2) begin
            $display ("   Quire Meta: ", fshow (rg_quire_meta));
            fa_print_quire (rg_quire);
         end
      end
   endmethod

   interface Get read_rsp = toGet (posit_rsp_f);
endmodule

endpackage
