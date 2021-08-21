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
//
// Known Problems:
//    1. The pipelined accumulation system does not detect NaN in
//       in the quire or in the input from the multiplier.
// --------------------------------------------------------------

// Library imports
import FIFO                :: *;
import GetPut              :: *;
import FShow               :: *;
import DefaultValue        :: *;
import BUtils              :: *; // for zExtendLSB and friends
import Vector              :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import Fused_Commons       :: *;
import Extracter           :: *;
import Normalizer          :: *;
import Utils               :: *;

// --------
// Local Types
// --------
// The segment in the segmented adder
typedef Bit #(32) Segment;
typedef Bit #(33) SegmentWCarry;

// Number of segments that make up a quire
typedef TDiv #(QuireWidth, 32) N_Segs;
typedef TSub #(N_Segs, 1) N_SegsSub1;

// --------
// Interface Definition
// --------
//
interface Quire_IFC;
   method Action accumulate (Quire_Acc x);   // add a value into the quire
   method Action init (Posit_Extract x);     // initialize a valu in quire
   method Action read_req;                   // start quire read
   interface Get #(Prenorm_Posit) read_rsp;  // quire read response
endinterface

// --------
// Functions
// --------
//
// checks for nan 100..00
function Bool is_nan (Bit#(1) sign, Bool is_zero);
   return ((sign == 1'b1) && is_zero);
endfunction

// Get the int-frac value from the scale and frac values
// function Tuple4 #(Bit #(LogQuireWidth)
//                 , Bool
//                 , Bool
function Bit#(IntFracWidthQ) fv_calc_frac_int (Bit #(FracWidth) f
                                             , Int #(ScaleWidthPlus1) s);

   Bit #(IntFracWidthQ) f_new = extend(f);
   Bit #(FracWidthQ) qf = zExtendLSB (f); 
   Bit #(IntWidthQ) qi = 1;

   // Compose the fixed-point quire

   // Positive scale. Shift radix point to the right or {qi, qf} to the left
   Bit #(IntFracWidthQ) qif = {qi, qf};
   if (s >= 0) begin // strictly > is sufficient, but >= infers simpler logic
      Bit #(IntFracWidthQ) shftamt = extend (pack (s));
      qif = qif << shftamt;
   end

   // Negative scale. Shift radix point to the left or {qi, qf} to the right
   else begin
      s = abs(s);
      Bit #(IntFracWidthQ) shftamt = extend (pack (s));
      qif = qif >> shftamt;
   end

   return qif;
endfunction

// Checks if the scale value has exceeded the limits max and min set due to the
// restricted availability of regime bits fraction bits will be shifted to take
// care of the scale value change due to it being bounded output : bounded
// scale value and the shift in frac bits
function Tuple2 #(Bool, Int #(ScaleWidthPlus1)) fn_bound_scale (
     Int #(LogQuireWidth) scale
   , Int #(ScaleWidthPlus1) maxB
   , Int #(ScaleWidthPlus1) minB
);
   Int#(ScaleWidthPlus1) scale0 = truncate (scale);
   Bool bounded = True;
   // frac_change gives the number of bits that are more or less than scale bounds
   // so that we can shift the frac bits to not lose scale information 
   if (scale < signExtend(minB)) begin
      scale0 = minB; // min bound scale
      bounded = False;
   end
   else if (scale > signExtend(maxB)) begin
      scale0 = maxB; // max bound scale
      bounded = False;
   end
   return (tuple2 (bounded, scale0));
endfunction


// --------
// Segmented leading zero counter
// --------
interface CounterIFC;
   method Action count (Quire x);
endinterface

module mkSegZeroCounter #(
     Vector #(N_Segs, Reg #(Bit#(1))) vrg_quire_seg_zero
   , FIFO #(Bit #(LogQuireWidth)) ff_numZeros
) (CounterIFC);
   Reg #(Bool) rg_seg_counter_busy <- mkReg (False);
   Reg #(Bit #(TLog #(N_Segs))) rg_numZeroSegs <- mkRegU;
   Reg #(Segment) rg_firstNonZeroSeg <- mkRegU;

   rule rl_countZerosInLeadingSeg (rg_seg_counter_busy);
      let zerosInNonZeroSeg = countZerosMSB (rg_firstNonZeroSeg);
      Bit #(LogQuireWidth) numZeroSegs = extend (rg_numZeroSegs);

      // Num-Zeros = (Num-Zero-Segs * 32) + (Num-Zero-Selected-Seg) - 1
      let numZeros = numZeroSegs << 5;
      numZeros = numZeros + extend (pack (zerosInNonZeroSeg));
      // To discard the extra zero counted inserted while packing cif into
      // QuireWidth (only applicable for QuireWidths which are multiples of
      // 32-bits).
      numZeros = numZeros - 1;

      ff_numZeros.enq (numZeros);
      rg_seg_counter_busy <= False;
   endrule

   method Action count (Quire x) if (!rg_seg_counter_busy);
      Vector #(N_Segs, Segment) v_longData = unpack (x);
      let v_segIsZero = readVReg (vrg_quire_seg_zero);
      Bit #(N_Segs) segIsZero = pack (v_segIsZero);

      // Count the number of leading segments which are 0s
      let segIsNonZero = ~segIsZero;
      let numZeroSegsMSB = countZerosMSB (segIsNonZero);
      rg_numZeroSegs <= truncate (pack (numZeroSegsMSB));

      // Rotate quire to align the first non-zero segment as MSB
      UInt #(TLog #(N_Segs)) rotationBy = truncate (numZeroSegsMSB);
      v_longData = rotateBy (v_longData, rotationBy);
      rg_firstNonZeroSeg <= v_longData [valueOf (N_Segs) - 1];

      rg_seg_counter_busy <= True;
   endmethod
endmodule

// --------
// Long adder
// Pipelined segmented adder
// --------
interface AdderIFC;
   method Action acc (Quire x);
   method Bool   busy;
endinterface

module mkSegAdder #(
        Vector #(N_Segs, Reg #(Segment)) vrg_accumulator
      , Vector #(N_Segs, Reg #(Bit#(1))) vrg_segIsZero
   ) (AdderIFC);
   Vector #(N_Segs, FIFO #(Segment)) vff_in <- replicateM (mkSizedFIFO (4));
`ifndef PWIDTH_8
   Vector #(N_SegsSub1, FIFO#(Bit #(1))) vff_carry <- replicateM (mkSizedFIFO (4));
`endif
   Reg #(Bit #(TLog #(N_Segs))) rg_inFlight <- mkReg(0);

   Bool accIsBusy = (rg_inFlight != 0);

   rule acc_stage_0 (accIsBusy);
      SegmentWCarry acc = extend (vrg_accumulator[0]);
      SegmentWCarry in  = extend (vff_in[0].first); vff_in[0].deq;
      acc = acc + in;
`ifdef PWIDTH_8
      // 8-bit posits just have a single segment
      rg_inFlight <= rg_inFlight - 1;
`else
      vff_carry[0].enq (msb (acc));
`endif

      Segment acc_seg = truncate (acc);
      vrg_accumulator[0] <= acc_seg;
      vrg_segIsZero[0] <= pack (acc_seg == 0);
   endrule

`ifndef PWIDTH_8
   for (Integer i = 1; i < valueOf (N_Segs); i = i+1) begin
      rule acc_stage_i (accIsBusy);
         SegmentWCarry acc = extend (vrg_accumulator[i]);
         SegmentWCarry in  = extend (vff_in[i].first); vff_in[i].deq;
         SegmentWCarry cin = extend (vff_carry[i-1].first); vff_carry[i-1].deq;
         acc = acc + in + cin;

         Segment acc_seg = truncate (acc);
         vrg_accumulator[i] <= acc_seg;
         vrg_segIsZero[i] <= pack (acc_seg == 0);

         if (i == valueOf (N_SegsSub1)) 
            rg_inFlight <= rg_inFlight - 1;
         
         else 
            vff_carry[i].enq (msb (acc));
      endrule
   end
`endif

   method Action acc (Quire x);
      Vector #(N_Segs, Segment) v_x = unpack (x);
      for (Integer i = 0; i < valueOf (N_Segs); i = i+1)
         vff_in [i].enq (v_x[i]);
      rg_inFlight <= rg_inFlight + 1;
   endmethod

   method Bool busy = accIsBusy;
endmodule


// Return the absolute value of a quire
(* noinline *)
function Bit #(QuireWidthMinus1) fn_twosC_quire (
     Vector #(N_Segs, Segment) quire
   , Vector #(N_Segs, Bit#(1)) quire_seg_zero
);
   let sign = msb (quire [valueOf (N_Segs) - 1]);
   Vector #(N_Segs, Segment) v_quire = quire;

`ifdef PWIDTH_8
   for (Integer i=0; i<valueOf(N_Segs); i=i+1) begin
      v_quire[i] = (~v_quire[i]) + 1;
   end
`else
   // Once the quire is inverted the all zero segs become all ones
   Bit #(N_Segs) one_segs = pack (quire_seg_zero);

   // Capture carry propagation through the all ones segs map
   one_segs = one_segs + 1;
   Vector #(N_Segs, Bit #(1)) v_one_segs_carry_prop = unpack (one_segs);

   for (Integer i=0; i<valueOf(N_Segs); i=i+1) begin
      // Carry has effected this segment
      if (v_one_segs_carry_prop[i] != quire_seg_zero[i]) begin
         // Cases:
         // The segment was all zeros, got inverted to all ones, then a carry
         // propagated through it, turning it back into all zeros. 

         // The segment was non-zero. Got inverted. Carry propagated through
         // it, incrementing it by 1.
         if (v_one_segs_carry_prop[i] == 1'b1) v_quire[i] = (~v_quire[i]) + 1;
      end
      
      // Carry has no effect on this segment. So, just the inverted value
      else v_quire[i] = ~v_quire[i];
   end
`endif

   Bit #(QuireWidthMinus1) cif = pack (v_quire)[valueOf(QuireWidthMinus2):0];
   return (cif);
endfunction

// --------
// The Quire top-level
// --------
(* synthesize *)
module mkQuire #(Bit #(2) verbosity) (Quire_IFC);
   Vector #(N_Segs, Reg #(Segment)) vrg_quire         <- replicateM (mkReg(0));
   Vector #(N_Segs, Reg #(Bit#(1))) vrg_quire_seg_zero<- replicateM (mkReg(1));
   Reg    #(Bool)                   rg_seg_zero_upd   <- mkReg (True);
   FIFO   #(Bit #(LogQuireWidth))   ff_num_msb_zeros  <- mkFIFO1;
   Reg    #(Bool)                   rg_read_busy      <- mkReg (False);

   // Pipelined long adder
   AdderIFC                         seg_adder         <- mkSegAdder (vrg_quire, vrg_quire_seg_zero);
   CounterIFC                       zero_counter      <- mkSegZeroCounter (vrg_quire_seg_zero, ff_num_msb_zeros);

   // Pre-normalized posit output from reading the quire
   FIFO  #(Prenorm_Posit)           posit_rsp_f       <- mkFIFO1;

   Int#(ScaleWidthPlus1) maxB, minB;

   Bool quire_is_zero = (rg_seg_zero_upd
                      && unpack (reduceAnd(pack(readVReg(vrg_quire_seg_zero)))));

   // max scale value is defined here... have to saturate the scale value 
   // max value = (N-2)*(2^es) 
   // scale = regime*(2^es) + expo.... max value of regime = N-2(00...1)
   maxB = fromInteger((valueOf(PositWidth) - 2)*(2**(valueOf(ExpWidth))));

   // similarly calculate the min 
   minB = -maxB;	

   // --------
   // Behavior
   // --------
   //
   // Check if a segment is zero.
   function Bit #(1) fn_seg_is_zero (Segment x);
      return (pack (x == 0));
   endfunction

   // Absolute value of the quire
   function Tuple2 #(Bit #(1), Bit #(QuireWidthMinus1)) fn_abs_quire;
      Quire quire = pack (readVReg (vrg_quire));
      Bit #(QuireWidthMinus1) s_cif = quire [valueOf(QuireWidthMinus2):0];
      let sign = msb (vrg_quire [valueOf (N_Segs) - 1]);

      Bit #(QuireWidthMinus1) cif = (sign == 1'b0) ? s_cif
                                                   : twos_complement (s_cif);
      return (tuple2 (sign, cif));
   endfunction

   // Update the zero segment indicator for the quire. Only runs when quiescent.
   rule rl_upd_zero_seg (   (!rg_seg_zero_upd)
                         && (!seg_adder.busy)
                         && (!rg_read_busy)
                        );
      let v_quire = readVReg (vrg_quire);
      Vector #(N_Segs, Bit#(1)) v_quire_seg_zero = map (fn_seg_is_zero, v_quire);
      writeVReg (vrg_quire_seg_zero, v_quire_seg_zero);
      rg_seg_zero_upd <= True;
   endrule

   // Complete the steps to generate a read response to a quire read request
   rule rl_read_response (   (rg_read_busy)
                          && (rg_seg_zero_upd)
                          && (!seg_adder.busy)
                         );
      if (verbosity > 1) $display ("%0d: %m.rl_read_response", cur_cycle);

      let msbZeros = ff_num_msb_zeros.first; ff_num_msb_zeros.deq;


      let sign = msb (pack (readVReg (vrg_quire)));
      let cif = pack (readVReg (vrg_quire))[valueOf(QuireWidthMinus2):0];

      if (sign == 1'b1) cif = fn_twosC_quire (
         readVReg (vrg_quire), readVReg (vrg_quire_seg_zero));

      // calculate scale
      Int #(LogQuireWidth) quire_scale =
         boundedMinus (  fromInteger (valueof (CarryWidthPlusIntWidthQ))
                       , (unpack (extend (msbZeros))+1));

      // saturate scale beyond maxB, minB
      match {.bounded, .scale} = fn_bound_scale (quire_scale, maxB, minB);

      if (verbosity > 2) begin
         $display ("    msbZeros: %0d", msbZeros);
         $display ("    quire_scale: %0d", quire_scale);
         $display ("    quire_scale: %0d", quire_scale);
         $display ("    bounded ", fshow (bounded), " scale: %0d", scale);
         $display ("    max: %0d, min: %0d", maxB, minB);
      end

      // Shift and create the fraction based on the scale value taking care of
      // overflows and underflows. The fraction bits are aligned starting from
      // the msb of cif. The hidden bit is discarded.

      Bit#(FracWidth) frac;
      Bit#(1) frac_round_msb;
      Bit#(1) frac_round_zero; 

      // Bounded scale
      if (bounded) begin
         Bit #(QuireWidthMinus1) shftamt = extend (msbZeros);
         cif = cif << shftamt;   // align 1.ffff with msb of cif
         cif = cif << 1;         // get rid of the hidden bit

         // extract the frac bits from the shifted quire bits
         frac = truncateLSB (cif);

         // Extract the flags for rounding:
         // The msb of the remaining frac bits indicating if we are over 0.5
         // (msb = 1) or under (msb = 0).  
         // Excluding the msb, is the rest of the bits zero (lower quadrant: 0.00
         // to 0.25 or 0.50 to 0.75) or non-zero (upper quadrant)
         cif = cif << valueOf (FracWidth);
         frac_round_msb = msb (cif);
         cif = cif << 1;
         frac_round_zero = (cif == 0) ? 1'b1 : 1'b0;
      end

      // Overflow/Underflow
      else begin
         frac = '1;
         frac_round_msb = 1'b1;
         frac_round_zero = 1'b0;
      end

      let prenorm_posit = Prenorm_Posit {
           sign      : sign
         , nan       : !bounded
         , zi        : REGULAR
         , scale     : pack (scale)
         , frac      : frac
         , frac_msb  : frac_round_msb
         , frac_zero : frac_round_zero
      };

      posit_rsp_f.enq (prenorm_posit);
      rg_read_busy <= False;
      if (verbosity > 2) fa_print_quire (pack (readVReg (vrg_quire)));
   endrule

   // --------
   // Interfaces
   // --------
   method Action accumulate (Quire_Acc q) if (!rg_read_busy);
      // initiate the accumulation of the input into the quire
      let input_is_zero = (q.meta.zi == ZERO);

      // Create the quire value by appending the zero carry and take a 2's
      // complement of the whole quire
      Bit #(CarryWidthQ) carry = 0;
      Quire abs_quire_in = {1'b0, carry, q.qif};
      Vector #(N_Segs, Segment) v_quire = unpack (abs_quire_in);
      Vector #(N_Segs, Bit#(1)) v_quire_seg_zero = map (fn_seg_is_zero, v_quire);

      // Create the input quire by selectively taking twos complement of the cif
      // depending on the value of sign
      Quire quire_in = abs_quire_in;
      if (q.sign == 1'b1) quire_in = {
         1'b1, fn_twosC_quire (v_quire, v_quire_seg_zero)};

      // --------
      // Special Cases

      // a: the quire is zero and the segmented adder is quiescent
      // No need for addition, the input quire becomes the quire value
      
      // b: the input is zero, no need for additon, this input can be skipped.

      if (!input_is_zero) begin
         if (!seg_adder.busy) begin
            if (quire_is_zero) begin
               // Special Case (a)
               Vector #(N_Segs, Segment) v_quire_in = unpack (quire_in);
               writeVReg (vrg_quire, v_quire_in);
               writeVReg (vrg_quire_seg_zero, v_quire_seg_zero);
               rg_seg_zero_upd <= True;
            end

            // The quire is not zero. Send the new input for accumulation
            else begin
               seg_adder.acc (quire_in);
               rg_seg_zero_upd <= False;
            end
         end

         // The adder is computing so it is not possible to determine the state of
         // the quire at this point. Send the input for accumulation
         else begin
            seg_adder.acc (quire_in);
            rg_seg_zero_upd <= False;
         end
      end
   endmethod

   method Action init (Posit_Extract p) if (   (!seg_adder.busy)
                                            && (!rg_read_busy));
      let qif = fv_calc_frac_int (p.frac, p.scale);

      // Create the quire value by appending the zero carry and take a 2's
      // complement of the whole quire
      Bit #(CarryWidthQ) carry = 0;

      // Remove the sign - just the absolute value of qif. Count the number of
      // zero segs in this absolute value for the fast 2's Complement
      Quire abs_quire_in = {1'b0, carry, qif};
      Vector #(N_Segs, Segment) v_quire = unpack (abs_quire_in);

      Vector #(N_Segs, Bit#(1)) v_quire_seg_zero = map (fn_seg_is_zero, v_quire);
      // If zero, overwrite the known pattern into the seg_zero vector
      if (p.ziflag == ZERO) v_quire_seg_zero = replicate (1'b1);

      Vector #(N_Segs, Segment) v_s_cif = v_quire;
      if (p.sign == 1'b1) v_s_cif = unpack ({1'b1, fn_twosC_quire (
         v_quire, v_quire_seg_zero)});

      writeVReg (vrg_quire, v_s_cif);
      writeVReg (vrg_quire_seg_zero, v_quire_seg_zero);
      rg_seg_zero_upd <= True;

      if (verbosity > 1) begin
         $display ("%0d: %m: init: ", cur_cycle);
         if (verbosity > 2) begin
            $display ("    qif: 0x%0h", qif);
            $display ("    v_quire_seg_zero: ", fshow (v_quire_seg_zero));
            fa_print_quire (pack (v_s_cif));
         end
      end
   endmethod

   // Read the quire value. Will only run after all preceeding operations have
   // completed.
   method Action read_req if (   (!seg_adder.busy)
                              && (!rg_read_busy)
                              && (rg_seg_zero_upd));

      if (verbosity > 1)
         $display ("%0d: %m: read_req: ", cur_cycle);

      // Special cases -- zero quire
      if (quire_is_zero) begin
         // initialize prenorm_posit with value for a zero (or NaR) quire
         let prenorm_posit = Prenorm_Posit {
              sign      : 0
            , nan       : False  // look at known problems
            , zi        : ZERO
            , scale     : 0
            , frac      : 0
            , frac_msb  : 1'b0
            , frac_zero : 1'b1
         };
         posit_rsp_f.enq (prenorm_posit);
         if (verbosity > 2) $display ("    Zero Quire");
      end

      // REGULAR quire
      else begin
         // Depending on the sign of the quire, interpret the rest for the bits
         let sign = msb (pack (readVReg (vrg_quire)));
         let cif = pack (readVReg (vrg_quire))[valueOf(QuireWidthMinus2):0];
         if (sign == 1'b1) cif = fn_twosC_quire (
            readVReg (vrg_quire), readVReg (vrg_quire_seg_zero));

         // Count leading zeros on the absolute value of the quire value
         zero_counter.count (pack ({1'b0, cif}));
         rg_read_busy <= True;

         if (verbosity > 2) $display ("    cif: 0x%0h", cif);
      end

   endmethod

   interface Get read_rsp = toGet (posit_rsp_f);
endmodule

endpackage
