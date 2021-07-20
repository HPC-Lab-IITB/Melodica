package PositCore_accel;

// --------------------------------------------------------------
// This package implements the top-level of the Posit Arithmetic Unit that
// integrates into Clarinet's pipeline as a functional unit peer of the FPU.
// ifdef ACCEL macros are used in Quills
// --------------------------------------------------------------

// Library imports
import FIFO          :: *;
import FShow         :: *;
import SpecialFIFOs  :: *;
import GetPut        :: *;
import ClientServer  :: *;
import FloatingPoint :: *;

// Project imports
import Extracter     :: *;
import Normalizer    :: *;
import Fused_Commons :: *;

`ifndef ONLY_POSITS
import FtoP_PNE_PC   :: *;
import PtoF_PNE_PC   :: *;
`endif

import Multiplier_fma:: *;

`ifdef INCLUDE_PDIV
import Divider_fda   :: *;
`endif

import Quire         :: *;

import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Utils  :: *;




// Standalone compilation independent of a RISC-V core
`ifdef STANDALONE
// Type definitions
typedef FloatingPoint#(11,52) FDouble;
typedef FloatingPoint#(8,23)  FSingle;

typedef union tagged {
   FDouble D;
   FSingle S;
   Bit #(PositWidth) P;
`ifdef ACCEL
	Quire_Acc Q;
`endif
} FloatU deriving (Bits, Eq, FShow);

typedef Tuple2#( FloatU, FloatingPoint::Exception ) Fpu_Rsp;
`else
import FPU_Types     :: *; // CPU-side typedefs
`endif

// --------
// Request-response interface
typedef enum {
     FMA_P
   , FMS_P
`ifdef INCLUDE_PDIV
   , FDA_P
   , FDS_P
`endif
`ifndef ONLY_POSITS
   , FCVT_P_S
   , FCVT_S_P
`endif
   , FCVT_P_R
   , FCVT_R_P
`ifdef ACCEL
		,RD_Q
		,RST_Q
`endif
} PositCmds deriving (Bits, Eq, FShow);

typedef Tuple4 #(FloatU, FloatU, RoundMode, PositCmds) Posit_Req;

interface PositCore_IFC;
   interface Server #(Posit_Req, Fpu_Rsp) server_core;
endinterface

// --------
(* synthesize *)
`ifdef STANDALONE
module mkPositCore (PositCore_IFC);
   Bit #(2) verbosity = 1;
`else
module mkPositCore #(Bit #(2) verbosity) (PositCore_IFC);
`endif

   // Two extracters for a maximum of two operands
   Server #(Posit, Posit_Extract)      extracter1     <- mkExtracter (verbosity);
   Server #(Posit, Posit_Extract)      extracter2     <- mkExtracter (verbosity);

   // Output normalizer
   Server #(Prenorm_Posit, Norm_Posit) normalizer     <- mkNormalizer (verbosity);

   // Multiplier part of FMA/FMS
   Server #(  Tuple2 #(  Posit_Extract
                       , Posit_Extract)
            , Quire_Acc)               multiplier     <- mkMultiplier (verbosity);
`ifdef INCLUDE_PDIV
   // Divider part of FDA/FDS
   Server #(  Tuple2 #(  Posit_Extract
                       , Posit_Extract)
            , Quire_Acc)               divider        <- mkMultiplier (verbosity);
`endif

   // The Quire -- includes the accumulator for fused operations
   Quire_IFC                           quire          <- mkQuire (verbosity);

`ifndef ONLY_POSITS
   // Float-Posit converters
   Server #(Bit#(FloatWidth), Prenorm_Posit) ftop     <- mkFtoP_PNE (verbosity);        
   Server #(Posit_Extract, Float_Extract)    ptof     <- mkPtoF_PNE (verbosity);        
`endif

`ifdef ACCEL
   Reg #(Bit#(3))  ops_in_flight[3]	<- mkCReg(3,0);
`endif	

   FIFO #(PositCmds)                   cmd_stg2_f     <- mkSizedFIFO(4);
   FIFO #(PositCmds)                   cmd_stg3_f     <- mkSizedFIFO(4);

   FIFO #(Posit_Req)                   ffI            <- mkSizedFIFO(4);
   FIFO #(Fpu_Rsp)                     ffO            <- mkSizedFIFO(4);


   let no_excep = FloatingPoint::Exception {
        invalid_op   : False
      , divide_0     : False
      , overflow     : False
      , underflow    : False
      , inexact      : False
   };

   // --------
   // Input/Extraction Phase
   // Stage 1: Extract posit values
   match {.op1, .op2, .rounding, .cmd} = ffI.first;
   let is_negating_op = (
         (cmd == FMS_P)
`ifdef INCLUDE_PDIV
      || (cmd == FDS_P)
`endif
   );
`ifndef ONLY_POSITS
   rule extract_stg1 ((cmd != FCVT_P_S) && (cmd != FCVT_P_R) && (cmd != RD_Q) && (cmd != RST_Q));
`else
   rule extract_stg1 ((cmd != FCVT_P_R) && (cmd != RD_Q) && (cmd != RST_Q));
`endif
      extracter1.request.put (op1.P);
      extracter2.request.put (is_negating_op ? twos_complement (op2.P) : op2.P);
      cmd_stg2_f.enq (cmd);
      ffI.deq;
`ifdef ACCEL
			ops_in_flight[0] <= ops_in_flight[0] + 1;
`endif

      if (verbosity > 1)
         $display ("%0d: %m: rl_extract_stg1: ", cur_cycle, fshow (cmd));
   endrule

   // Stage 1: Initiate float to posit conversion
`ifndef ONLY_POSITS
   rule rl_float_to_posit_stg1 (cmd == FCVT_P_S);
      let float_val = op1.S;
      Bit #(FloatWidth) f = {pack (float_val.sign), float_val.exp, float_val.sfd};
      ftop.request.put (f); 
      cmd_stg2_f.enq (cmd); ffI.deq;
      if (verbosity > 1) begin
         $display ("%0d: %m.rl_float_to_posit_stg1: convert ", cur_cycle);
         if (verbosity > 2)
            $display ("   ", fshow (float_val));
      end
   endrule
`endif

   rule rl_read_quire_stg1 (cmd == FCVT_P_R); 
      quire.read_req;
      cmd_stg2_f.enq (cmd);
      ffI.deq;

      if (verbosity > 1)
         $display ("%0d: %m.rl_read_quire_stg1: read ", cur_cycle);
   endrule

`ifdef ACCEL
   rule rl_read_quire (cmd == RD_Q && ops_in_flight[2] == 3'b0);
      let z = quire.read_quire;
	  FloatU quire_out = tagged Q z;
      ffO.enq (tuple2(quire_out,no_excep));
      ffI.deq;

      if (verbosity > 1)
         $display ("%0d: %m.rl_read_quire: quire output ", cur_cycle);
   endrule

   rule rl_reset_quire (cmd == RST_Q && ops_in_flight[2] == 3'b0);
	  let p = Posit_Extract {ziflag : ZERO};
      quire.init.put(p);
      ffI.deq;

      if (verbosity > 1)
         $display ("%0d: %m.rl_reset_quire ", cur_cycle);
   endrule
`endif
   // --------
   // Fused Operation MUL/DIV Phase: Stage 2
   let cmd_stg2 = cmd_stg2_f.first;

   // Stage 2: FMA/FMS Compute: Multiplication
   rule rl_fma_stg2 ((cmd_stg2 == FMA_P) || (cmd_stg2 == FMS_P));
      // Do not deq the cmd_stg2_f as the multiplication and addition are atomic
      // for a FMA/FMS/FDA/FDS
      let ext_out1 <- extracter1.response.get();
      let ext_out2 <- extracter2.response.get();
      multiplier.request.put (tuple2 (ext_out1, ext_out2));
      cmd_stg3_f.enq (cmd_stg2); cmd_stg2_f.deq;

      // Complete this operation as far as the CPU is concerned
      FloatU posit_out = tagged P 0;
	`ifndef ACCEL
      ffO.enq(tuple2(posit_out, no_excep));
	`endif

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_fma_stg2: multiply ", cur_cycle);
         if (verbosity > 2) begin
            $display ("   ext_out1: ", fshow (ext_out1));
            $display ("   ext_out2: ", fshow (ext_out2));
         end
      end
   endrule

`ifdef INCLUDE_PDIV
   // Stage 2: FDA/FDS Compute: Division
   rule rl_fda_stg2 ((cmd_stg2 == FDA_P) || (cmd_stg2 == FDS_P));
      // Do not deq the cmd_stg2_f as the multiplication and addition are atomic
      // for a FMA/FMS/FDA/FDS
      let ext_out1 <- extracter1.response.get();
      let ext_out2 <- extracter2.response.get();
      divider.request.put (tuple2 (ext_out1, ext_out2));
      cmd_stg3_f.enq (cmd_stg2); cmd_stg2_f.deq;

      // Complete this operation as far as the CPU is concerned
      FloatU posit_out = tagged P 0;
	`ifndef ACCEL
      ffO.enq(tuple2(posit_out, no_excep));
	`endif

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_fda_stg2: divide ", cur_cycle);
         if (verbosity > 2) begin
            $display ("   ext_out1: ", fshow (ext_out1));
            $display ("   ext_out2: ", fshow (ext_out2));
         end
      end
   endrule
`endif

`ifndef ONLY_POSITS
   // Stage 2: Convert stage of posit-to-float converter
   rule rl_posit_to_float_stg2 (cmd_stg2 == FCVT_S_P);
      let ext_out1 <- extracter1.response.get();
      let discard  <- extracter2.response.get();
      ptof.request.put (ext_out1);
      cmd_stg3_f.enq (cmd_stg2); cmd_stg2_f.deq;
      if (verbosity > 1) begin
         $display ("%0d: %m.rl_posit_to_float_stg2: convert ", cur_cycle);
         if (verbosity > 2)
            $display ("   ext_out1: ", fshow (ext_out1));
      end
   endrule

   // Stage 2: Normalize stage of float-to-posit converter
   rule rl_float_to_posit_stg2 (cmd_stg2 == FCVT_P_S);
      let o <- ftop.response.get ();
      normalizer.request.put (o);            
      cmd_stg3_f.enq (cmd_stg2); cmd_stg2_f.deq;

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_float_to_posit_stg2: normalize ", cur_cycle);
         if (verbosity > 2)
            $display ("   ftop out: ", fshow (o));
      end
   endrule
`endif

   // Stage 2: Initialize the quire (aka posit-to-quire)
   rule rl_init_quire_stg2 (cmd_stg2 == FCVT_R_P);
      let ext_out1 <- extracter1.response.get();
      let discard  <- extracter2.response.get();
      quire.init.put (ext_out1);
      cmd_stg2_f.deq;

      // Complete this operation as far as the CPU is concerned
      FloatU posit_out = tagged P 0;
	`ifndef ACCEL
      ffO.enq(tuple2(posit_out, no_excep));
	`endif

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_init_quire_stg2: initialize ", cur_cycle);
         if (verbosity > 2)
            $display ("   ext_out1: ", fshow (ext_out1));
      end
   endrule

   // Stage 2: Normalize stage of quire read (aka quire-to-posit)
   rule rl_read_quire_stg2 (cmd_stg2 == FCVT_P_R);
      let o <- quire.read_rsp.get ();
      normalizer.request.put (o);            
      cmd_stg3_f.enq (cmd_stg2); cmd_stg2_f.deq;

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_read_quire_stg2: normalize ", cur_cycle);
         if (verbosity > 2)
            $display ("   qtop out: ", fshow (o));
      end
   endrule

   // --------
   // Accumulate Phase / Normalize Phase / Output Phase: Stage 3
   let cmd_stg3 = cmd_stg3_f.first;

   // Stage 3: FMA/FMS Compute: Accumulate
   rule rl_fma_stg3 ((cmd_stg3 == FMA_P) || (cmd_stg3 == FMS_P));
      let quire_increment <- multiplier.response.get ();
      quire.accumulate.put (quire_increment);
      cmd_stg3_f.deq;

`ifdef ACCEL
			ops_in_flight[1] <= ops_in_flight[1] - 1;
`endif

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_fma_stg3: accumulate ", cur_cycle);
         if (verbosity > 2)
            $display ("   mul out: ", fshow (quire_increment));
      end
   endrule

`ifdef INCLUDE_PDIV
   // Stage 3: FDA/FDS Compute: Accumulate
   rule rl_fda_stg3 ((cmd_stg3 == FDA_P) || (cmd_stg3 == FDS_P));
      let quire_increment <- divider.response.get ();
      quire.accumulate.put (quire_increment);
      cmd_stg3_f.deq;

`ifdef ACCEL
			ops_in_flight[1] <= ops_in_flight[1] - 1;
`endif

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_fda_stg3: accumulate ", cur_cycle);
         if (verbosity > 2)
            $display ("   div out: ", fshow (quire_increment));
      end
   endrule
`endif

`ifndef ONLY_POSITS
   // Stage 3: Output of posit-to-float converter
   rule rl_posit_to_float_stg3 (cmd_stg3 == FCVT_S_P);
      let o <- ptof.response.get ();
      let fval = FSingle {
         sign  : unpack (msb (o.float_out)),
         exp   : (o.float_out[valueOf(FloatExpoBegin):valueOf(FloatFracWidth)]),
         sfd   : truncate (o.float_out)
      };

      // Exception flags
      let excep = no_excep;
      excep.overflow = (o.ziflag == INF);
      excep.underflow = (o.ziflag == ZERO) && (o.rounding);
      excep.inexact = o.rounding;

      // Complete this operation as far as the CPU is concerned
      FloatU fout = tagged S fval;
      ffO.enq(tuple2(fout, excep));
      cmd_stg3_f.deq;

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_posit_to_float_stg3: out ", cur_cycle);
         if (verbosity > 2) begin
            $display ("   float: ", fshow (fout));
            $display ("   exception: ", fshow (excep));
         end
      end
   endrule

   // Stage 3: Output of float-to-posit converter
   rule rl_float_to_posit_stg3 (cmd_stg3 == FCVT_P_S);
      let norm_out <- normalizer.response.get ();
      let excep = no_excep;
      excep.invalid_op  = norm_out.nan;
      excep.overflow    = (norm_out.zi == INF);
      excep.underflow   = (norm_out.zi == ZERO) && norm_out.rounding;
      excep.inexact     = norm_out.rounding;

      // Complete this operation as far as the CPU is concerned
      FloatU posit_out = tagged P norm_out.posit;
      ffO.enq (tuple2 (posit_out, excep));
      cmd_stg3_f.deq;

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_float_to_posit_stg3: out ", cur_cycle);
         if (verbosity > 2) begin
            $display ("   posit: ", fshow (posit_out));
            $display ("   exception: ", fshow (excep));
         end
      end
   endrule
`endif

   // Stage 3: Output of normalizer stage of read quire
   rule rl_read_quire_stg3 (cmd_stg3 == FCVT_P_R);
      let out_pf <- normalizer.response.get ();
      let excep = no_excep;
      excep.invalid_op  = (out_pf.nan);
      excep.overflow    = (out_pf.zi == INF);
      excep.underflow   = (out_pf.zi == ZERO) && out_pf.rounding;
      excep.inexact     = out_pf.rounding;

      // Complete this operation as far as the CPU is concerned
      FloatU posit_out = tagged P out_pf.posit;
      ffO.enq (tuple2 (posit_out, excep));
      cmd_stg3_f.deq;

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_read_quire_stg3: out ", cur_cycle);
         if (verbosity > 2) begin
            $display ("   posit: ", fshow (out_pf));
            $display ("   exception: ", fshow (excep));
         end
      end
   endrule

   interface server_core = toGPServer (ffI, ffO);
endmodule
endpackage
