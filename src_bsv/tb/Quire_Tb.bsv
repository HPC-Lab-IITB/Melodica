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
// THE SOFTWARE.package Extracter_Types;
//
package Quire_Tb;
//
// -----------------------------------------------------------------
// This package defines:
//
// mkTestbench : Tests Quire functionality. Test sequences:
//   1. quire.init, quire.read, (input == output) => PASS
//   2. quire.init, (quire.accumulate * x), quire.read
//      Compare with posit adder output. The accumulate input can be
//      driven by sending (posit, 1) to the multiplier.
// Supports running on FPGA. Exhaustive testing of sequence 1 possible
// on FPGA for all supported posit sizes.
// -----------------------------------------------------------------
//
import Vector              :: *;
import FIFO                :: *;
import GetPut              :: *;
import ClientServer        :: *;
import FShow               :: *;
import LFSR                :: *;

import Utils               :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types    :: *;
import Quire               :: *;
import Normalizer          :: *;
import Extracter           :: *;

// Number of random tests to be run
`ifdef P8
typedef 255 Num_Tests;
`elsif P16
typedef 8192 Num_Tests;
`elsif P32
typedef 8192 Num_Tests;
`endif

typedef enum {SEED, TST, STOP} TB_State deriving (Bits, Eq, FShow);
typedef enum {EXT, INIT, READREQ, READRSP, NORM} COMP_State deriving (Bits, Eq, FShow);

(* synthesize *)
`ifdef FPGA
module mkTestbench (LED_IFC);
`else
module mkTestbench (Empty);
`endif

Bit #(2) verbosity = 3;

`ifdef RANDOM
LFSR  #(Bit #(32))               lfsr1          <- mkLFSR_32;
`endif

// Extracter for init operations
Server #(Posit, Posit_Extract)   extracter      <- mkExtracter (verbosity);

// Output normalizer
Server #(  Prenorm_Posit
         , Norm_Posit)           normalizer     <- mkNormalizer (verbosity);

Quire_IFC                        quire          <- mkQuire (verbosity);

FIFO  #(Bit #(PositWidth))       posit_in_f     <- mkFIFO;
Reg   #(TB_State)                rg_tb_state    <- mkReg (SEED);
Reg   #(COMP_State)              rg_comp_state  <- mkReg (EXT);
Reg   #(Bit #(PositWidth))       rg_ip_num      <- mkReg (0);
Reg   #(Bool)                    rg_error       <- mkReg (False);


function Action fa_report_test_pass;
   action
`ifdef FPGA
   noAction;
`else
   $display ("%0d: %m: ALL TEST PASS", cur_cycle);
   $finish;
`endif
   endaction
endfunction

function Action fa_report_test_fail (Bit #(PositWidth) test_num);
   action
   rg_error <= True;
`ifndef FPGA
   $display ("%0d: %m: TEST FAILURE: %0d", cur_cycle, test_num);
   $finish;
`endif
   endaction
endfunction

// -----------------------------------------------------------------

rule rl_seed_lfsr (rg_tb_state == SEED);
`ifdef RANDOM
   lfsr1.seed('h12345678); // to create different random series
`endif
   rg_tb_state <= TST;
   rg_comp_state <= EXT;
endrule

rule rl_tst_ext ((rg_tb_state == TST) && (rg_comp_state == EXT));
`ifdef RANDOM
   // Get random posit from LFSR
   Posit inPosit = truncate (lfsr1.value());
   lfsr1.next ();
`else
   // Get posit from counter
   Posit inPosit = truncate (rg_ip_num);
`endif

   extracter.request.put (inPosit);
   rg_comp_state <= INIT;

   // Bookkeeping
   rg_ip_num <= rg_ip_num + 1;
   posit_in_f.enq (inPosit);

   if (verbosity > 0) begin
      $display ("%0d: %m.rl_tst_ext: Test %d ", cur_cycle, (rg_ip_num+1));
      if (verbosity > 1)
         $display ("   inPosit: 0x%08h", inPosit);
   end
endrule

rule rl_tst_init ((rg_tb_state == TST) && (rg_comp_state == INIT));
   let ext_out <- extracter.response.get();
   quire.init (ext_out);
   rg_comp_state <= READREQ;
   if (verbosity > 0) begin
      $display ("%0d: %m.rl_tst_init: Test %d ", cur_cycle, rg_ip_num);
      if (verbosity > 1)
         $display ("   ext_out1: ", fshow (ext_out));
   end
endrule

rule rl_tst_readreq ((rg_tb_state == TST) && (rg_comp_state == READREQ));
   quire.read_req;
   rg_comp_state <= READRSP;

   if (verbosity > 0) begin
      $display ("%0d: %m.rl_tst_readreq: Test %d ", cur_cycle, rg_ip_num);
   end
endrule

rule rl_tst_readrsp ((rg_tb_state == TST) && (rg_comp_state == READRSP));
   let o <- quire.read_rsp.get ();
   normalizer.request.put (o);            
   rg_comp_state <= NORM;

   if (verbosity > 0) begin
      $display ("%0d: %m.rl_tst_readrsp: Test %d ", cur_cycle, rg_ip_num);
      if (verbosity > 1)
         $display ("   quire out: ", fshow (o));
   end
endrule

rule rl_tst_norm ((rg_tb_state == TST) && (rg_comp_state == NORM));
   let norm_out <- normalizer.response.get ();
   let posit_in = posit_in_f.first; posit_in_f.deq;
   rg_comp_state <= EXT;

   Bool stop_condn = False;

`ifdef RANDOM
   // Stop condition
   stop_condn = (rg_ip_num == fromInteger (valueOf (Num_Tests)));
`else
   // Stop condition
   stop_condn = (rg_ip_num == 0);
`endif

   if (verbosity > 0) begin
      $display ("%0d: %m.rl_tst_norm: Test %d ", cur_cycle, rg_ip_num);
      if (verbosity > 1) begin
         $display ("   norm_out: ", fshow (norm_out));
         $display ("   posit_in: ", fshow (posit_in));
      end
   end

   // Stop the test or continue
   if (stop_condn) begin
      if (posit_in != norm_out.posit) fa_report_test_fail (rg_ip_num - 1);
      else if (rg_error) fa_report_test_fail (rg_ip_num - 1);
      else fa_report_test_pass;
      rg_tb_state <= STOP;
   end
   else begin
      if (posit_in != norm_out.posit) fa_report_test_fail (rg_ip_num - 1);
   end

endrule

`ifdef FPGA
(* always_ready *)
method Bool running = (rg_tb_state != STOP);
(* always_ready *)
method Bool test_pass = ((rg_tb_state == STOP) && !rg_error);
(* always_ready *)
method Bool test_fail = rg_error;
`endif
endmodule


endpackage
