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

package PNE;

// -----------------------------------------------------------------
// This package defines:
//
//    The different artefacts your package defines. One per line
//    with a small description per line, please.
//
// -----------------------------------------------------------------

import ClientServer     :: *;
import GetPut           :: *;
import FIFO             :: *;
import Extracter_Types	:: *;
import Extracter	:: *;
import Normalizer_Types	:: *;
import Normalizer	:: *;
import Adder_Types 	:: *;
import Adder		:: *;
import Multiplier_Types	:: *;
import Multiplier	:: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Utils :: *;
typedef 3 Pipe_Depth;      // Estimated pipeline depth of the PNE
interface PNE ;
   interface Server #(InputThreePosit,Normalized_Posit) compute;
endinterface

typedef enum {RDY, MUL} MAC_State deriving (Bits, Eq, FShow);


//
// Module definition
module mkPNE (PNE );
   // 2-element FIFO to hold the 3rd operand temporarily to save an extractor
   FIFO #(Posit) ff_posit_op_3 <- mkFIFO;
   Reg #(MAC_State)  rg_state <- mkReg (RDY);

   // Extractors
   Extracter_IFC  extracter1 <- mkExtracter;
   Extracter_IFC  extracter2 <- mkExtracter;

   Multiplier_IFC  multiplier <- mkMultiplier;
   Adder_IFC  adder <- mkAdder;

   Normalizer_IFC   normalizer <- mkNormalizer;

   rule rl_multiply;
      // extracter outputs for the multiplier operands
      let extOut1 <- extracter1.inoutifc.response.get();
      let extOut2 <- extracter2.inoutifc.response.get();

      // extract the third operand
      extracter1.inoutifc.request.put (ff_posit_op_3.first);
      ff_posit_op_3.deq;

      rg_state <= RDY;

      // the fraction and scale are extended since operation is on quire. using signed extension
      // for scale value. fraction value is zero extended but also shifted to make the MSB the
      // highest valued fraction bit
      multiplier.inoutifc.request.put (Inputs_Mul {
           sign1: extOut1.sign,
           nanflag1: 1'b0,
           zero_infinity_flag1: extOut1.zero_infinity_flag ,
           scale1 : extOut1.scale,
           frac1 : extOut1.frac,
           sign2: extOut2.sign,
           nanflag2: 1'b0,
           zero_infinity_flag2: extOut2.zero_infinity_flag ,
           scale2 : extOut2.scale,
           frac2 : extOut2.frac});

   endrule

   rule rl_accumulate;
      let mulOut <- multiplier.inoutifc.response.get();
      let extOut3 <- extracter1.inoutifc.response.get();
      adder.inoutifc.request.put (Inputs_a {
           sign1: mulOut.sign,
           nanflag1: mulOut.nan_flag,
           zero_infinity_flag1: mulOut.zero_infinity_flag ,
           scale1 : mulOut.scale,
           frac1 : mulOut.frac,
           round_frac_f1 : mulOut.truncated_frac_msb | ~mulOut.truncated_frac_zero,
           sign2: extOut3.sign,
           nanflag2: 1'b0,
           zero_infinity_flag2: extOut3.zero_infinity_flag ,
           scale2 : extOut3.scale,
           frac2 : extOut3.frac,
           round_frac_f2 : 1'b0});
   endrule

   rule rl_normalize;
      let addOut <- adder.inoutifc.response.get();
      normalizer.inoutifc.request.put (Prenorm_Posit {
           sign: addOut.sign,
           zero_infinity_flag: addOut.zero_infinity_flag ,
           nan_flag: addOut.nan_flag,
           scale :  pack(addOut.scale),
           frac : addOut.frac,
           truncated_frac_msb : addOut.truncated_frac_msb,
           truncated_frac_zero : addOut.truncated_frac_zero});
   endrule

//truncated_frac_msb :value of MSB lost to see if its more than or less than half
//truncated_frac_zero : check if res of  the frac bits are zero to check equidistance

   /*rule rl_out;
      let normOut <- normalizer.inoutifc.response.get ();
      ffO.enq(normOut);
   endrule*/


interface Server compute;
   interface Put request;
      method Action put (InputThreePosit p) if (rg_state == RDY);
	 extracter1.inoutifc.request.put (p.posit_inp1);
	 extracter2.inoutifc.request.put (p.posit_inp2);
	 ff_posit_op_3.enq(p.posit_inp3);
         rg_state <= MUL;
      endmethod
   endinterface
   interface Get response = normalizer.inoutifc.response;
endinterface


endmodule

(* synthesize *)

module mkPNE_test (PNE );
   let _ifc <- mkPNE;
   return (_ifc);
endmodule

endpackage

// -----------------------------------------------------------------


