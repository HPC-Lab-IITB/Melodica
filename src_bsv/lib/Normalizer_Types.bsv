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

package Normalizer_Types;

import GetPut       :: *;
import ClientServer :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;

// Prenorm_Posit is the input to the normalizer as received from a compute pipeline
// Value consists of sign of posit, zero and infinity flag, NaN flag, scale bits, fraction field
// of the fraction bits that were truncated due to size reduction, truncated_frac_msb gives the
// MSB of the truncated bits and truncated_frac_zero checks if all the other bits (other than
// msb) of the trucated fraction are zero or not: if all bits are zero the truncated_frac_zero
// is 1

typedef struct {Bit#(1) sign;
		PositType zero_infinity_flag;
		Bit#(1) nan_flag;
		Bit#(ScaleWidthPlus1) scale;
		Bit#(FracWidth) frac;
		Bit#(1) truncated_frac_msb;
		Bit#(1) truncated_frac_zero;} Prenorm_Posit deriving(Bits,FShow);


// --------
// Following struct definitions are for a deeply pipelined extracter
// Stage0_n consists of sign of posit, zero and infinity flag, NaN flag, regime field, exponent
// field, fraction field, number of bits in regime field
typedef struct {Bit#(1) sign;
		PositType zero_infinity_flag;
		Bit#(1) nan_flag;
		Bit#(PositWidthMinus1) k;
		Bit#(ExpWidth) expo;
		Bit#(FracWidth) frac;
		Bit#(1) truncated_frac_msb;
		Bit#(1) truncated_frac_zero;} Stage0_n deriving(Bits,FShow);

// Stage1_n is the data available at the end of second pipeline. Stage1_n consists of sign of
// posit, zero and infinity flag, NaN flag, regime and exponent field combined, fraction field,
// number of bits in regime field
typedef struct {Bit#(1) sign;
		PositType zero_infinity_flag;
		Bit#(1) nan_flag;
		Bit#(PositWidthMinus1) k_expo;
		Bit#(FracWidth) frac;
		UInt#(BitsPerPositWidth) shift_1;
		Bit#(1) flag_endcase;
		Bit#(1) truncated_frac_msb;
		Bit#(1) truncated_frac_zero;} Stage1_n deriving(Bits,FShow);

// Stage2_n is the data available at the end of second pipeline. Stage2_n consists of sign of
// posit, zero and infinity flag, NaN flag, regime and exponent and fraction field combined
typedef struct {Bit#(1) sign;
		PositType zero_infinity_flag;
		Bit#(1) nan_flag;
		Bit#(PositWidthMinus1) k_expo_frac;
		Bool rounding;} Stage2_n deriving(Bits,FShow);

// --------
// Normalized_Posit is the output of the normalizer
typedef struct {Bit#(1) nan_flag;
		Bit#(PositWidth) out_posit;
		PositType zero_infinity_flag;
		Bool rounding;} Normalized_Posit deriving(Bits,FShow);

interface Normalizer_IFC;
   interface Server #(Prenorm_Posit, Normalized_Posit) inoutifc;
endinterface

endpackage: Normalizer_Types
