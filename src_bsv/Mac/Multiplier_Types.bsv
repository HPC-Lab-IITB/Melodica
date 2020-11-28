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

package Multiplier_Types;

import GetPut       :: *;
import ClientServer :: *;
import FShow :: *;
import Posit_User_Types :: *;
import Posit_Numeric_Types :: *;

// Inputs_Mul is the input to the multiplier
typedef struct {Bit#(1) sign1;
		Bit#(1) nanflag1;
		PositType zero_infinity_flag1;
		Int#(ScaleWidthPlus1 ) scale1;
		Bit#(FracWidth ) frac1;
		Bit#(1) sign2;
		Bit#(1) nanflag2;
		PositType zero_infinity_flag2;
		Int#(ScaleWidthPlus1 ) scale2;
		Bit#(FracWidth ) frac2;} Inputs_Mul deriving(Bits,FShow);

// Input to second pipeline stage of multiplier
// Consists of zero flag, infinity flag, sign of posit, fraction, fracshift and only scale for 2 inputs
typedef struct {Bit#(1) nanflag;
		PositType ziflag;
		Bit#(1) sign;
		Int#(ScaleWidthPlus1 ) scale1;
		Int#(ScaleWidthPlus1 ) scale2;
		Bit#(FracWidthMul4Plus2) frac;
		Bit#(ScaleWidthPlus1) fracshift;} Stage0_m deriving(Bits,FShow);

// Multiplier output
typedef struct {Bit#(1) sign;
		PositType zero_infinity_flag;
		Bit#(1) nan_flag;
		Int#(ScaleWidthPlus1 ) scale;
		Bit#(FracWidthMul4) frac;
		Bit#(1) truncated_frac_msb;
		Bit#(1) truncated_frac_zero;
		} Outputs_Mul deriving(Bits,FShow);

interface Multiplier_IFC;
   interface Server #(Inputs_Mul,Outputs_Mul) inoutifc;
endinterface

endpackage: Multiplier_Types
