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

package Posit_User_Types;
import Posit_Numeric_Types :: *;
   typedef enum {REGULAR, INF, ZERO} PositType deriving(Bits, Eq, FShow);
   typedef TAdd#(FracWidth,FracWidth)            FracWidthMul2       ;
   typedef TAdd#(FracWidth,1)                    FracWidthPlus1       ;
   typedef TAdd#(FracWidth,2)                    FracWidthPlus2       ;
   typedef TAdd#(FracWidth,3)                    FracWidthPlus3       ;
   typedef TAdd#(FracWidthMul2,1)               FracWidthMul2Plus1   ;//FracWidthMul2Plus1 = FracWidth*2 + 1
   typedef TAdd#(FracWidthMul2Plus1,1)            FracWidthPlus1Mul2   ;//FracWidthPlus1Mul2 = (FracWidth+1)*2
   typedef TAdd#(FracWidthPlus1Mul2,1)            FracWidthPlus1Mul2Plus1   ;//FracWidthPlus1Mul2 = (FracWidth+1)*2 + 1
   typedef TSub#(FracWidth,1)               FracWidthMinus1    ;//FracWidthMinus1 = FracWidth - 1
   typedef TLog#(FracWidth)               LogFracWidth      ;//LogFracWidth = log(FracWidth)
   typedef TAdd#(LogFracWidth,1)               LogFracWidthPlus1   ;//LogFracWidthPlus1 = LogFracWidth + 1
   typedef TAdd#(ExpWidth,1)                ExpWidthPlus1      ;//ExpWidthPlus1 = ExpWidth + 1      
   
   //MAC
   typedef TMul#(FracWidth,4)               FracWidthMul4       ;//FW*4 = FW4
   typedef TAdd#(ScaleWidthPlus1,1)            ScaleWidthPlus2    ;//ScaleWidth + 2
   typedef TAdd#(FracWidthMul4,1)               FracWidthMul4Plus1   ;//FW4+1
   typedef TAdd#(FracWidthMul4Plus1,1)            FracWidthMul4Plus2   ;//FW4+2
   typedef TSub#(FracWidthMul4,FracWidth)            FracWidthMul4MinusFracWidth;//FW4-FW
   typedef TSub#(FracWidthMul4MinusFracWidth,FracWidth)      FracWidthMul4MinusFracWidthMul2;//FWQ-2*FW
   typedef TSub#(FracWidthMul4MinusFracWidth,1)         FracWidthMul4MinusFracWidthMinus1;//FW4-FW-1
   typedef TAdd#(FracWidthMul4MinusFracWidth,1)         FracWidthMul4MinusFracWidthPlus1;//FW4-FW+1
   typedef TLog#(FracWidthMul4)               LogFracWidthMul4   ;//logFW4
   typedef TAdd#(LogFracWidthMul4,1)               LogFracWidthMul4Plus1   ;//logFW4+1   

   //FDP
   typedef TDiv#(TMul#(PositWidth,PositWidth),2)            QuireWidth;//QW = (PW^2)/2
   typedef TSub#(QuireWidth,2)                  QuireWidthMinus2;//QW-2
   typedef TDiv#(QuireWidth,2)                  QuireWidthBy2;//QW/2
   typedef TDiv#(PositWidth,2)                  PositWidthBy2;//PW/2
   typedef TSub#(QuireWidthBy2,PositWidthBy2)            FracWidthQuire    ;//FWQ = QW/2 - PW/2
   typedef TSub#(PositWidth,1)                  CarryWidthQuire ;//PW-1
   typedef FracWidthQuire                     IntWidthQuire;//= FWQ
   typedef TSub#(QuireWidth,1)                  QuireWidthMinus1;//QW-1
   typedef QuireWidthMinus1                   CarryWidthPlusIntWidthPlusFracWidthQuire;//QW-1= CWQ+FWQ+IWQ
   typedef TLog#(CarryWidthPlusIntWidthPlusFracWidthQuire)         LogCarryWidthPlusIntWidthPlusFracWidthQuire;//log(QW-1)
   typedef TSub#(FracWidthQuire,FracWidth)               FracWidthQuireMinusFracWidth;//FWQ-FW
   typedef TSub#(FracWidthQuireMinusFracWidth,FracWidth)         FracWidthQuireMinusFracWidthMul2;//FWQ-FracWidth*2
   typedef TAdd#(FracWidthQuire,IntWidthQuire)            IntWidthQuirePlusFracWidthQuire;//IWQ+FWQ

   //Q-TO-P
   typedef TAdd#(LogCarryWidthPlusIntWidthPlusFracWidthQuire,1)      LogCarryWidthPlusIntWidthPlusFracWidthQuirePlus1;
   typedef TAdd #(TLog #(QuireWidth), 1) LogQuireWidth;
   typedef TAdd#(CarryWidthQuire,IntWidthQuire)            CarryWidthPlusIntWidthQuire;
   typedef TLog#(CarryWidthPlusIntWidthQuire)  LogCarryWidthPlusIntWidthQuire;
   typedef   TSub#(QuireWidthMinus2,FracWidth)  QuireWidthMinus2MinusFracWidth;   
   typedef   TSub#(QuireWidthMinus2MinusFracWidth,1)          QuireWidthMinus3MinusFracWidth;

   //f-To-P
   typedef TSub#(TAdd#(FloatFracWidth,FloatExpWidth),1)          FloatExpoBegin; //(FloatFracWidth+FloatExpWidth-1)
   typedef TAdd#(FloatExpWidth,1)                   FloatExpWidthPlus1; //(FloatExpWidth+1)
   typedef TSub#(FloatFracWidth,1)                FloatFracWidthMinus1; //(FloatFracWidth-1)
   typedef TSub#(FloatFracWidth,FracWidth)               FloatFracWidthMinusFracWidth; //(FloatFracWidth-FracWidth)
   typedef TSub#(FloatFracWidthMinusFracWidth,1)            FloatFracWidthMinusFracWidthMinus1;
   typedef TSub#(FloatFracWidthMinusFracWidth,2)            FloatFracWidthMinusFracWidthMinus2;

   //P-to-F
   typedef TLog#(FloatFracWidth)                  LogFloatFracWidth;//LogFracWidth = log(FracWidth)
   typedef TAdd#(LogFloatFracWidth,1)               LogFloatFracWidthPlus1;//LogFloatFWPlus1 =LogFloatFW + 1
   typedef TSub#(FracWidth,FloatFracWidth)               FracWidthMinusFloatFracWidth; //(FracWidth-FloatFracWidth)
   typedef TAdd#(FloatFracWidth,1)                FloatFracWidthPlus1; //(FloatFracWidth-1)
   

   //Divider
   typedef  FracWidthMul2Plus1                  DividerQuotientBits;//Width of quotient at divider output 

        // Posit number
        typedef Bit #(PositWidth) Posit;

        // Quire number
        typedef Bit #(QuireWidth) Quire;

   typedef struct {Posit posit_inp1;
         Posit posit_inp2;
         } InputTwoPosit deriving(Bits,FShow);

   typedef struct {Posit posit_inp1;
         Posit posit_inp2;
         Posit posit_inp3;
         } InputThreePosit deriving(Bits,FShow);

   typedef struct {Quire quire_inp;
         Posit posit_inp1;
         Posit posit_inp2;
         } InputQuireTwoPosit deriving(Bits,FShow);


   //generic fuction to find Two's complement for any number
   function Bit#(n) twos_complement(Bit#(n) x);
      //truncate from log(n-1) bits to log(n-1)-1 bits
      return (truncate((1<<(valueOf(n)+1))-x)) ;   
   endfunction
/*
   function Bit #(TLog#(n)) msb_zeros_n (
      Bit #(n) in, Integer msb, Integer lsb);
      Bit #(TLog#(n)) num_z = 0;
      Integer wd = (msb - lsb) + 1;

      if (wd == 2) begin
         // end of recursion
         if (in[msb:lsb] == 0) return (extend (2'b10));
         else if (in [msb] == 1'b0) return (extend (2'b01));
         else return (extend (2'b00));
      end

      else begin
         // Align the input bits with the LSB
         Bit #(n) n_in = zeroExtend (in[msb:lsb]);

         Integer hmsb = wd - 1;
         Integer hlsb = wd / 2;
         Integer lmsb = hlsb - 1;
         let num_z_h = msb_zeros_n (in, hmsb, hlsb);
         let num_z_l = msb_zeros_n (in, lmsb, 0);

         if (in [hmsb:hlsb] == 0)
            num_z = num_z_h + num_z_l;
         else num_z = num_z_h;
         return (num_z);
      end
   endfunction

   function Bit #(10) msb_zeros_512 (Bit #(512) in);
      Bit #(10) num_z = 0;
      let num_z_h = msb_zeros_256 (in [511:256]);
      let num_z_l = msb_zeros_256 (in [255:0]);

      if (in [511:256] == 0)
         num_z = extend (num_z_h) + extend (num_z_l);
      else num_z = extend (num_z_h);
      return (num_z);
   endfunction

   function Bit #(9) msb_zeros_256 (Bit #(256) in);
      Bit #(9) num_z = 0;
      let num_z_h = msb_zeros_128 (in [255:128]);
      let num_z_l = msb_zeros_128 (in [127:0]);

      if (in [255:128] == 0)
         num_z = extend (num_z_h) + extend (num_z_l);
      else num_z = extend (num_z_h);
      return (num_z);
   endfunction

   function Bit #(8) msb_zeros_128 (Bit #(128) in);
      Bit #(8) num_z = 0;
      let num_z_h = msb_zeros_64 (in [127:64]);
      let num_z_l = msb_zeros_64 (in [63:0]);

      if (in [127:64] == 0)
         num_z = extend (num_z_h) + extend (num_z_l);
      else num_z = extend (num_z_h);
      return (num_z);
   endfunction

   function Bit #(7) msb_zeros_64 (Bit #(64) in);
      Bit #(7) num_z = 0;
      let num_z_h = msb_zeros_32 (in [63:32]);
      let num_z_l = msb_zeros_32 (in [31:0]);

      if (in [63:32] == 0)
         num_z = extend (num_z_h) + extend (num_z_l);
      else num_z = extend (num_z_h);
      return (num_z);
   endfunction

   function Bit #(6) msb_zeros_32 (Bit #(32) in);
      Bit #(6) num_z = 0;
      let num_z_h = msb_zeros_16 (in [31:16]);
      let num_z_l = msb_zeros_16 (in [15:0]);

      if (in [31:16] == 0)
         num_z = extend (num_z_h) + extend (num_z_l);
      else num_z = extend (num_z_h);
      return (num_z);
   endfunction

   function Bit #(5) msb_zeros_16 (Bit #(16) in);
      Bit #(5) num_z = 0;
      let num_z_h = msb_zeros_8 (in [15:8]);
      let num_z_l = msb_zeros_8 (in [7:0]);

      if (in [15:8] == 0) num_z = extend (num_z_h) + extend (num_z_l);
      else num_z = extend (num_z_h);
      return (num_z);
   endfunction

   function Bit #(4) msb_zeros_8 (Bit #(8) in);
      Bit #(4) num_z = 0;
      let num_z_h = msb_zeros_4 (in [7:4]);
      let num_z_l = msb_zeros_4 (in [3:0]);

      if (in [7:4] == 0) num_z = extend (num_z_h) + extend (num_z_l);
      else num_z = extend (num_z_h);
      return (num_z);
   endfunction

   function Bit #(3) msb_zeros_4 (Bit #(4) in);
      Bit #(3) num_z = 0;
      let num_z_h = msb_zeros_2 (in [3:2]);
      let num_z_l = msb_zeros_2 (in [1:0]);

      if (in [3:2] == 0) num_z = extend (num_z_h) + extend (num_z_l);
      else num_z = extend (num_z_h);
      return (num_z);
   endfunction

   function Bit #(2) msb_zeros_2 (Bit #(2) in);
      if (in == 0) return (2'b10);
      else if (in [1] == 1'b0) return (2'b01);
      else return (2'b00);
   endfunction

   function Bit#(LogQuireWidth) msb_zeros_quire (Bit #(QuireWidthMinus1) q);
  //  if (valueOf (PositWidth) == 32) begin
         Bit #(512) in = 0;
         in [511:(512 - valueOf(QuireWidthMinus1))] = q;
         return (msb_zeros_512 (in));
  //  end
      else if (valueOf (PositWidth) == 24) begin
         Bit #(512) in = 0;
         in [511:(512 - valueOf(QuireWidthMinus1))] = q;
         return (msb_zeros_512 (in));
      end
      else if (valueOf (PositWidth) == 16) begin
         Bit #(128) in = 0;
         in [127:(128 - valueOf(QuireWidthMinus1))] = q;
         return (msb_zeros_128 (in));
      end
      else if (valueOf (PositWidth) == 8) begin
         Bit #(32) in = 0;
         in [31:(32 - valueOf(QuireWidthMinus1))] = q;
         return (msb_zeros_32 (in));
      end
      else return (0); 
   endfunction 
   */

endpackage: Posit_User_Types
