package IntDivide_generic;

import FIFO                :: *;
import GetPut              :: *;
import ClientServer        :: *;
import Posit_User_Types    :: *;
import Posit_Numeric_Types :: *;
import Utils               :: *;

typedef FracWidthPlus1 Denominator;
typedef DividerQuotientBits Quotient;
typedef TAdd#(TSub#(Quotient,1),Denominator) Numerator;
typedef Denominator Remainder;

                   //         numerator            denominator
typedef Server #(  Tuple2 #(Bit #(Denominator), Bit #(Denominator))
                   //         numerator      frac_msb  frac_zero
                 , Tuple3 #(Bit #(Quotient), Bit #(1), Bit #(1))) IntDivide_IFC;

typedef enum {Div_START, Div_LOOP1, Div_LOOP2, Div_DONE} DivState
   deriving (Eq, Bits, FShow);


module mkIntDivide #(Bit #(2) verbosity) (IntDivide_IFC);

   FIFO #(Tuple3 #(Bit #(Quotient), Bit #(1), Bit #(1))) ffo <- mkFIFO;

   Reg #(DivState)  rg_state     <- mkReg (Div_START);
   Reg #(Bit #(Numerator))  rg_numer    <- mkRegU;
   Reg #(Bit #(Denominator))  rg_denom    <- mkRegU;
   Reg #(Bit #(Numerator))  rg_denom2    <- mkRegU;
   Reg #(Bit #(Quotient))  rg_n         <- mkRegU;
   Reg #(Bit #(Quotient))  rg_quo       <- mkRegU;

   rule rl_loop1 (rg_state == Div_LOOP1);
      if (rg_denom2 <= (rg_numer >> 1)) begin
         rg_denom2 <= rg_denom2 << 1;
         rg_n <= rg_n << 1;
      end

      else rg_state <= Div_LOOP2;
   endrule

   rule rl_loop2 (rg_state == Div_LOOP2);
      if (rg_numer < zeroExtend(rg_denom)) begin
         rg_state <= Div_DONE;
         let quo = rg_quo;
         let rem = rg_numer;
         Bit #(Remainder) rem_truncate= truncate(rem);
         Bit #(1) trunc_frac_msb = msb(rem_truncate);
         Bit #(1) trunc_frac_zero = pack((rem_truncate<<1) == 0);
         ffo.enq (tuple3 (quo, trunc_frac_msb, trunc_frac_zero));
         if (verbosity > 1) begin
            $display ("%0d: %m: rl_loop2", cur_cycle);
         end
      end

      else if (rg_numer >= rg_denom2) begin
         rg_numer <= rg_numer - rg_denom2;
         rg_quo <= rg_quo + rg_n;
      end

      else begin
         rg_denom2 <= rg_denom2 >> 1;
         rg_n <= rg_n >> 1;
      end

      if (verbosity > 1) begin
         $display ("%0d: %m: rl_loop2 ", cur_cycle);
         $display ("   rg_numer %b",rg_numer);
         $display ("   rg_denom %b",rg_denom);
         $display ("   rg_quo %h",rg_quo);
      end
   endrule

   interface Put request;
      method Action put (Tuple2 #(Bit #(Denominator), Bit #(Denominator)) p)
         if ((rg_state == Div_START) || (rg_state == Div_DONE));
         match {.numer, .denom} = p;

         // divide by zero
         if (denom == 0) begin
            let trunc_frac_msb = 1'b0; let trunc_frac_zero = 1'b1;
            rg_quo <= '1;
            rg_state <= Div_DONE;
            ffo.enq (tuple3 (rg_quo, trunc_frac_msb, trunc_frac_zero));
            
            if (verbosity > 1) begin
               $display ("%0d: %m: request: ", cur_cycle);
               $display ("   Divide by zero ");
            end
         end

         else begin
            rg_numer    <= {numer, '0};
            rg_denom    <= denom;
            rg_denom2   <= zeroExtend(denom);
            rg_quo      <= 0;
            rg_n        <= 1;
            rg_state    <= Div_LOOP1;

            if (verbosity > 1) begin
               $display ("%0d: %m: request: ", cur_cycle);
               $display ("   rg_numer %b",rg_numer);
               $display ("   rg_denom %b",rg_denom);
               $display ("   rg_quo %h",rg_quo);
            end
         end
      endmethod
   endinterface
   interface Get response = toGet (ffo);
endmodule


endpackage: IntDivide_generic
