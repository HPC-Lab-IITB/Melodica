// This package instantiates the Coproc version of Positcore to be
// used as a RoCC accelerator.

package XMelodica;

import GetPut           :: *;
import ClientServer     :: *;
import DefaultValue     :: *;
import RoccIfc          :: *;
import FIFOF            :: *;
import Clocks           :: *;
import PositCore_Coproc :: *;

typedef enum {IDLE, RSP, ERR} XDummyState deriving (Bits, Eq, FShow);

module mkXMelodica (Rocc_IFC #(xlen, flen, coreMaxAddrBits, paddrBits))
   provisos (
      Max#(xlen,flen, coreDataBits),
      Div#(coreDataBits, 8, coreDataBytes),
      // per request of bsc
      Div#(TMax#(xlen, flen), 8, coreDataBytes),
      Add#(a__, xlen, 64),
      Add#(b__, 32, xlen)
   );

   Wire #(Bool)                     w_cmd_ready             <- mkDWire(True);
   Wire #(Bool)                     w_cmd_valid             <- mkBypassWire;
   Wire #(Bit #(7))                 w_cmd_bits_inst_funct   <- mkBypassWire;
   Wire #(Bit #(5))                 w_cmd_bits_inst_rs2     <- mkBypassWire;
   Wire #(Bit #(5))                 w_cmd_bits_inst_rs1     <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_inst_xd      <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_inst_xs1     <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_inst_xs2     <- mkBypassWire;
   Wire #(Bit #(5))                 w_cmd_bits_inst_rd      <- mkBypassWire;
   Wire #(Bit #(7))                 w_cmd_bits_inst_opcode  <- mkBypassWire;
   Wire #(Bit #(xlen))              w_cmd_bits_rs1          <- mkBypassWire;
   Wire #(Bit #(xlen))              w_cmd_bits_rs2          <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_debug <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_cease <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_wfi   <- mkBypassWire;
   Wire #(Bit #(32))                w_cmd_bits_status_isa   <- mkBypassWire;
   Wire #(Bit #(PRV_SZ))            w_cmd_bits_status_dprv  <- mkBypassWire;
   Wire #(Bit #(PRV_SZ))            w_cmd_bits_status_prv   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_sd    <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_zero2 <- mkBypassWire;
   Wire #(Bit #(2))                 w_cmd_bits_status_sxl   <- mkBypassWire;
   Wire #(Bit #(2))                 w_cmd_bits_status_uxl   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_sd_rv32<- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_zero1 <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_tsr   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_tw    <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_tvm   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_mxr   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_sum   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_mprv  <- mkBypassWire;
   Wire #(Bit #(2))                 w_cmd_bits_status_xs    <- mkBypassWire;
   Wire #(Bit #(2))                 w_cmd_bits_status_fs    <- mkBypassWire;
   Wire #(Bit #(2))                 w_cmd_bits_status_vs    <- mkBypassWire;
   Wire #(Bit #(2))                 w_cmd_bits_status_mpp   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_spp   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_mpie  <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_hpie  <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_spie  <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_upie  <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_mie   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_hie   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_sie   <- mkBypassWire;
   Wire #(Bit #(1))                 w_cmd_bits_status_uie   <- mkBypassWire;
   Wire #(Bit #(1))                 w_resp_ready            <- mkBypassWire;
   Wire #(Bit #(1))                 w_resp_valid            <- mkDWire(0);
   Wire #(Bit #(5))                 w_resp_bits_rd          <- mkDWire(?);
   Wire #(Bit #(xlen))              w_resp_bits_data        <- mkDWire(?);
   Wire #(Bit #(1))                 w_mem_req_ready         <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_req_valid         <- mkDWire(0);
   Wire #(Bit #(coreMaxAddrBits))   w_mem_req_bits_addr     <- mkDWire(?);
   Wire #(Bit #(ZdcacheReqTagBits)) w_mem_req_bits_tag      <- mkDWire(?);
   Wire #(Bit #(M_SZ))              w_mem_req_bits_cmd      <- mkDWire(?);
   Wire#(Bit#(Zmem_req_bits_size_width)) w_mem_req_bits_size <- mkDWire(?);
   Wire #(Bit #(1))                 w_mem_req_bits_signed   <- mkDWire(?);
   Wire #(Bit #(1))                 w_mem_req_bits_phys     <- mkDWire(?);
   Wire #(Bit #(1))                 w_mem_req_bits_no_alloc <- mkDWire(?);
   Wire #(Bit #(1))                 w_mem_req_bits_no_xcpt  <- mkDWire(?);
   Wire #(Bit #(2))                 w_mem_req_bits_dprv     <- mkDWire(?);
   Wire #(Bit #(coreDataBits))      w_mem_req_bits_data     <- mkDWire(?);
   Wire #(Bit #(coreDataBytes))     w_mem_req_bits_mask     <- mkDWire(?);
   Wire #(Bit #(1))                 w_mem_s1_kill           <- mkDWire(0);
   Wire #(Bit #(coreDataBits))      w_mem_s1_data_data      <- mkDWire(?);
   Wire #(Bit #(coreDataBytes))     w_mem_s1_data_mask      <- mkDWire(?);
   Wire #(Bit #(1))                 w_mem_s2_nack           <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_s2_nack_cause_raw <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_s2_kill           <- mkDWire(0);
   Wire #(Bit #(1))                 w_mem_s2_uncached       <- mkBypassWire;
   Wire #(Bit #(paddrBits))         w_mem_s2_paddr          <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_resp_valid        <- mkBypassWire;
   Wire #(Bit #(coreMaxAddrBits))   w_mem_resp_bits_addr    <- mkBypassWire;
   Wire #(Bit #(ZdcacheReqTagBits)) w_mem_resp_bits_tag     <- mkBypassWire;
   Wire #(Bit #(M_SZ))              w_mem_resp_bits_cmd     <- mkBypassWire;
   Wire#(Bit#(Zmem_req_bits_size_width)) w_mem_resp_bits_size <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_resp_bits_signed  <- mkBypassWire;
   Wire #(Bit #(coreDataBits))      w_mem_resp_bits_data    <- mkBypassWire;
   Wire #(Bit #(coreDataBytes))     w_mem_resp_bits_mask    <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_resp_bits_replay  <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_resp_bits_has_data<- mkBypassWire;
   Wire#(Bit#(coreDataBits)) w_mem_resp_bits_data_word_bypass <- mkBypassWire;
   Wire#(Bit#(coreDataBits)) w_mem_resp_bits_data_raw <- mkBypassWire;
   Wire#(Bit#(coreDataBits)) w_mem_resp_bits_store_data <- mkBypassWire;
   Wire #(Bit #(2))                 w_mem_resp_bits_dprv    <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_replay_next       <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_s2_xcpt_ma_ld     <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_s2_xcpt_ma_st     <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_s2_xcpt_pf_ld     <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_s2_xcpt_pf_st     <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_s2_xcpt_ae_ld     <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_s2_xcpt_ae_st     <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_ordered           <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_acquire      <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_release      <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_grant        <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_tlbMiss      <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_blocked      <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_canAcceptStoreThenLoad <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_canAcceptStoreThenRMW <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_canAcceptLoadThenLoad <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_storeBufferEmptyAfterLoad <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_perf_storeBufferEmptyAfterStore <- mkBypassWire;
   Wire #(Bit #(1))                 w_mem_keep_clock_enabled<- mkDWire(0);
   Wire #(Bit #(1))                 w_mem_clock_enabled     <- mkBypassWire;
   Wire #(Bit #(1))                 w_busy                  <- mkDWire(0);
   Wire #(Bit #(1))                 w_interrupt             <- mkDWire(0);
   Wire #(Bit #(1))                 w_exception             <- mkBypassWire;
   Wire #(Bit #(1))                 w_fpu_req_ready         <- mkBypassWire;
   Wire #(Bit #(1))                 w_fpu_req_valid         <- mkDWire(0);
   Wire #(Bit #(1))                 w_fpu_req_bits_ldst     <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_wen      <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_ren1     <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_ren2     <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_ren3     <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_swap12   <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_swap23   <- mkDWire(?);
   Wire #(Bit #(2))                 w_fpu_req_bits_typeTagIn<- mkDWire(?);
   Wire #(Bit #(2))                 w_fpu_req_bits_typeTagOut<- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_fromint  <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_toint    <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_fastpipe <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_fma      <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_div      <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_sqrt     <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_req_bits_wflags   <- mkDWire(?);
   Wire #(Bit #(RM_SZ))             w_fpu_req_bits_rm       <- mkDWire(?);
   Wire #(Bit #(2))                 w_fpu_req_bits_fmaCmd   <- mkDWire(?);
   Wire #(Bit #(2))                 w_fpu_req_bits_typ      <- mkDWire(?);
   Wire #(Bit #(2))                 w_fpu_req_bits_fmt      <- mkDWire(?);
   Wire #(Bit #(TAdd#(flen,1)))     w_fpu_req_bits_in1      <- mkDWire(?);
   Wire #(Bit #(TAdd#(flen,1)))     w_fpu_req_bits_in2      <- mkDWire(?);
   Wire #(Bit #(TAdd#(flen,1)))     w_fpu_req_bits_in3      <- mkDWire(?);
   Wire #(Bit #(1))                 w_fpu_resp_ready        <- mkDWire(0);
   Wire #(Bit #(1))                 w_fpu_resp_valid        <- mkBypassWire;
   Wire #(Bit #(TAdd#(flen,1)))     w_fpu_resp_bits_data    <- mkBypassWire;
   Wire #(Bit #(FLAGS_SZ))          w_fpu_resp_bits_exc     <- mkBypassWire;

   FIFOF#(Bit #(5))                 f_rd                    <- mkFIFOF;

   Reg  #(XDummyState)              r_state                 <- mkReg (IDLE);
   Reg  #(Bool)                     r_busy                  <- mkReg(False);
   FIFOF#(Resp # (xlen, flen, coreMaxAddrBits, paddrBits)) f_resp <- mkFIFOF;

   PositCore_IFC core <- mkPositCore;

   // ---- Accelerator operations ----
   // Assuming one-hot encodings of the funct7 field since we have
   // fewer than seven instcutions in this accelerator
   let do_op_init_quire          = (w_cmd_bits_inst_funct[0] == 1);
   let do_op_read_quire          = (w_cmd_bits_inst_funct[1] == 1);  // xd=1
   let do_op_fma_quire           = (w_cmd_bits_inst_funct[2] == 1);
   let do_op_fms_quire           = (w_cmd_bits_inst_funct[3] == 1);
   let do_op_conv_quire_to_posit = (w_cmd_bits_inst_funct[4] == 1);  // xd=1

   rule rl_new_cmd ((r_state == IDLE) && w_cmd_valid && w_cmd_ready);
      let nstate = IDLE;
      if (w_cmd_bits_inst_xd == 1) begin
         nstate = RSP;
         f_rd.enq (w_cmd_bits_inst_rd);
         r_busy <= True;
      end

      Bit #(64) rs1_val = extend (w_cmd_bits_rs1);
      Bit #(64) rs2_val = extend (w_cmd_bits_rs2);

      // Based on the operation send args and commands to the
      // accelerator's "core" 
      FloatU arg1 = tagged G rs1_val;
      FloatU arg2 = tagged G rs2_val;
      if (do_op_fma_quire || do_op_fms_quire) begin
         arg1 = tagged P truncate (rs1_val);
         arg2 = tagged P truncate (rs2_val);
      end
      PositCmds cmd = ?;

      if (do_op_init_quire)
         cmd = WRITE_Q;

      else if (do_op_read_quire)
         cmd = READ_Q;

      else if (do_op_fma_quire)
         cmd = FMA_P;

      else if (do_op_fms_quire)
         cmd = FMS_P;

      else if (do_op_conv_quire_to_posit)
         cmd = FCVT_P_R;

      else nstate = ERR;

      if (nstate != ERR)
         core.server_core.request.put (tuple4 (arg1, arg2, ?, cmd));

      r_state <= nstate;
   endrule

   rule rl_rsp_enq (r_state == RSP);
      // response from accelerator. Ignore the exception flags.
      let r <- core.server_core.response.get ();
      f_rd.deq;
      Resp #(xlen, flen, coreMaxAddrBits, paddrBits) rsp = defaultValue;
      rsp.rd = f_rd.first;
      if (tpl_1 (r) matches tagged P .posit)
         rsp.data = extend (posit);
      else if (tpl_1 (r) matches tagged G .general)
         rsp.data = truncate (general);
      f_resp.enq (rsp);
      r_state <= IDLE;
   endrule

   rule rl_rsp_drv;
      let rsp = f_resp.first;
      w_resp_bits_rd <= rsp.rd;
      w_resp_bits_data <= rsp.data;
   endrule

   rule rl_rsp_deq (w_resp_ready == 1'b1);
      r_state <= IDLE;
      r_busy <= False; f_resp.deq;
   endrule

   // Interface
   // Command/Request methods
   method rocc_cmd_ready = pack (w_cmd_ready);
   method Action rocc_cmd_valid (Bit #(1) x);
      w_cmd_valid <= unpack (x);
   endmethod 

   method rocc_cmd_bits_inst_funct = w_cmd_bits_inst_funct._write;
   method rocc_cmd_bits_inst_rs2 = w_cmd_bits_inst_rs2._write;
   method rocc_cmd_bits_inst_rs1 = w_cmd_bits_inst_rs1._write;
   method rocc_cmd_bits_inst_xd = w_cmd_bits_inst_xd._write;
   method rocc_cmd_bits_inst_xs1 = w_cmd_bits_inst_xs1._write;
   method rocc_cmd_bits_inst_xs2 = w_cmd_bits_inst_xs2._write;
   method rocc_cmd_bits_inst_rd = w_cmd_bits_inst_rd._write;
   method rocc_cmd_bits_inst_opcode = w_cmd_bits_inst_opcode._write;
   method rocc_cmd_bits_rs1 = w_cmd_bits_rs1._write;
   method rocc_cmd_bits_rs2 = w_cmd_bits_rs2._write;
   method rocc_cmd_bits_status_debug = w_cmd_bits_status_debug._write;
   method rocc_cmd_bits_status_cease = w_cmd_bits_status_cease._write;
   method rocc_cmd_bits_status_wfi = w_cmd_bits_status_wfi._write;
   method rocc_cmd_bits_status_isa = w_cmd_bits_status_isa._write;
   method rocc_cmd_bits_status_dprv = w_cmd_bits_status_dprv._write;
   method rocc_cmd_bits_status_prv = w_cmd_bits_status_prv._write;
   method rocc_cmd_bits_status_sd = w_cmd_bits_status_sd._write;
   method rocc_cmd_bits_status_zero2 = w_cmd_bits_status_zero2._write;
   method rocc_cmd_bits_status_sxl = w_cmd_bits_status_sxl._write;
   method rocc_cmd_bits_status_uxl = w_cmd_bits_status_uxl._write;
   method rocc_cmd_bits_status_sd_rv32 = w_cmd_bits_status_sd_rv32._write;
   method rocc_cmd_bits_status_zero1 = w_cmd_bits_status_zero1._write;
   method rocc_cmd_bits_status_tsr = w_cmd_bits_status_tsr._write;
   method rocc_cmd_bits_status_tw = w_cmd_bits_status_tw._write;
   method rocc_cmd_bits_status_tvm = w_cmd_bits_status_tvm._write;
   method rocc_cmd_bits_status_mxr = w_cmd_bits_status_mxr._write;
   method rocc_cmd_bits_status_sum = w_cmd_bits_status_sum._write;
   method rocc_cmd_bits_status_mprv = w_cmd_bits_status_mprv._write;
   method rocc_cmd_bits_status_xs = w_cmd_bits_status_xs._write;
   method rocc_cmd_bits_status_fs = w_cmd_bits_status_fs._write;
   method rocc_cmd_bits_status_vs = w_cmd_bits_status_vs._write;
   method rocc_cmd_bits_status_mpp = w_cmd_bits_status_mpp._write;
   method rocc_cmd_bits_status_spp = w_cmd_bits_status_spp._write;
   method rocc_cmd_bits_status_mpie = w_cmd_bits_status_mpie._write;
   method rocc_cmd_bits_status_hpie = w_cmd_bits_status_hpie._write;
   method rocc_cmd_bits_status_spie = w_cmd_bits_status_spie._write;
   method rocc_cmd_bits_status_upie = w_cmd_bits_status_upie._write;
   method rocc_cmd_bits_status_mie = w_cmd_bits_status_mie._write;
   method rocc_cmd_bits_status_hie = w_cmd_bits_status_hie._write;
   method rocc_cmd_bits_status_sie = w_cmd_bits_status_sie._write;
   method rocc_cmd_bits_status_uie = w_cmd_bits_status_uie._write;
    

   // Response signals
   method rocc_resp_bits_rd   = w_resp_bits_rd;
   method rocc_resp_bits_data = w_resp_bits_data;
   method rocc_resp_valid = pack (f_resp.notEmpty);
   method rocc_resp_ready = w_resp_ready._write;

   // Other signals
   method rocc_busy = pack (r_busy);

   // Other unused signals
   method rocc_mem_req_ready = w_mem_req_ready._write;
   method rocc_mem_req_valid = w_mem_req_valid._read;
   method rocc_mem_req_bits_addr = w_mem_req_bits_addr._read;
   method rocc_mem_req_bits_tag = w_mem_req_bits_tag._read;
   method rocc_mem_req_bits_cmd = w_mem_req_bits_cmd._read;
   method rocc_mem_req_bits_size = w_mem_req_bits_size._read;
   method rocc_mem_req_bits_signed = w_mem_req_bits_signed._read;
   method rocc_mem_req_bits_phys = w_mem_req_bits_phys._read;
   method rocc_mem_req_bits_no_alloc = w_mem_req_bits_no_alloc._read;
   method rocc_mem_req_bits_no_xcpt = w_mem_req_bits_no_xcpt._read;
   method rocc_mem_req_bits_dprv = w_mem_req_bits_dprv._read;
   method rocc_mem_req_bits_data = w_mem_req_bits_data._read;
   method rocc_mem_req_bits_mask = w_mem_req_bits_mask._read;
   method rocc_mem_s1_kill = w_mem_s1_kill._read;
   method rocc_mem_s1_data_data = w_mem_s1_data_data._read;
   method rocc_mem_s1_data_mask = w_mem_s1_data_mask._read;
   method rocc_mem_s2_nack = w_mem_s2_nack._write;
   method rocc_mem_s2_nack_cause_raw = w_mem_s2_nack_cause_raw._write;
   method rocc_mem_s2_kill = w_mem_s2_kill._read;
   method rocc_mem_s2_uncached = w_mem_s2_uncached._write;
   method rocc_mem_s2_paddr = w_mem_s2_paddr._write;
   method rocc_mem_resp_valid = w_mem_resp_valid._write;
   method rocc_mem_resp_bits_addr = w_mem_resp_bits_addr._write;
   method rocc_mem_resp_bits_tag = w_mem_resp_bits_tag._write;
   method rocc_mem_resp_bits_cmd = w_mem_resp_bits_cmd._write;
   method rocc_mem_resp_bits_size = w_mem_resp_bits_size._write;
   method rocc_mem_resp_bits_signed = w_mem_resp_bits_signed._write;
   method rocc_mem_resp_bits_data = w_mem_resp_bits_data._write;
   method rocc_mem_resp_bits_mask = w_mem_resp_bits_mask._write;
   method rocc_mem_resp_bits_replay = w_mem_resp_bits_replay._write;
   method rocc_mem_resp_bits_has_data = w_mem_resp_bits_has_data._write;
   method rocc_mem_resp_bits_data_word_bypass = w_mem_resp_bits_data_word_bypass._write;
   method rocc_mem_resp_bits_data_raw = w_mem_resp_bits_data_raw._write;
   method rocc_mem_resp_bits_store_data = w_mem_resp_bits_store_data._write;
   method rocc_mem_resp_bits_dprv = w_mem_resp_bits_dprv._write;
   method rocc_mem_replay_next = w_mem_replay_next._write;
   method rocc_mem_s2_xcpt_ma_ld = w_mem_s2_xcpt_ma_ld._write;
   method rocc_mem_s2_xcpt_ma_st = w_mem_s2_xcpt_ma_st._write;
   method rocc_mem_s2_xcpt_pf_ld = w_mem_s2_xcpt_pf_ld._write;
   method rocc_mem_s2_xcpt_pf_st = w_mem_s2_xcpt_pf_st._write;
   method rocc_mem_s2_xcpt_ae_ld = w_mem_s2_xcpt_ae_ld._write;
   method rocc_mem_s2_xcpt_ae_st = w_mem_s2_xcpt_ae_st._write;
   method rocc_mem_ordered = w_mem_ordered._write;
   method rocc_mem_perf_acquire = w_mem_perf_acquire._write;
   method rocc_mem_perf_release = w_mem_perf_release._write;
   method rocc_mem_perf_grant = w_mem_perf_grant._write;
   method rocc_mem_perf_tlbMiss = w_mem_perf_tlbMiss._write;
   method rocc_mem_perf_blocked = w_mem_perf_blocked._write;
   method rocc_mem_perf_canAcceptStoreThenLoad = w_mem_perf_canAcceptStoreThenLoad._write;
   method rocc_mem_perf_canAcceptStoreThenRMW = w_mem_perf_canAcceptStoreThenRMW._write;
   method rocc_mem_perf_canAcceptLoadThenLoad = w_mem_perf_canAcceptLoadThenLoad._write;
   method rocc_mem_perf_storeBufferEmptyAfterLoad = w_mem_perf_storeBufferEmptyAfterLoad._write;
   method rocc_mem_perf_storeBufferEmptyAfterStore = w_mem_perf_storeBufferEmptyAfterStore._write;
   method rocc_mem_keep_clock_enabled = w_mem_keep_clock_enabled._read;
   method rocc_mem_clock_enabled = w_mem_clock_enabled._write;
   method rocc_interrupt = w_interrupt;
   method rocc_exception = w_exception._write;
   method rocc_fpu_req_ready = w_fpu_req_ready._write;
   method rocc_fpu_req_valid = w_fpu_req_valid._read;
   method rocc_fpu_req_bits_ldst = w_fpu_req_bits_ldst._read;
   method rocc_fpu_req_bits_wen = w_fpu_req_bits_wen._read;
   method rocc_fpu_req_bits_ren1 = w_fpu_req_bits_ren1._read;
   method rocc_fpu_req_bits_ren2 = w_fpu_req_bits_ren2._read;
   method rocc_fpu_req_bits_ren3 = w_fpu_req_bits_ren3._read;
   method rocc_fpu_req_bits_swap12 = w_fpu_req_bits_swap12._read;
   method rocc_fpu_req_bits_swap23 = w_fpu_req_bits_swap23._read;
   method rocc_fpu_req_bits_typeTagIn = w_fpu_req_bits_typeTagIn._read;
   method rocc_fpu_req_bits_typeTagOut = w_fpu_req_bits_typeTagOut._read;
   method rocc_fpu_req_bits_fromint = w_fpu_req_bits_fromint._read;
   method rocc_fpu_req_bits_toint = w_fpu_req_bits_toint._read;
   method rocc_fpu_req_bits_fastpipe = w_fpu_req_bits_fastpipe._read;
   method rocc_fpu_req_bits_fma = w_fpu_req_bits_fma._read;
   method rocc_fpu_req_bits_div = w_fpu_req_bits_div._read;
   method rocc_fpu_req_bits_sqrt = w_fpu_req_bits_sqrt._read;
   method rocc_fpu_req_bits_wflags = w_fpu_req_bits_wflags._read;
   method rocc_fpu_req_bits_rm = w_fpu_req_bits_rm._read;
   method rocc_fpu_req_bits_fmaCmd = w_fpu_req_bits_fmaCmd._read;
   method rocc_fpu_req_bits_typ = w_fpu_req_bits_typ._read;
   method rocc_fpu_req_bits_fmt = w_fpu_req_bits_fmt._read;
   method rocc_fpu_req_bits_in1 = w_fpu_req_bits_in1._read;
   method rocc_fpu_req_bits_in2 = w_fpu_req_bits_in2._read;
   method rocc_fpu_req_bits_in3 = w_fpu_req_bits_in3._read;
   method rocc_fpu_resp_ready = w_fpu_resp_ready._read;
   method rocc_fpu_resp_valid = w_fpu_resp_valid._write;
   method rocc_fpu_resp_bits_data = w_fpu_resp_bits_data._write;
   method rocc_fpu_resp_bits_exc = w_fpu_resp_bits_exc._write;
endmodule

(* synthesize *)
(* clock_prefix="clock", reset_prefix="reset" *)
module mkXMelodica_64_64_40_40 (Rocc_IFC#(64, 64, 40, 40));
   (* hide *)
   let reset <- exposeCurrentReset;
   let rst_n <- mkResetInverter(reset);
   let _ifc <- mkXMelodica(reset_by rst_n);
   return _ifc;
endmodule

endpackage
