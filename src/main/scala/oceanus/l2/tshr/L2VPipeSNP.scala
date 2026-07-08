package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import oceanus.l2._
import oceanus.compactchi.CCHIComponent
import oceanus.chi.CHICohResps
import oceanus.chi.bundle._
import oceanus.chi.opcode._
import org.chipsalliance.cde.config.Parameters

class L2VPipeSNP(clientComponents: Seq[CCHIComponent], tshrId: Int = 0, nodeId: Int = 0)(implicit val p: Parameters)
    extends Module
    with CHIRNFOpcodesSNP
    with CHIRNFOpcodesRSP
    with CHIRNFOpcodesDAT
    with HasL2Params {

  val io = IO(new Bundle {
    val DnRXSNP = Flipped(Valid(new CHIBundleSNP))

    val DnTXRSP = Decoupled(new CHIBundleRSP)
    val DnTXDAT = Decoupled(new CHIBundleDAT)

    val tshr_paddr = Input(UInt(paramL2.physicalAddrWidth.W))
    val tshr_dirResult = Input(new L2Directory.MetaReadResult)

    val tshr_meta_write_en = Output(new L2Directory.MetaWriteMask)
    val tshr_meta_write_meta = Output(new L2Directory.Meta)

    val toSA = Output(new L2SnoopAgent.PathToSnoopAgent)
    val fromSA = Input(new L2SnoopAgent.PathFromSnoopAgent)

    val ds_read_en = Output(Bool())
    val ds_read_done = Input(Bool()) 

    val blockRBE = Output(new L2RBE.PathVPipeBlock)
    val free = Output(Bool())
  
    val EVT_active = Input(Bool())
  })

  // 1. RXSNP Opcode Decoder --------------------------------
  val rxsnp_opcode = Module(new SNPOpcodeDecoder(Seq(
    CHI_SnpOnce,
    CHI_SnpClean,
    CHI_SnpNotSharedDirty,
    CHI_SnpShared,
    CHI_SnpUnique,
    CHI_SnpPreferUnique,
    CHI_SnpCleanShared,
    CHI_SnpCleanInvalid,
    CHI_SnpMakeInvalid,
    CHI_SnpQuery
  ), paramEnableUnsupportedCheck = true))

  rxsnp_opcode.io.valid := io.DnRXSNP.fire
  rxsnp_opcode.io.opcode := io.DnRXSNP.bits.Opcode.get

  val rxsnp_query = io.DnRXSNP.bits.Opcode.get === CHI_SnpQuery.U
  val rxsnp_to_shared = rxsnp_opcode.is(
    CHI_SnpClean,
    CHI_SnpNotSharedDirty,
    CHI_SnpShared
  )
  val rxsnp_to_invalid = rxsnp_opcode.is(
    CHI_SnpUnique,
    CHI_SnpPreferUnique,
    CHI_SnpCleanInvalid,
    CHI_SnpMakeInvalid
  )
  val rxsnp_clean_shared = rxsnp_opcode.is(CHI_SnpCleanShared)
  val rxsnp_make_invalid = rxsnp_opcode.is(CHI_SnpMakeInvalid)

  val rxsnp_ret_to_src = io.DnRXSNP.bits.RetToSrc.get.asBool
  val rxsnp_local_valid = io.tshr_dirResult.state =/= L2Directory.MetaState.I
  val rxsnp_has_clients = io.tshr_dirResult.clients.asUInt.orR
  // TODO: confirm inclusive L1-L2, if non-inclusive, L1 could be valid
  val rxsnp_need_tx_data = !rxsnp_query && !rxsnp_make_invalid &&
                           ((rxsnp_ret_to_src && (rxsnp_local_valid || rxsnp_has_clients)) ||
                            io.tshr_dirResult.dirty)
  val rxsnp_need_local_ds_read = rxsnp_need_tx_data && rxsnp_local_valid &&
                                 (rxsnp_ret_to_src || io.tshr_dirResult.dirty)
  val rxsnp_need_meta_write = rxsnp_to_shared || rxsnp_to_invalid || rxsnp_clean_shared
  
  // Meta update 
  def metaToResp(state: UInt, dirty: Bool): UInt = {
    val resp = Wire(UInt(CHICohResps.WIDTH.W))
    resp := CHICohResps.I
    when (state === L2Directory.MetaState.S) {
      resp := Mux(dirty, CHICohResps.SD, CHICohResps.SC)
    }.elsewhen (state === L2Directory.MetaState.US || state === L2Directory.MetaState.UU) {
      resp := Mux(dirty, CHICohResps.UD, CHICohResps.UC)
    }
    resp
  }

  def passDirtyResp(state: UInt): UInt = {
    val resp = Wire(UInt(CHICohResps.WIDTH.W))
    resp := CHICohResps.I_PD
    when (state === L2Directory.MetaState.S) {
      resp := CHICohResps.SC_PD
    }.elsewhen (state === L2Directory.MetaState.US || state === L2Directory.MetaState.UU) {
      resp := CHICohResps.UC_PD
    }
    resp
  }

  val rxsnp_meta_write_en = WireDefault(0.U.asTypeOf(new L2Directory.MetaWriteMask))
  val rxsnp_meta_write_meta = WireDefault(0.U.asTypeOf(new L2Directory.Meta))

  rxsnp_meta_write_meta.state := io.tshr_dirResult.state
  rxsnp_meta_write_meta.dirty := io.tshr_dirResult.dirty
  rxsnp_meta_write_meta.clients.zip(io.tshr_dirResult.clients).foreach { case (dst, src) => dst := src }
  rxsnp_meta_write_meta.alias := io.tshr_dirResult.alias

  when (rxsnp_to_invalid) {
    rxsnp_meta_write_en.state := true.B
    rxsnp_meta_write_en.dirty := true.B
    rxsnp_meta_write_en.clients.foreach(_ := true.B)

    rxsnp_meta_write_meta.state := L2Directory.MetaState.I
    rxsnp_meta_write_meta.dirty := false.B
    rxsnp_meta_write_meta.clients.foreach(_ := false.B)
  }.elsewhen (rxsnp_to_shared) {
    rxsnp_meta_write_en.state := true.B
    rxsnp_meta_write_en.dirty := true.B

    rxsnp_meta_write_meta.state := Mux(
      // confirm EVT will not effect
      io.tshr_dirResult.state === L2Directory.MetaState.I,
      L2Directory.MetaState.I,
      L2Directory.MetaState.S
    )
    rxsnp_meta_write_meta.dirty := false.B
  }.elsewhen (rxsnp_clean_shared) {
    rxsnp_meta_write_en.dirty := true.B
    rxsnp_meta_write_meta.dirty := false.B
  }

  val current_resp = metaToResp(io.tshr_dirResult.state, io.tshr_dirResult.dirty)
  val rxsnp_make_clean_resp = metaToResp(rxsnp_meta_write_meta.state, false.B)
  val rxsnp_pass_dirty_resp = passDirtyResp(rxsnp_meta_write_meta.state)
  // ---------------------------------------------------------

  // 2. Interact with Snoop Agent signals --------------------
  io.toSA.SnpMakeInvalid := io.DnRXSNP.fire && rxsnp_opcode.is(
    CHI_SnpMakeInvalid
  )

  io.toSA.SnpToInvalid := io.DnRXSNP.fire && rxsnp_opcode.is(
    CHI_SnpUnique,
    CHI_SnpPreferUnique,
    CHI_SnpCleanInvalid
  )

  io.toSA.SnpToShared := io.DnRXSNP.fire && rxsnp_opcode.is(
    CHI_SnpClean,
    CHI_SnpNotSharedDirty,
    CHI_SnpShared
  )

  io.toSA.SnpToClean := io.DnRXSNP.fire && rxsnp_opcode.is(
    CHI_SnpOnce,
    CHI_SnpCleanShared
  )
  // ---------------------------------------------------------

  // 3. Private payload registers ----------------------------
  val p_rxsnp = RegInit(0.U.asTypeOf(new CHIBundleSNP))

  val p_rxsnp_need_tx_data = RegInit(false.B)
  val p_rxsnp_need_local_ds_read = RegInit(false.B)
  val p_rxsnp_need_meta_write = RegInit(false.B)
  val p_snpresp_resp = RegInit(0.U(CHICohResps.WIDTH.W))
  val p_snprespdata_resp = RegInit(0.U(CHICohResps.WIDTH.W))

  val p_meta_write_en = RegInit(0.U.asTypeOf(new L2Directory.MetaWriteMask))
  val p_meta_write_meta = RegInit(0.U.asTypeOf(new L2Directory.Meta))

  val p_make_clean_resp = RegInit(0.U(CHICohResps.WIDTH.W))
  val p_pass_dirty_resp = RegInit(0.U(CHICohResps.WIDTH.W))

  val p_rxsnp_query = p_rxsnp.Opcode.get === CHI_SnpQuery.U
  val p_rxsnp_make_invalid = p_rxsnp.Opcode.get === CHI_SnpMakeInvalid.U
  val p_rxsnp_to_shared = p_rxsnp.Opcode.get === CHI_SnpClean.U ||
                           p_rxsnp.Opcode.get === CHI_SnpNotSharedDirty.U ||
                           p_rxsnp.Opcode.get === CHI_SnpShared.U
  val p_rxsnp_to_invalid = p_rxsnp.Opcode.get === CHI_SnpUnique.U ||
                            p_rxsnp.Opcode.get === CHI_SnpPreferUnique.U ||
                            p_rxsnp.Opcode.get === CHI_SnpCleanInvalid.U ||
                            p_rxsnp.Opcode.get === CHI_SnpMakeInvalid.U
  val p_rxsnp_clean_shared = p_rxsnp.Opcode.get === CHI_SnpCleanShared.U
  // ---------------------------------------------------------

  // 4. State machine ----------------------------------------
  // interaction with Snoop Agent
  val w_snpresp0 = RegInit(false.B) // Waiting for response from Snoop Agent (DataID = 0)
  val w_snpresp2 = RegInit(false.B) // Waiting for response from Snoop Agent (DataID = 2)
  val s_snpcompack = RegInit(false.B) // Scheduling SnpCompAck to Snoop Agent

  // interaction with TSHR
  val s_ds_read = RegInit(false.B) // Scheduling Data Storage read to TSHR buffer
  val w_ds_read = RegInit(false.B) // Waiting for Data Storage read done
  val w_evt_done = RegInit(false.B) // Waiting for EVT writeback data/meta to become visible before SNP continues

  val s_dn_snpresp = RegInit(false.B) // Scheduling downstream TXRSP SnpResp
  val s_dn_snprespdata0 = RegInit(false.B) // Scheduling downstream TXDAT SnpRespData (DataID = 0)
  val s_dn_snprespdata2 = RegInit(false.B) // Scheduling downstream TXDAT SnpRespData (DataID = 2)

  val active = s_dn_snpresp || s_dn_snprespdata0 || s_dn_snprespdata2 || s_ds_read ||
               w_ds_read || w_snpresp0 || w_snpresp2 || w_evt_done || s_snpcompack

  // Treat this input as EVT_blocks_SNP_progress, not a generic EVT busy bit.
  val evt_blocks_snp_progress = io.EVT_active

  // Control signals -----------------------------------------
  val enter = io.DnRXSNP.fire
  val enter_query = enter && rxsnp_query
  val enter_send_sa = io.toSA.SnpMakeInvalid || io.toSA.SnpToInvalid || io.toSA.SnpToShared || io.toSA.SnpToClean

  val need_local_ds_read = Mux(enter, rxsnp_need_local_ds_read, p_rxsnp_need_local_ds_read)

  val sa_snpresp = io.fromSA.SnpResp && (enter_send_sa || (w_snpresp0 && w_snpresp2))
  val sa_data0 = io.fromSA.SnpRespData0 && w_snpresp0
  val sa_data2 = io.fromSA.SnpRespData2 && w_snpresp2
  val sa_data_full = !sa_snpresp && ((sa_data0 && !w_snpresp2) || (sa_data2 && !w_snpresp0))

  val enter_query_to_evt = enter_query && evt_blocks_snp_progress
  val enter_query_to_rsp = enter_query && !evt_blocks_snp_progress

  val sa_no_data_to_evt = sa_snpresp && evt_blocks_snp_progress
  val sa_no_data_to_ds = sa_snpresp && !evt_blocks_snp_progress && need_local_ds_read
  val sa_no_data_to_rsp = sa_snpresp && !evt_blocks_snp_progress && !need_local_ds_read

  val sa_data_to_evt = sa_data_full && evt_blocks_snp_progress
  val sa_data_to_rsp = sa_data_full && !evt_blocks_snp_progress && !p_rxsnp_need_tx_data
  val sa_data_to_dat = sa_data_full && !evt_blocks_snp_progress && p_rxsnp_need_tx_data

  val evt_done = w_evt_done && !evt_blocks_snp_progress
  val evt_done_update_rxsnp_need_tx_data = !p_rxsnp_query && !p_rxsnp_make_invalid && 
                                           (p_rxsnp_need_tx_data || io.tshr_dirResult.dirty)
  val evt_done_to_rsp = evt_done && !evt_done_update_rxsnp_need_tx_data
  val evt_done_to_dat = evt_done && evt_done_update_rxsnp_need_tx_data

  val ds_read_blocked_by_evt = s_ds_read && evt_blocks_snp_progress
  val ds_read_issue = s_ds_read && !evt_blocks_snp_progress
  val ds_read_done_fast = ds_read_issue && io.ds_read_done
  val ds_read_wait = ds_read_issue && !io.ds_read_done
  val ds_wait_blocked_by_evt = w_ds_read && evt_blocks_snp_progress
  val ds_wait_done = w_ds_read && !evt_blocks_snp_progress && io.ds_read_done

  val dn_txrsp_fire = s_dn_snpresp && io.DnTXRSP.fire
  val dn_txdat0_fire = s_dn_snprespdata0 && io.DnTXDAT.fire
  val dn_txdat2_fire = s_dn_snprespdata2 && io.DnTXDAT.fire
  // TODO: confirm meta write time point
  val meta_write = dn_txrsp_fire || dn_txdat0_fire
  val snp_side_effect_pending = w_snpresp0 || w_snpresp2 || w_evt_done || s_ds_read || w_ds_read ||
                                s_dn_snpresp || s_dn_snprespdata0 || s_dn_snprespdata2
  val snpcompack_fire = s_snpcompack && !snp_side_effect_pending

  // State updates
  when (enter) {
    p_rxsnp := io.DnRXSNP.bits
    p_rxsnp_need_tx_data := rxsnp_need_tx_data
    p_rxsnp_need_local_ds_read := rxsnp_need_local_ds_read
    p_rxsnp_need_meta_write := rxsnp_need_meta_write

    p_meta_write_en := rxsnp_meta_write_en
    p_meta_write_meta := rxsnp_meta_write_meta
  }

  when (enter) {
    p_snpresp_resp := MuxCase(current_resp, Seq(
      rxsnp_query -> current_resp,
      rxsnp_to_invalid -> CHICohResps.I,
      rxsnp_to_shared -> rxsnp_make_clean_resp,
      rxsnp_clean_shared -> rxsnp_make_clean_resp
    ))
    p_snprespdata_resp := MuxCase(current_resp, Seq(
      rxsnp_to_invalid -> Mux(io.tshr_dirResult.dirty, CHICohResps.I_PD, CHICohResps.I),
      rxsnp_to_shared -> Mux(io.tshr_dirResult.dirty, rxsnp_pass_dirty_resp, rxsnp_make_clean_resp),
      rxsnp_clean_shared -> Mux(io.tshr_dirResult.dirty, rxsnp_pass_dirty_resp, rxsnp_make_clean_resp)
    ))

    p_make_clean_resp := rxsnp_make_clean_resp
    p_pass_dirty_resp := rxsnp_pass_dirty_resp
  }.elsewhen (evt_done) {
    p_snpresp_resp := MuxCase(current_resp, Seq(
      p_rxsnp_to_invalid -> CHICohResps.I,
      p_rxsnp_to_shared -> p_make_clean_resp,
      p_rxsnp_clean_shared -> p_make_clean_resp
    ))
    p_snprespdata_resp := MuxCase(current_resp, Seq(
      p_rxsnp_to_invalid -> Mux(io.tshr_dirResult.dirty, CHICohResps.I_PD, CHICohResps.I),
      p_rxsnp_to_shared -> Mux(io.tshr_dirResult.dirty, p_pass_dirty_resp, p_make_clean_resp),
      p_rxsnp_clean_shared -> Mux(io.tshr_dirResult.dirty, p_pass_dirty_resp, p_make_clean_resp)
    ))
  }

  when (enter_send_sa && !sa_snpresp) {
    w_snpresp0 := true.B
    w_snpresp2 := true.B
  }

  when (io.toSA.SnpToClean) {
    s_snpcompack := true.B
  }

  when (enter_query_to_evt || sa_no_data_to_evt || sa_data_to_evt || ds_read_blocked_by_evt || ds_wait_blocked_by_evt) {
    w_evt_done := true.B
  }

  when (enter_query_to_rsp || sa_no_data_to_rsp || sa_data_to_rsp || evt_done_to_rsp) {
    s_dn_snpresp := true.B
  }

  when (sa_no_data_to_ds && !ds_read_done_fast) {
    s_ds_read := true.B
  }.elsewhen (ds_read_blocked_by_evt || ds_read_issue) {
    s_ds_read := false.B
  }

  when (sa_data_to_dat || evt_done_to_dat || ds_read_done_fast || ds_wait_done) {
    s_dn_snprespdata0 := true.B
  }

  when (sa_snpresp) {
    w_snpresp0 := false.B
    w_snpresp2 := false.B
  }

  when (sa_data0) {
    w_snpresp0 := false.B
  }

  when (sa_data2) {
    w_snpresp2 := false.B
  }

  when (evt_done) {
    w_evt_done := false.B
  }

  when (ds_read_wait) {
    w_ds_read := true.B
  }

  when (ds_wait_blocked_by_evt || ds_wait_done) {
    w_ds_read := false.B
  }

  when (dn_txdat0_fire) {
    s_dn_snprespdata0 := false.B
    s_dn_snprespdata2 := true.B
  }

  when (dn_txdat2_fire) {
    s_dn_snprespdata2 := false.B
  }

  when (dn_txrsp_fire) {
    s_dn_snpresp := false.B
  }

  when (snpcompack_fire) {
    s_snpcompack := false.B
  }

  io.toSA.SnpCompAck := snpcompack_fire
  // ---------------------------------------------------------

  // 5. Send DnTXRSP & DnTXDAT -------------------------------
  io.DnTXRSP.valid := s_dn_snpresp
  io.DnTXRSP.bits := DontCare
  io.DnTXRSP.bits.QoS.get := p_rxsnp.QoS.get
  io.DnTXRSP.bits.TgtID.get := p_rxsnp.SrcID.get
  io.DnTXRSP.bits.SrcID.get := nodeId.U
  io.DnTXRSP.bits.TxnID.get := p_rxsnp.TxnID.get
  io.DnTXRSP.bits.Opcode.get := CHI_SnpResp.U
  io.DnTXRSP.bits.RespErr.get := 0.U
  io.DnTXRSP.bits.Resp.get := p_snpresp_resp
  io.DnTXRSP.bits.FwdState_DataPull.foreach(_ := 0.U)
  io.DnTXRSP.bits.CBusy.foreach(_ := 0.U)
  io.DnTXRSP.bits.DBID_PGroupID_StashGroupID_TagGroupID.foreach(_ := 0.U)
  io.DnTXRSP.bits.PCrdType.get := 0.U
  io.DnTXRSP.bits.TagOp.foreach(_ := 0.U)
  io.DnTXRSP.bits.TraceTag.get := p_rxsnp.TraceTag.get

  io.DnTXDAT.valid := s_dn_snprespdata0 || s_dn_snprespdata2
  io.DnTXDAT.bits := DontCare
  io.DnTXDAT.bits.QoS.get := p_rxsnp.QoS.get
  io.DnTXDAT.bits.TgtID.get := p_rxsnp.SrcID.get
  io.DnTXDAT.bits.SrcID.get := nodeId.U
  io.DnTXDAT.bits.TxnID.get := p_rxsnp.TxnID.get
  io.DnTXDAT.bits.HomeNID.get := p_rxsnp.SrcID.get
  io.DnTXDAT.bits.Opcode.get := CHI_SnpRespData.U
  io.DnTXDAT.bits.RespErr.get := 0.U
  io.DnTXDAT.bits.Resp.get := p_snprespdata_resp
  io.DnTXDAT.bits.FwdState_DataPull_DataSource.foreach(_ := 0.U)
  io.DnTXDAT.bits.CBusy.foreach(_ := 0.U)
  io.DnTXDAT.bits.DBID.get := p_rxsnp.TxnID.get
  io.DnTXDAT.bits.CCID.get := 0.U
  io.DnTXDAT.bits.DataID.get := Mux(s_dn_snprespdata0, 0.U, 2.U) 
  io.DnTXDAT.bits.TagOp.foreach(_ := 0.U)
  io.DnTXDAT.bits.Tag.foreach(_ := 0.U)
  io.DnTXDAT.bits.TU.foreach(_ := 0.U)
  io.DnTXDAT.bits.TraceTag.get := p_rxsnp.TraceTag.get
  io.DnTXDAT.bits.RSVDC.foreach(_ := 0.U)
  io.DnTXDAT.bits.BE.get := Fill(paramCHI.datBEWidth, true.B)
  // TODO: fill from TSHR buffer in the outer TXDAT mux.
  io.DnTXDAT.bits.Data.get := 0.U(paramCHI.datDataWidth.W) 
  io.DnTXDAT.bits.DataCheck.foreach(_ := 0.U)
  io.DnTXDAT.bits.Poison.foreach(_ := 0.U)
  // ---------------------------------------------------------

  // 6. Other IO signals -------------------------------------
  io.tshr_meta_write_en := Mux(meta_write, p_meta_write_en, 0.U.asTypeOf(new L2Directory.MetaWriteMask))
  io.tshr_meta_write_meta := p_meta_write_meta
  io.ds_read_en := s_ds_read && !evt_blocks_snp_progress

  io.blockRBE.EVT := s_dn_snpresp || s_dn_snprespdata0 || s_dn_snprespdata2
  io.blockRBE.SNP := active 
  io.blockRBE.REQ := active
  io.free := !active
  // ---------------------------------------------------------

  // 7. assertion --------------------------------------------

  // ---------------------------------------------------------
}
