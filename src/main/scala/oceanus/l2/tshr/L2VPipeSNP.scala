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

  // -- FSM State ------------------------------------------
  val s_meta_write = RegInit(false.B) // Scheduling meta write
  val s_home_resp0 = RegInit(false.B) // Scheduling downstream TXDAT SnpRespData (DataID = 0)
  val s_home_resp2 = RegInit(false.B) // Scheduling downstream TXDAT SnpRespData (DataID = 2)
  val s_dct_resp0 = RegInit(false.B) // Scheduling downstream TXDAT SnpRespData (DataID = 0)
  val s_dct_resp2 = RegInit(false.B) // Scheduling downstream TXDAT SnpRespData (DataID = 2)
  val s_snpcompack = RegInit(false.B) // Scheduling SnpCompAck to Snoop Agent

  val w_snpresp0 = RegInit(false.B) // Waiting for response from Snoop Agent (DataID = 0)
  val w_snpresp2 = RegInit(false.B) // Waiting for response from Snoop Agent (DataID = 2)
  val w_ds_read = RegInit(false.B) // Waiting for Data Storage read done
  val active = s_home_resp0 || s_home_resp2 || s_dct_resp0 || s_dct_resp2 ||
               s_meta_write || w_ds_read || w_snpresp0 || w_snpresp2 || s_snpcompack

  val p_rxsnp = RegInit(0.U.asTypeOf(new CHIBundleSNP))
  val p_fwd_state = RegInit(0.U(CHICohResps.WIDTH.W))
  val p_txdat_dataID = RegInit(0.U(paramCHI.datDataIDWidth.W))
  val p_home_txrsp_valid = RegInit(false.B)
  val p_home_txdat_valid = RegInit(false.B)
  val p_home_txrsp_resp = RegInit(0.U(CHICohResps.WIDTH.W))
  val p_home_txdat_resp = RegInit(0.U(CHICohResps.WIDTH.W))

  val p_dct_txdat_valid = RegInit(false.B)
  val p_dct_txdat_resp = RegInit(0.U(CHICohResps.WIDTH.W))
  val p_rxsnp_need_dct_txdat = RegInit(false.B)
  // --------------------------------------------------------

  // -- RXSNP Opcode Decoder --------------------------------
  // TODO: SnoopOnce is toSC now
  val rxsnp_opcode = Module(new SNPOpcodeDecoder(Seq(
    CHI_SnpShared,
    CHI_SnpClean,
    CHI_SnpOnce,
    CHI_SnpNotSharedDirty,
    CHI_SnpUnique,
    CHI_SnpPreferUnique,
    CHI_SnpCleanShared,
    CHI_SnpCleanInvalid,
    CHI_SnpMakeInvalid,
    CHI_SnpSharedFwd,
    CHI_SnpCleanFwd,
    CHI_SnpOnceFwd,
    CHI_SnpNotSharedDirtyFwd,
    CHI_SnpUniqueFwd,
    CHI_SnpPreferUniqueFwd,
    // CHI_SnpUniqueStash,
    // CHI_SnpMakeInvalidStash,
    // CHI_SnpStashUnique,
    // CHI_SnpStashShared,
    CHI_SnpQuery,
    // CHI_SnpDVMOp
    
  ), paramEnableUnsupportedCheck = true))

  rxsnp_opcode.io.valid := io.DnRXSNP.fire || active
  rxsnp_opcode.io.opcode := Mux(io.DnRXSNP.fire, io.DnRXSNP.bits.Opcode.get, p_rxsnp.Opcode.get)

  val rxsnp_to_shared = rxsnp_opcode.is(
    CHI_SnpClean,
    CHI_SnpNotSharedDirty,
    CHI_SnpShared,
    CHI_SnpCleanShared
  )
  val rxsnp_to_invalid = rxsnp_opcode.is(
    CHI_SnpUnique,
    CHI_SnpPreferUnique,
    CHI_SnpCleanInvalid,
    CHI_SnpMakeInvalid
  )
  val rxsnp_query = rxsnp_opcode.is(CHI_SnpQuery)
  val rxsnp_make_invalid = rxsnp_opcode.is(CHI_SnpMakeInvalid)
  val rxsnp_fwd = rxsnp_opcode.is(
    CHI_SnpSharedFwd,
    CHI_SnpCleanFwd,
    CHI_SnpOnceFwd,
    CHI_SnpNotSharedDirtyFwd,
    CHI_SnpUniqueFwd,
    CHI_SnpPreferUniqueFwd
  )
  val rxsnp_fwd_once = rxsnp_opcode.is(CHI_SnpOnceFwd)
  val rxsnp_fwd_cleanFwd = rxsnp_opcode.is(CHI_SnpCleanFwd)
  val rxsnp_fwd_uniqueFwd = rxsnp_opcode.is(CHI_SnpUniqueFwd)
  val rxsnp_fwd_sharedFwd = rxsnp_opcode.is(CHI_SnpSharedFwd)
  val rxsnp_fwd_prefetchUniqueFwd = rxsnp_opcode.is(CHI_SnpPreferUniqueFwd)
  val rxsnp_fwd_notSharedDirtyFwd = rxsnp_opcode.is(CHI_SnpNotSharedDirtyFwd)
  val rxsnp_fwd_to_shared = rxsnp_opcode.is(
    CHI_SnpSharedFwd,
    CHI_SnpCleanFwd,
    CHI_SnpOnceFwd,
    CHI_SnpNotSharedDirtyFwd,
  )
  val rxsnp_fwd_to_invalid = rxsnp_opcode.is(
    CHI_SnpUniqueFwd,
    CHI_SnpPreferUniqueFwd
  )

  def metaToResp(state: UInt, dirty: Bool, passDirty: Bool): UInt = {
    val resp = Wire(UInt(CHICohResps.WIDTH.W))
    resp := Mux(passDirty, CHICohResps.I_PD, CHICohResps.I)
    when (state === L2Directory.MetaState.S) {
      resp := Mux(passDirty, CHICohResps.SC_PD, Mux(dirty, CHICohResps.SD, CHICohResps.SC))
    }.elsewhen (state === L2Directory.MetaState.US || state === L2Directory.MetaState.UU) {
      resp := Mux(passDirty, CHICohResps.UC_PD, Mux(dirty, CHICohResps.UD, CHICohResps.UC))
    }
    resp
  }

  val rxsnp_meta_write_en = WireDefault(0.U.asTypeOf(new L2Directory.MetaWriteMask))
  val rxsnp_meta_write_meta = WireDefault(0.U.asTypeOf(new L2Directory.Meta))

  val meta_write_invalid = rxsnp_to_invalid || rxsnp_fwd_to_invalid
  val meta_write_shared = rxsnp_to_shared || rxsnp_fwd_to_shared

  rxsnp_meta_write_en.state := meta_write_invalid || meta_write_shared
  rxsnp_meta_write_en.dirty := meta_write_invalid || meta_write_shared
  rxsnp_meta_write_en.clients.foreach(_ := meta_write_invalid)

  rxsnp_meta_write_meta.state := MuxCase(io.tshr_dirResult.state, Seq(
    meta_write_invalid -> L2Directory.MetaState.I,
    meta_write_shared -> Mux(io.tshr_dirResult.state === L2Directory.MetaState.I, 
                             L2Directory.MetaState.I, L2Directory.MetaState.S)
  ))
  rxsnp_meta_write_meta.dirty := MuxCase(io.tshr_dirResult.dirty, Seq(
    meta_write_invalid -> false.B,
    meta_write_shared -> false.B
  ))
  // rxsnp_meta_write_meta.clients only when to invalid
  rxsnp_meta_write_meta.clients.foreach(_ := false.B)
  rxsnp_meta_write_meta.alias := io.tshr_dirResult.alias

  val current_resp = metaToResp(io.tshr_dirResult.state, io.tshr_dirResult.dirty, false.B)
  val make_clean_resp = metaToResp(rxsnp_meta_write_meta.state, false.B, false.B)
  val pass_dirty_resp = metaToResp(rxsnp_meta_write_meta.state, false.B, true.B)

  val fwd_resp_state = MuxCase(CHICohResps.I, Seq(
    rxsnp_fwd_once -> CHICohResps.I,
    rxsnp_fwd_cleanFwd -> CHICohResps.SC,
    rxsnp_fwd_sharedFwd -> CHICohResps.SC,
    rxsnp_fwd_notSharedDirtyFwd -> CHICohResps.SC,
    rxsnp_fwd_uniqueFwd -> Mux(io.tshr_dirResult.dirty, CHICohResps.UD_PD, CHICohResps.UC),
    rxsnp_fwd_prefetchUniqueFwd -> Mux(io.tshr_dirResult.dirty, CHICohResps.UD_PD, CHICohResps.UC)
  ))
  val home_resp_state = MuxCase(current_resp, Seq(
    rxsnp_query -> current_resp,
    meta_write_invalid -> CHICohResps.I,
    meta_write_shared -> make_clean_resp
  ))
  val home_data_resp_state = MuxCase(current_resp, Seq(
    meta_write_invalid -> Mux(io.tshr_dirResult.dirty, CHICohResps.I_PD, CHICohResps.I),
    meta_write_shared -> Mux(io.tshr_dirResult.dirty, pass_dirty_resp, make_clean_resp)
  ))
  // ---------------------------------------------------------

  // -- State Transition ----------------------------------------
  // Control signals
  val enter_send_sa_uop = io.toSA.SnpMakeInvalid || io.toSA.SnpToInvalid || io.toSA.SnpToShared || io.toSA.SnpToClean

  // we can decide rxsnp_need_local_ds_read at enter because:
  // io.tshr_dirResult.dirty 0->1: dirty data come from upstream and stay in tshr buffer
  // io.tshr_dirResult.dirty 1->0: impossible
  val enter_need_local_ds_read = io.DnRXSNP.fire && !io.ds_read_done && io.tshr_dirResult.state =/= L2Directory.MetaState.I &&
                                 (rxsnp_fwd || (!rxsnp_fwd && (io.DnRXSNP.bits.RetToSrc.get.asBool || io.tshr_dirResult.dirty) && 
                                 !rxsnp_query && !rxsnp_make_invalid))
  val enter_need_meta_write = io.DnRXSNP.fire && (meta_write_shared || meta_write_invalid)
  val need_home_txdat = (!rxsnp_fwd && (p_rxsnp.RetToSrc.get.asBool || io.tshr_dirResult.dirty) && !rxsnp_make_invalid && 
                        !rxsnp_query && io.tshr_dirResult.state =/= L2Directory.MetaState.I) ||
                        (rxsnp_fwd && (p_rxsnp.RetToSrc.get.asBool || io.tshr_dirResult.dirty) && !rxsnp_fwd_uniqueFwd && 
                        !rxsnp_fwd_prefetchUniqueFwd && io.tshr_dirResult.state =/= L2Directory.MetaState.I)
  val enter_need_dct_txdat = io.DnRXSNP.fire && rxsnp_fwd &&
                             io.tshr_dirResult.state =/= L2Directory.MetaState.I
  val need_dct_txdat = p_rxsnp_need_dct_txdat
  val no_tx_inflight = !p_home_txrsp_valid && !p_home_txdat_valid && !p_dct_txdat_valid

  val fire_home_data0 = io.DnTXDAT.fire && p_txdat_dataID === 0.U && p_home_txdat_valid
  val fire_home_data2 = io.DnTXDAT.fire && p_txdat_dataID === 2.U && p_home_txdat_valid
  val fire_dct_data0 = io.DnTXDAT.fire && p_txdat_dataID === 0.U && p_dct_txdat_valid
  val fire_dct_data2 = io.DnTXDAT.fire && p_txdat_dataID === 2.U && p_dct_txdat_valid
  
  val do_home_snpresp = s_home_resp0 && s_home_resp2 && !w_snpresp0 && !w_snpresp2 && !w_ds_read && 
                        !io.EVT_active && !need_home_txdat && no_tx_inflight 
  val do_home_snprespdata0 = s_home_resp0 && !w_snpresp0 && !w_snpresp2 && !w_ds_read &&
                             !io.EVT_active && need_home_txdat && no_tx_inflight
  val do_dct_snprespdata0 = s_dct_resp0 && !s_home_resp0 && !s_home_resp2 &&
                            !w_snpresp0 && !w_snpresp2 && !w_ds_read && need_dct_txdat && no_tx_inflight

  val do_meta_write = s_meta_write && (io.DnTXRSP.fire || fire_home_data0)
  val do_sa_snpcompack = s_snpcompack && !(w_snpresp0 || w_snpresp2 || w_ds_read || s_home_resp0 || s_home_resp2 ||
                         s_dct_resp0 || s_dct_resp2)


  val block_vpipe_evt = do_home_snpresp || do_home_snprespdata0 || p_home_txdat_valid || p_home_txrsp_valid || 
                        do_dct_snprespdata0 || p_dct_txdat_valid

  // State update
  when (io.DnRXSNP.fire) {
    p_rxsnp := io.DnRXSNP.bits
    p_rxsnp_need_dct_txdat := enter_need_dct_txdat

    s_meta_write := enter_need_meta_write
    s_home_resp0 := true.B
    s_home_resp2 := true.B
    s_dct_resp0 := enter_need_dct_txdat
    s_dct_resp2 := enter_need_dct_txdat
    s_snpcompack := io.toSA.SnpToClean

    w_ds_read := enter_need_local_ds_read && !io.ds_read_done
    w_snpresp0 := enter_send_sa_uop && !io.fromSA.SnpResp
    w_snpresp2 := enter_send_sa_uop && !io.fromSA.SnpResp
  }

  when (io.ds_read_done) {
    w_ds_read := false.B
  }

  when (io.fromSA.SnpRespData0) {
    w_snpresp0 := false.B
  }

  when (io.fromSA.SnpRespData2) {
    w_snpresp2 := false.B
  }

  when (io.fromSA.SnpResp && !io.DnRXSNP.fire) {
    w_snpresp0 := false.B
    w_snpresp2 := false.B
  }

  when (io.DnTXRSP.fire) {
    s_home_resp0 := false.B
    s_home_resp2 := false.B
    p_home_txrsp_valid := false.B
  }

  when (fire_home_data0) {
    s_home_resp0 := false.B
    p_txdat_dataID := 2.U
  }

  when (fire_home_data2) {
    s_home_resp2 := false.B
    p_home_txdat_valid := false.B
  }

  when (fire_dct_data0) {
    s_dct_resp0 := false.B
    p_txdat_dataID := 2.U
  }

  when (fire_dct_data2) {
    s_dct_resp2 := false.B
    p_dct_txdat_valid := false.B
  }

  when (do_meta_write) {
    s_meta_write := false.B
  }

  when (io.toSA.SnpCompAck) {
    s_snpcompack := false.B
  }

  when (do_home_snpresp) {
    p_home_txrsp_valid := true.B
    p_home_txrsp_resp := home_resp_state
    p_fwd_state := Mux(need_dct_txdat, fwd_resp_state, 0.U)
  }

  when (do_home_snprespdata0) {
    p_dct_txdat_valid := false.B
    p_home_txdat_valid := true.B
    p_home_txdat_resp := home_data_resp_state
    p_txdat_dataID := 0.U
    p_fwd_state := Mux(need_dct_txdat, fwd_resp_state, 0.U)
  }

  when (do_dct_snprespdata0) {
    p_dct_txdat_valid := true.B
    p_dct_txdat_resp := p_fwd_state
    p_txdat_dataID := 0.U
    p_fwd_state := 0.U
  }
  // ---------------------------------------------------------

  // -- Interact with Snoop Agent signals --------------------
  io.toSA.SnpMakeInvalid := io.DnRXSNP.fire && rxsnp_opcode.is(
    CHI_SnpMakeInvalid
  )

  io.toSA.SnpToInvalid := io.DnRXSNP.fire && rxsnp_opcode.is(
    CHI_SnpUnique,
    CHI_SnpPreferUnique,
    CHI_SnpCleanInvalid,
    CHI_SnpUniqueFwd,
    CHI_SnpPreferUniqueFwd
  )

  io.toSA.SnpToShared := io.DnRXSNP.fire && rxsnp_opcode.is(
    CHI_SnpClean,
    CHI_SnpNotSharedDirty,
    CHI_SnpShared,
    CHI_SnpOnce,
    CHI_SnpSharedFwd,
    CHI_SnpCleanFwd,
    CHI_SnpOnceFwd,
    CHI_SnpNotSharedDirtyFwd,
  )

  io.toSA.SnpToClean := io.DnRXSNP.fire && rxsnp_opcode.is(
    CHI_SnpCleanShared
  )

  io.toSA.SnpCompAck := do_sa_snpcompack
  // ---------------------------------------------------------

  // -- Send DnTXRSP & DnTXDAT -------------------------------
  io.DnTXRSP.valid := p_home_txrsp_valid
  io.DnTXRSP.bits := DontCare
  io.DnTXRSP.bits.QoS.get := p_rxsnp.QoS.get
  io.DnTXRSP.bits.TgtID.get := p_rxsnp.SrcID.get
  io.DnTXRSP.bits.SrcID.get := nodeId.U
  io.DnTXRSP.bits.TxnID.get := p_rxsnp.TxnID.get
  io.DnTXRSP.bits.Opcode.get := Mux(need_dct_txdat, CHI_SnpRespFwded.U, CHI_SnpResp.U)
  io.DnTXRSP.bits.RespErr.get := 0.U
  io.DnTXRSP.bits.Resp.get := p_home_txrsp_resp
  io.DnTXRSP.bits.FwdState(p_fwd_state)
  // io.DnTXRSP.bits.DataPull(0.U)
  io.DnTXRSP.bits.CBusy.foreach(_ := 0.U)
  io.DnTXRSP.bits.DBID_PGroupID_StashGroupID_TagGroupID.foreach(_ := 0.U)
  io.DnTXRSP.bits.PCrdType.get := 0.U
  io.DnTXRSP.bits.TagOp.foreach(_ := 0.U)
  io.DnTXRSP.bits.TraceTag.get := p_rxsnp.TraceTag.get

  io.DnTXDAT.valid := p_home_txdat_valid || p_dct_txdat_valid
  io.DnTXDAT.bits := DontCare
  io.DnTXDAT.bits.QoS.get := p_rxsnp.QoS.get
  io.DnTXDAT.bits.TgtID.get := Mux(p_dct_txdat_valid, p_rxsnp.FwdNID.get, p_rxsnp.SrcID.get)
  io.DnTXDAT.bits.SrcID.get := nodeId.U
  io.DnTXDAT.bits.TxnID.get := Mux(p_dct_txdat_valid, p_rxsnp.FwdTxnID.get, p_rxsnp.TxnID.get)
  io.DnTXDAT.bits.HomeNID.get := p_rxsnp.SrcID.get
  io.DnTXDAT.bits.Opcode.get := Mux(p_dct_txdat_valid, CHI_CompData.U, Mux(need_dct_txdat, CHI_SnpRespDataFwded.U, CHI_SnpRespData.U))
  io.DnTXDAT.bits.RespErr.get := 0.U
  io.DnTXDAT.bits.Resp.get := Mux(p_dct_txdat_valid, p_dct_txdat_resp, p_home_txdat_resp)
  io.DnTXDAT.bits.FwdState(p_fwd_state)
  // io.DnTXDAT.bits.DataPull(0.U)
  // io.DnTXDAT.bits.DataSource(0.U)
  io.DnTXDAT.bits.CBusy.foreach(_ := 0.U)
  io.DnTXDAT.bits.DBID.get := p_rxsnp.TxnID.get
  io.DnTXDAT.bits.CCID.get := 0.U
  io.DnTXDAT.bits.DataID.get := p_txdat_dataID
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

  // -- Other IO signals -------------------------------------
  io.tshr_meta_write_en := Mux(do_meta_write, rxsnp_meta_write_en, 0.U.asTypeOf(new L2Directory.MetaWriteMask))
  io.tshr_meta_write_meta := rxsnp_meta_write_meta
  io.ds_read_en := enter_need_local_ds_read && !io.ds_read_done

  io.blockRBE.EVT := block_vpipe_evt
  io.blockRBE.SNP := active 
  io.blockRBE.REQ := active
  io.free := !active
  // ---------------------------------------------------------

  // -- Assertion --------------------------------------------
  val supported_rxsnp = rxsnp_opcode.is(
    CHI_SnpShared,
    CHI_SnpClean,
    CHI_SnpOnce,
    CHI_SnpNotSharedDirty,
    CHI_SnpUnique,
    CHI_SnpPreferUnique,
    CHI_SnpCleanShared,
    CHI_SnpCleanInvalid,
    CHI_SnpMakeInvalid,
    CHI_SnpSharedFwd,
    CHI_SnpCleanFwd,
    CHI_SnpOnceFwd,
    CHI_SnpNotSharedDirtyFwd,
    CHI_SnpUniqueFwd,
    CHI_SnpPreferUniqueFwd,
    CHI_SnpQuery
  )
  val rxsnp_ret_to_src_must_be_zero = rxsnp_opcode.is(
    CHI_SnpQuery,
    CHI_SnpCleanShared,
    CHI_SnpCleanInvalid,
    CHI_SnpMakeInvalid,
    CHI_SnpOnceFwd,
    CHI_SnpUniqueFwd,
    CHI_SnpPreferUniqueFwd
  )
  val rxsnp_do_not_go_to_sd_must_be_one = rxsnp_opcode.is(
    CHI_SnpUnique,
    CHI_SnpCleanShared,
    CHI_SnpCleanInvalid,
    CHI_SnpMakeInvalid,
    CHI_SnpUniqueFwd
  )
  val sa_uops = Seq(
    io.toSA.SnpMakeInvalid,
    io.toSA.SnpToInvalid,
    io.toSA.SnpToShared,
    io.toSA.SnpToClean
  )
  val meta_write_any = io.tshr_meta_write_en.asUInt.orR

  val txrsp_stalled_q = RegNext(io.DnTXRSP.valid && !io.DnTXRSP.ready, false.B)
  val txrsp_bits_q = RegNext(io.DnTXRSP.bits.asUInt)
  val txdat_stalled_q = RegNext(io.DnTXDAT.valid && !io.DnTXDAT.ready, false.B)
  val txdat_bits_q = RegNext(io.DnTXDAT.bits.asUInt)
  val active_q = RegNext(active, false.B)
  val p_rxsnp_q = RegNext(p_rxsnp.asUInt)

  // RXSNP acceptance and CHI request-field legality.
  assert(!(io.DnRXSNP.fire && active),
    s"TSHR #${tshrId} SNP vPipe accepted a new snoop while active")
  when (io.DnRXSNP.fire || active) {
    assert(io.tshr_dirResult.state =/= L2Directory.MetaState.I ||
           (!io.tshr_dirResult.dirty && !io.tshr_dirResult.clients.asUInt.orR),
      s"TSHR #${tshrId} SNP vPipe observed dirty data or clients while local meta was invalid")
  }
  when (io.DnRXSNP.fire) {
    assert(supported_rxsnp,
      s"TSHR #${tshrId} SNP vPipe accepted an unsupported snoop opcode")
    assert(PopCount(sa_uops) <= 1.U,
      s"TSHR #${tshrId} SNP vPipe issued multiple SnoopAgent uops")
    assert(rxsnp_query || PopCount(sa_uops) === 1.U,
      s"TSHR #${tshrId} SNP vPipe did not issue exactly one SnoopAgent uop for a non-query snoop")
    assert(!rxsnp_query || PopCount(sa_uops) === 0.U,
      s"TSHR #${tshrId} SNP vPipe issued a SnoopAgent uop for SnpQuery")
    assert(!rxsnp_ret_to_src_must_be_zero || !io.DnRXSNP.bits.RetToSrc.get.asBool,
      s"TSHR #${tshrId} SNP vPipe received RetToSrc=1 on an opcode that requires zero")
    assert(!rxsnp_do_not_go_to_sd_must_be_one || io.DnRXSNP.bits.DoNotGoToSD.get.asBool,
      s"TSHR #${tshrId} SNP vPipe received DoNotGoToSD=0 on an opcode that requires one")
    assert(!rxsnp_query || !io.DnRXSNP.bits.DoNotGoToSD.get.asBool,
      s"TSHR #${tshrId} SNP vPipe received DoNotGoToSD=1 on SnpQuery")
    assert(rxsnp_fwd || (io.DnRXSNP.bits.FwdNID.get === 0.U && io.DnRXSNP.bits.FwdTxnID.get === 0.U),
      s"TSHR #${tshrId} SNP vPipe received forwarding IDs on a non-Fwd snoop")
  }

  // External completion events must match an outstanding wait state.
  assert(!io.fromSA.SnpRespData0 || w_snpresp0,
    s"TSHR #${tshrId} SNP vPipe received unexpected or duplicate SA DataID 0")
  assert(!io.fromSA.SnpRespData2 || w_snpresp2,
    s"TSHR #${tshrId} SNP vPipe received unexpected or duplicate SA DataID 2")
  assert(!io.fromSA.SnpResp || w_snpresp0 || w_snpresp2 || enter_send_sa_uop,
    s"TSHR #${tshrId} SNP vPipe received an unexpected SA SnpResp")
  assert(!(io.fromSA.SnpResp && (io.fromSA.SnpRespData0 || io.fromSA.SnpRespData2)),
    s"TSHR #${tshrId} SNP vPipe received SA SnpResp and SnpRespData together")

  // State bits and transaction ownership.
  assert(!(s_home_resp0 && !s_home_resp2),
    s"TSHR #${tshrId} SNP vPipe has Home DataID 0 pending after DataID 2 completed")
  assert(!(s_dct_resp0 && !s_dct_resp2),
    s"TSHR #${tshrId} SNP vPipe has DCT DataID 0 pending after DataID 2 completed")
  assert(PopCount(Seq(p_home_txrsp_valid, p_home_txdat_valid, p_dct_txdat_valid)) <= 1.U,
    s"TSHR #${tshrId} SNP vPipe has multiple downstream transmit owners")
  assert(PopCount(Seq(do_home_snpresp, do_home_snprespdata0, do_dct_snprespdata0)) <= 1.U,
    s"TSHR #${tshrId} SNP vPipe scheduled multiple downstream responses")
  assert(!(s_dct_resp0 || s_dct_resp2 || p_dct_txdat_valid) || need_dct_txdat,
    s"TSHR #${tshrId} SNP vPipe scheduled DCT without a latched DCT decision")
  assert(!p_dct_txdat_valid || (!s_home_resp0 && !s_home_resp2 &&
                                !p_home_txrsp_valid && !p_home_txdat_valid),
    s"TSHR #${tshrId} SNP vPipe started DCT before the Home response completed")
  assert(!p_home_txrsp_valid || (s_home_resp0 && s_home_resp2),
    s"TSHR #${tshrId} SNP vPipe TXRSP owner does not match Home scheduling state")
  when (p_home_txdat_valid) {
    assert((p_txdat_dataID === 0.U && s_home_resp0 && s_home_resp2) ||
           (p_txdat_dataID === 2.U && !s_home_resp0 && s_home_resp2),
      s"TSHR #${tshrId} SNP vPipe Home TXDAT DataID disagrees with scheduling state")
  }
  when (p_dct_txdat_valid) {
    assert((p_txdat_dataID === 0.U && s_dct_resp0 && s_dct_resp2) ||
           (p_txdat_dataID === 2.U && !s_dct_resp0 && s_dct_resp2),
      s"TSHR #${tshrId} SNP vPipe DCT TXDAT DataID disagrees with scheduling state")
  }
  assert(!active_q || !active || p_rxsnp.asUInt === p_rxsnp_q,
    s"TSHR #${tshrId} SNP vPipe request payload changed during an active transaction")

  // Output-channel exclusivity, ownership, ordering, and stability.
  assert(!(io.DnTXRSP.valid && io.DnTXDAT.valid),
    s"TSHR #${tshrId} SNP vPipe asserted TXRSP and TXDAT valid together")
  assert(!(io.DnTXDAT.valid && p_txdat_dataID =/= 0.U && p_txdat_dataID =/= 2.U),
    s"TSHR #${tshrId} SNP vPipe emitted an unsupported TXDAT DataID")
  assert(!io.DnTXRSP.valid || active,
    s"TSHR #${tshrId} SNP vPipe asserted TXRSP valid while inactive")
  assert(!io.DnTXDAT.valid || active,
    s"TSHR #${tshrId} SNP vPipe asserted TXDAT valid while inactive")
  assert(!(io.DnTXRSP.fire && (w_snpresp0 || w_snpresp2 || w_ds_read)),
    s"TSHR #${tshrId} SNP vPipe sent TXRSP before all data sources completed")
  assert(!(io.DnTXDAT.fire && (w_snpresp0 || w_snpresp2 || w_ds_read)),
    s"TSHR #${tshrId} SNP vPipe sent TXDAT before all data sources completed")
  assert(!io.DnTXRSP.valid || io.DnTXRSP.bits.Resp.get < CHICohResps.I_PD,
    s"TSHR #${tshrId} SNP vPipe used PassDirty on a response without data")
  when (io.DnTXRSP.valid) {
    assert(io.DnTXRSP.bits.Opcode.get === Mux(need_dct_txdat, CHI_SnpRespFwded.U, CHI_SnpResp.U),
      s"TSHR #${tshrId} SNP vPipe emitted an incorrect Home TXRSP opcode")
    assert(io.DnTXRSP.bits.TgtID.get === p_rxsnp.SrcID.get &&
           io.DnTXRSP.bits.TxnID.get === p_rxsnp.TxnID.get,
      s"TSHR #${tshrId} SNP vPipe emitted incorrect Home TXRSP routing IDs")
  }
  when (p_dct_txdat_valid) {
    assert(io.DnTXDAT.bits.Opcode.get === CHI_CompData.U,
      s"TSHR #${tshrId} SNP vPipe DCT data does not use CompData")
    assert(io.DnTXDAT.bits.FwdState_DataPull_DataSource.get === 0.U,
      s"TSHR #${tshrId} SNP vPipe drove FwdState/DataPull/DataSource on CompData")
    assert(p_dct_txdat_resp =/= CHICohResps.SD && p_dct_txdat_resp =/= CHICohResps.SD_PD,
      s"TSHR #${tshrId} SNP vPipe emitted an unsupported SD DCT response")
    assert(io.DnTXDAT.bits.TgtID.get === p_rxsnp.FwdNID.get &&
           io.DnTXDAT.bits.TxnID.get === p_rxsnp.FwdTxnID.get &&
           io.DnTXDAT.bits.HomeNID.get === p_rxsnp.SrcID.get &&
           io.DnTXDAT.bits.DBID.get === p_rxsnp.TxnID.get,
      s"TSHR #${tshrId} SNP vPipe emitted incorrect DCT routing IDs")
  }
  when (p_home_txdat_valid) {
    assert(io.DnTXDAT.bits.Opcode.get === Mux(need_dct_txdat, CHI_SnpRespDataFwded.U, CHI_SnpRespData.U),
      s"TSHR #${tshrId} SNP vPipe emitted an incorrect Home TXDAT opcode")
    assert(io.DnTXDAT.bits.TgtID.get === p_rxsnp.SrcID.get &&
           io.DnTXDAT.bits.TxnID.get === p_rxsnp.TxnID.get,
      s"TSHR #${tshrId} SNP vPipe emitted incorrect Home TXDAT routing IDs")
  }
  when (io.DnTXDAT.valid) {
    assert(io.DnTXDAT.bits.BE.get.andR,
      s"TSHR #${tshrId} SNP vPipe emitted a full-line response without all byte enables")
  }
  when (txrsp_stalled_q) {
    assert(io.DnTXRSP.valid && io.DnTXRSP.bits.asUInt === txrsp_bits_q,
      s"TSHR #${tshrId} SNP vPipe changed TXRSP payload while stalled")
  }
  when (txdat_stalled_q) {
    assert(io.DnTXDAT.valid && io.DnTXDAT.bits.asUInt === txdat_bits_q,
      s"TSHR #${tshrId} SNP vPipe changed TXDAT payload while stalled")
  }

  // Meta updates and buffer ownership.
  assert(meta_write_any === do_meta_write,
    s"TSHR #${tshrId} SNP vPipe meta-write output disagrees with its commit event")
  when (meta_write_any) {
    assert(active && io.tshr_meta_write_en.state && io.tshr_meta_write_en.dirty,
      s"TSHR #${tshrId} SNP vPipe emitted an incomplete or inactive meta write")
    assert(!rxsnp_query,
      s"TSHR #${tshrId} SNP vPipe modified meta for SnpQuery")
    assert(!io.tshr_meta_write_meta.dirty &&
           (io.tshr_meta_write_meta.state === L2Directory.MetaState.I ||
            io.tshr_meta_write_meta.state === L2Directory.MetaState.S),
      s"TSHR #${tshrId} SNP vPipe committed a state outside the I/SC clean subset")
    when (io.tshr_meta_write_en.clients.asUInt.orR) {
      assert(io.tshr_meta_write_meta.state === L2Directory.MetaState.I &&
             !io.tshr_meta_write_meta.clients.asUInt.orR,
        s"TSHR #${tshrId} SNP vPipe client invalidation did not write local I/no-clients")
    }
  }
  assert(!io.ds_read_en || io.DnRXSNP.fire,
    s"TSHR #${tshrId} SNP vPipe asserted ds_read_en outside the request-entry cycle")
  assert(!(io.DnTXRSP.valid || io.DnTXDAT.valid) || io.blockRBE.EVT,
    s"TSHR #${tshrId} SNP vPipe did not block EVT while owning a downstream response")
  assert(io.free === !active && io.blockRBE.SNP === active && io.blockRBE.REQ === active,
    s"TSHR #${tshrId} SNP vPipe free/RBE ownership outputs disagree with active state")
  assert(io.blockRBE.EVT === block_vpipe_evt,
    s"TSHR #${tshrId} SNP vPipe EVT block output disagrees with buffer ownership")
  when (io.toSA.SnpCompAck) {
    assert(active && rxsnp_opcode.is(CHI_SnpCleanShared) &&
           !w_snpresp0 && !w_snpresp2 && !w_ds_read &&
           !s_home_resp0 && !s_home_resp2 &&
           !s_dct_resp0 && !s_dct_resp2,
      s"TSHR #${tshrId} SNP vPipe emitted SnpCompAck before SnpCleanShared completed")
  }

  // Opcode-specific negative behavior.
  when (active && rxsnp_query) {
    assert(!w_snpresp0 && !w_snpresp2 && !w_ds_read && !s_meta_write &&
           !s_dct_resp0 && !s_dct_resp2 && !io.DnTXDAT.valid,
      s"TSHR #${tshrId} SNP vPipe performed data, SA, DCT, or meta work for SnpQuery")
  }
  when (active && rxsnp_make_invalid) {
    assert(!w_ds_read && !need_dct_txdat && !io.DnTXDAT.valid,
      s"TSHR #${tshrId} SNP vPipe read or returned data for SnpMakeInvalid")
  }
  when (active && !rxsnp_fwd) {
    assert(!need_dct_txdat && !s_dct_resp0 && !s_dct_resp2 && !p_dct_txdat_valid,
      s"TSHR #${tshrId} SNP vPipe scheduled DCT for a non-Fwd snoop")
  }
  // ---------------------------------------------------------
}
