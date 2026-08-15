package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import oceanus.l2._
import oceanus.compactchi._
import oceanus.chi.bundle._
import oceanus.chi.opcode._
import utility._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import oceanus.l2.L2Directory.MetaState
import oceanus.chi.field.CHIFieldSize._
import oceanus.chi.field.CHIFieldOrder._
import oceanus.chi.field.CHIFieldMemAttr._
import oceanus.chi.field.CHIFieldResp
import coupledL2.tl2chi.MemAttr

object L2VPipeREQ {

}

class L2VPipeREQ(clientComponents: Seq[CCHIComponent], tshrId: Int, nodeId: Int)(implicit val p: Parameters) 
    extends Module 
    with CHIRNFOpcodesREQ 
    with CHIRNFOpcodesRSP
    with CHIRNFOpcodesDAT
    with HasL2Params {

  val io = IO(new Bundle {
    val UpRXREQ = Valid(Input(new FlitREQStripped))

    val DnTXREQ = Decoupled(new CHIBundleREQ)

    val DnRXRSP = Flipped(Valid(new CHIBundleRSP))
    val DnRXDAT = Flipped(Valid(new CHIBundleDAT))

    val DnTXRSP = Decoupled(new CHIBundleRSP)
    val DnTXDAT = Decoupled(new CHIBundleDAT)

    val UpRXRSP = Flipped(Valid(new FlitUpRSP))
    val UpRXDAT = Flipped(Valid(new FlitUpDATWithoutData))

    val UpTXRSP = Decoupled(new FlitDnRSP)
    val UpTXDAT = Decoupled(new FlitDnDATWithoutData)

    val UpTXREQ = Decoupled(new FlitREQ)

    val tshr_paddr = Input(UInt(paramL2.physicalAddrWidth.W))

    val tshr_dirResult = Input(new L2Directory.MetaReadResult)

    val tshr_tag_write_en = Output(Bool())
    val tshr_meta_write_en = Output(new L2Directory.MetaWriteMask)
    val tshr_meta_write_meta = Output(new L2Directory.Meta)

    val toSA = Output(new L2SnoopAgent.PathToSnoopAgent)
    val fromSA = Input(new L2SnoopAgent.PathFromSnoopAgent)

    val blockRBE = Output(new L2RBE.PathVPipeBlock)
    val free = Output(Bool())

    val ds_rd_en = Output(Bool())
    val ds_rd_cancel = Output(Bool())
    val ds_rd_done = Input(Bool())

    val repl_en = Output(Bool())
    val repl_retry = Input(Bool())
    val repl_done = Input(Bool())
    val repl_resp = Input(new L2Directory.ReplReadResult)

    val dir_wb_locked = Output(Bool())
    val ds_wb_locked = Output(Bool())

    val dir_wb_aux = Output(Bool())

    val toPCreditPool = Valid(new L2PCreditPool.Entry)
    val fromPCreditPool = Input(Bool())

    val toClientTable = Output(UInt(8.W)) // TODO: configurable with upstream nodeId width
    val fromClientTable = Input(Vec(1, Bool())) // TODO: parameterize with coherent l2 client count

    val peer_unlock_dir = Output(Vec(paramL2.mshrSize, Bool()))
    val peer_unlock_ds = Output(Vec(paramL2.mshrSize, Bool()))

    val self_unlock_dir = Input(Bool())
    val self_unlock_ds = Input(Bool())

    val L1EVT_active = Input(Bool())
  })

  val dirResult = io.tshr_dirResult
  
  val configEnableMakeReadUnique = true
  val configInclusiveReadOnce = true

  // Whether, for a L1 ReadShared, the returning permission was promoted from Shared to Unique
  // when the transaction hit local US state with zero client.
  // * NOTICE: Might be necessary when Coherent L1 I-Cache (Type 2 components) supported.
  val configReadSharedPromotionFromUS = true.B

  // Whether, for a L1 ReadShared, the returning permission was demoted from Unique to Shared
  // when the transaction hit local/remote UU state with zero client, with local transition of UU to US
  // * NOTICE: Might be necessary when Coherent L1 I-Cache (Type 2 components) supported.
  val configReadSharedDemotionFromUU = false.B
  
  // Whether, for a L1 ReadShared, the returning permission was promoted from Shared to Unique
  // when the transaction missed with HN returning Unique
  val configReadSharedPromotionFromI = true.B

  //
  val configReadUniqueHitSPreferReadDS = false.B

  // Changes the eviction behaviour (HN transaction type selection) when EvictBack hits a clean cacheline:
  //                    ByWriteEvictFull    ByWriteEvictOrEvict Dynamic
  // Evict              0                   0                   0
  // WriteEvictFull     1                   0                   0
  // WriteEvictOrEvict  x                   1                   0
  // Trained Dynamic    x                   x                   1
  val configEvictBackByWriteEvictFull = false.B
  val configEvictBackByWriteEvictOrEvict = true.B
  val configEvictBackDynamic = false.B

  val configEvictBackRetryCancel = true.B

  // Refill locking Directory and Data Storage write if:
  // 1. A total miss read has not issued any ReplRd
  // 2. ReplRd returned with a non-Invalid way


  // -- Enchantment modules and signals of entrance-time
  val rxreq = io.UpRXREQ.bits
  val rxreq_fire = io.UpRXREQ.fire

  val rxreq_opcode = Module(new CCHIREQOpcodeDecoder)
  rxreq_opcode.io.valid := rxreq_fire
  rxreq_opcode.io.opcode := rxreq.Opcode

  val rxreq_readunique = rxreq_opcode.is(CCHIOpcode.ReadUnique)
  val rxreq_readshared = rxreq_opcode.is(CCHIOpcode.ReadShared)

  def satisfied(opcode: CCHIOpcode, state: UInt): (Bool, Bool) = (
    rxreq_opcode.is(opcode) && (dirResult.state >= state && dirResult.hit), 
    rxreq_opcode.is(opcode) && (dirResult.state < state || !dirResult.hit))

  val (rxreq_satisfied_stashshared, rxreq_unsatisfied_stashshared) = satisfied(CCHIOpcode.StashShared, MetaState.S)
  val (rxreq_satisfied_stashunique, rxreq_unsatisfied_stashunique) = satisfied(CCHIOpcode.StashUnique, MetaState.US)
  val (rxreq_satisfied_readonce, rxreq_unsatisfied_readonce) = satisfied(CCHIOpcode.ReadOnce, MetaState.S)
  val (rxreq_satisfied_readshared, rxreq_unsatisfied_readshared) = satisfied(CCHIOpcode.ReadShared, MetaState.S)
  val (rxreq_satisfied_writeuniqueptl, rxreq_unsatisfied_writeuniqueptl) = satisfied(CCHIOpcode.WriteUniquePtl, MetaState.US)
  val (rxreq_satisfied_writeuniquefull, rxreq_unsatisfied_writeuniquefull) = satisfied(CCHIOpcode.WriteUniqueFull, MetaState.US)
  val (rxreq_satisfied_readunique, rxreq_unsatisfied_readunique) = satisfied(CCHIOpcode.ReadUnique, MetaState.US)
  val (rxreq_satisfied_makeunique, rxreq_unsatisfied_makeunique) = satisfied(CCHIOpcode.MakeUnique, MetaState.US)

  val (rxreq_unsatisfied_evictback, rxreq_satisfied_evictback) = satisfied(CCHIOpcode.EvictBack, MetaState.S)
  // ----------------------------------------------------------------

  // -- Enchantment modules and signals of downstream RX channels
  val dn_rxrsp_opcode = Module(new RSPOpcodeDecoder)
  dn_rxrsp_opcode.io.valid := io.DnRXRSP.fire
  dn_rxrsp_opcode.io.opcode := io.DnRXRSP.bits.Opcode.get

  val dn_rxdat_opcode = Module(new DATOpcodeDecoder)
  dn_rxdat_opcode.io.valid := io.DnRXDAT.fire
  dn_rxdat_opcode.io.opcode := io.DnRXDAT.bits.Opcode.get
  // ----------------------------------------------------------------

  // -- Enchantment modules and signals of upstream RX channels
  val up_rxrsp_opcode = Module(new CCHIUpRSPOpcodeDecoder)
  up_rxrsp_opcode.io.valid := io.UpRXRSP.fire
  up_rxrsp_opcode.io.opcode := io.UpRXRSP.bits.Opcode

  val up_rxdat_opcode = Module(new CCHIUpDATOpcodeDecoder)
  up_rxdat_opcode.io.valid := io.UpRXDAT.fire
  up_rxdat_opcode.io.opcode := io.UpRXDAT.bits.Opcode
  // ----------------------------------------------------------------

  // -- Private payload registers and enchantment signals
  val p_rxreq = Reg(new FlitREQStripped)
  val p_rxreq_opcode = p_rxreq.Opcode
  val p_rxreq_stashshared       = p_rxreq_opcode === CCHIOpcode.StashShared.U
  val p_rxreq_stashunique       = p_rxreq_opcode === CCHIOpcode.StashUnique.U
  val p_rxreq_readnosnp         = p_rxreq_opcode === CCHIOpcode.ReadNoSnp.U
  val p_rxreq_readonce          = p_rxreq_opcode === CCHIOpcode.ReadOnce.U
  val p_rxreq_readshared        = p_rxreq_opcode === CCHIOpcode.ReadShared.U
  val p_rxreq_writenosnpptl     = p_rxreq_opcode === CCHIOpcode.WriteNoSnpPtl.U
  val p_rxreq_writenosnpfull    = p_rxreq_opcode === CCHIOpcode.WriteNoSnpFull.U
  val p_rxreq_writeuniqueptl    = p_rxreq_opcode === CCHIOpcode.WriteUniquePtl.U
  val p_rxreq_writeuniquefull   = p_rxreq_opcode === CCHIOpcode.WriteUniqueFull.U
  val p_rxreq_cleanshared       = p_rxreq_opcode === CCHIOpcode.CleanShared.U
  val p_rxreq_cleaninvalid      = p_rxreq_opcode === CCHIOpcode.CleanInvalid.U
  val p_rxreq_makeinvalid       = p_rxreq_opcode === CCHIOpcode.MakeInvalid.U
  val p_rxreq_readunique        = p_rxreq_opcode === CCHIOpcode.ReadUnique.U
  val p_rxreq_makeunique        = p_rxreq_opcode === CCHIOpcode.MakeUnique.U
  val p_rxreq_evictback         = p_rxreq_opcode === CCHIOpcode.EvictBack.U

  when (rxreq_fire) {
    p_rxreq := rxreq
  }

  val p_txreq_reissue = RegInit(false.B)
  val p_txreq_issued_opcode = Reg(UInt(paramCHI.reqOpcodeWidth.W))

  val p_retryack_pcrdtype = Reg(UInt(paramCHI.rspPCrdTypeWidth.W))
  val p_retryack_srcid = Reg(UInt(paramCHI.nodeIdWidth.W))

  val p_evict_cancelled = Reg(Bool())

  val p_dbid = Reg(UInt(paramCHI.rspDBIDWidth.W))
  val p_homenid = Reg(UInt(paramCHI.nodeIdWidth.W))

  // ----------------------------------------------------------------

  // -- Interaction with Client Table
  io.toClientTable := Mux(rxreq_fire, rxreq.SrcID, p_rxreq.SrcID)

  val rxreq_client = io.fromClientTable
  val p_rxreq_client = io.fromClientTable

  val rxreq_client_present = (rxreq_client.asUInt & dirResult.clients.asUInt).orR
  val p_rxreq_client_present = (p_rxreq_client.asUInt & dirResult.clients.asUInt).orR

  val rxreq_peer_present = (~rxreq_client.asUInt & dirResult.clients.asUInt).orR
  val p_rxreq_peer_present = (~p_rxreq_client.asUInt & dirResult.clients.asUInt).orR
  // ----------------------------------------------------------------

  // -- State bit registers
  val w_snpresp0 = RegInit(false.B) // Waiting for response from Snoop Agent (DataID = 0)
  val w_snpresp2 = RegInit(false.B) // Waiting for response from Snoop Agent (DataID = 2)

  val s_snpcompack = RegInit(false.B) // Scheduling SnpCompAck to Snoop Agent

  val s_dn_txreq = RegInit(false.B) // Scheduling downstream TXREQ
  val w_dn_pcrdgrant = RegInit(false.B) // Waiting for P-Credit from downstream after receiving downstream RetryAck

  val w_ds_resp = RegInit(false.B) // Waiting for response from Data Storage

  val w_s_dn_rd_compack = RegInit(false.B) // Waiting to schedule downstream TXRSP CompAck of Read and normal subsequence
  val s_dn_rd_compack = RegInit(false.B) // Scheduling downstream TXRSP CompAck of Read and normal subsequence

  val w_dn_rd_data0 = RegInit(false.B) // Waiting for downstream RXDAT CompData/DataSepResp (DataID = 0)
  val w_dn_rd_data2 = RegInit(false.B) // Waiting for downstream RXDAT CompData/DataSepResp (DataID = 2)
  val w_dn_rd_comp = RegInit(false.B) // Waiting for downstream RXRSP CompData/RespSepData

  val w_up_rd_compack = RegInit(false.B) // Waiting for Upstream RXRSP CompAck

  val w_s_up_rd_compdata0 = RegInit(false.B) // Waiting to schedule upstream TXDAT CompData (DataID = 0) of Read subsequence
  val w_s_up_rd_compdata2 = RegInit(false.B) // Waiting to schedule upstream TXDAT CompData (DataID = 2) of Read subsequence
  val s_up_rd_compdata0 = RegInit(false.B) // Scheduling upstream TXDAT CompData (DataID = 0) of Read subsequence
  val s_up_rd_compdata2 = RegInit(false.B) // Scheduling upstream TXDAT CompData (DataID = 2) of Read subsequence

  val w_s_up_rd_comp = RegInit(false.B) // Waiting to schedule upstream TXRSP Comp of Read subsequence
  val s_up_rd_comp = RegInit(false.B) // Scheduling upstream TXRSP Comp for Read subsequence

  val w_unlock_dir = RegInit(false.B) // Waiting for unlocking Directory Write-Back
  val w_unlock_ds = RegInit(false.B) // Waiting for unlocking Data Storage Write-Back

  val w_s_repl = RegInit(false.B) // Waiting to schedule Directory Replacer Read
  val s_repl = RegInit(false.B) // Scheduling Directory Replacer Read

  val s_evict = RegInit(false.B) // Scheduling local eviction RXREQ EvictBack

  val w_evict_s_dn_txreq = RegInit(false.B) // Waiting to schedule downstream TXREQ of EvictBack subsequence

  val w_dn_evict_comp = RegInit(false.B) // Waiting for downstream RXRSP Comp terminal of EvictBack subsequence
  val w_dn_evict_compdbid = RegInit(false.B) // Waiting for downstream RXRSP Comp/CompDBIDResp with DBID of EvictBack subsequence

  val w_s_dn_evict_cbwrdata0 = RegInit(false.B)
  val w_s_dn_evict_cbwrdata2 = RegInit(false.B)
  val s_dn_evict_cbwrdata0 = RegInit(false.B)
  val s_dn_evict_cbwrdata2 = RegInit(false.B)

  val s_dn_evict_compack = RegInit(false.B)

  val w_evict_peer_unlock_ds = RegInit(false.B)

  // TODO: more state bits here

  assert(!(rxreq_fire && w_snpresp0), s"RXREQ fired on valid 'w_snpresp0' in TSHR #${tshrId} REQ vPipe")
  assert(!(rxreq_fire && w_snpresp2), s"RXREQ fired on valid 'w_snpresp2' in TSHR #${tshrId} REQ vPipe")
  assert(!(rxreq_fire && s_snpcompack), s"RXREQ fired on valid 's_snpcompack' in TSHR #${tshrId} REQ vPipe")
  // ----------------------------------------------------------------

  // -- State enchantment signals
  val active = w_snpresp0 || w_snpresp2 || s_snpcompack ||
               s_dn_txreq || w_dn_pcrdgrant ||
               w_ds_resp ||
               s_dn_rd_compack || w_dn_rd_data0 || w_dn_rd_data2 || w_dn_rd_comp ||
               w_up_rd_compack ||
               w_s_up_rd_compdata0 || s_up_rd_compdata0 || w_s_up_rd_compdata2 || s_up_rd_compdata2 ||
               w_s_up_rd_comp || s_up_rd_comp ||
               w_unlock_dir || w_unlock_ds ||
               w_s_repl || s_repl || s_evict

  // ----------------------------------------------------------------

  // -- Interactions with Snoop Agent
  // decode upstream REQ opcodes to upstream SNP opcodes
  val sa_snpresp = io.fromSA.SnpResp
  val sa_snprespdata0 = io.fromSA.SnpRespData0
  val sa_snprespdata2 = io.fromSA.SnpRespData2

  val sa_resp_decision = sa_snpresp ||
                         sa_snprespdata0 && w_snpresp0 && w_snpresp2 ||
                         sa_snprespdata2 && w_snpresp0 && w_snpresp2

  val sa_resp_last = sa_snpresp ||
                     sa_snprespdata0 && w_snpresp0 && !w_snpresp2 ||
                     sa_snprespdata2 && !w_snpresp0 && w_snpresp2

  val sa_respdata_first = sa_snprespdata0 && w_snpresp0 && w_snpresp2 ||
                          sa_snprespdata2 && w_snpresp0 && w_snpresp2

  val sa_resp_decided = !(w_snpresp0 && w_snpresp2)

  io.toSA.SnpMakeInvalid := rxreq_opcode.is(
    CCHIOpcode.MakeInvalid
  )

  io.toSA.SnpToInvalid := rxreq_opcode.is(
    CCHIOpcode.WriteUniquePtl,
    CCHIOpcode.WriteUniqueFull,
    CCHIOpcode.CleanInvalid,
    CCHIOpcode.ReadUnique,
    CCHIOpcode.MakeUnique,
    CCHIOpcode.EvictBack
  )

  io.toSA.SnpToShared := rxreq_opcode.is(
    CCHIOpcode.ReadShared
  )

  io.toSA.SnpToClean := rxreq_opcode.is(
    CCHIOpcode.ReadOnce,
    CCHIOpcode.CleanShared
  )

  // set and unset SnpCompAck scheduling
  //  - SnpCompAck should be issued with scheduling bit set, and after all data beats to upstream/downstream
  //    were sent.
  when (io.toSA.SnpToClean) {
    s_snpcompack := true.B
  }

  when (io.toSA.SnpCompAck) {
    s_snpcompack := false.B
  }

  io.toSA.SnpCompAck := false.B // TODO: interact with upstream data sending

  // waiting state transitions
  //  - SnpResp/SnpRespData0/SnpRespData2 were all allowed to be received on the same cycle of the issue of
  //    SnpMakeInvalid/SnpToInvalid/SnpToShared/SnpToClean.
  when (io.toSA.SnpMakeInvalid || io.toSA.SnpToInvalid || io.toSA.SnpToShared || io.toSA.SnpToClean) {
    w_snpresp0 := true.B
    w_snpresp2 := true.B
  }

  when (sa_snpresp || sa_snprespdata0) {
    w_snpresp0 := false.B
  }

  when (sa_snpresp || sa_snprespdata2) {
    w_snpresp2 := false.B
  }
  // ----------------------------------------------------------------

  // -- Interactions with downstream TXREQ and downstream Retry Mechanism
  // TODO: complete other CCHI opcodes

  val allow_txreq_evictback = sa_resp_decision

  val sched_txreq_evictback = rxreq_unsatisfied_evictback

  when (sched_txreq_evictback) {
    w_evict_s_dn_txreq := true.B
  }

  val issue_txreq_stashshared = rxreq_unsatisfied_stashshared
  val issue_txreq_stashunique = rxreq_unsatisfied_stashunique
  val issue_txreq_readonce = rxreq_unsatisfied_readonce
  val issue_txreq_readshared = rxreq_unsatisfied_readshared
  val issue_txreq_readunique = rxreq_unsatisfied_readunique
  val issue_txreq_makeunique = rxreq_unsatisfied_makeunique

  val issue_txreq_evictback = w_evict_s_dn_txreq && allow_txreq_evictback

  val issue_txreq = issue_txreq_stashshared ||
                    issue_txreq_stashunique ||
                    issue_txreq_readonce ||
                    issue_txreq_readshared ||
                    issue_txreq_readunique ||
                    issue_txreq_makeunique ||
                    issue_txreq_evictback

  val sched_compack_txreq_rd = issue_txreq_readshared ||
                               issue_txreq_readunique

  val reissue_txreq = io.fromPCreditPool

  val reissue_evict_cancel = configEvictBackRetryCancel &&
                             reissue_txreq &&
                             p_rxreq_evictback && // this predication might not be necessary
                             (!dirResult.hit || dirResult.state === MetaState.I)

  when (io.DnTXREQ.fire) {
    s_dn_txreq := false.B
    p_txreq_issued_opcode := io.DnTXREQ.bits.Opcode.get
  }

  when (reissue_evict_cancel) {
    p_evict_cancelled := true.B
  }

  when (issue_txreq_evictback) {
    w_evict_s_dn_txreq := false.B
    p_evict_cancelled := false.B
  }

  when (issue_txreq || reissue_txreq) {
    s_dn_txreq := true.B
    p_txreq_reissue := reissue_txreq
  }

  when (sched_compack_txreq_rd) {
    w_s_dn_rd_compack := true.B
  }

  when (dn_rxrsp_opcode.is(CHI_RetryAck)) {
    w_dn_pcrdgrant := true.B
    p_retryack_pcrdtype := io.DnRXRSP.bits.PCrdType.get
    p_retryack_srcid := io.DnRXRSP.bits.SrcID.get
  }

  when (io.fromPCreditPool) {
    w_dn_pcrdgrant := false.B
  }

  io.toPCreditPool.valid := w_dn_pcrdgrant
  io.toPCreditPool.bits.pCrdType := p_retryack_pcrdtype
  io.toPCreditPool.bits.srcId := p_retryack_srcid

  assert(!(io.fromPCreditPool && !w_dn_pcrdgrant), s"P-Credit granted on non-retry state in TSHR #${tshrId} REQ vPipe")
  // ----------------------------------------------------------------

  // -- Interactions with downstream CHI TXREQ channel
  val readonce_txreq_opcode = { if (configInclusiveReadOnce) CHI_ReadNotSharedDirty.U else CHI_ReadOnce.U } // TODO: better policy

  val xunique_txreq_opcode = {
    if (configEnableMakeReadUnique)
      Mux(p_txreq_reissue, 
        p_txreq_issued_opcode,
        Mux(dirResult.state === MetaState.S, CHI_MakeReadUnique.U, CHI_ReadUnique.U)
      )
    else
      CHI_ReadUnique.U
  }

  val stashunique_txreq_opcode = xunique_txreq_opcode
  val readunique_txreq_opcode = xunique_txreq_opcode

  val cleanshared_txreq_opcode = 0.U // TODO: switch between WriteCleanFull and CleanShared

  val cleaninvalid_txreq_opcode = 0.U // TODO: switch between WriteBackFull and CleanInvalid

  val makeinvalid_txreq_opcode = CHI_MakeInvalid.U // silent eviction

  val evictback_txreq_opcode = Mux(p_txreq_reissue,
                                 Mux(p_evict_cancelled, CHI_PCrdReturn.U, p_txreq_issued_opcode),
                                 Mux(dirResult.dirty, CHI_WriteBackFull.U,
                                   Mux(configEvictBackDynamic || configEvictBackByWriteEvictOrEvict, CHI_WriteEvictOrEvict.U,
                                   Mux(configEvictBackByWriteEvictFull, CHI_WriteEvictFull.U,
                                   CHI_Evict.U)))
                               )

  val txreq_opcode = ParallelMux(Seq(
    (p_rxreq_stashshared,     CHI_ReadNotSharedDirty.U),
    (p_rxreq_stashunique,     stashunique_txreq_opcode),
    (p_rxreq_readnosnp,       CHI_ReadNoSnp.U),
    (p_rxreq_readonce,        readonce_txreq_opcode),
    (p_rxreq_readshared,      CHI_ReadNotSharedDirty.U),
    (p_rxreq_writenosnpptl,   CHI_WriteNoSnpPtl.U),
    (p_rxreq_writenosnpfull,  CHI_WriteNoSnpFull.U),
    (p_rxreq_writeuniqueptl,  CHI_WriteUniquePtl.U), // TODO: only happens after L1 SA complete
    (p_rxreq_writeuniquefull, CHI_WriteUniqueFull.U), // TODO: only happens after L1 SA complete
    (p_rxreq_cleanshared,     cleanshared_txreq_opcode),
    (p_rxreq_cleaninvalid,    cleaninvalid_txreq_opcode),
    (p_rxreq_makeinvalid,     makeinvalid_txreq_opcode),
    (p_rxreq_readunique,      readunique_txreq_opcode),
    (p_rxreq_makeunique,      CHI_MakeUnique.U),
    (p_rxreq_evictback,       evictback_txreq_opcode) // TODO: only happens after L1 SA complete
  ))

  val txreq_pcrdreturn = txreq_opcode === CHI_PCrdReturn.U

  // Field 'Size':
  //    [ CCHI Opcode ]     [ CHI Opcode ]        [ Value / Source ]
  //  -  ReadShared          ReadNotSharedDirty    64B
  //  -  ReadUnique          ReadUnique            64B
  //                         MakeReadUnique        64B
  val txreq_size = ParallelMux(Seq(
    (p_rxreq_readshared,      Size64B.U),
    (p_rxreq_readunique,      Size64B.U)
  ))

  // Field 'LikelyShared':
  //    [ CCHI Opcode ]     [ CHI Opcode ]        [ Value / Source ]
  //  -  ReadShared          ReadNotSharedDirty    0 (1 not utilized for now)
  //  -  ReadUnique          ReadUnique            0
  //                         MakeReadUnique        0
  val txreq_likelyshared = ParallelMux(Seq(
    (p_rxreq_readshared,      false.B),
    (p_rxreq_readunique,      false.B)
  ))

  // Field 'Order':
  //    [ CCHI Opcode ]     [ CHI Opcode ]        [ Value / Source ]
  //  -  ReadShared          ReadNotSharedDirty    0b00 (No Ordering)
  //  -  ReadUnique          ReadUnique            0b00 (No Ordering)
  //                         MakeReadUnique        0b00 (No Ordering)
  val txreq_order = ParallelMux(Seq(
    (p_rxreq_readshared,      NoOrdering.U),
    (p_rxreq_readunique,      NoOrdering.U)
  ))

  // Field 'MemAttr':
  //    [ CCHI Opcode ]     [ CHI Opcode ]        [ Value / Source ]
  //  -  ReadShared          ReadNotSharedDirty    Cacheable + EWA + Allocate
  //  -  ReadUnique          ReadUnique            Cacheable + EWA + Allocate
  //                         MakeReadUnique        Cacheable + EWA + Allocate
  val txreq_memattr = ParallelMux(Seq(
    (p_rxreq_readshared,      Cacheable.U | EWA.U | Allocate.U),
    (p_rxreq_readunique,      Cacheable.U | EWA.U | Allocate.U)
  ))

  // Field 'SnpAttr':
  //    [ CCHI Opcode ]     [ CHI Opcode ]        [ Value / Source ]
  //  -  ReadShared          ReadNotSharedDirty    1
  //  -  ReadUnique          ReadUnique            1
  //                         MakeReadUnique        1
  val txreq_snpattr = ParallelMux(Seq(
    (p_rxreq_readshared,      true.B),
    (p_rxreq_readunique,      true.B)
  ))

  // Field 'ExpCompAck':
  //    [ CCHI Opcode ]     [ CHI Opcode ]        [ Value / Source ]
  //  -  ReadShared          ReadNotSharedDirty    1
  //  -  ReadUnique          ReadUnique            1
  //                         MakeReadUnique        1
  val txreq_expcompack = ParallelMux(Seq(
    (p_rxreq_readshared,      true.B),
    (p_rxreq_readunique,      true.B)
  ))

  io.DnTXREQ.valid := s_dn_txreq
  io.DnTXREQ.bits.QoS.get := 14.U // Default at 14, (**DOT NOT use 15**, maybe better policy in future)
  io.DnTXREQ.bits.TgtID.get := 0.U // Support E-SAM only currently
  io.DnTXREQ.bits.SrcID.get := nodeId.U
  io.DnTXREQ.bits.TxnID.get := 0.U // TODO: TxnID translations
  io.DnTXREQ.bits.ReturnNID_StashNID_SLCRepHint.get := 0.U // Not providing SLCRepHint/StashNID, default to 0
  io.DnTXREQ.bits.StashNIDValid_Endian_Deep.get := 0.U
  io.DnTXREQ.bits.ReturnTxnID_StashLPIDValid_StashLPID.get := 0.U
  io.DnTXREQ.bits.Opcode.get := txreq_opcode
  io.DnTXREQ.bits.Size.get := Mux(txreq_pcrdreturn, 0.U, txreq_size)
  io.DnTXREQ.bits.Addr.get := Mux(txreq_pcrdreturn, 0.U, io.tshr_paddr)
  io.DnTXREQ.bits.NS.get := 0.U // TODO: confirm default NS value or NS mechanism
  io.DnTXREQ.bits.LikelyShared.get := Mux(txreq_pcrdreturn, 0.U, txreq_likelyshared)
  io.DnTXREQ.bits.AllowRetry.get := !p_txreq_reissue
  io.DnTXREQ.bits.Order.get := Mux(txreq_pcrdreturn, 0.U, txreq_order)
  io.DnTXREQ.bits.PCrdType.get := Mux(!p_txreq_reissue, 0.U, p_retryack_pcrdtype)
  io.DnTXREQ.bits.MemAttr.get := Mux(txreq_pcrdreturn, 0.U, txreq_memattr)
  io.DnTXREQ.bits.SnpAttr_DoDWT.get := Mux(txreq_pcrdreturn, 0.U, txreq_snpattr)
  io.DnTXREQ.bits.LPID_PGroupID_StashGroupID_TagGroupID.get := 0.U // Not supporting Persistence and MTE
  io.DnTXREQ.bits.Excl_SnoopMe.get := 0.U
  io.DnTXREQ.bits.ExpCompAck.get := Mux(txreq_pcrdreturn, 0.U, txreq_expcompack)
  io.DnTXREQ.bits.TagOp.get := 0.U // Not supporting MTE
  io.DnTXREQ.bits.TraceTag.get := 0.U // TODO: maybe wire with L1-L2 TraceTag
  io.DnTXREQ.bits.MPAM.foreach(_ := 0.U) // TODO: Not supporting MTE, wire up with NS bit
  io.DnTXREQ.bits.RSVDC.foreach(_ := 0.U)

  val fire_txreq_evict = io.DnTXREQ.fire && io.DnTXREQ.bits.Opcode.get === CHI_Evict.U
  val fire_txreq_writeevictfull = io.DnTXREQ.fire && io.DnTXREQ.bits.Opcode.get === CHI_WriteEvictFull.U
  val fire_txreq_writeevictorevict = io.DnTXREQ.fire && io.DnTXREQ.bits.Opcode.get === CHI_WriteEvictOrEvict.U
  val fire_txreq_writebackfull = io.DnTXREQ.fire && io.DnTXREQ.bits.Opcode.get === CHI_WriteBackFull.U
  // ----------------------------------------------------------------

  // -- Interactions with downstream CHI RXRSP, RXDAT channel
  // TODO: handle RespErr here or somewhere else
  val dn_rxdat_compdata = dn_rxdat_opcode.is(CHI_CompData)
  val dn_rxdat_compdata_first = dn_rxdat_compdata && w_dn_rd_data0 && w_dn_rd_data2
  val dn_rxdat_datasepresp = dn_rxdat_opcode.is(CHI_DataSepResp)
  val dn_rxdat_datasepresp_first = dn_rxdat_datasepresp && w_dn_rd_data0 && w_dn_rd_data2

  val dn_rxrsp_comp = dn_rxdat_opcode.is(CHI_Comp)
  val dn_rxrsp_respsepdata = dn_rxdat_opcode.is(CHI_RespSepData)
  val dn_rxrsp_compdbidresp = dn_rxdat_opcode.is(CHI_CompDBIDResp)

  val expect_dn_rd_comp_and_data = rxreq_unsatisfied_readshared ||
                                   rxreq_unsatisfied_readunique

  val expect_dn_evict_comp = fire_txreq_evict

  val expect_dn_evict_compdbid = fire_txreq_writebackfull ||
                                 fire_txreq_writeevictfull ||
                                 fire_txreq_writeevictorevict

  val dn_rd_decided = !(w_dn_rd_data0 && w_dn_rd_data2)

  when (expect_dn_rd_comp_and_data) {
    w_dn_rd_data0 := true.B
    w_dn_rd_data2 := true.B
    w_dn_rd_comp := true.B
  }

  when (expect_dn_evict_comp) {
    w_dn_evict_comp := true.B
  }

  when (expect_dn_evict_compdbid) {
    w_dn_evict_compdbid := true.B
  }

  when (dn_rxrsp_comp) {
    p_homenid := io.DnRXRSP.bits.SrcID.get
    p_dbid := io.DnRXRSP.bits.DBID.get
    w_dn_rd_data0 := false.B
    w_dn_rd_data2 := false.B
    w_dn_rd_comp := false.B
    w_dn_evict_comp := false.B
    w_dn_evict_compdbid := false.B
  }

  when (dn_rxrsp_compdbidresp) {
    p_homenid := io.DnRXRSP.bits.SrcID.get
    p_dbid := io.DnRXRSP.bits.DBID.get
    w_dn_evict_compdbid := false.B
  }

  when (dn_rxdat_compdata) {
    p_homenid := io.DnRXDAT.bits.HomeNID.get
    p_dbid := io.DnRXDAT.bits.DBID.get
    w_dn_rd_comp := false.B
    when (io.DnRXDAT.bits.DataID.get === 0.U) {
      w_dn_rd_data0 := false.B
    }.otherwise {
      w_dn_rd_data2 := false.B
    }
  }

  when (dn_rxrsp_respsepdata) {
    p_homenid := io.DnRXRSP.bits.SrcID.get
    p_dbid := io.DnRXRSP.bits.DBID.get
    w_dn_rd_comp := false.B
  }

  when (dn_rxdat_datasepresp) {
    p_homenid := io.DnRXDAT.bits.HomeNID.get
    p_dbid := io.DnRXDAT.bits.DBID.get
    when (io.DnRXDAT.bits.DataID.get === 0.U) {
      w_dn_rd_data0 := false.B
    }.otherwise {
      w_dn_rd_data2 := false.B
    }
  }

  assert(!(w_dn_evict_comp && dn_rxrsp_compdbidresp), "")
  // ----------------------------------------------------------------

  // -- Interactions with downstream CHI TXRSP channel
  val dn_txrsp_compack = io.DnTXRSP.fire && io.DnTXRSP.bits.Opcode.get === CHI_CompAck.U

  val allow_dn_rd_compack = dn_rxdat_compdata_first ||
                            dn_rxrsp_comp ||
                            dn_rxrsp_respsepdata

  val issue_dn_rd_compack = w_s_dn_rd_compack && allow_dn_rd_compack

  val issue_dn_evict_compack = w_dn_evict_compdbid && dn_rxrsp_comp

  when (issue_dn_rd_compack) {
    s_dn_rd_compack := true.B
  }

  when (issue_dn_evict_compack) {
    s_dn_evict_compack := true.B
  }

  when (dn_txrsp_compack) {
    s_dn_rd_compack := false.B
    s_dn_evict_compack := false.B
  }

  io.DnTXRSP.valid := s_dn_rd_compack || s_dn_evict_compack
  io.DnTXRSP.bits.QoS.get := 14.U // Default at 14, (**DOT NOT use 15**, maybe better policy in future)
  io.DnTXRSP.bits.TgtID.get := p_homenid
  io.DnTXRSP.bits.SrcID.get := nodeId.U
  io.DnTXRSP.bits.TxnID.get := p_dbid
  io.DnTXRSP.bits.Opcode.get := CHI_CompAck.U
  io.DnTXRSP.bits.RespErr.get := 0.U
  io.DnTXRSP.bits.Resp.get := 0.U
  io.DnTXRSP.bits.FwdState.get := 0.U
  io.DnTXRSP.bits.CBusy.get := 0.U
  io.DnTXRSP.bits.DBID.get := 0.U
  io.DnTXRSP.bits.PCrdType.get := 0.U
  io.DnTXRSP.bits.TagOp.get := 0.U
  io.DnTXRSP.bits.TraceTag.get := 0.U // TODO: maybe wire with L2-L3 TraceTag
  // ----------------------------------------------------------------
  
  // -- Interactions with downstream CHI TXDAT channel
  val dn_txdat_cbwrdata0 = io.DnTXDAT.fire && io.DnTXDAT.bits.Opcode.get === CHI_CopyBackWrData.U && io.DnTXDAT.bits.DataID.get === 0.U

  val allow_dn_evict_cbwrdata0 = !w_snpresp0 && !w_ds_resp
  val allow_dn_evict_cbwrdata2 = !w_snpresp2 && !w_ds_resp

  val sched_dn_evict_cbwrdata = w_dn_evict_compdbid && dn_rxrsp_compdbidresp

  val issue_dn_evict_cbwrdata0 = w_s_dn_evict_cbwrdata0 && allow_dn_evict_cbwrdata0
  val issue_dn_evict_cbwrdata2 = w_s_dn_evict_cbwrdata2 && allow_dn_evict_cbwrdata2

  when (sched_dn_evict_cbwrdata) {
    w_s_dn_evict_cbwrdata0 := true.B
    w_s_dn_evict_cbwrdata2 := true.B
  }

  when (issue_dn_evict_cbwrdata0) {
    w_s_dn_evict_cbwrdata0 := false.B
    s_dn_evict_cbwrdata0 := true.B
  }

  when (issue_dn_evict_cbwrdata2) {
    w_s_dn_evict_cbwrdata2 := false.B
    s_dn_evict_cbwrdata2 := true.B
  }

  io.DnTXDAT.valid := s_dn_evict_cbwrdata0 || s_dn_evict_cbwrdata2
  io.DnTXDAT.bits.QoS.get := 14.U // Default at 14, (**DOT NOT use 15**, maybe better policy in future)
  io.DnTXDAT.bits.TgtID.get := p_homenid
  io.DnTXDAT.bits.SrcID.get := nodeId.U
  io.DnTXDAT.bits.TxnID.get := p_dbid
  io.DnTXDAT.bits.HomeNID.get := 0.U
  io.DnTXDAT.bits.Opcode.get := CHI_CopyBackWrData.U
  io.DnTXDAT.bits.RespErr.get := 0.U // TODO: RespErr
  io.DnTXDAT.bits.Resp.get := Mux(
    dirResult.state === MetaState.US || dirResult.state === MetaState.UU,
    Mux(dirResult.dirty, CHIFieldResp.CopyBackWrData_UD_PD.U, CHIFieldResp.CopyBackWrData_UC.U),
    Mux(dirResult.state === MetaState.S, CHIFieldResp.CopyBackWrData_SC.U, CHIFieldResp.CopyBackWrData_I.U)
  )
  io.DnTXDAT.bits.FwdState.get := 0.U
  io.DnTXDAT.bits.CBusy.get := 0.U
  io.DnTXDAT.bits.DBID.get := 0.U
  io.DnTXDAT.bits.CCID.get := 0.U
  io.DnTXDAT.bits.DataID.get := Mux(s_dn_evict_cbwrdata0, 0.U, 2.U)
  io.DnTXDAT.bits.TagOp.get := 0.U
  io.DnTXDAT.bits.Tag.get := 0.U
  io.DnTXDAT.bits.TU.get := 0.U
  io.DnTXDAT.bits.TraceTag.get := 0.U // TODO: maybe wire with L2-L3 TraceTag
  io.DnTXDAT.bits.RSVDC.foreach(_ := 0.U)
  io.DnTXDAT.bits.BE.get := Fill(paramCHI.datBEWidth, dirResult.state =/= MetaState.I)
  // *NOTICE: Data, DataCheck, Poison is assigned in TSHR top
  // ----------------------------------------------------------------

  // -- Interactions with upstream CCHI RXRSP channel
  val up_rxrsp_compack = up_rxrsp_opcode.is(CCHIOpcode.CompAck)

  val expect_up_rd_compack_sat = rxreq_satisfied_readunique ||
                                 rxreq_satisfied_readshared

  val expect_up_rd_compack_unsat = (p_rxreq_readunique || p_rxreq_readshared) &&
                                   (dn_rxrsp_comp || dn_rxdat_compdata_first || dn_rxrsp_respsepdata)

  val expect_up_rd_compack = expect_up_rd_compack_sat || expect_up_rd_compack_unsat

  when (expect_up_rd_compack) {
    w_up_rd_compack := true.B
  }

  when (up_rxrsp_compack) {
    w_up_rd_compack := false.B
  }

  assert(!(up_rxrsp_compack && !w_up_rd_compack), s"Receiving upstream RXRSP CompAck on non-valid 'w_up_rd_compack' in TSHR #${tshrId} REQ vPipe")
  // ----------------------------------------------------------------

  // -- Interactions with TSHR local meta
  val dn_rxdat_compdata_first_UC = dn_rxdat_compdata_first && io.DnRXDAT.bits.Resp.get === CHIFieldResp.CompData_UC.U
  val dn_rxdat_compdata_first_UD_PD = dn_rxdat_compdata_first && io.DnRXDAT.bits.Resp.get === CHIFieldResp.CompData_UD_PD.U
  val dn_rxdat_compdata_first_SC = dn_rxdat_compdata_first && io.DnRXDAT.bits.Resp.get === CHIFieldResp.CompData_SC.U

  val dn_rxdat_datasepresp_first_UC = dn_rxdat_datasepresp_first && io.DnRXDAT.bits.Resp.get === CHIFieldResp.DataSepResp_UC.U
  val dn_rxdat_datasepresp_first_UD_PD = dn_rxdat_datasepresp_first && io.DnRXDAT.bits.Resp.get === CHIFieldResp.DataSepResp_UD_PD.U
  val dn_rxdat_datasepresp_first_SC = dn_rxdat_datasepresp_first && io.DnRXDAT.bits.Resp.get === CHIFieldResp.DataSepResp_SC.U

  // - ReadUnique related meta/tag updates
  val meta_wr_state_readunique_UU_sat = active && p_rxreq_readunique &&
                                        sa_resp_decision &&
                                        dirResult.state === MetaState.US

  // 'Resp' not checked for this, because it was already constrained by CHI specification, and only goes to UU
  // with assertions provided
  val meta_wr_state_readunique_UU_unsat = active && p_rxreq_readunique && /*(
                                            dn_rxdat_compdata_first_UC ||
                                            dn_rxdat_compdata_first_UD_PD ||
                                            dn_rxdat_datasepresp_first_UC ||
                                            dn_rxdat_datasepresp_first_UD_PD
                                          ) && */ (dn_rxdat_compdata_first || dn_rxdat_datasepresp_first)

  assert(!(meta_wr_state_readunique_UU_unsat && dn_rxdat_compdata_first && !dn_rxdat_compdata_first_UC && !dn_rxdat_compdata_first_UD_PD),
    "The subsequent of upstream ReadUnique received CompData with unexpected Resp from downstream (expecting UC, UD_PD)")
  assert(!(meta_wr_state_readunique_UU_unsat && dn_rxdat_datasepresp_first && !dn_rxdat_datasepresp_first_UC && !dn_rxdat_datasepresp_first_UD_PD),
    "The subsequent of upstream ReadUnique received DataSepResp with unexpected Resp from downstream (expecting UC, UD_PD)")

  val meta_wr_state_readunique_UU = meta_wr_state_readunique_UU_sat || meta_wr_state_readunique_UU_unsat

  val meta_wr_dirty_readunique_set = active && p_rxreq_readunique &&
                                     (dn_rxdat_compdata_first_UD_PD || dn_rxdat_datasepresp_first_UD_PD)

  val meta_wr_client_readunique_set = active && p_rxreq_readunique &&
                                      up_rxrsp_compack

  val tag_wr_readunique = rxreq_readunique && !dirResult.hit
  // --------------------------------

  // - ReadShared related meta/tag updates
  val meta_wr_state_readshared_UU_sat = active && p_rxreq_readshared &&
                                        sa_resp_decision &&
                                        dirResult.state === MetaState.US &&
                                        !p_rxreq_peer_present &&
                                        configReadSharedPromotionFromUS

  val meta_wr_state_readshared_US_sat = active && p_rxreq_readshared &&
                                        sa_resp_decision &&
                                        dirResult.state === MetaState.UU &&
                                        (configReadSharedDemotionFromUU || p_rxreq_peer_present)

  val meta_wr_state_readshared_UU_unsat = active && p_rxreq_readshared && (
                                            dn_rxdat_compdata_first_UC ||
                                            dn_rxdat_compdata_first_UD_PD ||
                                            dn_rxdat_datasepresp_first_UC ||
                                            dn_rxdat_datasepresp_first_UD_PD
                                          ) && !configReadSharedDemotionFromUU

  val meta_wr_state_readshared_US_unsat = active && p_rxreq_readshared && (
                                            dn_rxdat_compdata_first_UC ||
                                            dn_rxdat_compdata_first_UD_PD ||
                                            dn_rxdat_datasepresp_first_UC ||
                                            dn_rxdat_datasepresp_first_UD_PD
                                          ) && configReadSharedDemotionFromUU

  val meta_wr_state_readshared_S_unsat = active && p_rxreq_readshared && (
                                            dn_rxdat_compdata_first_SC ||
                                            dn_rxdat_datasepresp_first_SC  
                                         )

  val meta_wr_state_readshared_UU = meta_wr_state_readshared_UU_sat ||
                                    meta_wr_state_readshared_UU_unsat
  val meta_wr_state_readshared_US = meta_wr_state_readshared_US_sat ||
                                    meta_wr_state_readshared_US_unsat
  val meta_wr_state_readshared_S = meta_wr_state_readshared_S_unsat

  val meta_wr_dirty_readshared_set = active && p_rxreq_readshared &&
                                     (dn_rxdat_compdata_first_UD_PD || dn_rxdat_datasepresp_first_UD_PD)

  val meta_wr_client_readshared_set = p_rxreq_readshared &&
                                      up_rxrsp_compack
  
  val tag_wr_readshared = rxreq_readshared && !dirResult.hit
  // --------------------------------

  // - EvictBack related meta/tag updates
  val meta_wr_state_evictback_I_evict = s_evict && evictback_txreq_opcode === CHI_Evict.U

  val meta_wr_state_evictback_I_write = w_dn_evict_compdbid && 
                                        (dn_rxrsp_comp || dn_rxrsp_compdbidresp)

  val meta_wr_state_evictback_I = meta_wr_state_evictback_I_evict ||
                                  meta_wr_state_evictback_I_write

  val meta_wr_dirty_evictback_clr = meta_wr_state_evictback_I
  // --------------------------------

  val meta_wr_dirty_sa_set = sa_resp_decision && io.fromSA.PASSDIRTY
  // --------------------------------

  val meta_wr_state_UU = meta_wr_state_readunique_UU ||
                         meta_wr_state_readshared_UU

  val meta_wr_state_US = meta_wr_state_readshared_US

  val meta_wr_state_S = meta_wr_state_readshared_S

  val meta_wr_state_I = meta_wr_state_evictback_I

  val meta_wr_state = meta_wr_state_UU || meta_wr_state_US || meta_wr_state_S || meta_wr_state_I

  val meta_wr_dirty_set = meta_wr_dirty_sa_set ||
                          meta_wr_dirty_readunique_set

  val meta_wr_dirty_clr = meta_wr_dirty_evictback_clr

  val meta_wr_dirty = meta_wr_dirty_set || meta_wr_dirty_clr

  val meta_wr_client_set = meta_wr_client_readunique_set ||
                           meta_wr_client_readshared_set

  val meta_wr_client_clr = false.B

  val meta_wr_client = meta_wr_client_set || meta_wr_client_clr

  val tag_wr = tag_wr_readunique ||
               tag_wr_readshared

  io.tshr_tag_write_en := tag_wr

  io.tshr_meta_write_en.state := meta_wr_state
  io.tshr_meta_write_meta.state := ParallelMux(Seq(
    (meta_wr_state_UU, MetaState.UU),
    (meta_wr_state_US, MetaState.US),
    (meta_wr_state_S , MetaState.S ),
    (meta_wr_state_I , MetaState.I )
  ))

  io.tshr_meta_write_en.dirty := meta_wr_dirty
  io.tshr_meta_write_meta.dirty := meta_wr_dirty_set

  io.tshr_meta_write_en.clients.zip(p_rxreq_client).foreach { case (en, client) => en := client && meta_wr_client }
  io.tshr_meta_write_meta.clients.zip(p_rxreq_client).foreach { case (meta, client) => meta := client && meta_wr_client_set }
  // ----------------------------------------------------------------

  // -- Interactions with TSHR local data and Data Storage Read
  val ds_rd_readunique_sat = rxreq_satisfied_readunique &&
                             (rxreq.ExpCompData || !rxreq_client_present)

  val ds_rd_readunique_unsat_lazy = p_rxreq_readunique &&
                                    (p_rxreq.ExpCompData || !p_rxreq_client_present) &&
                                    dn_rxrsp_comp

  val ds_rd_readunique_unsat_early = rxreq_unsatisfied_readunique &&
                                     dirResult.hit &&
                                     (rxreq.ExpCompData || !rxreq_client_present)

  val ds_rd_readunique_unsat = Mux(configReadUniqueHitSPreferReadDS,
                                 ds_rd_readunique_unsat_early,
                                 ds_rd_readunique_unsat_lazy)

  val ds_rd_readunique = ds_rd_readunique_sat || ds_rd_readunique_unsat

  val ds_rd_readshared = rxreq_readshared && dirResult.hit

  // *NOTE: If SA received SnpRespData from upstream, the DS Read would be dropped by Data Storage Proxy.
  val ds_rd_evictback = p_rxreq_evictback &&
                        (fire_txreq_writeevictfull ||
                         fire_txreq_writeevictorevict ||
                         fire_txreq_writebackfull)

  val ds_rd = ds_rd_readunique ||
              ds_rd_readshared ||
              ds_rd_evictback

  val ds_cancel_readunique = p_rxreq_readunique &&
                             (sa_respdata_first ||
                              dn_rxdat_compdata_first || dn_rxrsp_respsepdata)

  val ds_cancel_readshared = p_rxreq_readshared &&
                             sa_respdata_first

  val ds_cancel_evictback = p_rxreq_evictback &&
                            dn_rxrsp_comp

  val ds_cancel = ds_cancel_readunique ||
                  ds_cancel_readshared ||
                  ds_cancel_evictback

  when (ds_rd) {
    w_ds_resp := true.B
  }

  when (ds_cancel) {
    w_ds_resp := false.B
  }

  when (io.ds_rd_done) {
    w_ds_resp := false.B
  }

  io.ds_rd_en := ds_rd
  io.ds_rd_cancel := w_ds_resp && ds_cancel
  // ----------------------------------------------------------------

  // -- Interactions with upstream CCHI TXRSP channel
  val up_txrsp_comp = io.UpTXRSP.fire && io.UpTXRSP.bits.Opcode === CCHIOpcode.Comp.U

  val allow_up_rd_comp = sa_resp_decided && 
                         dn_rd_decided && 
                         !w_dn_rd_comp

  val sched_up_rd_comp_readunique_sat = rxreq_satisfied_readunique &&
                                        rxreq_client_present && !rxreq.ExpCompData

  val sched_up_rd_comp_readunique_unsat = p_rxreq_readunique &&
                                          p_rxreq_client_present && !p_rxreq.ExpCompData &&
                                          (dn_rxrsp_comp || dn_rxdat_compdata_first || dn_rxdat_datasepresp_first)

  val sched_up_rd_comp_readunique = sched_up_rd_comp_readunique_sat ||
                                    sched_up_rd_comp_readunique_unsat

  val sched_up_rd_comp = sched_up_rd_comp_readunique

  when (sched_up_rd_comp) {
    w_s_up_rd_comp := true.B
  }

  val issue_up_rd_comp = w_s_up_rd_comp && allow_up_rd_comp

  when (issue_up_rd_comp) {
    w_s_up_rd_comp := false.B
    s_up_rd_comp := true.B
  }

  when (up_txrsp_comp) {
    s_up_rd_comp := false.B
  }

  val up_txrsp_opcode = ParallelPriorityMux(Seq(
    (s_up_rd_comp, CCHIOpcode.Comp.U)
  ))

  val up_txrsp_resp = ParallelPriorityMux(Seq(
    (s_up_rd_comp, ParallelPriorityMux(Seq(
      (p_rxreq_readunique, CCHIResp.UC.U),
      (p_rxreq_readshared, Mux(dirResult.state === MetaState.UU || meta_wr_state_UU, CCHIResp.UC.U, CCHIResp.SC.U))
    )))
  ))

  io.UpTXRSP.valid := s_up_rd_comp
  io.UpTXRSP.bits.TxnID := p_rxreq.TxnID
  io.UpTXRSP.bits.SrcID := nodeId.U
  io.UpTXRSP.bits.TgtID := p_rxreq.SrcID
  io.UpTXRSP.bits.DBID := 0.U // TODO: TxnID translations
  io.UpTXRSP.bits.Opcode := up_txrsp_opcode
  io.UpTXRSP.bits.RespErr := 0.U // TODO: RespErr
  io.UpTXRSP.bits.Resp := up_txrsp_resp
  io.UpTXRSP.bits.CBusy := 0.U // TODO: CBusy, may be assigned in TSHR top
  io.UpTXRSP.bits.WayValid := s_up_rd_comp
  io.UpTXRSP.bits.Way := dirResult.way
  io.UpTXRSP.bits.TraceTag := false.B // TODO: TraceTag propagation
  // ----------------------------------------------------------------

  // -- Interactions with upstream CCHI TXDAT channel
  val up_txdat_compdata0 = io.UpTXDAT.fire && io.UpTXDAT.bits.Opcode === CCHIOpcode.CompData.U && io.UpTXDAT.bits.DataID === 0.U
  val up_txdat_compdata2 = io.UpTXDAT.fire && io.UpTXDAT.bits.Opcode === CCHIOpcode.CompData.U && io.UpTXDAT.bits.DataID === 1.U

  val allow_up_rd_compdata0 = !w_ds_resp && !w_snpresp0 && !w_dn_rd_data0 && !io.L1EVT_active
  val allow_up_rd_compdata2 = !w_ds_resp && !w_snpresp2 && !w_dn_rd_data2 && !io.L1EVT_active

  val sched_up_rd_compdata_sat_readunique = rxreq_satisfied_readunique && 
                                            (!rxreq_client_present || rxreq.ExpCompData)
                                
  val sched_up_rd_compdata_sat_readshared = rxreq_satisfied_readshared

  val sched_up_rd_compdata_sat = sched_up_rd_compdata_sat_readunique ||
                                 sched_up_rd_compdata_sat_readshared

  val sched_up_rd_compdata_unsat_readunique = (rxreq_unsatisfied_readunique &&
                                               rxreq.ExpCompData) ||
                                              (p_rxreq_readunique &&
                                               (!p_rxreq_client_present && !p_rxreq.ExpCompData) &&
                                               (dn_rxrsp_comp || dn_rxdat_compdata_first || dn_rxdat_datasepresp_first))

  val sched_up_rd_compdata_unsat_readshared = rxreq_unsatisfied_readshared

  val sched_up_rd_compdata_unsat = sched_up_rd_compdata_unsat_readunique ||
                                   sched_up_rd_compdata_unsat_readshared

  val sched_up_rd_compdata = sched_up_rd_compdata_sat ||
                             sched_up_rd_compdata_unsat

  when (sched_up_rd_compdata) {
    w_s_up_rd_compdata0 := true.B
    w_s_up_rd_compdata2 := true.B
  }

  val issue_up_rd_compdata0 = w_s_up_rd_compdata0 && allow_up_rd_compdata0
  val issue_up_rd_compdata2 = w_s_up_rd_compdata2 && allow_up_rd_compdata2

  when (issue_up_rd_compdata0) {
    w_s_up_rd_compdata0 := false.B
    s_up_rd_compdata0 := true.B
  }

  when (issue_up_rd_compdata2) {
    w_s_up_rd_compdata2 := false.B
    s_up_rd_compdata2 := true.B
  }

  when (up_txdat_compdata0) {
    s_up_rd_compdata0 := false.B
  }

  when (up_txdat_compdata2) {
    s_up_rd_compdata2 := false.B
  }

  val up_txdat_resp = ParallelPriorityMux(Seq(
    (p_rxreq_readunique, CCHIResp.UC.U),
    (p_rxreq_readshared, Mux(dirResult.state === MetaState.UU || meta_wr_state_UU, CCHIResp.UC.U, CCHIResp.SC.U))
  ))

  val up_txdat_dataid = Mux(s_up_rd_compdata0, 0.U, 1.U) // TODO: cirtical word first maybe

  io.UpTXDAT.valid := s_up_rd_compdata0 || s_up_rd_compdata2
  io.UpTXDAT.bits.TxnID := p_rxreq.TxnID
  io.UpTXDAT.bits.SrcID := nodeId.U
  io.UpTXDAT.bits.TgtID := p_rxreq.SrcID
  io.UpTXDAT.bits.DBID := 0.U // TODO: TxnID translations
  io.UpTXDAT.bits.Opcode := CCHIOpcode.CompData.U
  io.UpTXDAT.bits.RespErr := 0.U // TODO: RespErr
  io.UpTXDAT.bits.Resp := up_txdat_resp
  io.UpTXDAT.bits.DataSource := 0.U // TODO: DataSource
  io.UpTXDAT.bits.CBusy := 0.U //  TODO: CBusy, may be assigned in TSHR top
  io.UpTXDAT.bits.WayValid := true.B
  io.UpTXDAT.bits.Way := dirResult.way
  io.UpTXDAT.bits.DataID := up_txdat_dataid
  io.UpTXDAT.bits.TraceTag := false.B // TODO: TraceTag propagation
  // ----------------------------------------------------------------

  // -- Interactions with replacer and eviction through loop-back REQ
  val txreq_evictback_peer = io.UpTXREQ.fire && io.UpTXREQ.bits.Opcode === CCHIOpcode.EvictBack.U

  val expect_replace = !dirResult.hit && (
                           rxreq_readunique ||
                           rxreq_readshared)

  val trigger_replace = w_s_repl &&
                        (dn_rxdat_compdata_first || dn_rxdat_datasepresp_first)

  when (expect_replace) {
    w_s_repl := true.B
  }

  when (trigger_replace) {
    w_s_repl := false.B
    s_repl := true.B
  }

  when (io.repl_done) {
    s_repl := false.B
    s_evict := true.B
  }

  when (txreq_evictback_peer) {
    s_evict := false.B
  }

  val lock_dir = expect_replace || p_rxreq_evictback
  val lock_ds = expect_replace || p_rxreq_evictback

  val unlock_self_evictback = RegNext(meta_wr_state_evictback_I) && !meta_wr_state_evictback_I

  val unlock_dir = io.self_unlock_dir || unlock_self_evictback
  val unlock_ds = io.self_unlock_ds || unlock_self_evictback

  when (lock_dir) {
    w_unlock_dir := true.B
  }

  when (lock_ds) {
    w_unlock_ds := true.B
  }

  when (unlock_dir) {
    w_unlock_dir := false.B
  }

  when (unlock_ds) {
    w_unlock_ds := false.B
  }

  io.repl_en := s_repl

  io.dir_wb_locked := w_unlock_dir
  io.ds_wb_locked := w_unlock_ds

  // Activate Directory write-back immediately on replacement Directory lock released by eviction
  // to clear the replacer reading lock in Directory
  io.dir_wb_aux := unlock_dir

  io.UpTXREQ.valid := s_evict
  io.UpTXREQ.bits.TxnID := 0.U // TODO: TxnID translations
  io.UpTXREQ.bits.SrcID := nodeId.U
  io.UpTXREQ.bits.TgtID := nodeId.U
  io.UpTXREQ.bits.Opcode := CCHIOpcode.EvictBack.U
  io.UpTXREQ.bits.Size := CCHISize.B64.U
  io.UpTXREQ.bits.Addr := io.repl_resp.paddr
  io.UpTXREQ.bits.NS := false.B
  io.UpTXREQ.bits.Order := 0.U
  io.UpTXREQ.bits.MemAttr := 0.U
  io.UpTXREQ.bits.Excl := 0.U
  io.UpTXREQ.bits.ExpCompData := false.B
  io.UpTXREQ.bits.WayValid := false.B
  io.UpTXREQ.bits.Way := 0.U
  io.UpTXREQ.bits.TraceTag := false.B
  // ----------------------------------------------------------------

  // -- Interactions with peer Refill unlock
  val evictback_peer_unlock_dir = p_rxreq_evictback

  val evictback_peer_unlock_ds_immediate = p_rxreq_evictback &&
                                           io.ds_rd_done

  val evictback_peer_unlock_ds_wait = p_rxreq_evictback &&
                                      !io.ds_rd_done

  val evictback_peer_unlock_ds_late = w_evict_peer_unlock_ds &&
                                      io.ds_rd_done

  val evictback_peer_unlock_ds = evictback_peer_unlock_ds_immediate ||
                                 evictback_peer_unlock_ds_late                                      

  when (evictback_peer_unlock_ds_wait) {
    w_evict_peer_unlock_ds := true.B
  }

  when (io.ds_rd_done) {
    w_evict_peer_unlock_ds := false.B
  }

  io.peer_unlock_dir.zipWithIndex.foreach { case (unlock_dir, i) => {
    unlock_dir := evictback_peer_unlock_dir && p_rxreq.TxnID === i.U
  }}

  io.peer_unlock_ds.zipWithIndex.foreach { case (unlock_ds, i) => {
    unlock_ds := evictback_peer_unlock_ds && p_rxreq.TxnID === i.U
  }}
  // ----------------------------------------------------------------

  // -- Blocking same-PA RXSNP, on waiting of L1 CompAck
  io.blockRBE.SNP := w_up_rd_compack
  // ----------------------------------------------------------------


  // TODO list:
}
