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

object L2VPipeREQ {

  class PathTSHRPayloadRead(clientComponents: Seq[CCHIComponent]) extends Bundle {
    val p_paddr = UInt(48.W)
    val p_meta_state = new L2CacheState(clientComponents)
    val p_buf_data0_valid = Bool()
    val p_buf_data2_valid = Bool()
  }

  class PathTSHRPayloadWrite(clientComponents: Seq[CCHIComponent]) extends Bundle {
    val p_meta_state = new L2CacheState(clientComponents)
    val p_meta_wen = new Bundle {
      val local = Bool()
      val clients = Vec(clientComponents.size, Bool())
    }
  }

  class PathVPipeToTSHR(clientComponents: Seq[CCHIComponent]) extends Bundle {
    val UPTARGET = Output(Vec(clientComponents.size, Bool()))

    val DSRd = Output(Bool())
    val DSRdTXDATUp = Output(Bool())
    val DSRdTXDATDn = Output(Bool())
    val DropDSRd = Output(Bool())
    val BufWrComp0 = Input(Bool())
    val BufWrComp2 = Input(Bool())
    val BufWrTXDATUpComp0 = Input(Bool())
    val BufWrTXDATUpComp2 = Input(Bool())
  }
}

class L2VPipeREQ(clientComponents: Seq[CCHIComponent], tshrId: Int, nodeId: Int)(implicit val p: Parameters) 
    extends Module 
    with CHIRNFOpcodesREQ 
    with CHIRNFOpcodesRSP
    with CHIRNFOpcodesDAT {

  val io = IO(new Bundle {
    val UpRXREQ = Flipped(Valid(new FlitREQ))
    val UpRXRSP = Flipped(Valid(new FlitUpRSP))
    val UpRXDAT = Flipped(Valid(new FlitUpDATWithoutData))
    val UpTXRSP = Flipped(Decoupled(new FlitDnRSP))
    val UpTXDAT = Flipped(Decoupled(new FlitDnDATWithoutData))
    val DnRXRSP = Flipped(Valid(new CHIBundleRSP))
    val DnRXDAT = Flipped(Valid(new CHIBundleDAT))
    val DnTXRSP = Flipped(Valid(new CHIBundleRSP))
    val DnTXDAT = Flipped(Valid(new CHIBundleDAT))
    val reqSource = Input(Vec(clientComponents.size, Bool()))
    val payloadRead = Input(new L2VPipeREQ.PathTSHRPayloadRead(clientComponents))
    val payloadWrite = Output(new L2VPipeREQ.PathTSHRPayloadWrite(clientComponents))
    val blockRBE = Output(new L2RBE.PathVPipeBlock)
    val l1snp = Flipped(VecInit(clientComponents.map(new L2SnoopAgent.PathTSHRSnp(_))))
    val free = Output(Bool())
    val tshr = new L2VPipeREQ.PathVPipeToTSHR(clientComponents)
  })

  // === enchantment signals ===
  val meta_state_rd = io.payloadRead.p_meta_state

  val buf_data0_valid_rd = io.payloadRead.p_buf_data0_valid
  val buf_data2_valid_rd = io.payloadRead.p_buf_data2_valid

  val buf_data0_valid_next = buf_data0_valid_rd || io.tshr.BufWrComp0
  val buf_data2_valid_next = buf_data2_valid_rd || io.tshr.BufWrComp2

  // === vPipe payloads ===
  val p_req_source = Reg(Vec(clientComponents.size, Bool()))

  // === vPipe states ===
  val s_ds_rd = Reg(Bool())
  val s_ds_rduptxdat = Reg(Bool())
  val s_ds_rddntxdat = Reg(Bool())

  val s_dn_txreq = Reg(Bool())

  val s_dn_txdat0 = Reg(Bool())
  val s_dn_txdat2 = Reg(Bool())

  val w_dn_rxdat0 = Reg(Bool())
  val w_dn_rxdat2 = Reg(Bool())

  val s_up_txdat0 = Reg(Bool())
  val s_up_txdat2 = Reg(Bool())

  val w_up_rxdat0 = Reg(Bool())
  val w_up_rxdat2 = Reg(Bool())

  val w_dn_comp = Reg(Bool())
  val w_dn_dbid = Reg(Bool())
  val s_dn_compack = Reg(Bool())

  val w_up_compack = Reg(Bool())
  val s_up_compdbidresp = Reg(Bool())

  val w_up_snpresp0 = Reg(Vec(clientComponents.size, Bool()))
  val w_up_snpresp2 = Reg(Vec(clientComponents.size, Bool()))

  val w_buf_data0 = Reg(Bool())
  val w_buf_data2 = Reg(Bool())

  val w_up_txdat0_buf_data0 = Reg(Bool())
  val w_up_txdat2_buf_data2 = Reg(Bool())

  val w_up_txdat0_snpresp0 = Reg(Bool())
  val w_up_txdat2_snpresp2 = Reg(Bool())

  val this_active = ParallelOR(Seq(
    s_dn_txreq // TODO
  ))

  io.free := !this_active

  // === vPipe entrance flit opcode decoding ===
  val reqDecoder = Module(new CCHIREQOpcodeDecoder)
  reqDecoder.io.valid := io.UpRXREQ.valid
  reqDecoder.io.opcode := io.UpRXREQ.bits.Opcode

  val req_EvictBack = reqDecoder.is(CCHIOpcode.EvictBack)
  val req_ReadShared = reqDecoder.is(CCHIOpcode.ReadShared)
  val req_ReadUnique = reqDecoder.is(CCHIOpcode.ReadUnique)

  // === vPipe entrance REQ path classification (satisfied/unsatisifed) ===
  val req_satisfied = ParallelOR(Seq(
    req_ReadShared && meta_state_rd.local.satS,
    req_ReadUnique && meta_state_rd.local.satU
  ))

  // === vPipe entrance Snoop Agent interaction
  val req_l1snp_pending = VecInit(meta_state_rd.clients.map(_.surpass(meta_state_rd.local)))

  val req_l1snp_pending_any = req_l1snp_pending.asUInt.orR

  val req_l1snp_en = VecInit(meta_state_rd.clients.zip(io.reqSource).zip(req_l1snp_pending).map {
    case ((client, source), l1snp_pending) => ParallelOR(Seq(
      req_ReadShared && (!source /*|| alias */) && (l1snp_pending || client.satU),
      req_ReadUnique && (!source /*|| alias */) && (l1snp_pending || client.notI)
  ))})

  val req_l1snp_en_any = req_l1snp_en.asUInt.orR

  val req_l1snp_any = req_l1snp_pending_any || req_l1snp_en_any

  val req_l1snp_SnpMakeInvalid = false.B

  val req_l1snp_SnpToInvalid = ParallelOR(Seq(
    /*alias,*/
    req_ReadUnique
  ))

  val req_l1snp_SnpToShared = ParallelOR(Seq(
    req_ReadShared /* && !alias*/
  ))

  val req_l1snp_SnpToClean = false.B

  // === vPipe entrance payload initialization ===
  when (io.UpRXREQ.valid) {

    p_req_source := io.reqSource
  }

  // === vPipe entrance state initialization ===
  when (io.UpRXREQ.valid) {

    when (req_ReadShared || req_ReadUnique) {
      // set waiting for TSHR Buffer when pending snoop
      w_up_txdat0_snpresp0 := req_l1snp_any
      w_up_txdat2_snpresp2 := req_l1snp_any

      // set sending data to L1 immediately when data valid, no pending snoop and DS not forwarded
      s_up_txdat0 := buf_data0_valid_next && !req_l1snp_any
      s_up_txdat2 := buf_data2_valid_next && !req_l1snp_any
    }

    // when L1 REQ satisfied
    when (req_satisfied) {

      when (req_ReadShared || req_ReadUnique) {
        // set waiting for TSHR Buffer when data not available
        w_up_txdat0_buf_data0 := !buf_data0_valid_next
        w_up_txdat2_buf_data2 := !buf_data2_valid_next

        // set waiting for L1 CompAck
        w_up_compack := true.B
      }
    }

    // when L1 REQ unsatisfied
    .otherwise {

      // set scheduling for downstream TXREQ
      s_dn_txreq := true.B

      when (req_ReadShared || req_ReadUnique) {
        // set waiting for TSHR Buffer
        w_up_txdat0_buf_data0 := true.B
        w_up_txdat2_buf_data2 := true.B

        // set waiting for HN return data
        w_dn_rxdat0 := true.B
        w_dn_rxdat2 := true.B
      }

      // TODO
    }

    // 
  }

  req_l1snp_en.zip(w_up_snpresp0.zip(w_up_snpresp2)).foreach { case (en, (w0, w2)) => 
    when (en) { 
      w0 := true.B
      w2 := true.B
    } 
  }

  // initiating Data Storage reads by TSHR
  s_ds_rd := false.B
  s_ds_rddntxdat := false.B
  s_ds_rduptxdat := false.B
  
  when (io.UpRXREQ.valid && !buf_data0_valid_next && !buf_data2_valid_next) {

    when (req_ReadShared || req_ReadUnique) {
      s_ds_rduptxdat := true.B
    }
  }

  io.tshr.UPTARGET := io.reqSource

  io.tshr.DSRd := s_ds_rd
  io.tshr.DSRdTXDATUp := s_ds_rduptxdat
  io.tshr.DSRdTXDATDn := s_ds_rddntxdat

  // === L1 SNP waiting states related ===
  val clr_w_up_snpresp0 = VecInit(io.l1snp.map(sa => sa.SnpResp || sa.SnpRespData0Ptl))
  val clr_w_up_snpresp2 = VecInit(io.l1snp.map(sa => sa.SnpResp || sa.SnpRespData2Ptl))

  val next_w_up_snpresp0 = VecInit(w_up_snpresp0.zip(clr_w_up_snpresp0).map { case (w, clr) => w && !clr })
  val next_w_up_snpresp2 = VecInit(w_up_snpresp2.zip(clr_w_up_snpresp2).map { case (w, clr) => w && !clr })

  val end_w_up_snpresp0 = w_up_snpresp0.orR && !next_w_up_snpresp0.orR
  val end_w_up_snpresp2 = w_up_snpresp2.orR && !next_w_up_snpresp2.orR

  w_up_snpresp0.zip(clr_w_up_snpresp0).foreach { case (w_up_snpresp0, clr) => when (clr) { w_up_snpresp0 := false.B } }
  w_up_snpresp2.zip(clr_w_up_snpresp2).foreach { case (w_up_snpresp2, clr) => when (clr) { w_up_snpresp2 := false.B } }

  // waiting states before upstream/downstream TXDAT could get valid data
  // ! TODO: 需要处理所有 L1 Snoop 与 HN Read 数据交织的情况，且他们都有可能不返回数据
  // ! TODO: 此时需要考虑 L1 EVT 数据未完成的情况（仅对于非一致性读取 ReadOnce，一致读取不应该发生交织）
  val up_snpresp0_data_any = VecInit(io.l1snp.map(_.SnpRespData0Ptl)).orR
  val up_snpresp2_data_any = VecInit(io.l1snp.map(_.SnpRespData2Ptl)).orR

  val clr_w_up_txdat0_buf_data0 = io.tshr.BufWrComp0 || up_snpresp0_data_any && !w_dn_rxdat0
  val clr_w_up_txdat2_buf_data2 = io.tshr.BufWrComp2 || up_snpresp2_data_any && !w_dn_rxdat2

  val next_w_up_txdat0_buf_data0 = w_up_txdat0_buf_data0 && !clr_w_up_txdat0_buf_data0
  val next_w_up_txdat2_buf_data2 = w_up_txdat2_buf_data2 && !clr_w_up_txdat2_buf_data2

  val clr_w_up_txdat0_snpresp0 = end_w_up_snpresp0
  val clr_w_up_txdat2_snpresp2 = end_w_up_snpresp2

  val next_w_up_txdat0_snpresp0 = w_up_txdat0_snpresp0 && !clr_w_up_txdat0_snpresp0
  val next_w_up_txdat2_snpresp2 = w_up_txdat2_snpresp2 && !clr_w_up_txdat2_snpresp2

  val end_w_up_txdat0 = ParallelOR(Seq(w_up_txdat0_buf_data0, w_up_txdat0_snpresp0)) &&
    !ParallelOR(Seq(next_w_up_txdat0_buf_data0, next_w_up_txdat0_snpresp0))

  val end_w_up_txdat2 = ParallelOR(Seq(w_up_txdat2_buf_data2, w_up_txdat2_snpresp2)) &&
    !ParallelOR(Seq(next_w_up_txdat2_buf_data2, next_w_up_txdat2_snpresp2))

  when (clr_w_up_txdat0_buf_data0) { w_up_txdat0_buf_data0 := false.B }
  when (clr_w_up_txdat2_buf_data2) { w_up_txdat2_buf_data2 := false.B }

  when (clr_w_up_txdat0_snpresp0) { w_up_txdat0_snpresp0 := false.B }
  when (clr_w_up_txdat2_snpresp2) { w_up_txdat2_snpresp2 := false.B }

  io.tshr.DropDSRd := up_snpresp0_data_any && !w_dn_rxdat0 && w_up_txdat0_buf_data0 ||
                      up_snpresp2_data_any && !w_dn_rxdat2 && w_up_txdat2_buf_data2
                      
  // === Downstream RXDAT ===
  val dnRXDATDecoder = Module(new DATOpcodeDecoder(Seq(
    CHI_CompData
  ), true))
  dnRXDATDecoder.io.valid := io.DnRXDAT.valid
  dnRXDATDecoder.io.opcode := io.DnRXDAT.bits.Opcode.get

  // receiving HN CompData from RXDAT
  val clr_w_dn_rxdat0 = dnRXDATDecoder.is(CHI_CompData) && io.DnRXDAT.bits.DataID.get === 0.U
  val clr_w_dn_rxdat2 = dnRXDATDecoder.is(CHI_CompData) && io.DnRXDAT.bits.DataID.get === 2.U

  when (clr_w_dn_rxdat0) { w_dn_rxdat0 := false.B }
  when (clr_w_dn_rxdat2) { w_dn_rxdat2 := false.B }

  val first_dn_rxdat = (w_dn_rxdat0 && w_dn_rxdat2) && (clr_w_dn_rxdat0 || clr_w_dn_rxdat2)

  when (first_dn_rxdat) {
    w_up_compack := true.B
    s_dn_compack := true.B
  }
  // ======

  // scheduling upstream TXDAT
  when (end_w_up_txdat0) { s_up_txdat0 := true.B }
  when (end_w_up_txdat2) { s_up_txdat2 := true.B }

  // TODO

  // RBE blockings
  io.blockRBE.EVT := false.B
  io.blockRBE.SNP := req_satisfied || w_up_compack
  io.blockRBE.REQ // TODO
  
  // Snoop Agent connections
  io.l1snp.zip(req_l1snp_en).foreach { case (sa, en) =>
    sa.SnpMakeInvalid := en && req_l1snp_SnpMakeInvalid
    sa.SnpToInvalid := en && req_l1snp_SnpToInvalid
    sa.SnpToShared := en && req_l1snp_SnpToShared
    sa.SnpToClean := en && req_l1snp_SnpToClean
  }

  // TSHR connections
  io.tshr.DSRd // TODO
}
