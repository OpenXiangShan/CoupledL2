package oceanus.l2

import chisel3._
import chisel3.util._
import coupledL2.MSHRBuffer
import oceanus.compactchi.FlitREQ
import oceanus.chi.bundle.{CHIBundleDAT, CHIBundleREQ, CHIBundleRSP}
import oceanus.l2.tshr.L2RBE
import oceanus.l2.tshr.L2RBE.PathVPipeBlock
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate

object TSHRAlloc {
  class AllocBundle(implicit val p: Parameters) extends Bundle with HasL2Params {
    val idx = UInt(log2Ceil(nMSHR).W)
    val req = new FlitREQ
  }
}

class TSHRAlloc(implicit val p: Parameters) extends Module with HasL2Params {
  import TSHRAlloc._

  val io = IO(new Bundle {
    val rxreq = Flipped(Decoupled(new FlitREQ))
    val tshrIdle = Input(Vec(nMSHR, Bool()))
    val tshrAddr = Input(Vec(nMSHR, UInt(paramL2.physicalAddrWidth.W)))
    val alloc = Output(Valid(new AllocBundle))
    val mshrFull = Output(Bool())
    val addrConflict = Output(Bool())
    val blockFromVPipe = Output(new PathVPipeBlock)
  })

  val hasIdle = io.tshrIdle.asUInt.orR
  val allocOH = PriorityEncoderOH(io.tshrIdle)
  val allocIdx = OHToUInt(allocOH)
  val addrConflict = VecInit((0 until nMSHR).map(i =>
    !io.tshrIdle(i) && io.tshrAddr(i) === io.rxreq.bits.Addr
  )).asUInt.orR

  io.rxreq.ready := hasIdle && !addrConflict
  io.alloc.valid := io.rxreq.valid && io.rxreq.ready
  io.alloc.bits.idx := allocIdx
  io.alloc.bits.req := io.rxreq.bits
  io.mshrFull := !hasIdle
  io.addrConflict := io.rxreq.valid && addrConflict
  io.blockFromVPipe := 0.U.asTypeOf(new PathVPipeBlock)
  io.blockFromVPipe.REQ := !io.rxreq.ready
}

class DataArbiter(implicit val p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle {
    val fromFSM = Flipped(Vec(nMSHR, Decoupled(new CHIBundleDAT)))
    val txdat = Decoupled(new CHIBundleDAT)
  })

  private val arb = Module(new Arbiter(new CHIBundleDAT, nMSHR))
  arb.io.in <> io.fromFSM
  io.txdat <> arb.io.out
}

class ReqArbiter(implicit val p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle {
    val fromFSM = Flipped(Vec(nMSHR, Decoupled(new CHIBundleREQ)))
    val txreq = Decoupled(new CHIBundleREQ)
  })

  private val arb = Module(new Arbiter(new CHIBundleREQ, nMSHR))
  arb.io.in <> io.fromFSM
  io.txreq <> arb.io.out
}

class RspArbiter(implicit val p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle {
    val fromFSM = Flipped(Vec(nMSHR, Decoupled(new CHIBundleRSP)))
    val txrsp = Decoupled(new CHIBundleRSP)
  })

  private val arb = Module(new Arbiter(new CHIBundleRSP, nMSHR))
  arb.io.in <> io.fromFSM
  io.txrsp <> arb.io.out
}

class L2Slice(implicit val p: Parameters) extends Module with HasL2Params {
  private val enableDirStateProbe = p(L2SliceDirStateProbeEnableKey)
  private val enableDirPlruProbe = p(L2SliceDirPlruProbeEnableKey)
  val io = IO(new Bundle {
    val rxreq = Flipped(Decoupled(new FlitREQ))
    val txdat = Decoupled(new CHIBundleDAT)
    val txreq = Decoupled(new CHIBundleREQ)
    val txrsp = Decoupled(new CHIBundleRSP)
    val rxdat = Flipped(Valid(new CHIBundleDAT))
    val rxrsp = Flipped(Valid(new CHIBundleRSP))
    val mshrFull = Output(Bool())
    val tshrIdle = Output(Vec(nMSHR, Bool()))
    val debugReqTxnId = Output(Vec(nMSHR, UInt(paramCHI.rspTxnIDWidth.W)))
    val debugRxDatMatch = Output(Vec(nMSHR, Bool()))
    val debugReqState = Output(Vec(nMSHR, UInt(5.W)))
    val debugDsBufRdResp = Output(Vec(nMSHR, Bool()))
    val debugFromDsDataLo = Output(Vec(nMSHR, UInt(paramCHI.dataWidth.W)))
    val debugDsDataLo = Output(Vec(nMSHR, UInt(paramCHI.dataWidth.W)))
    val debugBufReadValid = Output(Bool())
    val debugBufReadId = Output(UInt(log2Ceil(nMSHR).W))
    val debugBufRespData = Output(UInt(blockBits.W))
    val debugBufWriteValid = Output(Vec(nMSHR, Bool()))
    val debugBufWriteId = Output(Vec(nMSHR, UInt(log2Ceil(nMSHR).W)))
    val debugBufWriteData = Output(Vec(nMSHR, UInt(blockBits.W)))
    val debugDsWinIdx = Output(UInt(log2Ceil(nMSHR).W))
    val debugDsReqSet = Output(UInt(setBits.W))
    val debugDsReqWay = Output(UInt(wayBits.W))
    val debugDsReqWen = Output(Bool())
    val debugDsReqPrevSet = Output(UInt(setBits.W))
    val debugDsReqPrevWay = Output(UInt(wayBits.W))
    val debugDsReqPrevWen = Output(Bool())
    val debugDsDatReqSet = Output(UInt(32.W))
    val debugDsDatReqWay = Output(UInt(32.W))
    val debugDsDatReqWen = Output(Bool())
    val debugDsTsReqSet = Output(Vec(nMSHR, UInt(32.W)))
    val debugDsTsReqWay = Output(Vec(nMSHR, UInt(32.W)))
    val debugDsTsReqWen = Output(Vec(nMSHR, Bool()))
    val debug = if (enableDirStateProbe) {
      Some(Output(new Bundle {
        val dirStateProbeValid = Bool()
        val dirStateProbeSet = UInt(setBits.W)
        val dirStateProbeWay = UInt(wayBits.W)
        val dirStateProbeState = L2Directory.MetaState()
      }))
    } else {
      None
    }
    val debugPlru = if (enableDirPlruProbe) {
      Some(Output(new Bundle {
        val dirPlruProbeValid = Bool()
        val dirPlruProbeSet = UInt(setBits.W)
        val dirPlruProbeState = UInt(PLRU4.nBits.W)
      }))
    } else {
      None
    }
  })

  val reqFSMs = Seq.tabulate(nMSHR)(_ => Module(new ReqFSM))
  val directory = Module(new Directory)
  val datStore = Module(new DataStorage)
  val reqRBE = Module(new L2RBE(flow = true))
  val tshrAlloc = Module(new TSHRAlloc)
  val datArb = Module(new DataArbiter)
  val reqArb = Module(new ReqArbiter)
  val rspArb = Module(new RspArbiter)
  val refillBuf = Module(new MSHRBuffer(nMSHR))
  val refillMods = Seq.tabulate(nMSHR)(_ => Module(new RefillModule))

  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    fsm.io.mshrId := i.U
    fsm.io.rxdat := io.rxdat
    fsm.io.bufResp := refillBuf.io.resp
  }

  reqRBE.io.blockFromVPipe := tshrAlloc.io.blockFromVPipe
  reqRBE.io.directoryReadDone := true.B
  reqRBE.io.directoryReadNeed := false.B
  reqRBE.io.in <> io.rxreq
  tshrAlloc.io.rxreq <> reqRBE.io.out
  io.tshrIdle := VecInit(reqFSMs.map(_.io.idle))
  io.debugReqTxnId := VecInit(reqFSMs.map(_.io.debugReqTxnId))
  io.debugRxDatMatch := VecInit(reqFSMs.map(_.io.debugRxDatMatch))
  io.debugReqState := VecInit(reqFSMs.map(_.io.debugState))
  io.debugDsBufRdResp := VecInit(reqFSMs.map(_.io.debugDsBufRdResp))
  io.debugFromDsDataLo := VecInit(reqFSMs.map(_.io.debugFromDsDataLo))
  io.debugDsDataLo := VecInit(reqFSMs.map(_.io.debugDsDataLo))
  io.debugBufReadValid := refillBuf.io.r.valid
  io.debugBufReadId := refillBuf.io.resp.id
  io.debugBufRespData := refillBuf.io.resp.data.data
  io.debugBufWriteValid := VecInit(refillBuf.io.w.map(_.valid))
  io.debugBufWriteId := VecInit(refillBuf.io.w.map(_.bits.id))
  io.debugBufWriteData := VecInit(refillBuf.io.w.map(_.bits.data.data))
  io.debugDsWinIdx := datStore.io.debugWinIdx
  io.debugDsReqSet := datStore.io.debugReqSet
  io.debugDsReqWay := datStore.io.debugReqWay
  io.debugDsReqWen := datStore.io.debugReqWen
  io.debugDsReqPrevSet := datStore.io.debugReqPrevSet
  io.debugDsReqPrevWay := datStore.io.debugReqPrevWay
  io.debugDsReqPrevWen := datStore.io.debugReqPrevWen
  io.debugDsDatReqSet := datStore.io.debugDatReqSet
  io.debugDsDatReqWay := datStore.io.debugDatReqWay
  io.debugDsDatReqWen := datStore.io.debugDatReqWen
  io.debugDsTsReqSet := datStore.io.debugTsReqSet
  io.debugDsTsReqWay := datStore.io.debugTsReqWay
  io.debugDsTsReqWen := datStore.io.debugTsReqWen
  tshrAlloc.io.tshrIdle := io.tshrIdle
  tshrAlloc.io.tshrAddr := VecInit(reqFSMs.map(_.io.addr))
  io.mshrFull := tshrAlloc.io.mshrFull

  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    fsm.io.alloc.valid := tshrAlloc.io.alloc.valid && tshrAlloc.io.alloc.bits.idx === i.U
    fsm.io.alloc.bits := tshrAlloc.io.alloc.bits
  }

  directory.io.toDir := VecInit(reqFSMs.map(_.io.toDir))
  reqFSMs.foreach(_.io.fromDir := 0.U.asTypeOf(new L2Directory.PathFromDirectory))
  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    fsm.io.fromDir := directory.io.fromDir(i)
  }

  if (enableDirPlruProbe) {
    io.debugPlru.get.dirPlruProbeValid := directory.io.debugPlru.get.valid
    io.debugPlru.get.dirPlruProbeSet := directory.io.debugPlru.get.set
    // This PLRU probe is a validation-only sideband for model/debug checking.
    // It must not become a source of truth for model victim prediction.
    io.debugPlru.get.dirPlruProbeState := directory.io.debugPlru.get.state
    dontTouch(io.debugPlru.get.dirPlruProbeValid)
    dontTouch(io.debugPlru.get.dirPlruProbeSet)
    dontTouch(io.debugPlru.get.dirPlruProbeState)
  }

  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    fsm.io.fromDS := datStore.io.toTSHR(i)
  }
  datStore.io.fromTSHR := VecInit(reqFSMs.map(_.io.toDS))

  datArb.io.fromFSM <> VecInit(reqFSMs.map(_.io.txdat))
  io.txdat <> datArb.io.txdat
  reqArb.io.fromFSM <> VecInit(reqFSMs.map(_.io.txreq))
  io.txreq <> reqArb.io.txreq
  rspArb.io.fromFSM <> VecInit(reqFSMs.map(_.io.txrsp))
  io.txrsp <> rspArb.io.txrsp

  reqFSMs.foreach(_.io.rxrsp := io.rxrsp)

  reqFSMs.zip(refillMods).zipWithIndex.foreach { case ((fsm, refill), i) =>
    refill.io.mshrId := i.U
    refill.io.refillReq <> fsm.io.refillReq
    refill.io.evictReq.ready := true.B
    refill.io.evictGnt := 0.U.asTypeOf(refill.io.evictGnt)
    refill.io.evictDatOut := 0.U.asTypeOf(refill.io.evictDatOut)
    fsm.io.victimDatSafeOut := refill.io.victimDatSafeOut
  }

  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    refillBuf.io.w(i) <> fsm.io.bufWrite
  }
  val bufReadValid = reqFSMs.map(_.io.bufRead.valid)
  refillBuf.io.r.valid := VecInit(bufReadValid).asUInt.orR
  refillBuf.io.r.bits.id := PriorityMux(bufReadValid, reqFSMs.map(_.io.bufRead.bits.id))

  if (enableDirStateProbe) {
    io.debug.get.dirStateProbeValid := directory.io.debugStateWrite.get.valid
    io.debug.get.dirStateProbeSet := directory.io.debugStateWrite.get.set
    io.debug.get.dirStateProbeWay := directory.io.debugStateWrite.get.way
    io.debug.get.dirStateProbeState := directory.io.debugStateWrite.get.state
    dontTouch(io.debug.get.dirStateProbeValid)
    dontTouch(io.debug.get.dirStateProbeSet)
    dontTouch(io.debug.get.dirStateProbeWay)
    dontTouch(io.debug.get.dirStateProbeState)
  }

  assert(PopCount(reqFSMs.map(_.io.alloc.valid)) <= 1.U,
    "L2Slice: multiple FSMs allocated same cycle")

  XSPerfAccumulate("slice_rxreq_stall", io.rxreq.valid && !io.rxreq.ready)
  XSPerfAccumulate("slice_mshr_full", tshrAlloc.io.mshrFull)
  XSPerfAccumulate("slice_addr_conflict", tshrAlloc.io.addrConflict)
  XSPerfAccumulate("slice_txdat_fire", io.txdat.fire)
  XSPerfAccumulate("slice_txdat_stall", io.txdat.valid && !io.txdat.ready)
  XSPerfAccumulate("slice_compAck", io.rxrsp.valid)
  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    XSPerfAccumulate(s"slice_tshr${i}_busy", !fsm.io.idle)
  }
}
