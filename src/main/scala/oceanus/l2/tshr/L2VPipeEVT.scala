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

class L2VPipeEVT(
    clientComponents: Seq[CCHIComponent],
    tshrId: Int = 0,
    nodeId: Int = 0
)(implicit val p: Parameters)
    extends Module
    with HasL2Params
    with CHIRNFOpcodesREQ
    with CHIRNFOpcodesRSP
    with CHIRNFOpcodesDAT {

  val io = IO(new Bundle {
    val UpRXEVT = Flipped(Valid(new FlitEVT))
    val UpTXRSP = Decoupled(new FlitDnRSP)
    val UpRXDAT = Flipped(Valid(new CHIBundleDAT))

    val tshrId = Input(UInt(log2Ceil(nMSHR).W))
    val tshr_dirResult = Input(new L2Directory.MetaReadResult)

    val tshr_tag_write_en = Output(Bool())
    val tshr_meta_write_en = Output(new L2Directory.MetaWriteMask)
    val tshr_meta_write_meta = Output(new L2Directory.Meta)

    val EVT_active = Output(Bool())
    val evtDataReadyOut = Output(Bool())

    val free = Output(Bool())
  })

  val Seq(
    sIdle,
    sSendCompDBIDResp,
    sWaitData,
    sWaitCommit,
    sSendComp
  ) = Enum(5)

  val state = RegInit(sIdle)

  val pIsWbFull = RegInit(false.B)
  val pPaddr = Reg(UInt(paramL2.physicalAddrWidth.W))
  val pTxnId = Reg(UInt(8.W))
  val pTraceTag = Reg(UInt(1.W))
  val pReqWay = Reg(UInt(4.W))
  val pMissMeta = Reg(new L2Directory.MetaReadResult)
  val pDbid = Reg(UInt(8.W))
  val dataArmed = RegInit(false.B)
  val dirHit = RegInit(false.B)
  val inflightEvict = RegInit(false.B)

  io.EVT_active := inflightEvict
  io.evtDataReadyOut := RegNext(io.UpRXDAT.valid &&
    io.UpRXDAT.bits.TxnID.get === pDbid &&
    io.UpRXDAT.bits.Opcode.get === CHI_CopyBackWrData.asUIntForDAT &&
    io.UpRXDAT.bits.DataID.get === 2.U, false.B)

  io.free := state === sIdle

  val dirResult = io.tshr_dirResult

  val opcode = io.UpRXEVT.bits.Opcode
  val isEvict = opcode === CCHIOpcode.Evict.U
  val isWbFull = opcode === CCHIOpcode.WriteBackFull.U

  val copyBackWrDataMatch =
    dataArmed &&
      io.UpRXDAT.valid &&
      io.UpRXDAT.bits.TxnID.get === pDbid &&
      io.UpRXDAT.bits.Opcode.get === CHI_CopyBackWrData.asUIntForDAT
  val isBeat0 = copyBackWrDataMatch && io.UpRXDAT.bits.DataID.get === 0.U
  val isBeat2 = copyBackWrDataMatch && io.UpRXDAT.bits.DataID.get === 2.U
  val evtDataReady = RegNext(isBeat2, false.B)

  when (state === sIdle && io.UpRXEVT.valid) {
    pIsWbFull := isWbFull
    pPaddr := io.UpRXEVT.bits.Addr
    pTxnId := io.UpRXEVT.bits.TxnID
    pTraceTag := io.UpRXEVT.bits.TraceTag
    pDbid := io.tshrId
    pReqWay := dirResult.way
    pMissMeta := dirResult
    dirHit := dirResult.hit
    dataArmed := false.B
    inflightEvict := true.B
    state := Mux(
      isWbFull,
      sSendCompDBIDResp,
      Mux(dirResult.hit, sWaitCommit, sSendComp)
    )
  }

  when (state === sSendCompDBIDResp && io.UpTXRSP.fire) {
    dataArmed := true.B
    state := sWaitData
  }

  when (state === sWaitData && evtDataReady) {
    state := sWaitCommit
  }

  when (state === sWaitCommit) {
    state := sSendComp
  }

  when (state === sSendComp && io.UpTXRSP.fire) {
    state := sIdle
    dataArmed := false.B
    inflightEvict := false.B
  }

  io.UpTXRSP.valid := state === sSendCompDBIDResp || state === sSendComp
  io.UpTXRSP.bits := 0.U.asTypeOf(new FlitDnRSP)
  io.UpTXRSP.bits.TxnID := pTxnId
  io.UpTXRSP.bits.DBID := pDbid
  io.UpTXRSP.bits.Opcode := Mux(state === sSendCompDBIDResp, CCHIOpcode.CompDBIDResp.U, CCHIOpcode.Comp.U)
  io.UpTXRSP.bits.RespErr := 0.U
  io.UpTXRSP.bits.Resp := 0.U
  io.UpTXRSP.bits.CBusy := 0.U
  io.UpTXRSP.bits.WayValid := true.B
  io.UpTXRSP.bits.Way := pReqWay
  io.UpTXRSP.bits.TraceTag := pTraceTag

  val nextStateAfterClientDrop = WireDefault(L2Directory.MetaState.I)
  nextStateAfterClientDrop := MuxLookup(
    pMissMeta.state,
    L2Directory.MetaState.I
  )(Seq(
    L2Directory.MetaState.UU -> L2Directory.MetaState.US,
    L2Directory.MetaState.US -> L2Directory.MetaState.US,
    L2Directory.MetaState.S -> L2Directory.MetaState.S,
    L2Directory.MetaState.I -> L2Directory.MetaState.I
  ))

  val newMeta = Wire(new L2Directory.Meta)
  newMeta := pMissMeta
  newMeta.state := nextStateAfterClientDrop
  newMeta.clients(0) := false.B
  when (pIsWbFull) {
    newMeta.dirty := true.B
  }

  val metaMask = Wire(new L2Directory.MetaWriteMask)
  metaMask.state := false.B
  metaMask.dirty := false.B
  metaMask.alias := false.B
  metaMask.clients.foreach(_ := false.B)
  when (state === sWaitCommit && (pIsWbFull || dirHit)) {
    metaMask.state := true.B
    metaMask.clients(0) := true.B
    when (pIsWbFull) {
      metaMask.dirty := true.B
    }
  }

  io.tshr_tag_write_en := state === sWaitCommit && (pIsWbFull || dirHit)
  io.tshr_meta_write_en := metaMask
  io.tshr_meta_write_meta := newMeta

  assert(!(copyBackWrDataMatch && evtDataReady), "EVT: unexpected extra CopyBackWrData after evtDataReady")
  assert(!(state === sWaitData && !dataArmed), "EVT: data arrived before CompDBIDResp armed reception")
  assert(!(state === sWaitCommit && pIsWbFull && pMissMeta.state === L2Directory.MetaState.S),
    "EVT: WriteBackFull must not originate from shared-clean directory state")
  assert(!(state === sWaitCommit && pIsWbFull && newMeta.state === L2Directory.MetaState.UU && !newMeta.clients.asUInt.orR),
    "EVT: directory writeback must not leave unique-owner state without any client")
}
