package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import oceanus.compactchi._
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters

object SnoopOpcodeDerive {
  object Intent extends ChiselEnum {
    val SnpMakeInvalid, SnpToInvalid, SnpToShared, SnpToClean = Value
  }

  final case class Result(needSnoop: Boolean, opcode: Int)

  private def isInvalidate(intent: Intent.Type): Bool =
    intent === Intent.SnpMakeInvalid || intent === Intent.SnpToInvalid

  private def isInvalidate(intent: Int): Boolean =
    intent == Intent.SnpMakeInvalid.litValue.toInt || intent == Intent.SnpToInvalid.litValue.toInt

  // The uop encodes the caller's intent; the emitted SNP opcode is derived here
  // from the directory snapshot. Directory dirty is intentionally excluded:
  // L2 dirty belongs to DS ownership, while the directory cannot know L1 dirty.
  def apply(intent: Intent.Type, state: UInt, clients: UInt, hit: Bool): (Bool, UInt) = {
    val hasClient = clients.orR
    val invalidate = isInvalidate(intent)
    val needSnoop = hit && hasClient && (invalidate || state === L2Directory.MetaState.UU)
    val opcode = Mux(
      invalidate,
      Mux(state === L2Directory.MetaState.UU, CCHIOpcode.SnpToInvalid.U, CCHIOpcode.SnpMakeInvalid.U),
      CCHIOpcode.SnpToShared.U
    )

    (needSnoop, opcode)
  }

  def apply(intent: Int, state: Int, clients: Int, hit: Boolean): Result = {
    val hasClient = clients != 0
    val invalidate = isInvalidate(intent)
    val needSnoop = hit && hasClient && (invalidate || state == L2Directory.MetaState.UU.litValue.toInt)
    val opcode =
      if (invalidate) {
        if (state == L2Directory.MetaState.UU.litValue.toInt) {
          CCHIOpcode.SnpToInvalid.opcode
        } else {
          CCHIOpcode.SnpMakeInvalid.opcode
        }
      } else {
        CCHIOpcode.SnpToShared.opcode
      }

    Result(needSnoop, opcode)
  }
}

object L2SnoopAgent {

  class PathToSnoopAgentUOPs extends Bundle {
    val SnpMakeInvalid = Bool()
    val SnpToInvalid = Bool()
    val SnpToShared = Bool()
    val SnpToClean = Bool()

    val SnpCompAck = Bool()
  }

  class PathFromSnoopAgentUOPs extends Bundle {
    val SnpResp = Bool()
    val SnpRespData0 = Bool()
    val SnpRespData2 = Bool()
  }

  class PathToSnoopAgent extends PathToSnoopAgentUOPs {
    val CLIENTS = Vec(1, Bool())  
    val ALIAS = UInt(2.W)
  }

  class PathFromSnoopAgent extends PathFromSnoopAgentUOPs {
    val PASSDIRTY = Bool()
  }
}

class L2SnoopAgent(tshrId: Int)(implicit val p: Parameters) extends Module with HasL2Params {

  val io = IO(new Bundle {
    val uopFromSNP = Flipped(Valid(new L2SnoopAgent.PathToSnoopAgent))
    val uopFromREQ = Flipped(Valid(new L2SnoopAgent.PathToSnoopAgent))

    val tshr_paddr = Input(UInt(paramL2.physicalAddrWidth.W))
    val tshr_dirResult = Input(new L2Directory.MetaReadResult)

    val txSnp = Decoupled(new FlitSNP)

    val UpRXRSP = Flipped(Valid(new FlitUpRSP))
    val UpRXDAT = Flipped(Valid(new FlitUpDAT))

    val fromSA = Output(new L2SnoopAgent.PathFromSnoopAgent)
  })

  val sIdle :: sSnpReq :: sWaitCore :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val busy = RegInit(false.B)
  val armed = RegInit(true.B)
  val holdCount = RegInit(0.U(3.W))

  val slotUop = RegInit(0.U.asTypeOf(new L2SnoopAgent.PathToSnoopAgent))
  val slotPaddr = Reg(UInt(paramL2.physicalAddrWidth.W))
  val slotOpcode = Reg(UInt(2.W))

  val seenData0 = RegInit(false.B)
  val passDirtyReg = RegInit(false.B)

  val uopValids = Seq(io.uopFromSNP.valid, io.uopFromREQ.valid)
  val uopAnyValid = uopValids.reduce(_ || _)
  val uopBitsIn = Wire(new L2SnoopAgent.PathToSnoopAgent)
  uopBitsIn := Mux(io.uopFromSNP.valid, io.uopFromSNP.bits, io.uopFromREQ.bits)

  val intent = Wire(SnoopOpcodeDerive.Intent())
  intent := SnoopOpcodeDerive.Intent.SnpToShared
  when (uopBitsIn.SnpMakeInvalid) {
    intent := SnoopOpcodeDerive.Intent.SnpMakeInvalid
  }.elsewhen (uopBitsIn.SnpToInvalid) {
    intent := SnoopOpcodeDerive.Intent.SnpToInvalid
  }.elsewhen (uopBitsIn.SnpToClean) {
    intent := SnoopOpcodeDerive.Intent.SnpToClean
  }

  val uopBits = Seq(
    uopBitsIn.SnpMakeInvalid,
    uopBitsIn.SnpToInvalid,
    uopBitsIn.SnpToShared,
    uopBitsIn.SnpToClean
  )
  val accept = uopAnyValid && !busy && armed
  val (acceptNeedSnoop, acceptOpcode) =
    SnoopOpcodeDerive(intent, io.tshr_dirResult.state, uopBitsIn.CLIENTS.asUInt, io.tshr_dirResult.hit)

  val rspMatch =
    state === sWaitCore &&
      io.UpRXRSP.valid &&
      io.UpRXRSP.bits.TxnID === tshrId.U &&
      io.UpRXRSP.bits.Opcode === CCHIOpcode.SnpResp.U

  val datMatch =
    state === sWaitCore &&
      io.UpRXDAT.valid &&
      io.UpRXDAT.bits.TxnID === tshrId.U &&
      io.UpRXDAT.bits.Opcode === CCHIOpcode.SnpRespData.U

  val datBeat0 = datMatch && io.UpRXDAT.bits.DataID === 0.U
  val datBeat2 = datMatch && io.UpRXDAT.bits.DataID === 2.U

  val done = rspMatch || datBeat2
  val directDone = accept && !acceptNeedSnoop

  io.fromSA.SnpResp := rspMatch || directDone
  io.fromSA.SnpRespData0 := datBeat0
  io.fromSA.SnpRespData2 := datBeat2
  io.fromSA.PASSDIRTY := Mux(rspMatch, io.UpRXRSP.bits.Resp(2), Mux(datBeat2, passDirtyReg || io.UpRXDAT.bits.Resp(2), false.B))

  io.txSnp.valid := state === sSnpReq
  io.txSnp.bits := 0.U.asTypeOf(new FlitSNP)
  io.txSnp.bits.TxnID := tshrId.U
  io.txSnp.bits.Opcode := slotOpcode
  io.txSnp.bits.Addr := slotPaddr >> 3
  io.txSnp.bits.alias := slotUop.ALIAS

  when (!uopAnyValid) {
    armed := true.B
    holdCount := 0.U
  }.elsewhen (!armed && holdCount =/= 7.U) {
    holdCount := holdCount + 1.U
  }

  when (datBeat0) {
    seenData0 := true.B
    passDirtyReg := io.UpRXDAT.bits.Resp(2)
  }

  when (done) {
    busy := false.B
    armed := false.B
    seenData0 := false.B
    passDirtyReg := false.B
    state := sIdle
  }.elsewhen (directDone) {
    busy := false.B
    armed := false.B
  }.elsewhen (accept && acceptNeedSnoop) {
    busy := true.B
    slotUop := uopBitsIn
    slotPaddr := io.tshr_paddr
    slotOpcode := acceptOpcode
    state := sSnpReq
  }

  switch (state) {
    is (sSnpReq) {
      when (io.txSnp.fire) {
        state := sWaitCore
      }
    }
  }

  when (accept) {
    assert(PopCount(uopBits) === 1.U,
      s"L2SnoopAgent #${tshrId}: accepted invalid or multi-hot uop")
    assert(io.tshr_dirResult.state === L2Directory.MetaState.I ||
           io.tshr_dirResult.state === L2Directory.MetaState.S ||
           io.tshr_dirResult.state === L2Directory.MetaState.US ||
           io.tshr_dirResult.state === L2Directory.MetaState.UU,
      s"L2SnoopAgent #${tshrId}: accepted uop with illegal directory state")
    assert(io.tshr_dirResult.hit || !acceptNeedSnoop,
      s"L2SnoopAgent #${tshrId}: missed directory line requested an upstream snoop")
  }

  assert(!(rspMatch && datMatch),
    s"L2SnoopAgent #${tshrId}: matched dataless and data response in the same cycle")
  assert(PopCount(uopValids) <= 1.U,
    s"L2SnoopAgent #${tshrId}: SNP and REQ uops arrived in the same cycle")
  assert(!(datBeat2 && !seenData0),
    s"L2SnoopAgent #${tshrId}: SnpRespData DataID 2 arrived before DataID 0")
  assert(!(uopAnyValid && !armed && holdCount >= 4.U),
    s"L2SnoopAgent #${tshrId}: uop input valid stayed high after completion; deassert before reusing the request")

}
