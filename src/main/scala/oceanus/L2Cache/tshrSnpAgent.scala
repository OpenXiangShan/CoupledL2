package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2._
import coupledL2.tl2chi.CHICohStates._

// ============================================================
//  I/O Bundles
// ============================================================

class SnoopAgentProbeReq(implicit p: Parameters) extends L2Bundle {
  val valid     = Bool()
  val chiOpcode = UInt(OPCODE_WIDTH.W)
  val alias     = UInt(2.W)          // pprobe: from localMeta; rprobe: from req payload
}

class SnoopAgentProbeResp(implicit p: Parameters) extends L2Bundle {
  val needSnoop = Bool()             // combinational, same cycle as req.valid
  val done      = Bool()             // pulse: snoop complete (data or no-data)
  val hasData   = Bool()             // valid when done
}

class LocalMeta(implicit p: Parameters) extends L2Bundle {
  val state   = UInt(2.W)           // INVALID/BRANCH/TRUNK/TIP
  val dirty   = Bool()
  val clients = Bool()              // orR of clients bits
  val alias   = UInt(2.W)
}

class SnoopAgentIO(implicit p: Parameters) extends L2Bundle {
  // ---------- vPIPE side ----------
  val pprobeReq  = Input(new SnoopAgentProbeReq)
  val pprobeResp = Output(new SnoopAgentProbeResp)

  val rprobeReq  = Input(new SnoopAgentProbeReq)
  val rprobeResp = Output(new SnoopAgentProbeResp)

  // ---------- local meta (read-only, wire from dirResult) ----------
  val localMeta  = Input(new LocalMeta)

  // ---------- txSnp (to core, via 2-level arbiter) ----------
  val txSnp = new Bundle {
    val valid  = Output(Bool())
    val ready  = Input(Bool())
    val pa     = Output(UInt(PAddrBits.W))
    val opcode = Output(UInt(OPCODE_WIDTH.W))
    val alias  = Output(UInt(2.W))
  }

  // ---------- rxDat (from core, dirty data path) ----------
  val rxDat = new Bundle {
    val valid  = Input(Bool())
    val ready  = Output(Bool())
    val dataID = Input(UInt(2.W))
  }

  // ---------- rxRsp (from core, no-data SnpResp path) ----------
  val rxRsp = new Bundle {
    val valid  = Input(Bool())
    val ready  = Output(Bool())
  }

  // ---------- address (from TSHR, for txSnp.pa) ----------
  val pa = Input(UInt(PAddrBits.W))
}

// ============================================================
//  Slot entry
// ============================================================

class SnpSlot(implicit p: Parameters) extends L2Bundle {
  val valid     = Bool()
  val chiOpcode = UInt(OPCODE_WIDTH.W)
  val alias     = UInt(2.W)
  val isPProbe  = Bool()
}

// ============================================================
//  SnoopAgent
// ============================================================

class SnoopAgent(implicit p: Parameters) extends L2Module with HasCHIOpcodes {

  val io = IO(new SnoopAgentIO)

  // ----------------------------------------------------------
  // 1. needSnoop combinational decision (same cycle as valid)
  // ----------------------------------------------------------

  def needCoreSnp(opcode: UInt, clients: Bool): Bool =
    clients && (isSnpToN(opcode) || isSnpToB(opcode))

  io.pprobeResp.needSnoop := io.pprobeReq.valid &&
                             needCoreSnp(io.pprobeReq.chiOpcode, io.localMeta.clients)
  io.rprobeResp.needSnoop := io.rprobeReq.valid &&
                             needCoreSnp(io.rprobeReq.chiOpcode, io.localMeta.clients)

  // ----------------------------------------------------------
  // 2. Static 2-slot queue
  //    slot(0) = pprobe, slot(1) = rprobe
  //    Enqueued once per TSHR lifetime on needSnoop
  // ----------------------------------------------------------

  val slot = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new SnpSlot))))

  // slot(0): pprobe — alias from localMeta (mirrors b_task)
  when (io.pprobeReq.valid && io.pprobeResp.needSnoop) {
    slot(0).valid     := true.B
    slot(0).chiOpcode := io.pprobeReq.chiOpcode
    slot(0).alias     := io.localMeta.alias
    slot(0).isPProbe  := true.B
  }

  // slot(1): rprobe — alias from req payload
  when (io.rprobeReq.valid && io.rprobeResp.needSnoop) {
    slot(1).valid     := true.B
    slot(1).chiOpcode := io.rprobeReq.chiOpcode
    slot(1).alias     := io.rprobeReq.alias
    slot(1).isPProbe  := false.B
  }

  // ----------------------------------------------------------
  // 3. State machine: IDLE -> SNP_REQ -> WAIT_CORE -> IDLE
  //    Priority: slot(0)/pprobe > slot(1)/rprobe
  // ----------------------------------------------------------

  val sIDLE :: sSNP_REQ :: sWAIT_CORE :: Nil = Enum(3)
  val state      = RegInit(sIDLE)
  val curSlotIdx = RegInit(0.U(1.W))
  val curSlot    = slot(curSlotIdx)

  // txSnp opcode: isSnpToN -> SnpCleanInvalid, else -> SnpCleanShared
  val txSnpOpcode = Mux(isSnpToN(curSlot.chiOpcode), SnpCleanInvalid, SnpCleanShared)

  // rxDat beat identification via dataID (mirrors RXDAT module)
  val rxDatLast = io.rxDat.valid && (io.rxDat.dataID === "b10".U)

  // always-accept on both response channels
  io.rxDat.ready := true.B
  io.rxRsp.ready := true.B

  // txSnp default (overridden in sSNP_REQ)
  io.txSnp.valid  := false.B
  io.txSnp.pa     := 0.U
  io.txSnp.opcode := 0.U
  io.txSnp.alias  := 0.U

  // sSNP_REQ state
  is (sSNP_REQ) {
     io.txSnp.valid  := true.B
     io.txSnp.pa     := io.pa
     io.txSnp.opcode := Mux(isSnpToN(curSlot.chiOpcode), SnpCleanInvalid, SnpCleanShared)
     io.txSnp.alias  := curSlot.alias
}

  // done/hasData: one-cycle pulse outputs
  val pprobe_done    = RegInit(false.B)
  val pprobe_hasData = RegInit(false.B)
  val rprobe_done    = RegInit(false.B)
  val rprobe_hasData = RegInit(false.B)

  io.pprobeResp.done    := pprobe_done
  io.pprobeResp.hasData := pprobe_hasData
  io.rprobeResp.done    := rprobe_done
  io.rprobeResp.hasData := rprobe_hasData

  // auto-clear pulses
  when (pprobe_done) {
     pprobe_done    := false.B
     pprobe_hasData := false.B
}
  when (rprobe_done) {
     rprobe_done    := false.B
     rprobe_hasData := false.B
}
  // helper: fire done for current slot
  def fireDone(hasData: Bool): Unit = {
    slot(curSlotIdx).valid := false.B
    when (curSlot.isPProbe) {
      pprobe_done    := true.B
      pprobe_hasData := hasData
    }.otherwise {
      rprobe_done    := true.B
      rprobe_hasData := hasData
    }
  }

  switch (state) {

    is (sIDLE) {
      // slot(0) priority, then slot(1)
      when (slot(0).valid) {
        curSlotIdx := 0.U
        state      := sSNP_REQ
      }.elsewhen (slot(1).valid) {
        curSlotIdx := 1.U
        state      := sSNP_REQ
      }
    }

    is (sSNP_REQ) {
      // hold txSnp.valid until arbiter grants
      io.txSnp.valid := true.B
      when (io.txSnp.ready) {
        state := sWAIT_CORE
      }
    }

    is (sWAIT_CORE) {
      // two exit paths:
      // 1. rxDat last beat  -> core returned dirty data
      // 2. rxRsp valid      -> core returned SnpResp (no data)
      when (rxDatLast) {
        fireDone(hasData = true.B)
        state := sIDLE
      }.elsewhen (io.rxRsp.valid) {
        fireDone(hasData = false.B)
        state := sIDLE
      }
    }
  }

  // ----------------------------------------------------------
  // 4. No-snoop path
  //    needSnoop=0 is itself the completion signal for vPIPE.
  //    No slot is enqueued, no done pulse is ne
}
