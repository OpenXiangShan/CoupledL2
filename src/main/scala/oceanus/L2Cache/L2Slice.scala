package coupledL2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

// ─────────────────────────────────────────────────────────────────
// L2Slice  —  one bank's hit-path pipeline
//
// Integrates:
//   Dir side : DirArbiter  →  L2Directory
//   Dat side : DSArbiter   →  DataStorage
//   TSHR array: nMSHR × ReqFSM
//
// Fanout topology:
//   Dir response  : Directory.pathOut → broadcast (all FSMs filter by TSHRADDR)
//   DS  response  : DSArbiter.toTSHR(i) → ReqFSM(i)  (already per-TSHR)
//   CompAck rxrsp : broadcast (all FSMs filter by txnID)
//   txdat         : locked priority arbiter (2-beat atomicity)
// ─────────────────────────────────────────────────────────────────
class L2Slice(implicit p: Parameters) extends TL2CHIL2Module {

  val nMSHR = paramL2.nMSHR

  val io = IO(new Bundle {
    // ── L1 → L2 request ─────────────────────────────────────────
    val rxreq = Flipped(Decoupled(new FlitREQ))

    // ── L2 → L1 CompData ─────────────────────────────────────────
    val txdat = Decoupled(new CHIBundleDAT)

    // ── L1 → L2 CompAck ──────────────────────────────────────────
    val rxrsp = Flipped(Valid(new CHIBundleRSP))

    // ── debug ─────────────────────────────────────────────────────
    val mshrFull = Output(Bool())
  })

  // ══════════════════════════════════════════════════════════════
  // Module instantiation
  // ══════════════════════════════════════════════════════════════

  val reqFSMs  = Seq.tabulate(nMSHR)(i => Module(new ReqFSM))
  val dirArb   = Module(new DirArbiter)
  val directory= Module(new L2Directory)
  val dsArb    = Module(new DSArbiter)
  val datStore = Module(new DataStorage)

  // assign static TSHR IDs
  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    fsm.io.mshrId := i.U
  }

  // ══════════════════════════════════════════════════════════════
  // TSHRAlloc  —  PA-conflict CAM + idle slot arbitration
  //
  // Inputs:
  //   tshrIdle : per-TSHR idle flag from each ReqFSM
  //   tshrAddr : current in-flight PA from each ReqFSM (for conflict CAM)
  //              meaningful only when !idle; module masks with !tshrIdle
  // Outputs:
  //   alloc.valid/bits → drives the winning ReqFSM's alloc port
  //   rxreq.ready      → back-pressures the upstream request channel
  // ══════════════════════════════════════════════════════════════

  val tshrAlloc = Module(new TSHRAlloc)

  tshrAlloc.io.rxreq    <> io.rxreq
  tshrAlloc.io.tshrIdle := VecInit(reqFSMs.map(_.io.idle))
  tshrAlloc.io.tshrAddr := VecInit(reqFSMs.map(_.io.addr))

  io.mshrFull := tshrAlloc.io.mshrFull

  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    fsm.io.alloc.valid := tshrAlloc.io.alloc.valid &&
                          (tshrAlloc.io.alloc.bits.idx === i.U)
    fsm.io.alloc.bits  := tshrAlloc.io.alloc.bits.req
  }

  // ══════════════════════════════════════════════════════════════
  // Dir side
  //
  //   ReqFSM(i).toDir  →  DirArbiter.toDir(i)
  //   DirArbiter.dirIn →  Directory.pathIn
  //   Directory.pathOut →  broadcast to all ReqFSMs (filter by TSHRADDR)
  //               also  →  DirArbiter.dirOut (satisfies its port)
  // ══════════════════════════════════════════════════════════════

  // TSHR → arbiter
  dirArb.io.toDir.zipWithIndex.foreach { case (port, i) =>
    port := reqFSMs(i).io.toDir
  }

  // arbiter winner → directory
  directory.io.pathIn := dirArb.io.dirIn

  // directory response → arbiter (port requirement) + broadcast
  dirArb.io.dirOut := directory.io.pathOut

  // broadcast: each ReqFSM filters on io.fromDir.TSHRADDR === io.mshrId
  reqFSMs.foreach { fsm =>
    fsm.io.fromDir := directory.io.pathOut
  }

  // ══════════════════════════════════════════════════════════════
  // Dat side
  //
  //   ReqFSM(i).toDS      →  DSArbiter.fromTSHR(i)
  //   DSArbiter.toTSHR(i) →  ReqFSM(i).fromDS   (per-TSHR, no broadcast)
  //   DSArbiter.ds*        →  DataStorage
  // ══════════════════════════════════════════════════════════════

  // TSHR → DS arbiter
  dsArb.io.fromTSHR.zipWithIndex.foreach { case (port, i) =>
    port := reqFSMs(i).io.toDS
  }

  // DS arbiter → each TSHR (already split per-TSHR in DSArbiter)
  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    fsm.io.fromDS := dsArb.io.toTSHR(i)
  }

  // DS arbiter → DataStorage
  datStore.io.en    := dsArb.io.dsEn
  datStore.io.req   := dsArb.io.dsReq
  datStore.io.wdata := dsArb.io.dsWdata
  dsArb.io.dsRdata  := datStore.io.rdata

  // ══════════════════════════════════════════════════════════════
  // DataArbiter  —  txdat locked priority arbiter
  //   2-beat atomicity + last-beat lookahead → 1.0 beats/cycle
  //   (naive lock: 0.67 beats/cycle; verified in behavioral sim)
  // ══════════════════════════════════════════════════════════════

  val datArb = Module(new DataArbiter)

  datArb.io.fromFSM.zipWithIndex.foreach { case (port, i) =>
    port.valid            := reqFSMs(i).io.txdat.valid
    port.bits             := reqFSMs(i).io.txdat.bits
    reqFSMs(i).io.txdat.ready := port.ready
  }

  io.txdat <> datArb.io.txdat

  // ══════════════════════════════════════════════════════════════
  // rxrsp (CompAck)  —  broadcast; each ReqFSM filters by txnID
  // ══════════════════════════════════════════════════════════════

  reqFSMs.foreach(_.io.rxrsp := io.rxrsp)

  // ══════════════════════════════════════════════════════════════
  // Assertions
  // ══════════════════════════════════════════════════════════════

  // at most one FSM allocated per cycle
  assert(PopCount(reqFSMs.map(_.io.alloc.valid)) <= 1.U,
    "L2Slice: multiple FSMs allocated same cycle")

  // txdat lock assertions live inside DataArbiter

  // ══════════════════════════════════════════════════════════════
  // Performance counters
  // ══════════════════════════════════════════════════════════════

  XSPerfAccumulate("slice_rxreq_stall",    io.rxreq.valid && !io.rxreq.ready)
  XSPerfAccumulate("slice_mshr_full",      tshrAlloc.io.mshrFull)
  XSPerfAccumulate("slice_addr_conflict",  tshrAlloc.io.addrConflict)
  XSPerfAccumulate("slice_txdat_fire",  io.txdat.fire)
  XSPerfAccumulate("slice_txdat_stall", io.txdat.valid && !io.txdat.ready)
  XSPerfAccumulate("slice_compAck",       io.rxrsp.valid)

  // per-TSHR utilization
  reqFSMs.zipWithIndex.foreach { case (fsm, i) =>
    XSPerfAccumulate(s"slice_tshr${i}_busy", !fsm.io.idle)
  }
}
