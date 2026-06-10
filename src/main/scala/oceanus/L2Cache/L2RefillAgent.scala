/*
 * Manages the L2 cache miss refill flow for one TSHR slot.
 *
 * Precondition (IMPORTANT):
 *   The refill flow STARTS once all CMN data has arrived. VPipe FSM
 *   receives the CMN CompData (Read* path) / Comp (MakeUnique path),
 *   and only then fires refillReq. This module therefore does NOT
 *   touch the CMN downstream interface and never waits on CMN data.
 *
 * Responsibilities:
 *   - On dirty victim: coordinate with an EvictTSHR to safely read out
 *     the old cache line before allowing DataStorage to be overwritten.
 *   - Signal victimDatSafeOut to the local DS FSM so it can proceed
 *     with DSBufWb (writing the new line into DataStorage).
 *
 * Out of scope:
 *   - TXREQ to CMN                         (owned by VPipe FSM)
 *   - Receiving CMN CompData / Comp        (owned by VPipe FSM)
 *   - ReplRd to Directory                  (owned by VPipe FSM; victim
 *     way + meta are selected AFTER data arrives and passed in via
 *     refillReq)
 *   - RetryAck handling                    (owned by VPipe FSM)
 *   - CoreResp to L1                        (owned by VPipe FSM)
 *   - DSBufWb itself                        (owned by DS FSM; triggered
 *     by victimDatSafeOut)
 *
 * Key interactions:
 *
 *   VPipe FSM (on miss):
 *     sends TXREQ to CMN, waits for all CMN data
 *     on data complete: sends ReplRd to Directory
 *     on ReplRdResp: fires refillReq with victim way + meta
 *
 *   RefillModule (dirty victim):
 *     refillReq.fire (data already present)
 *       → evictReq → wait evictGnt → wait evictDatOut
 *       → victimDatSafeOut → DS FSM proceeds with DSBufWb
 *
 *   RefillModule (clean victim):
 *     refillReq.fire
 *       → victimDatSafeOut immediately (no dirty line to protect)
 *       → DS FSM proceeds with DSBufWb
 *
 *   EvictTSHR (dirty path):
 *     allocated by TSHRAlloc on evictReq
 *     DS FSM reads old cache line (DSBufRd)
 *     on DSBufRdResp: broadcasts EvictDatReadComp with refillMshrId
 *     continues independently with WriteBack to CMN
 *
 * Cross-TSHR notification uses broadcast + refillMshrId filter,
 * consistent with the existing fromDir / fromDS broadcast pattern.
 *
 * s_* / w_* convention (same as coupledL2 MSHR):
 *   s_* : true = fired/done, false = needs to fire
 *   w_* : true = completed,  false = still waiting
 */

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2._

// ── Allocation request (sent by VPipe FSM after data + ReplRd done) ──
// CMN data is already received by VPipe; this request marks the start
// of the refill flow and carries the selected victim way + meta.

class RefillAllocReq(implicit p: Parameters) extends TL2CHIL2Bundle {
  val mshrId  = UInt(mshrBits.W)
  val paddr   = UInt(physAddrBits.W)
  val alias   = UInt(2.W)             // VA[13:12] for alias handling
  val way     = UInt(wayBits.W)       // victim way, selected after data arrives
  val isDirty = Bool()                // whether victim data needs eviction
  val meta    = new L2Directory.Meta  // victim meta (state / dirty / clients)
}

// ── Evict TSHR allocation request (dirty victim path) ────────────────

class EvictAllocReq(implicit p: Parameters) extends TL2CHIL2Bundle {
  val mshrId       = UInt(mshrBits.W)  // requesting RefillTSHR
  val refillMshrId = UInt(mshrBits.W)  // RefillTSHR to notify on completion
  val paddr        = UInt(physAddrBits.W)
  val way          = UInt(wayBits.W)
  val meta         = new L2Directory.Meta
}

// ── Evict TSHR allocation grant ───────────────────────────────────────

class EvictAllocGnt(implicit p: Parameters) extends TL2CHIL2Bundle {
  val refillMshrId = UInt(mshrBits.W)  // which RefillTSHR this grant is for
  val evictMshrId  = UInt(mshrBits.W)  // allocated EvictTSHR index
}

// ── EvictTSHR → all RefillModules broadcast ───────────────────────────
// Fired when EvictTSHR DS FSM completes DSBufRd (old data safely read out)

class EvictDatReadComp(implicit p: Parameters) extends TL2CHIL2Bundle {
  val evictMshrId  = UInt(mshrBits.W)  // which EvictTSHR is sending this
  val refillMshrId = UInt(mshrBits.W)  // which RefillTSHR to notify
}

// ── RefillModule ──────────────────────────────────────────────────────

class RefillModule(implicit p: Parameters) extends TL2CHIL2Module {

  val io = IO(new Bundle {

    // ── Allocation (VPipe FSM fires after data received + ReplRd) ──
    // Carries victim way and meta. CMN data is already in hand at the
    // VPipe side; this module does not touch CMN or Directory.
    // ready=false when busy with a prior refill → stalls refill pipeline
    val refillReq = Flipped(Decoupled(new RefillAllocReq))

    // ── Evict TSHR handshake (dirty victim path only) ─────────────
    // Step 1: request EvictTSHR allocation from TSHRAlloc
    val evictReq  = Decoupled(new EvictAllocReq)
    // Step 2: TSHRAlloc grants an EvictTSHR (broadcast, filtered by refillMshrId)
    val evictGnt  = Flipped(Valid(new EvictAllocGnt))
    // Step 3: EvictTSHR signals old data safely read out (broadcast, filtered by refillMshrId)
    val evictDatOut = Flipped(Valid(new EvictDatReadComp))

    // ── Notify own DS FSM that old data is safely out ─────────────
    // Single-cycle pulse. Fires on the rising edge of evictDatOut
    // (dirty) or on refillReq.fire (clean). DS FSM may then proceed
    // with DSBufWb.
    val victimDatSafeOut = Output(Bool())

    // ── MSHR identifier ──────────────────────────────────────────
    val mshrId = Input(UInt(mshrBits.W))
  })

  // ── Registers ────────────────────────────────────────────────────

  val paddr   = RegInit(0.U(physAddrBits.W))
  val alias   = RegInit(0.U(2.W))
  val way     = RegInit(0.U(wayBits.W))
  val isDirty = RegInit(false.B)
  val meta    = RegInit(0.U.asTypeOf(new L2Directory.Meta))  // RegInit: no X at reset

  // Allocated EvictTSHR index (recorded on evictGnt, dirty path only)
  val evictMshrId = RegInit(0.U(mshrBits.W))

  // ── s_* state registers ───────────────────────────────────────────

  val s_evictReq = RegInit(true.B)  // evictReq fired (dirty path only)

  // ── w_* state registers ──────────────────────────────────────────

  val w_evictGnt    = RegInit(true.B)  // EvictTSHR allocated (dirty path)
  val w_evictDatOut = RegInit(true.B)  // old data safely read out (dirty path)

  // ── Allocation: activate on refillReq.fire ────────────────────────

  // Accept new request only when the prior evict handshake is complete
  val idle = w_evictGnt && w_evictDatOut && s_evictReq
  io.refillReq.ready := idle

  when (io.refillReq.fire) {
    paddr   := io.refillReq.bits.paddr
    alias   := io.refillReq.bits.alias
    way     := io.refillReq.bits.way
    isDirty := io.refillReq.bits.isDirty
    meta    := io.refillReq.bits.meta

    // dirty: arm evictReq and wait for grant + datOut
    // clean: skip entire evict handshake
    s_evictReq    := !io.refillReq.bits.isDirty
    w_evictGnt    := !io.refillReq.bits.isDirty
    w_evictDatOut := !io.refillReq.bits.isDirty
  }

  // ── Evict TSHR request (dirty path) ──────────────────────────────
  // Fire evictReq as soon as possible after refillReq.fire.

  io.evictReq.valid             := !s_evictReq
  io.evictReq.bits.mshrId       := io.mshrId
  io.evictReq.bits.refillMshrId := io.mshrId
  io.evictReq.bits.paddr        := paddr
  io.evictReq.bits.way          := way
  io.evictReq.bits.meta         := meta

  when (io.evictReq.fire) {
    s_evictReq := true.B
  }

  // ── Evict TSHR grant received ─────────────────────────────────────

  val evictGntForThisTSHR = io.evictGnt.valid &&
                            io.evictGnt.bits.refillMshrId === io.mshrId

  when (evictGntForThisTSHR && !w_evictGnt) {
    w_evictGnt  := true.B
    evictMshrId := io.evictGnt.bits.evictMshrId
  }

  // ── EvictTSHR old data read out (dirty path) ──────────────────────

  val evictDatOutForThisTSHR = io.evictDatOut.valid &&
                               io.evictDatOut.bits.refillMshrId === io.mshrId

  when (evictDatOutForThisTSHR && !w_evictDatOut) {
    w_evictDatOut := true.B
  }

  // ── victimDatSafeOut to DS FSM ────────────────────────────────────
  // Single-cycle pulse telling the local DS FSM the victim's old data
  // is no longer needed in DataStorage, so DSBufWb may overwrite it.
  // CMN data is a *precondition* of refillReq.fire, so there is no CMN
  // gating here.
  //
  // Two distinct pulse sources, kept explicit so neither path's timing
  // is buried in a shared term:
  //   clean victim : nothing to evict → pulse on refillReq.fire.
  //                  Uses refillReq.bits.isDirty (input), since the
  //                  pulse is in the fire cycle itself, before the
  //                  isDirty register updates next cycle.
  //   dirty victim : pulse on the rising edge of w_evictDatOut, i.e.
  //                  when EvictTSHR signals the old line is safely in
  //                  its buffer (EvictDatReadComp received).

  val cleanSafe = io.refillReq.fire && !io.refillReq.bits.isDirty

  val w_evictDatOut_prev = RegNext(w_evictDatOut, init = true.B)
  val dirtySafe = isDirty && w_evictDatOut && !w_evictDatOut_prev

  io.victimDatSafeOut := cleanSafe || dirtySafe

  // ── Assertions ────────────────────────────────────────────────────

  // evictReq must not fire on clean victim.
  // Guarded against the refillReq transition cycle.
  assert(!(io.evictReq.valid && !isDirty && !io.refillReq.fire),
    "RefillModule: evictReq fired on clean victim")

  // victimDatSafeOut on the dirty path must coincide with the
  // evictDatOut rising edge (old data confirmed out of DataStorage).
  assert(!(io.victimDatSafeOut && isDirty && !w_evictDatOut),
    "RefillModule: victimDatSafeOut before evictDatOut on dirty path")

}
