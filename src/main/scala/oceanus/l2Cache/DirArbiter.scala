/*
 * DirArbiter.scala
 *
 * Arbitrates access to the Directory (single-port SRAM) from nMSHR TSHRs.
 *
 * Input: Vec(nMSHR, PathToDirectory) — each TSHR asserts at most one of
 *   ReplRd / DirRd / DirWb per cycle.
 *
 * Internal grouping by operation type:
 *   Group ReplRd (Level 1, preemptive):
 *     Any ReplRd wins over Level-2 EXCEPT when a Level-2 group is
 *     starved, or when Directory asserts replStall (PLRU RAW-hazard
 *     window). ReplRd has no starvation counter but cannot starve
 *     indefinitely (stalled ≤2 cycles by replStall, or by a starved
 *     Level-2 group which clears within MAX_AGE).
 *
 *   Group DirWb  (Level 2, static priority: highest):
 *   Group DirRd  (Level 2, static priority: lowest):
 *     Arbitrated by weighted-age counter (4-bit starvation counter per group).
 *     Each cycle a Level-2 group with pending request but no grant: counter++
 *     On grant: winner counter → 0.
 *     Tie-break (equal counter): DirWb wins over DirRd.
 *     Within a group (multiple TSHRs): Round-Robin.
 *
 * Starvation protection:
 *   If any group's counter reaches MAX_AGE (15):
 *     → That group is forced to win this cycle regardless of priority.
 *     → Prevents indefinite starvation of any request type.
 *     → Guarantees every request is served within MAX_AGE arbitration cycles.
 */
package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

class DirArbiter(implicit p: Parameters) extends Module with HasL2Params {

  val nMSHR   = paramL2.mshrSize
  val MAX_AGE = 15.U(4.W)
  val idxW    = log2Ceil(mshrSize)

  val io = IO(new Bundle {

    // ── Requests from all TSHRs ───────────────────────────────────
    val toDir  = Flipped(Vec(nMSHR, new L2Directory.PathToDirectory))

    // ── Single output to Directory ────────────────────────────────
    val dirIn  = new L2Directory.PathToDirectory

    // ── Hazard hint FROM Directory: do not issue ReplRd this cycle ─
    val replStall = Input(Bool())
  })

  // ── Decode requests by operation type ────────────────────────────

  val replRdVec = VecInit(io.toDir.map(_.ReplRd))
  val dirWbVec  = VecInit(io.toDir.map(_.DirWb))
  val dirRdVec  = VecInit(io.toDir.map(_.DirRd))

  val anyReplRd = replRdVec.asUInt.orR
  val anyDirWb  = dirWbVec.asUInt.orR
  val anyDirRd  = dirRdVec.asUInt.orR
  val anyLevel2 = anyDirWb || anyDirRd

  // ── Starvation counters (one per Level-2 group) ──────────────────
  // Incremented when group has a pending request but loses arbitration.
  // Reset to 0 only when that group actually wins a grant.

  val ageWb = RegInit(0.U(4.W))
  val ageRd = RegInit(0.U(4.W))

  val wbStarved  = anyDirWb && (ageWb === MAX_AGE)
  val rdStarved  = anyDirRd && (ageRd === MAX_AGE)
  val anyStarved = wbStarved || rdStarved

  // ── Level-2 winner selection ──────────────────────────────────────
  //
  // Priority rules (evaluated in order):
  //   1. Starvation override: starved group wins unconditionally.
  //      If both starved, higher age wins; tie → DirWb.
  //   2. Age-based: group with larger counter wins.
  //      Tie (equal age): DirWb wins over DirRd.

  val wbWinsAge = anyDirWb && (!anyDirRd || (ageWb >= ageRd))

  val level2WinIsWb = anyLevel2 && Mux(anyStarved,
    Mux(wbStarved && rdStarved,
      ageWb >= ageRd,   // both starved: higher age wins; tie → DirWb
      wbStarved),       // only one starved: that one wins
    wbWinsAge)          // normal age arbitration

  val level2Valid = anyLevel2

  // ── Level-1 vs Level-2 ───────────────────────────────────────────
  // ReplRd preempts Level-2 unless:
  //   - a Level-2 group is starved, OR
  //   - Directory asserts replStall (PLRU RAW hazard window).

  val replRdWins = anyReplRd && !anyStarved && !io.replStall

  // ── Round-Robin within each group ────────────────────────────────

  val rrReplRd = RegInit(0.U(idxW.W))
  val rrDirWb  = RegInit(0.U(idxW.W))
  val rrDirRd  = RegInit(0.U(idxW.W))

  // Pick the next set bit in `mask` strictly after `last`, wrapping
  // around if necessary, and return its index in the ORIGINAL space.
  // Prefers slots after `last`; only wraps when none remain.
  def roundRobinNext(last: UInt, mask: UInt, n: Int): UInt = {
    val w        = log2Ceil(n)
    // rotate mask right so that the slot just after `last` lands at bit 0
    val doubled  = Cat(mask, mask)                 // 2n bits
    val startPos = Mux(last === (n - 1).U, 0.U, last + 1.U)  // avoid >> n overflow
    val rotated  = (doubled >> startPos)(n - 1, 0) // n bits, indexed from `startPos`
    val relIdx   = PriorityEncoder(rotated)        // first set bit in rotated space
    // fold relative index back to absolute: (startPos + relIdx) mod n
    val absRaw   = startPos +& relIdx              // +& to avoid truncation
    val abs      = Mux(absRaw >= n.U, absRaw - n.U, absRaw)
    abs(w - 1, 0)
  }

  val winReplRd = roundRobinNext(rrReplRd, replRdVec.asUInt, nMSHR)
  val winDirWb  = roundRobinNext(rrDirWb,  dirWbVec.asUInt,  nMSHR)
  val winDirRd  = roundRobinNext(rrDirRd,  dirRdVec.asUInt,  nMSHR)

  // ── Final grant ───────────────────────────────────────────────────

  val grantReplRd = replRdWins  && anyReplRd
  val grantDirWb  = !replRdWins && level2Valid &&  level2WinIsWb && anyDirWb
  val grantDirRd  = !replRdWins && level2Valid && !level2WinIsWb && anyDirRd

  val winIdx   = Mux(grantReplRd, winReplRd,
                 Mux(grantDirWb,  winDirWb,
                                  winDirRd))
  val anyGrant = grantReplRd || grantDirWb || grantDirRd

  // ── Drive output to Directory ─────────────────────────────────────

  io.dirIn := Mux(anyGrant,
    io.toDir(winIdx),
    0.U.asTypeOf(new L2Directory.PathToDirectory))

  // ── Update Round-Robin pointers ───────────────────────────────────

  when(grantReplRd) { rrReplRd := winReplRd }
  when(grantDirWb)  { rrDirWb  := winDirWb  }
  when(grantDirRd)  { rrDirRd  := winDirRd  }

  // ── Update starvation counters ────────────────────────────────────
  // A group's age resets ONLY when that group wins. Otherwise, if it
  // has a pending request it ages up. This means a ReplRd grant (or a
  // replStall stall with no Level-2 grant) correctly ages both groups.

  def incrCapped(cnt: UInt, en: Bool): UInt =
    Mux(en, Mux(cnt === MAX_AGE, MAX_AGE, cnt + 1.U), cnt)

  ageWb := Mux(grantDirWb, 0.U, incrCapped(ageWb, anyDirWb))
  ageRd := Mux(grantDirRd, 0.U, incrCapped(ageRd, anyDirRd))

  // ── Assertions ────────────────────────────────────────────────────

  // Each TSHR must assert at most one operation type per cycle
  io.toDir.zipWithIndex.foreach { case (req, i) =>
    assert(PopCount(Seq(req.ReplRd, req.DirRd, req.DirWb)) <= 1.U,
      s"DirArbiter: TSHR $i asserts multiple operation types")
  }

  // At most one grant per cycle
  assert(PopCount(Seq(grantReplRd, grantDirWb, grantDirRd)) <= 1.U,
    "DirArbiter: multiple grants in same cycle")

  // ReplRd must not be granted while Directory asserts replStall
  assert(!(grantReplRd && io.replStall),
    "DirArbiter: ReplRd granted while replStall asserted (PLRU hazard)")

  // Starvation guarantee: a starved DirWb must win unless a starved
  // DirRd with strictly higher age takes precedence.
  assert(!(wbStarved && !grantDirWb && !(rdStarved && (ageRd > ageWb))),
    "DirArbiter: DirWb starved but not granted")
  // A starved DirRd must win unless a starved DirWb with >= age takes precedence.
  assert(!(rdStarved && !grantDirRd && !(wbStarved && (ageWb >= ageRd))),
    "DirArbiter: DirRd starved but not granted")

  // Note: ReplRd has no starvation counter. It can only be delayed by a
  // starved Level-2 group or by replStall (≤2 cycles), so it cannot
  // starve indefinitely. Monitored by the perf counter below.

  // ── Performance counters ──────────────────────────────────────────

  XSPerfAccumulate("arb_grant_replRd",     grantReplRd)
  XSPerfAccumulate("arb_grant_dirWb",      grantDirWb)
  XSPerfAccumulate("arb_grant_dirRd",      grantDirRd)
  XSPerfAccumulate("arb_replRd_stalled",   anyReplRd && !grantReplRd)
  XSPerfAccumulate("arb_replRd_byStall",   anyReplRd && io.replStall)
  XSPerfAccumulate("arb_replRd_byStarve",  anyReplRd && !io.replStall && anyStarved)
  XSPerfAccumulate("arb_starve_wb",        wbStarved)
  XSPerfAccumulate("arb_starve_rd",        rdStarved)
}
