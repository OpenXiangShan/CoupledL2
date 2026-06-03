package coupledL2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters

class DSArbiter(implicit p: Parameters) extends Module with HasL2Params {

  val nMSHR   = paramL2.nMSHR
  val MAX_AGE = 15.U(4.W)

  val io = IO(new Bundle {

    // ── Requests from all TSHRs ───────────────────────────────────
    val fromTSHR = Flipped(Vec(nMSHR, new L2DataStorage.PathTSHRToDataStorage))

    // ── Responses back to each TSHR ───────────────────────────────
    // Vec(nMSHR) is required (unlike DirArbiter's single dirOut) because
    // ArbComp (cycle N), WbComp (cycle N+1), RdResp (cycle N+2) can
    // simultaneously target different TSHRs on consecutive grants.
    val toTSHR = Vec(nMSHR, new L2DataStorage.PathDataStorageToTSHR)

    // ── To physical DataStorage ───────────────────────────────────
    val dsEn    = Output(Bool())                  // clock-gate enable (cycle N only)
    val dsReq   = Output(ValidIO(new DSRequest))  // held valid for 2 cycles (MCP2)
    val dsWdata = Output(new DSBlock)
    val dsRdata = Input(new DSBlock)
  })

  // ══════════════════════════════════════════════════════════════════
  //  MCP2 stall logic
  //  datStop_r: high on cycle N+1; blocks all new requests (canIssue=0)
  //  datReq_r:  captures winning req at cycle N; feeds SRAM on cycle N+1
  //             to satisfy "req must hold for 2 cycles" constraint
  // ══════════════════════════════════════════════════════════════════

  val anyGrant  = Wire(Bool())            // forward-declare to break dependency
  val datStop_r = RegNext(anyGrant, false.B)
  val canIssue  = !datStop_r

  // ── Decode requests (gated by canIssue) ──────────────────────────

  val wbVec      = VecInit(io.fromTSHR.map(_.DSBufWb      && canIssue))  // Level-1
  val rdVec      = VecInit(io.fromTSHR.map(_.DSBufRd      && canIssue))  // Level-2 high
  val aheadRdVec = VecInit(io.fromTSHR.map(_.DSBufAheadRd && canIssue))  // Level-2 low

  val anyWb      = wbVec.asUInt.orR
  val anyRd      = rdVec.asUInt.orR
  val anyAheadRd = aheadRdVec.asUInt.orR
  val anyLevel2  = anyRd || anyAheadRd

  // ── Starvation counters (one per Level-2 group) ──────────────────

  val ageRd      = RegInit(0.U(4.W))
  val ageAheadRd = RegInit(0.U(4.W))

  val rdStarved      = anyRd      && (ageRd      === MAX_AGE)
  val aheadRdStarved = anyAheadRd && (ageAheadRd === MAX_AGE)
  val anyStarved     = rdStarved || aheadRdStarved

  val bothStarvedToggle = RegInit(false.B)
  when(rdStarved && aheadRdStarved) { bothStarvedToggle := !bothStarvedToggle }

  // ── Level-2 winner selection ──────────────────────────────────────

  val rdWinsAge     = anyRd && (!anyAheadRd || (ageRd >= ageAheadRd))
  val level2WinIsRd = Mux(rdStarved && aheadRdStarved,
    bothStarvedToggle,
    Mux(anyStarved,
      rdStarved,
      rdWinsAge))

  // ── Level-1 vs Level-2 ───────────────────────────────────────────

  val wbWins = anyWb && !anyStarved

  // ── Round-Robin state (per group) ────────────────────────────────

  val rrWb      = RegInit(0.U(log2Ceil(nMSHR).W))
  val rrRd      = RegInit(0.U(log2Ceil(nMSHR).W))
  val rrAheadRd = RegInit(0.U(log2Ceil(nMSHR).W))

  // Pick the next set bit in `mask` strictly after `last`, wrapping
  // around if necessary, and return its index in the ORIGINAL space.
  // Prefers slots after `last`; only wraps when none remain after it.
  def roundRobinNext(last: UInt, mask: UInt, n: Int): UInt = {
    val w        = log2Ceil(n)
    val doubled  = Cat(mask, mask)                          // 2n bits
    // Slot just after `last`; avoid shifting by n (overflow) when last==n-1.
    val startPos = Mux(last === (n - 1).U, 0.U, last + 1.U)
    val rotated  = (doubled >> startPos)(n - 1, 0)          // n bits, bit0 == startPos
    val relIdx   = PriorityEncoder(rotated)                 // first set bit in rotated space
    // Fold relative index back to absolute: (startPos + relIdx) mod n
    val absRaw   = startPos +& relIdx                       // +& avoids truncation
    val abs      = Mux(absRaw >= n.U, absRaw - n.U, absRaw)
    abs(w - 1, 0)
  }

  val winWb      = roundRobinNext(rrWb,      wbVec.asUInt,      nMSHR)
  val winRd      = roundRobinNext(rrRd,      rdVec.asUInt,      nMSHR)
  val winAheadRd = roundRobinNext(rrAheadRd, aheadRdVec.asUInt, nMSHR)

  // ── Final grant ───────────────────────────────────────────────────

  val grantWb      = canIssue &&  wbWins && anyWb
  val grantRd      = canIssue && !wbWins && anyLevel2 &&  level2WinIsRd && anyRd
  val grantAheadRd = canIssue && !wbWins && anyLevel2 && !level2WinIsRd && anyAheadRd

  anyGrant := grantWb || grantRd || grantAheadRd

  val winIdx = Mux(grantWb, winWb, Mux(grantRd, winRd, winAheadRd))

  // ── datReq_r: MCP2 hold register ─────────────────────────────────

  val datReq_r = RegEnable(io.fromTSHR(winIdx), anyGrant)

  // ── Drive DataStorage ─────────────────────────────────────────────

  io.dsEn           := anyGrant
  io.dsReq.valid    := anyGrant || datStop_r
  io.dsReq.bits.way := Mux(datStop_r,
    datReq_r.WAY(wayBits - 1, 0),
    io.fromTSHR(winIdx).WAY(wayBits - 1, 0))
  io.dsReq.bits.set := Mux(datStop_r,
    datReq_r.SET(setBits - 1, 0),
    io.fromTSHR(winIdx).SET(setBits - 1, 0))
  io.dsReq.bits.wen := Mux(datStop_r, datReq_r.DSBufWb, grantWb)
  io.dsWdata.data   := Mux(datStop_r, datReq_r.DATA, io.fromTSHR(winIdx).DATA)

  // ── Update Round-Robin state ──────────────────────────────────────

  when(grantWb)      { rrWb      := winWb      }
  when(grantRd)      { rrRd      := winRd      }
  when(grantAheadRd) { rrAheadRd := winAheadRd }

  // ── Update starvation counters ────────────────────────────────────

  def incrCapped(x: UInt, cond: Bool): UInt = Mux(cond && (x < MAX_AGE), x + 1.U, x)

  when(canIssue) {
    ageRd      := Mux(grantRd,      0.U, incrCapped(ageRd,      anyRd))
    ageAheadRd := Mux(grantAheadRd, 0.U, incrCapped(ageAheadRd, anyAheadRd))
  }
  // else (MCP2 stall): hold both ages.

  // ── Response pipeline ─────────────────────────────────────────────

  val grantWb_r       = RegNext(grantWb,         false.B)
  val winWb_r         = RegNext(winWb)

  val grantRd_rr      = RegNext(RegNext(grantRd,      false.B), false.B)
  val grantAheadRd_rr = RegNext(RegNext(grantAheadRd, false.B), false.B)
  val winRd_rr        = RegNext(RegNext(winRd))
  val winAheadRd_rr   = RegNext(RegNext(winAheadRd))

  val way_rr = RegNext(datReq_r.WAY)

  // ── Drive toTSHR ─────────────────────────────────────────────────

  for (i <- 0 until nMSHR) {
    val t = io.toTSHR(i)
    t.TSHRADDR := i.U

    // ArbComp: cycle N
    t.DSBufWbArbComp      := grantWb      && (winWb      === i.U)
    t.DSBufRdArbComp      := grantRd      && (winRd      === i.U)
    t.DSBufAheadRdArbComp := grantAheadRd && (winAheadRd === i.U)

    // WbComp: cycle N+1
    t.DSBufWbComp := grantWb_r && (winWb_r === i.U)

    // RdResp / AheadRdResp: cycle N+2
    t.DSBufRdResp      := grantRd_rr      && (winRd_rr      === i.U)
    t.DSBufAheadRdResp := grantAheadRd_rr && (winAheadRd_rr === i.U)

    t.DATA := io.dsRdata.data
    t.WAY  := way_rr
  }

  // ── Assertions ────────────────────────────────────────────────────

  io.fromTSHR.zipWithIndex.foreach { case (req, i) =>
    assert(PopCount(Seq(req.DSBufWb, req.DSBufRd, req.DSBufAheadRd)) <= 1.U,
      s"DSArbiter: TSHR $i asserts multiple DS operation types")
  }
  assert(PopCount(Seq(grantWb, grantRd, grantAheadRd)) <= 1.U,
    "DSArbiter: multiple grants in same cycle")
  assert(!(anyGrant && datStop_r),
    "DSArbiter: grant fired during MCP2 hold cycle")
  assert(!(grantWb && ageRd      === MAX_AGE && anyRd),
    "DSArbiter: DSBufRd starved beyond MAX_AGE")
  assert(!(grantWb && ageAheadRd === MAX_AGE && anyAheadRd),
    "DSArbiter: DSBufAheadRd starved beyond MAX_AGE")
  assert(!(grantRd && ageAheadRd === MAX_AGE && anyAheadRd),
    "DSArbiter: DSBufAheadRd starved beyond MAX_AGE (behind Rd)")

  // ── Performance counters ──────────────────────────────────────────

  XSPerfAccumulate("ds_arb_grant_wb",        grantWb)
  XSPerfAccumulate("ds_arb_grant_rd",        grantRd)
  XSPerfAccumulate("ds_arb_grant_aheadRd",   grantAheadRd)
  XSPerfAccumulate("ds_arb_stall_mcp2",      datStop_r)
  XSPerfAccumulate("ds_arb_idle",            !anyGrant && !datStop_r)
  XSPerfAccumulate("ds_arb_starve_rd",       rdStarved)
  XSPerfAccumulate("ds_arb_starve_aheadRd",  aheadRdStarved)
  XSPerfAccumulate("ds_arb_age_rd_max",      anyRd      && ageRd      === MAX_AGE)
  XSPerfAccumulate("ds_arb_age_aheadRd_max", anyAheadRd && ageAheadRd === MAX_AGE)
}
