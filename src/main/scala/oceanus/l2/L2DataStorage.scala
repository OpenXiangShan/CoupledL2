package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import coupledL2.utils.GatedSplittedSRAM
import org.chipsalliance.cde.config.Parameters
import utility.mbist.MbistPipeline

// ══════════════════════════════════════════════════════════════════════
// L2DataStorage companion object (shared interface)
// ══════════════════════════════════════════════════════════════════════

object L2DataStorage {
  class WbDone(implicit val p: Parameters) extends Bundle with HasL2Params {
    val set = UInt(setBits.W)
    val way = UInt(wayBits.W)
  }

  class PathTSHRToDataStorageUOPs extends Bundle {
    val DSBufAheadRd = Bool()
    val DSBufRd      = Bool()
    val DSBufWb      = Bool()
  }

  class PathTSHRToDataStorage(implicit val p: Parameters)
      extends PathTSHRToDataStorageUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val WAY      = UInt(32.W)   // TODO: replace with actual way index width
    val SET      = UInt(32.W)   // TODO: replace with actual set index width
    val DATA     = UInt(512.W)
  }

  class PathDataStorageToTSHRUOPs extends Bundle {
    val DSBufAheadRdArbComp = Bool()
    val DSBufAheadRdResp    = Bool()
    val DSBufRdArbComp      = Bool()
    val DSBufRdResp         = Bool()
    val DSBufWbArbComp      = Bool()
    val DSBufWbComp         = Bool()
  }

  class PathDataStorageToTSHR(implicit val p: Parameters)
      extends PathDataStorageToTSHRUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val DATA     = UInt(512.W)
    val WAY      = UInt(4.W)    // TODO: parameterize with L2 way count
  }
}

// ══════════════════════════════════════════════════════════════════════
// Internal SRAM bundles (not exposed outside DataStorage)
// ══════════════════════════════════════════════════════════════════════

class DSRequest(implicit val p: Parameters) extends Bundle with HasL2Params {
  val way = UInt(wayBits.W)
  val set = UInt(setBits.W)
  val wen = Bool()
}

class DSBeat(implicit val p: Parameters) extends Bundle with HasL2Params {
  val data = UInt((beatBytes * 8).W)
}

class DSBlock(implicit val p: Parameters) extends Bundle with HasL2Params {
  val data = UInt(blockBits.W)
}

class IssuedDSReq(implicit val p: Parameters) extends Bundle with HasL2Params {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val wen = Bool()
  val wdata = UInt(blockBits.W)
  val owner = UInt(log2Ceil(nMSHR).W)
  val isRead = Bool()
  val isRd = Bool()
}

class DSECCBankBlock(implicit val p: Parameters) extends Bundle with HasL2Params {
  val data = if (enableDataECC) {
    UInt((encDataPadBits + encBankBits * dataBankSplit).W)
  } else {
    UInt((dataBankBits * dataBankSplit).W)
  }
}

// ══════════════════════════════════════════════════════════════════════
// DataStorageSRAM: physical SRAM wrapper (MCP2) — internal use only
// ══════════════════════════════════════════════════════════════════════
//
// Timing contract (enforced by assertions):
//   io.en    high for exactly ONE cycle (N); must NOT be high two cycles
//            in a row — gates the SRAM clock.
//   io.req / io.wdata must hold UNCHANGED for TWO cycles (N and N+1).
//   io.rdata is valid at cycle N+2 (readMCP2 latency = 2).

class DataStorageSRAM(implicit val p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle {
    val en    = Input(Bool())
    val error = Output(Bool())
    val req   = Flipped(ValidIO(new DSRequest))
    val rdata = Output(new DSBlock)
    val wdata = Input(new DSBlock)
    val debugReadFire = Output(Bool())
  })

  val array = Module(new GatedSplittedSRAM(
    gen           = new DSECCBankBlock,
    set           = blocks,
    way           = 1,
    dataSplit     = dataBankSplit,
    singlePort    = true,
    readMCP2      = true,
    hasMbist      = paramL2.hasMbist,
    hasSramCtl    = paramL2.hasSramCtl,
    extraHold     = true,
    withClockGate = true
  ))
  array.io_en := io.en

  private val mbistPl =
    MbistPipeline.PlaceMbistPipeline(1, "L2DataStorage", paramL2.hasMbist)

  val arrayIdx = Cat(io.req.bits.way, io.req.bits.set)
  val wen = io.req.valid &&  io.req.bits.wen
  val ren = io.req.valid && !io.req.bits.wen
  val readFire = ren

  // ── Write path ────────────────────────────────────────────────────
  val arrayWrite = Wire(new DSECCBankBlock)
  val arrayWriteData = if (enableDataECC) {
    // NOTE: if encDataPadBits == 0, 0.U(0.W) is zero-width; Cat tolerates it.
    Cat(
      0.U(encDataPadBits.W),
      Cat(VecInit(Seq.tabulate(dataBankSplit)(i =>
        io.wdata.data(dataBankBits * (i + 1) - 1, dataBankBits * i)
      )).map(data => cacheParams.dataCode.encode(data)))
    )
  } else {
    io.wdata.data
  }
  arrayWrite.data := arrayWriteData

  // ── Read path ─────────────────────────────────────────────────────
  val arrayRead    = array.io.r.resp.data(0)
  val dataRead     = Wire(new DSBlock)
  val bankDataRead = if (enableDataECC) {
    Cat(VecInit(Seq.tabulate(dataBankSplit)(i =>
      arrayRead.data(
        encBankBits * (i + 1) - 1,
        encBankBits * i
      )(dataBankBits - 1, 0)
    )))
  } else {
    arrayRead.data
  }
  dataRead.data := bankDataRead

  // waymask = 1.U is structural: SRAM instantiated with way = 1
  // (way dimension folded into arrayIdx = Cat(way, set)). Do NOT parameterise.
  array.io.w.apply(wen, arrayWrite, arrayIdx, 1.U)
  array.io.r.apply(ren, arrayIdx)

  // ── ECC error (valid 2 cycles after read request) ─────────────────
  val error = if (enableDataECC) {
    VecInit(Seq.tabulate(dataBankSplit)(i =>
      arrayRead.data(encBankBits * (i + 1) - 1, encBankBits * i)
    )).map(data => cacheParams.dataCode.decode(data).error).reduce(_ | _) &&
      RegNext(RegNext(io.req.valid && !io.req.bits.wen))
  } else {
    false.B
  }

  io.rdata := dataRead
  io.error  := error
  io.debugReadFire := readFire

  // ── MCP2 timing-contract assertions ──────────────────────────────
  assert(!io.en || !RegNext(io.en, false.B),
    "DataStorageSRAM: continuous req prohibited under MCP2")
  assert(!(RegNext(io.en && io.req.bits.wen) &&
           (io.wdata.asUInt =/= RegNext(io.wdata.asUInt))),
    "DataStorageSRAM: wdata fails to hold for 2 cycles")
}

// ══════════════════════════════════════════════════════════════════════
// DataStorage (with integrated DSArbiter)
// ══════════════════════════════════════════════════════════════════════
//
// Each TSHR connects one-to-one: io.fromTSHR(i) / io.toTSHR(i).
//
// Internal arbiter priority (highest → lowest):
//   Level-1: DSBufWb   (preempts Level-2 unless Level-2 is starved)
//   Level-2: DSBufRd   (higher within Level-2, age-based vs AheadRd)
//            DSBufAheadRd
//
// MCP2 flow in the current functional SRAM model:
//   cycle N:   grant → dsEn=1, req/wdata driven from winner
//   cycle N+1: datStop_r=1 → req/wdata held from datReq_r, rdata observable
//              for the launched read and must stay aligned with owner/way
//
// Response pipeline (one-to-one per TSHR):
//   cycle N:   ArbComp  (combinational from grant)
//   cycle N+1: WbComp   (RegNext of grantWb)
//   cycle N+1: RdResp / AheadRdResp with aligned owner/way/data

class DataStorage(implicit val p: Parameters) extends Module with HasL2Params {

  val MAX_AGE = 15.U(4.W)
  val io = IO(new Bundle {
    val fromTSHR = Flipped(Vec(nMSHR, new L2DataStorage.PathTSHRToDataStorage))
    val toTSHR   = Vec(nMSHR, new L2DataStorage.PathDataStorageToTSHR)
    val error    = Output(Bool())
    val debugWinIdx = Output(UInt(log2Ceil(nMSHR).W))
    val debugReqSet = Output(UInt(setBits.W))
    val debugReqWay = Output(UInt(wayBits.W))
    val debugReqWen = Output(Bool())
    val debugReqPrevSet = Output(UInt(setBits.W))
    val debugReqPrevWay = Output(UInt(wayBits.W))
    val debugReqPrevWen = Output(Bool())
    val debugDatReqSet = Output(UInt(32.W))
    val debugDatReqWay = Output(UInt(32.W))
    val debugDatReqWen = Output(Bool())
    val debugTsReqSet = Output(Vec(nMSHR, UInt(32.W)))
    val debugTsReqWay = Output(Vec(nMSHR, UInt(32.W)))
    val debugTsReqWen = Output(Vec(nMSHR, Bool()))
  })

  // ── Instantiate physical SRAM ─────────────────────────────────────
  val sram = Module(new DataStorageSRAM)

  // ══════════════════════════════════════════════════════════════════
  // MCP2 stall logic
  // ══════════════════════════════════════════════════════════════════

  val anyGrant  = Wire(Bool())
  val datStop_r = RegNext(anyGrant, false.B)
  val canIssue  = !datStop_r

  // ── Decode requests (gated by canIssue) ──────────────────────────
  val wbVec      = VecInit(io.fromTSHR.map(_.DSBufWb      && canIssue))
  val rdVec      = VecInit(io.fromTSHR.map(_.DSBufRd      && canIssue))
  val aheadRdVec = VecInit(io.fromTSHR.map(_.DSBufAheadRd && canIssue))

  val anyWb      = wbVec.asUInt.orR
  val anyRd      = rdVec.asUInt.orR
  val anyAheadRd = aheadRdVec.asUInt.orR
  val anyLevel2  = anyRd || anyAheadRd

  // ── Starvation counters ───────────────────────────────────────────
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
    Mux(anyStarved, rdStarved, rdWinsAge))

  // ── Level-1 vs Level-2 ───────────────────────────────────────────
  val wbWins = anyWb && !anyStarved

  // ── Round-Robin state ─────────────────────────────────────────────
  val rrWb      = RegInit(0.U(log2Ceil(nMSHR).W))
  val rrRd      = RegInit(0.U(log2Ceil(nMSHR).W))
  val rrAheadRd = RegInit(0.U(log2Ceil(nMSHR).W))

  // Fixed roundRobinNext: correct index folding + startPos overflow guard
  def roundRobinNext(last: UInt, mask: UInt, n: Int): UInt = {
    val w        = log2Ceil(n)
    val doubled  = Cat(mask, mask)
    val startPos = Mux(last === (n - 1).U, 0.U, last + 1.U)
    val rotated  = (doubled >> startPos)(n - 1, 0)
    val relIdx   = PriorityEncoder(rotated)
    val absRaw   = startPos +& relIdx
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

  val winIdx   = Mux(grantWb, winWb, Mux(grantRd, winRd, winAheadRd))
  val issueSel = io.fromTSHR(winIdx)
  val issueReadFire = grantRd || grantAheadRd
  val issueReq = Wire(new IssuedDSReq)
  issueReq.set := issueSel.SET(setBits - 1, 0)
  issueReq.way := issueSel.WAY(wayBits - 1, 0)
  issueReq.wen := grantWb
  issueReq.wdata := issueSel.DATA
  issueReq.owner := winIdx
  issueReq.isRead := issueReadFire
  issueReq.isRd := grantRd
  val issuedReq_r = RegEnable(issueReq, 0.U.asTypeOf(issueReq), anyGrant)
  val heldReq = Wire(chiselTypeOf(issuedReq_r))
  heldReq := issuedReq_r

  // ── Update RR state ───────────────────────────────────────────────
  when(grantWb)      { rrWb      := winWb      }
  when(grantRd)      { rrRd      := winRd      }
  when(grantAheadRd) { rrAheadRd := winAheadRd }

  // ── Update starvation counters ────────────────────────────────────
  def incrCapped(x: UInt, cond: Bool): UInt =
    Mux(cond && (x < MAX_AGE), x + 1.U, x)

  when(grantWb) {
    ageRd      := incrCapped(ageRd,      anyRd)
    ageAheadRd := incrCapped(ageAheadRd, anyAheadRd)
  }.elsewhen(grantRd) {
    ageRd      := 0.U
    ageAheadRd := incrCapped(ageAheadRd, anyAheadRd)
  }.elsewhen(grantAheadRd) {
    ageAheadRd := 0.U
    ageRd      := incrCapped(ageRd, anyRd)
  }.otherwise {
    ageRd      := incrCapped(ageRd,      anyRd)
    ageAheadRd := incrCapped(ageAheadRd, anyAheadRd)
  }

  // ══════════════════════════════════════════════════════════════════
  // Drive physical SRAM (MCP2 contract)
  // ══════════════════════════════════════════════════════════════════

  sram.io.en           := anyGrant
  sram.io.req.valid    := anyGrant || datStop_r
  sram.io.req.bits.way := Mux(datStop_r, heldReq.way, issueReq.way)
  sram.io.req.bits.set := Mux(datStop_r, heldReq.set, issueReq.set)
  sram.io.req.bits.wen := Mux(datStop_r, heldReq.wen, issueReq.wen)
  sram.io.wdata.data   := Mux(datStop_r, heldReq.wdata, issueReq.wdata)

  val sramReqPrev = RegNext(sram.io.req.bits)
  io.debugWinIdx := winIdx
  io.debugReqSet := sram.io.req.bits.set
  io.debugReqWay := sram.io.req.bits.way
  io.debugReqWen := sram.io.req.bits.wen
  io.debugReqPrevSet := sramReqPrev.set
  io.debugReqPrevWay := sramReqPrev.way
  io.debugReqPrevWen := sramReqPrev.wen
  io.debugDatReqSet := issuedReq_r.set
  io.debugDatReqWay := issuedReq_r.way
  io.debugDatReqWen := issuedReq_r.wen
  io.debugTsReqSet := VecInit(io.fromTSHR.map(_.SET))
  io.debugTsReqWay := VecInit(io.fromTSHR.map(_.WAY))
  io.debugTsReqWen := VecInit(io.fromTSHR.map(_.DSBufWb))

  io.error := sram.io.error

  val sramReqHoldMismatch = sram.io.req.asUInt =/= RegNext(sram.io.req.asUInt)
  val sramReqHoldTrigger = datStop_r && sramReqHoldMismatch
  assert(
    !sramReqHoldTrigger,
    "DataStorage: req fails to hold for MCP2 extraHold cycle datStop_r=%d " +
      "req=0x%x prevReq=0x%x set=0x%x prevSet=0x%x way=0x%x prevWay=0x%x wen=%d prevWen=%d",
    datStop_r,
    sram.io.req.asUInt,
    RegNext(sram.io.req.asUInt),
    sram.io.req.bits.set,
    sramReqPrev.set,
    sram.io.req.bits.way,
    sramReqPrev.way,
    sram.io.req.bits.wen,
    sramReqPrev.wen
  )

  // ══════════════════════════════════════════════════════════════════
  // Response pipeline (one-to-one per TSHR)
  // ══════════════════════════════════════════════════════════════════

  val grantWb_r        = RegNext(grantWb, false.B)
  val winWb_r          = RegNext(winWb)
  val readReqFire      = issueReadFire
  val readReqFire_r    = RegNext(readReqFire, false.B)
  val readReqIsRd_r    = RegEnable(issueReq.isRd, false.B, readReqFire)
  val readReqOwner_r   = RegEnable(issueReq.owner, 0.U, readReqFire)
  val readReqWay_r     = RegEnable(issueReq.way, 0.U, readReqFire)

  for (i <- 0 until nMSHR) {
    val t = io.toTSHR(i)
    val rdRespHit = readReqFire_r && readReqIsRd_r  && (readReqOwner_r === i.U)
    val aheadRdRespHit = readReqFire_r && !readReqIsRd_r && (readReqOwner_r === i.U)
    t.TSHRID            := i.U
    t.DSBufWbArbComp    := false.B
    t.DSBufRdArbComp    := false.B
    t.DSBufAheadRdArbComp := false.B
    t.DSBufWbComp       := false.B
    t.DSBufRdResp       := false.B
    t.DSBufAheadRdResp  := false.B
    t.DATA              := sram.io.rdata.data
    t.WAY               := readReqWay_r

    // ArbComp: cycle N
    when(grantWb      && (winWb      === i.U)) { t.DSBufWbArbComp      := true.B }
    when(grantRd      && (winRd      === i.U)) { t.DSBufRdArbComp      := true.B }
    when(grantAheadRd && (winAheadRd === i.U)) { t.DSBufAheadRdArbComp := true.B }

    // WbComp: cycle N+1
    when(grantWb_r && (winWb_r === i.U)) { t.DSBufWbComp := true.B }

    // RdResp / AheadRdResp: cycle N+1
    when(rdRespHit) { t.DSBufRdResp := true.B }
    when(aheadRdRespHit) { t.DSBufAheadRdResp := true.B }

  }

  // ══════════════════════════════════════════════════════════════════
  // Assertions
  // ══════════════════════════════════════════════════════════════════

  io.fromTSHR.zipWithIndex.foreach { case (req, i) =>
    assert(PopCount(Seq(req.DSBufWb, req.DSBufRd, req.DSBufAheadRd)) <= 1.U,
      s"DataStorage: TSHR $i asserts multiple DS operation types")
  }
  assert(PopCount(Seq(grantWb, grantRd, grantAheadRd)) <= 1.U,
    "DataStorage: multiple grants in same cycle")
  assert(!(anyGrant && datStop_r),
    "DataStorage: grant fired during MCP2 hold cycle")
  assert(!(grantWb && ageRd      === MAX_AGE && anyRd),
    "DataStorage: DSBufRd starved beyond MAX_AGE")
  assert(!(grantWb && ageAheadRd === MAX_AGE && anyAheadRd),
    "DataStorage: DSBufAheadRd starved beyond MAX_AGE")
  assert(!(grantRd && ageAheadRd === MAX_AGE && anyAheadRd),
    "DataStorage: DSBufAheadRd starved beyond MAX_AGE (behind Rd)")

  // ══════════════════════════════════════════════════════════════════
  // Perf
  // ══════════════════════════════════════════════════════════════════

  XSPerfAccumulate("ds_grant_wb",        grantWb)
  XSPerfAccumulate("ds_grant_rd",        grantRd)
  XSPerfAccumulate("ds_grant_aheadRd",   grantAheadRd)
  XSPerfAccumulate("ds_stall_mcp2",      datStop_r)
  XSPerfAccumulate("ds_idle",            !anyGrant && !datStop_r)
  XSPerfAccumulate("ds_starve_rd",       rdStarved)
  XSPerfAccumulate("ds_starve_aheadRd",  aheadRdStarved)
  XSPerfAccumulate("ds_age_rd_max",      anyRd      && ageRd      === MAX_AGE)
  XSPerfAccumulate("ds_age_aheadRd_max", anyAheadRd && ageAheadRd === MAX_AGE)
}
