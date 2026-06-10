package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

// ══════════════════════════════════════════════════════════════════════
// L2Directory companion object (shared interface)
// ══════════════════════════════════════════════════════════════════════

object L2Directory {

  object MetaState {
    def apply() = UInt(2.W)
    def UU: UInt = 3.U(2.W)
    def US: UInt = 2.U(2.W)
    def S:  UInt = 1.U(2.W)
    def I:  UInt = 0.U(2.W)

    def isClientU(state: UInt, client: Bool) = Mux(state === UU, client, false.B)
    def isClientS(state: UInt, client: Bool) = Mux(state === US || state === S, client, false.B)

    def toLocal(state: UInt): L2CacheLocalState = {
      val value = Wire(new L2CacheLocalState)
      value.bits := ParallelLookUp(state, Seq(
        (UU, L2CacheStates.U),
        (US, L2CacheStates.U),
        (S,  L2CacheStates.S),
        (I,  L2CacheStates.I)
      ))
      value
    }
  }

  // Meta: coherence state (NO tag — tag is internal to Directory)
  class Meta(implicit val p: Parameters) extends Bundle with HasL2Params {
    val state   = MetaState()
    val dirty   = Bool()
    val clients = Vec(1, Bool())   // TODO: parameterize with coherent client count
    val alias   = UInt(2.W)        // TODO: parameterize with L2 alias width
  }

  class MetaWriteMask(implicit val p: Parameters) extends Bundle with HasL2Params {
    val state   = Bool()
    val dirty   = Bool()
    val clients = Vec(1, Bool())
    val alias   = Bool()

    def maskAndWrite(dst: Meta, src: Meta): Unit = {
      when(state) { dst.state := src.state }
      when(dirty) { dst.dirty := src.dirty }
      when(alias) { dst.alias := src.alias }
      clients.zip(dst.clients.zip(src.clients)).foreach {
        case (mask, (d, s)) => when(mask) { d := s }
      }
    }
    def maskAndWrite(dst: Meta, dstFlag: MetaWriteMask, src: Meta): Unit = {
      maskAndWrite(dst, src)
      when(state) { dstFlag.state := true.B }
      when(dirty) { dstFlag.dirty := true.B }
      when(alias) { dstFlag.alias := true.B }
      clients.zip(dstFlag.clients).foreach { case (mask, d) => when(mask) { d := true.B } }
    }
    def unmaskAndWrite(dst: Meta, src: Meta): Unit = {
      when(!state) { dst.state := src.state }
      when(!dirty) { dst.dirty := src.dirty }
      when(!alias) { dst.alias := src.alias }
      clients.zip(dst.clients.zip(src.clients)).foreach {
        case (mask, (d, s)) => when(!mask) { d := s }
      }
    }
  }
  object MetaWriteMask {
    def empty(implicit p: Parameters): MetaWriteMask = {
      val zero = Wire(new MetaWriteMask)
      zero.elements.foreach(_._2 := 0.U)
      zero
    }
  }

  class MetaReadResult(implicit override val p: Parameters) extends Meta with HasL2Params {
    val way = UInt(4.W)   // TODO: parameterize with l2 way count
    val hit = Bool()      // DirRd: tag hit. ReplRd: unused.
  }

  class ReplReadResult(implicit val p: Parameters) extends Bundle with HasL2Params {
    val paddr = UInt(paramL2.physicalAddrWidth.W)  // victim PA; valid when META.state =/= I
  }

  class PathToDirectoryUOPs extends Bundle {
    val DirRd  = Bool()
    val DirWb  = Bool()
    val ReplRd = Bool()
  }

  class PathToDirectory(implicit val p: Parameters) extends PathToDirectoryUOPs with HasL2Params {
    val TSHRID   = UInt(mshrIndexWidth.W)
    val PADDR    = UInt(paramL2.physicalAddrWidth.W)
    val WAY      = UInt(4.W)
    val META     = new L2Directory.Meta
    val META_WEN = new L2Directory.MetaWriteMask
    val TAG_WEN  = Bool()
  }

  class PathFromDirectoryUOPs extends Bundle {
    val DirRdArbComp   = Bool()
    val DirRdResp      = Bool()
    val DirWbArbComp   = Bool()
    val ReplRdArbComp  = Bool()
    val ReplRdResp     = Bool()
    val ReplRdRetryAck = Bool()
  }

  class PathFromDirectory(implicit val p: Parameters) extends PathFromDirectoryUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val META   = new L2Directory.MetaReadResult
    val REPL   = new L2Directory.ReplReadResult
  }

  // Internal SRAM entry: tag + meta together
  class DirEntry(implicit val p: Parameters) extends Bundle with HasL2Params {
    val tag  = UInt(paramL2.tagWidth.W)
    val meta = new L2Directory.Meta
  }
}

// ══════════════════════════════════════════════════════════════════════
// Tree-PLRU for 4-way (3 bits/set)
// ══════════════════════════════════════════════════════════════════════

object PLRU4 {
  val nBits = 3
  def getReplaceWay(state: UInt): UInt = {
    val way = WireDefault(0.U(2.W))
    when(state(0) === 0.U) { way := Mux(state(1) === 0.U, 0.U, 1.U) }
      .otherwise           { way := Mux(state(2) === 0.U, 2.U, 3.U) }
    way
  }
  def getNextState(state: UInt, touchWay: UInt): UInt = {
    val next = WireInit(VecInit(state.asBools))
    val inLeft = touchWay(1) === 0.U
    val first  = touchWay(0) === 0.U
    next(0) := Mux(inLeft, true.B, false.B)
    when(inLeft) { next(1) := Mux(first, true.B, false.B) }
      .otherwise { next(2) := Mux(first, true.B, false.B) }
    next.asUInt
  }
}

// ══════════════════════════════════════════════════════════════════════
// Directory (with integrated arbiter)
// ══════════════════════════════════════════════════════════════════════
//
// Each TSHR connects one-to-one: io.toDir(i) / io.fromDir(i).
// Internal arbiter selects one request per cycle (ReplRd > DirRd > DirWb,
// with starvation protection on Level-2 and round-robin within a group).
// The winner enters a 3-stage pipeline: S1 decode + SRAM/PLRU read,
// S2 latch, S3 hit/way/result.
//
// ArbComp: DirRd/ReplRd ArbComp asserted combinationally the cycle the
//   request wins arbitration (cycle N). DirWb ArbComp is delayed one
//   cycle (N+1) so the bitmask write has landed and later reads will hit.
// Resp: driven at S3, routed one-to-one to fromDir(s3_tshr).
//
// replStall (internal): a ReplRd in flight at S2/S3 blocks new ReplRd
//   grants, closing the PLRU register S3-write/S1-read RAW window.
// dirInFlight: a same-set DirRd in flight drives ReplRdRetryAck.

class Directory(implicit p: Parameters) extends Module with HasL2Params {

  val nMSHR   = paramL2.nMSHR
  val MAX_AGE = 15.U(4.W)
  val idxW    = log2Ceil(nMSHR)

  val io = IO(new Bundle {
    val toDir   = Flipped(Vec(nMSHR, new L2Directory.PathToDirectory))
    val fromDir = Vec(nMSHR, new L2Directory.PathFromDirectory)
  })

  val sets = paramL2.sets
  val ways = paramL2.ways
  require(ways == 4, "Directory uses PLRU4 hardwired for 4 ways")
  val offsetBits = paramL2.offsetBits
  val setBits    = paramL2.setBits

  // ── SRAM + PLRU ───────────────────────────────────────────────────
  val dirArray = Module(new SRAMTemplate(
    gen = new L2Directory.DirEntry, set = sets, way = ways,
    singlePort = true, useBitmask = true, hasMbist = paramL2.hasMbist
  ))
  val plruRegs = RegInit(VecInit(Seq.fill(sets)(0.U(PLRU4.nBits.W))))

  // ══════════════════════════════════════════════════════════════════
  // Internal arbiter
  // ══════════════════════════════════════════════════════════════════
  val replRdVec = VecInit(io.toDir.map(_.ReplRd))
  val dirWbVec  = VecInit(io.toDir.map(_.DirWb))
  val dirRdVec  = VecInit(io.toDir.map(_.DirRd))

  val anyReplRd = replRdVec.asUInt.orR
  val anyDirWb  = dirWbVec.asUInt.orR
  val anyDirRd  = dirRdVec.asUInt.orR
  val anyLevel2 = anyDirWb || anyDirRd

  // replStall (declared as wire; driven after S2/S3 regs exist)
  val replStall = Wire(Bool())

  // starvation counters for Level-2 groups
  val ageWb = RegInit(0.U(4.W))
  val ageRd = RegInit(0.U(4.W))
  val wbStarved  = anyDirWb && (ageWb === MAX_AGE)
  val rdStarved  = anyDirRd && (ageRd === MAX_AGE)
  val anyStarved = wbStarved || rdStarved

  val wbWinsAge = anyDirWb && (!anyDirRd || (ageWb >= ageRd))
  val level2WinIsWb = anyLevel2 && Mux(anyStarved,
    Mux(wbStarved && rdStarved, ageWb >= ageRd, wbStarved),
    wbWinsAge)

  // ReplRd preempts Level-2 unless a Level-2 group is starved or replStall
  val replRdWins = anyReplRd && !anyStarved && !replStall

  // round-robin pointers
  val rrReplRd = RegInit(0.U(idxW.W))
  val rrDirWb  = RegInit(0.U(idxW.W))
  val rrDirRd  = RegInit(0.U(idxW.W))

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

  val winReplRd = roundRobinNext(rrReplRd, replRdVec.asUInt, nMSHR)
  val winDirWb  = roundRobinNext(rrDirWb,  dirWbVec.asUInt,  nMSHR)
  val winDirRd  = roundRobinNext(rrDirRd,  dirRdVec.asUInt,  nMSHR)

  val grantReplRd = replRdWins  && anyReplRd
  val grantDirWb  = !replRdWins && anyLevel2 &&  level2WinIsWb && anyDirWb
  val grantDirRd  = !replRdWins && anyLevel2 && !level2WinIsWb && anyDirRd

  val winIdx   = Mux(grantReplRd, winReplRd, Mux(grantDirWb, winDirWb, winDirRd))
  val anyGrant = grantReplRd || grantDirWb || grantDirRd
  val winReq   = io.toDir(winIdx)

  when(grantReplRd) { rrReplRd := winReplRd }
  when(grantDirWb)  { rrDirWb  := winDirWb  }
  when(grantDirRd)  { rrDirRd  := winDirRd  }

  def incrCapped(x: UInt, cond: Bool): UInt = Mux(cond && (x < MAX_AGE), x + 1.U, x)
  ageWb := Mux(grantDirWb, 0.U, incrCapped(ageWb, anyDirWb))
  ageRd := Mux(grantDirRd, 0.U, incrCapped(ageRd, anyDirRd))

  // ══════════════════════════════════════════════════════════════════
  // S1: decode the arbitration winner
  // ══════════════════════════════════════════════════════════════════
  val isReplRd = grantReplRd
  val isDirRd  = grantDirRd
  val isDirWb  = grantDirWb
  val isRead   = isReplRd || isDirRd

  val reqSet = winReq.PADDR(offsetBits + setBits - 1, offsetBits)
  val reqTag = winReq.PADDR(paramL2.physicalAddrWidth - 1, offsetBits + setBits)

  val dirReadData_s2 = dirArray.io.r(isRead, reqSet).resp.data
  val plruState_s1   = plruRegs(reqSet)

  // ── S1 → S2 ───────────────────────────────────────────────────────
  val s2_valid   = RegNext(isRead,        false.B)
  val s2_isRepl  = RegNext(isReplRd,      false.B)
  val s2_isDirRd = RegNext(isDirRd,       false.B)
  val s2_tshr    = RegNext(winReq.TSHRID, 0.U)
  val s2_set     = RegNext(reqSet,        0.U)
  val s2_tag     = RegNext(reqTag,        0.U)
  val s2_plru    = RegNext(plruState_s1,  0.U)

  // ── S2 → S3 ───────────────────────────────────────────────────────
  val s3_valid   = RegNext(s2_valid,   false.B)
  val s3_isRepl  = RegNext(s2_isRepl,  false.B)
  val s3_isDirRd = RegNext(s2_isDirRd, false.B)
  val s3_tshr    = RegNext(s2_tshr,    0.U)
  val s3_set     = RegNext(s2_set,     0.U)
  val s3_tag     = RegNext(s2_tag,     0.U)
  val s3_plru    = RegNext(s2_plru,    0.U)

  // replStall: ReplRd in flight at S2/S3 (closes PLRU RAW window)
  replStall := (s2_valid && s2_isRepl) || (s3_valid && s3_isRepl)

  // ══════════════════════════════════════════════════════════════════
  // DirWb: bitmask partial write at S1
  // ══════════════════════════════════════════════════════════════════
  val wbEntry = Wire(new L2Directory.DirEntry)
  wbEntry.tag  := reqTag
  wbEntry.meta := winReq.META

  val wbMask = Wire(new L2Directory.DirEntry)
  wbMask.tag        := Fill(paramL2.tagWidth, winReq.TAG_WEN)
  wbMask.meta.state := Fill(2, winReq.META_WEN.state)
  wbMask.meta.dirty := winReq.META_WEN.dirty
  wbMask.meta.alias := Fill(2, winReq.META_WEN.alias)
  wbMask.meta.clients.zip(winReq.META_WEN.clients).foreach { case (m, w) => m := w }

  dirArray.io.w(
    valid   = isDirWb,
    data    = wbEntry,
    setIdx  = reqSet,
    waymask = UIntToOH(winReq.WAY),
    bitmask = wbMask.asUInt
  )

  // ══════════════════════════════════════════════════════════════════
  // S3: hit detect, way select, PLRU update
  // ══════════════════════════════════════════════════════════════════
  val dirData_s3 = RegEnable(dirReadData_s2, 0.U.asTypeOf(dirReadData_s2), s2_valid)

  val tagMatch = VecInit(dirData_s3.map(_.tag === s3_tag))
  val validVec = VecInit(dirData_s3.map(_.meta.state =/= L2Directory.MetaState.I))
  val hitVec   = VecInit(tagMatch.zip(validVec).map { case (t, v) => t && v })
  val hit_s3   = hitVec.asUInt.orR
  val hitWay   = PriorityEncoder(hitVec)

  // dirInFlight: same-set DirRd in S2/S3 (its hit way not yet in PLRU)
  val dirInFlight =
    (s2_valid && s2_isDirRd && (s2_set === reqSet)) ||
    (s3_valid && s3_isDirRd && (s3_set === reqSet))

  val invalidVec = VecInit(dirData_s3.map(_.meta.state === L2Directory.MetaState.I))
  val hasInvalid = invalidVec.asUInt.orR
  val invalidWay = PriorityEncoder(invalidVec)
  val plruWay    = PLRU4.getReplaceWay(s3_plru)
  val victimWay  = Mux(hasInvalid, invalidWay, plruWay)

  val replRetry = dirInFlight

  val way_s3   = Mux(s3_isRepl, victimWay, Mux(hit_s3, hitWay, 0.U))
  val entry_s3 = dirData_s3(way_s3)
  val metaValid = (s3_isDirRd && hit_s3) || (s3_isRepl && !replRetry)

  val plruWen = (s3_valid && s3_isDirRd && hit_s3) ||
                (s3_valid && s3_isRepl  && !replRetry)
  when(plruWen) {
    plruRegs(s3_set) := PLRU4.getNextState(s3_plru, way_s3)
  }

  // ══════════════════════════════════════════════════════════════════
  // Response (one-to-one per TSHR)
  // ══════════════════════════════════════════════════════════════════
  io.fromDir.zipWithIndex.foreach { case (o, i) =>
    o.TSHRID         := i.U
    o.META           := 0.U.asTypeOf(new L2Directory.MetaReadResult)
    o.REPL           := 0.U.asTypeOf(new L2Directory.ReplReadResult)
    o.DirRdArbComp   := false.B
    o.DirRdResp      := false.B
    o.DirWbArbComp   := false.B
    o.ReplRdArbComp  := false.B
    o.ReplRdResp     := false.B
    o.ReplRdRetryAck := false.B
  }

  // ArbComp — DirRd/ReplRd: combinational at grant cycle (N)
  when(grantDirRd)  { io.fromDir(winIdx).DirRdArbComp  := true.B }
  when(grantReplRd) { io.fromDir(winIdx).ReplRdArbComp := true.B }

  // ArbComp — DirWb: delayed one cycle (N+1) so the write has landed
  val dirWbArb_v    = RegNext(grantDirWb, false.B)
  val dirWbArb_tshr = RegNext(winReq.TSHRID, 0.U)
  when(dirWbArb_v) { io.fromDir(dirWbArb_tshr).DirWbArbComp := true.B }

  // Resp at S3 — routed one-to-one to the originating TSHR
  when(s3_valid) {
    val o = io.fromDir(s3_tshr)
    when(metaValid) {
      o.META.state   := entry_s3.meta.state
      o.META.dirty   := entry_s3.meta.dirty
      o.META.clients := entry_s3.meta.clients
      o.META.alias   := entry_s3.meta.alias
      o.META.way     := way_s3
      o.META.hit     := Mux(s3_isRepl, false.B, hit_s3)
    }
    when(s3_isRepl) {
      o.ReplRdResp     := !replRetry
      o.ReplRdRetryAck := replRetry
      when(!replRetry) {
        o.REPL.paddr := Cat(entry_s3.tag, s3_set, 0.U(offsetBits.W))
      }
    }.otherwise {
      o.DirRdResp := true.B
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // Assertions
  // ══════════════════════════════════════════════════════════════════
  io.toDir.zipWithIndex.foreach { case (req, i) =>
    assert(PopCount(Seq(req.ReplRd, req.DirRd, req.DirWb)) <= 1.U,
      s"Directory: TSHR $i asserts multiple op types")
  }
  assert(PopCount(Seq(grantReplRd, grantDirWb, grantDirRd)) <= 1.U,
    "Directory: multiple grants in one cycle")
  assert(!(grantReplRd && replStall),
    "Directory: ReplRd granted while replStall (PLRU hazard)")
  when(s3_valid && s3_isDirRd) {
    assert(PopCount(hitVec) <= 1.U,
      "Directory: multiple ways hit (tag uniqueness)")
  }

  // ══════════════════════════════════════════════════════════════════
  // Perf
  // ══════════════════════════════════════════════════════════════════
  XSPerfAccumulate("grant_dirRd",   grantDirRd)
  XSPerfAccumulate("grant_replRd",  grantReplRd)
  XSPerfAccumulate("grant_dirWb",   grantDirWb)
  XSPerfAccumulate("replStall",     replStall)
  XSPerfAccumulate("replRetry",     s3_valid && s3_isRepl && replRetry)
  XSPerfAccumulate("dirRd_hit",     s3_valid && s3_isDirRd && hit_s3)
  XSPerfAccumulate("dirRd_miss",    s3_valid && s3_isDirRd && !hit_s3)
  XSPerfAccumulate("plru_upd",      plruWen)
  XSPerfAccumulate("starve_wb",     wbStarved)
  XSPerfAccumulate("starve_rd",     rdStarved)
}
