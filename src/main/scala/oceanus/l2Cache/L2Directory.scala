package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

// ══════════════════════════════════════════════════════════════════════
// L2Directory companion object (interface per latest shared definition)
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

  // ── Meta: coherence state (NO tag — tag is internal to Directory) ──
  class Meta(implicit val p: Parameters) extends Bundle with HasL2Params {
    val state   = MetaState()
    val dirty   = Bool()
    val clients = Vec(1, Bool())   // TODO: parameterize with coherent client count
    val alias   = UInt(2.W)        // TODO: parameterize with L2 alias width
  }

  // ── Per-field write mask for DirWb (tag handled separately) ───────
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

  // ── DirRd result returned to TSHR (no tag) ────────────────────────
  class MetaReadResult(implicit override val p: Parameters) extends Meta with HasL2Params {
    val way = UInt(4.W)   // TODO: parameterize with l2 way count
    val hit = Bool()      // DirRd: tag hit. ReplRd: unused (0).
  }

  // ── ReplRd result: victim's full PA (meta/way come via META) ──────
  class ReplReadResult(implicit val p: Parameters) extends Bundle with HasL2Params {
    val paddr = UInt(paramL2.physicalAddrWidth.W)  // victim PA; valid when META.state =/= I
  }

  // ── Request operation flags ───────────────────────────────────────
  class PathToDirectoryUOPs extends Bundle {
    val DirRd  = Bool()
    val DirWb  = Bool()
    val ReplRd = Bool()
  }

  // ── Request from a TSHR (already arbitrated upstream) ─────────────
  class PathToDirectory(implicit val p: Parameters) extends PathToDirectoryUOPs with HasL2Params {
    val TSHRID   = UInt(mshrIndexWidth.W)
    val PADDR    = UInt(paramL2.physicalAddrWidth.W)
    val WAY      = UInt(4.W)                  // DirWb only: which way to write
    val META     = new L2Directory.Meta       // DirWb only: write data
    val META_WEN = new L2Directory.MetaWriteMask  // DirWb only: per-field write enable
    val TAG_WEN  = Bool()                      // DirWb only: write the tag (from PADDR)
  }

  // ── Response operation flags ──────────────────────────────────────
  class PathFromDirectoryUOPs extends Bundle {
    val DirRdArbComp   = Bool()
    val DirRdResp      = Bool()
    val DirWbArbComp   = Bool()
    val ReplRdArbComp  = Bool()
    val ReplRdResp     = Bool()
    val ReplRdRetryAck = Bool()
  }

  // ── Response (single broadcast bus; routed by one-hot valid) ──────
  class PathFromDirectory(implicit val p: Parameters) extends PathFromDirectoryUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val META   = new L2Directory.MetaReadResult
    val REPL   = new L2Directory.ReplReadResult
  }

  // ── Internal SRAM entry: tag + meta stored together ───────────────
  class DirEntry(implicit val p: Parameters) extends Bundle with HasL2Params {
    val tag   = UInt(paramL2.tagWidth.W)
    val meta  = new L2Directory.Meta
  }
}

// ══════════════════════════════════════════════════════════════════════
// Tree-PLRU for 4-way cache (3 bits per set)
//
// Node layout:
//        node[0]
//       /        \
//   node[1]    node[2]
//    /  \        /  \
//   w0  w1     w2   w3
//
// A node bit points to the LRU (victim) subtree:
//   node = 0 → victim is in the LEFT  child
//   node = 1 → victim is in the RIGHT child
// On access, each node on the path is flipped to point AWAY from the
// accessed way (the accessed side becomes "recently used").
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
// Directory module
// ══════════════════════════════════════════════════════════════════════
//
// Single arbitrated request per cycle (DirArbiter is external).
// Pipeline: S1 decode + SRAM/PLRU read → S2 latch → S3 hit/way/result.
// DirWb: bitmask partial write (no RMW). META_WEN gates meta fields,
//        TAG_WEN gates the tag (written from PADDR).
// tag is internal: hit/way computed inside; never returned to TSHR.
// PLRU: register file; replStall (output) closes the S3-write/S1-read
//        RAW window; dirInFlight drives ReplRdRetryAck.

class Directory(implicit p: Parameters) extends Module with HasL2Params {

  val io = IO(new Bundle {
    val toDir        = Flipped(Valid(new L2Directory.PathToDirectory))
    val fromDir      = Output(new L2Directory.PathFromDirectory)
    val fromDirValid = Output(Vec(paramL2.nMSHR, Bool()))
    val replStall    = Output(Bool())
  })

  val sets = paramL2.sets
  val ways = paramL2.ways
  require(ways == 4, "Directory uses PLRU4 hardwired for 4 ways")

  val offsetBits = paramL2.offsetBits
  val setBits    = paramL2.setBits

  // ── SRAM: tag+meta, bitmask partial write ─────────────────────────
  val dirArray = Module(new SRAMTemplate(
    gen        = new L2Directory.DirEntry,
    set        = sets,
    way        = ways,
    singlePort = true,
    useBitmask = true,
    hasMbist   = paramL2.hasMbist
  ))

  // ── PLRU register file ────────────────────────────────────────────
  val plruRegs = RegInit(VecInit(Seq.fill(sets)(0.U(PLRU4.nBits.W))))

  // ══════════════════════════════════════════════════════════════════
  // S1: decode
  // ══════════════════════════════════════════════════════════════════
  val req      = io.toDir.bits
  val reqVld   = io.toDir.valid
  val isReplRd = reqVld && req.ReplRd
  val isDirRd  = reqVld && req.DirRd
  val isDirWb  = reqVld && req.DirWb
  val isRead   = isReplRd || isDirRd

  val reqSet = req.PADDR(offsetBits + setBits - 1, offsetBits)
  val reqTag = req.PADDR(paramL2.physicalAddrWidth - 1, offsetBits + setBits)

  val dirReadData_s2 = dirArray.io.r(isRead, reqSet).resp.data
  val plruState_s1   = plruRegs(reqSet)

  // ── S1 → S2 ───────────────────────────────────────────────────────
  val s2_valid   = RegNext(isRead,       false.B)
  val s2_isRepl  = RegNext(isReplRd,     false.B)
  val s2_isDirRd = RegNext(isDirRd,      false.B)
  val s2_tshr    = RegNext(req.TSHRID,   0.U)
  val s2_set     = RegNext(reqSet,       0.U)
  val s2_tag     = RegNext(reqTag,       0.U)
  val s2_plru    = RegNext(plruState_s1, 0.U)

  // ── S2 → S3 ───────────────────────────────────────────────────────
  val s3_valid   = RegNext(s2_valid,   false.B)
  val s3_isRepl  = RegNext(s2_isRepl,  false.B)
  val s3_isDirRd = RegNext(s2_isDirRd, false.B)
  val s3_tshr    = RegNext(s2_tshr,    0.U)
  val s3_set     = RegNext(s2_set,     0.U)
  val s3_tag     = RegNext(s2_tag,     0.U)
  val s3_plru    = RegNext(s2_plru,    0.U)

  // ── replStall: close PLRU S3-write/S1-read RAW window ─────────────
  io.replStall := (s2_valid && s2_isRepl) || (s3_valid && s3_isRepl)

  // ══════════════════════════════════════════════════════════════════
  // DirWb: bitmask partial write at S1 (no RMW)
  // ══════════════════════════════════════════════════════════════════
  val wbEntry = Wire(new L2Directory.DirEntry)
  wbEntry.tag        := reqTag           // written only if TAG_WEN
  wbEntry.meta       := req.META

  // bit-level bitmask: 1 = write, 0 = retain (SRAM holds old bits)
  val wbMask = Wire(new L2Directory.DirEntry)
  wbMask.tag         := Fill(paramL2.tagWidth, req.TAG_WEN)
  wbMask.meta.state  := Fill(2, req.META_WEN.state)
  wbMask.meta.dirty  := req.META_WEN.dirty
  wbMask.meta.alias  := Fill(2, req.META_WEN.alias)
  wbMask.meta.clients.zip(req.META_WEN.clients).foreach { case (m, w) => m := w }

  dirArray.io.w(
    valid   = isDirWb,
    data    = wbEntry,
    setIdx  = reqSet,
    waymask = UIntToOH(req.WAY),
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

  // dirInFlight: same-set DirRd in S2/S3 → its hit way not yet in PLRU
  val dirInFlight =
    (s2_valid && s2_isDirRd && (s2_set === reqSet)) ||
    (s3_valid && s3_isDirRd && (s3_set === reqSet))

  // victim selection: invalid way first, else PLRU
  val invalidVec = VecInit(dirData_s3.map(_.meta.state === L2Directory.MetaState.I))
  val hasInvalid = invalidVec.asUInt.orR
  val invalidWay = PriorityEncoder(invalidVec)
  val plruWay    = PLRU4.getReplaceWay(s3_plru)
  val victimWay  = Mux(hasInvalid, invalidWay, plruWay)

  val replRetry = dirInFlight

  // resolved way: ReplRd→victim, DirRd hit→hitWay, DirRd miss→0
  val way_s3   = Mux(s3_isRepl, victimWay, Mux(hit_s3, hitWay, 0.U))
  val entry_s3 = dirData_s3(way_s3)

  val metaValid = (s3_isDirRd && hit_s3) || (s3_isRepl && !replRetry)

  // PLRU update: DirRd hit (promote) or successful ReplRd (insert)
  val plruWen = (s3_valid && s3_isDirRd && hit_s3) ||
                (s3_valid && s3_isRepl  && !replRetry)
  when(plruWen) {
    plruRegs(s3_set) := PLRU4.getNextState(s3_plru, way_s3)
  }

  // ══════════════════════════════════════════════════════════════════
  // Response: single bus + one-hot valid
  // ══════════════════════════════════════════════════════════════════
  io.fromDir.TSHRID         := s3_tshr
  io.fromDir.META           := 0.U.asTypeOf(new L2Directory.MetaReadResult)
  io.fromDir.REPL           := 0.U.asTypeOf(new L2Directory.ReplReadResult)
  io.fromDir.DirRdArbComp   := false.B
  io.fromDir.DirRdResp      := false.B
  io.fromDir.DirWbArbComp   := false.B
  io.fromDir.ReplRdArbComp  := false.B
  io.fromDir.ReplRdResp     := false.B
  io.fromDir.ReplRdRetryAck := false.B

  // ArbComp: one cycle after S1 (request consumed). Produced by Directory.
  val arb_valid  = RegNext(reqVld,     false.B)
  val arb_tshr   = RegNext(req.TSHRID, 0.U)
  val arb_isRd   = RegNext(isDirRd,    false.B)
  val arb_isWb   = RegNext(isDirWb,    false.B)
  val arb_isRepl = RegNext(isReplRd,   false.B)

  when(arb_valid) {
    io.fromDir.TSHRID        := arb_tshr
    io.fromDir.DirRdArbComp  := arb_isRd
    io.fromDir.DirWbArbComp  := arb_isWb
    io.fromDir.ReplRdArbComp := arb_isRepl
  }

  // S3 result (overrides TSHRID/META/REPL/Resp for the completing op)
  when(s3_valid) {
    io.fromDir.TSHRID := s3_tshr
    when(metaValid) {
      io.fromDir.META.state   := entry_s3.meta.state
      io.fromDir.META.dirty   := entry_s3.meta.dirty
      io.fromDir.META.clients := entry_s3.meta.clients
      io.fromDir.META.alias   := entry_s3.meta.alias
      io.fromDir.META.way     := way_s3
      io.fromDir.META.hit     := Mux(s3_isRepl, false.B, hit_s3)
    }
    when(s3_isRepl) {
      io.fromDir.ReplRdResp     := !replRetry
      io.fromDir.ReplRdRetryAck := replRetry
      // victim PA only meaningful on a successful ReplRd (not retry)
      when(!replRetry) {
        io.fromDir.REPL.paddr := Cat(entry_s3.tag, s3_set, 0.U(offsetBits.W))
      }
    }.otherwise {
      io.fromDir.DirRdResp := true.B
    }
  }

  // one-hot route by TSHRID (== MSHR index)
  val s3_routeOH  = UIntToOH(s3_tshr,  paramL2.nMSHR)
  val arb_routeOH = UIntToOH(arb_tshr, paramL2.nMSHR)
  io.fromDirValid.zipWithIndex.foreach { case (v, i) =>
    v := (s3_valid && s3_routeOH(i)) || (arb_valid && arb_routeOH(i))
  }

  // ══════════════════════════════════════════════════════════════════
  // Assertions
  // ══════════════════════════════════════════════════════════════════
  when(reqVld) {
    assert(PopCount(Seq(req.ReplRd, req.DirRd, req.DirWb)) === 1.U,
      "Directory: valid request must assert exactly one op")
  }
  assert(!(isReplRd && io.replStall),
    "Directory: ReplRd issued while replStall asserted (arbiter contract)")
  when(s3_valid && s3_isDirRd) {
    assert(PopCount(hitVec) <= 1.U,
      "Directory: multiple ways hit (tag uniqueness violation)")
  }

  // ══════════════════════════════════════════════════════════════════
  // Perf
  // ══════════════════════════════════════════════════════════════════
  XSPerfAccumulate("dirRd_cnt",      isDirRd)
  XSPerfAccumulate("replRd_cnt",     isReplRd)
  XSPerfAccumulate("dirWb_cnt",      isDirWb)
  XSPerfAccumulate("replStall_cnt",  io.replStall)
  XSPerfAccumulate("replRetry_cnt",  s3_valid && s3_isRepl && replRetry)
  XSPerfAccumulate("dirRd_hit_cnt",  s3_valid && s3_isDirRd && hit_s3)
  XSPerfAccumulate("dirRd_miss_cnt", s3_valid && s3_isDirRd && !hit_s3)
  XSPerfAccumulate("plru_upd_cnt",   plruWen)
}
