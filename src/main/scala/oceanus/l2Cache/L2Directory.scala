package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

// ══════════════════════════════════════════════════════════════════════
// L2Directory companion object
// ══════════════════════════════════════════════════════════════════════

object L2Directory {

  // ── Coherence state encoding ──────────────────────────────────────
  object MetaState {
    def apply() = UInt(2.W)
    def UU: UInt = 3.U(2.W)   // L2 Unique; L1 Unique
    def US: UInt = 2.U(2.W)   // L2 Unique; L1 Shared
    def S:  UInt = 1.U(2.W)   // L2 Shared; L1 may Shared
    def I:  UInt = 0.U(2.W)   // Invalid

    def isClientU(state: UInt, client: Bool) =
      Mux(state === UU, client, false.B)

    def isClientS(state: UInt, client: Bool) =
      Mux(state === US || state === S, client, false.B)

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

  // ── Directory entry stored in SRAM (tag + coherence state) ────────
  class DirEntry(implicit val p: Parameters) extends Bundle with HasL2Params {
    val tag     = UInt(paramL2.tagWidth.W)
    val state   = MetaState()
    val dirty   = Bool()
    val clients = UInt(1.W)   // TODO: parameterise with coherent client count
  }

  // ── Per-field write-enable for DirWb partial write ────────────────
  class DirEntryWen(implicit val p: Parameters) extends Bundle with HasL2Params {
    val tag     = Bool()
    val state   = Bool()
    val dirty   = Bool()
    val clients = Bool()
  }

  // ── Meta returned to TSHR (includes resolved way) ─────────────────
  class Meta(implicit val p: Parameters) extends Bundle with HasL2Params {
    val way     = UInt(paramL2.wayBits.W)
    val tag     = UInt(paramL2.tagWidth.W)
    val state   = MetaState()
    val dirty   = Bool()
    val clients = UInt(1.W)
  }

  // ── PathToDirectory ───────────────────────────────────────────────
  // Single request per cycle (already arbitrated upstream).
  // Exactly one of DirRd / DirWb / ReplRd is asserted when valid.
  class PathToDirectory(implicit val p: Parameters) extends Bundle with HasL2Params {
    val TSHRADDR = UInt(mshrIndexWidth.W)   // == MSHR index (0..nMSHR-1)
    val PADDR    = UInt(paramL2.physicalAddrWidth.W)
    val WAY      = UInt(paramL2.wayBits.W)  // DirWb: which way to update
    val META     = new DirEntry             // DirWb: write data; ignored on reads
    val META_WEN = new DirEntryWen          // DirWb: per-field write-enable

    val DirRd  = Bool()  // normal read: hit/miss check
    val DirWb  = Bool()  // partial write: update META fields for WAY
    val ReplRd = Bool()  // refill read: selects victim way
  }

  // ── PathFromDirectory ─────────────────────────────────────────────
  // Single data bus broadcast to all TSHRs; routed by a one-hot valid
  // vector (fromDirValid) indexed by TSHRADDR.
  class PathFromDirectory(implicit val p: Parameters) extends Bundle with HasL2Params {
    val TSHRADDR = UInt(mshrIndexWidth.W)
    val META     = new Meta  // valid when DirRdResp or ReplRdResp

    val DirRdResp    = Bool()  // S3: DirRd result ready; see DirRdHit
    val DirRdHit     = Bool()  // S3: hit/miss flag accompanying DirRdResp

    val ReplRdResp     = Bool()  // S3: victim META valid; proceed with refill
    val ReplRdRetryAck = Bool()  // S3: retry needed; re-arm ReplRd
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
  val nBits = 3  // ways - 1

  // Victim way from current PLRU state (root → leaf walk).
  def getReplaceWay(state: UInt): UInt = {
    val way = WireDefault(0.U(2.W))
    when(state(0) === 0.U) {
      // victim in left subtree (node[1]): 0 → w0, 1 → w1
      way := Mux(state(1) === 0.U, 0.U, 1.U)
    }.otherwise {
      // victim in right subtree (node[2]): 0 → w2, 1 → w3
      way := Mux(state(2) === 0.U, 2.U, 3.U)
    }
    way
  }

  // Next PLRU state after accessing touchWay.
  // way bit layout: touchWay(1) = subtree (0=left,1=right),
  //                 touchWay(0) = leaf within subtree (0=first,1=second).
  // Flip each node on the path to point away from touchWay.
  def getNextState(state: UInt, touchWay: UInt): UInt = {
    val next = WireInit(VecInit(state.asBools))  // keep all bits by default
    val inLeftSubtree = touchWay(1) === 0.U
    val touchedFirst  = touchWay(0) === 0.U

    // node[0]: accessed left → point right (1); accessed right → point left (0)
    next(0) := Mux(inLeftSubtree, true.B, false.B)

    when(inLeftSubtree) {
      // node[1]: accessed w0 → point to w1 (1); accessed w1 → point to w0 (0)
      next(1) := Mux(touchedFirst, true.B, false.B)
    }.otherwise {
      // node[2]: accessed w2 → point to w3 (1); accessed w3 → point to w2 (0)
      next(2) := Mux(touchedFirst, true.B, false.B)
    }
    next.asUInt
  }
}

// ══════════════════════════════════════════════════════════════════════
// Directory module
// ══════════════════════════════════════════════════════════════════════
//
// Requests are arbitrated UPSTREAM: at most one request enters per cycle,
// with exactly one operation type asserted.  No internal arbitration.
//
// Pipeline: S1 (decode + SRAM/PLRU read issued)
//        →  S2 (SRAM data available; latch to S3)
//        →  S3 (tag compare, way select, PLRU update, respond)
//
// Single-port dirArray: a DirWb write and a read never collide because the
// upstream arbiter never issues both in the same cycle.
//
// PLRU stored as registers (sets × 3 bits): combinational read at S1,
// sequential write at S3.  The S3-write / S1-read RAW hazard (2-cycle
// window) is handled by `replStall` (output to the upstream arbiter):
// the arbiter must not issue a new ReplRd while a refill is in flight.
//
// No wayLock:
//   blockRefill replaced by replStall (above).
//   dirHit      replaced by dirInFlight retry (ReplRdRetryAck).
//   Same-PA read protection is done upstream in TSHRAlloc.

class Directory(implicit p: Parameters) extends Module with HasL2Params {

  val io = IO(new Bundle {
    // Single, already-arbitrated request.
    val toDir = Flipped(Valid(new L2Directory.PathToDirectory))

    // Response: single data bus + one-hot valid routed by TSHRADDR.
    val fromDir      = Output(new L2Directory.PathFromDirectory)
    val fromDirValid = Output(Vec(paramL2.nMSHR, Bool()))

    // Hazard hint to the upstream arbiter: do not issue ReplRd this cycle.
    val replStall = Output(Bool())
  })

  val sets = paramL2.sets
  val ways = paramL2.ways
  require(ways == 4, "This Directory uses PLRU4 which is hardwired for 4 ways")

  // ── Combined tag+meta SRAM ────────────────────────────────────────
  val dirArray = Module(new SRAMTemplate(
    gen        = new L2Directory.DirEntry,
    set        = sets,
    way        = ways,
    singlePort = true,
    useBitmask = true,
    hasMbist   = paramL2.hasMbist
  ))

  // ── PLRU register file: sets × 3 bits ─────────────────────────────
  val plruRegs = RegInit(VecInit(Seq.fill(sets)(0.U(PLRU4.nBits.W))))

  // ══════════════════════════════════════════════════════════════════
  // S1: decode (no arbitration)
  // ══════════════════════════════════════════════════════════════════

  val req      = io.toDir.bits
  val reqVld   = io.toDir.valid
  val isReplRd = reqVld && req.ReplRd
  val isDirRd  = reqVld && req.DirRd
  val isDirWb  = reqVld && req.DirWb
  val isRead   = isReplRd || isDirRd

  // Address decode from PADDR
  val offsetBits = paramL2.offsetBits
  val reqSet = req.PADDR(offsetBits + paramL2.setBits - 1, offsetBits)
  val reqTag = req.PADDR(paramL2.physicalAddrWidth - 1,
                         offsetBits + paramL2.setBits)

  // ── SRAM read issued at S1 (reads only) ───────────────────────────
  val dirReadData_s2 = dirArray.io.r(isRead, reqSet).resp.data

  // ── PLRU combinational read at S1 ─────────────────────────────────
  val plruState_s1 = plruRegs(reqSet)

  // ══════════════════════════════════════════════════════════════════
  // S1 → S2 pipeline registers
  // ══════════════════════════════════════════════════════════════════

  val s2_valid    = RegNext(isRead,        false.B)
  val s2_isRepl   = RegNext(isReplRd,      false.B)
  val s2_isDirRd  = RegNext(isDirRd,       false.B)
  val s2_tshr     = RegNext(req.TSHRADDR,  0.U)
  val s2_set      = RegNext(reqSet,        0.U)
  val s2_tag      = RegNext(reqTag,        0.U)
  val s2_plru     = RegNext(plruState_s1,  0.U)

  // ══════════════════════════════════════════════════════════════════
  // S2 → S3 pipeline registers
  // ══════════════════════════════════════════════════════════════════

  val s3_valid    = RegNext(s2_valid,    false.B)
  val s3_isRepl   = RegNext(s2_isRepl,   false.B)
  val s3_isDirRd  = RegNext(s2_isDirRd,  false.B)
  val s3_tshr     = RegNext(s2_tshr,     0.U)
  val s3_set      = RegNext(s2_set,      0.U)
  val s3_tag      = RegNext(s2_tag,      0.U)
  val s3_plru     = RegNext(s2_plru,     0.U)

  // ── replStall: prevent the S3-write / S1-read PLRU RAW hazard ─────
  // Output to the upstream arbiter; it must not issue a ReplRd while a
  // refill is in flight at S2 or S3.  Conservative: no set comparison.
  io.replStall := (s2_valid && s2_isRepl) || (s3_valid && s3_isRepl)

  // ══════════════════════════════════════════════════════════════════
  // DirWb: partial write at S1 (no pipeline stage)
  // ══════════════════════════════════════════════════════════════════

  // Expand per-field write-enable into a bit-level bitmask for SRAMTemplate.
  val dirWbBitmask = Wire(new L2Directory.DirEntry)
  dirWbBitmask.tag     := Fill(paramL2.tagWidth, req.META_WEN.tag)
  dirWbBitmask.state   := Fill(2,                req.META_WEN.state)
  dirWbBitmask.dirty   := req.META_WEN.dirty
  dirWbBitmask.clients := req.META_WEN.clients  // 1-bit field

  dirArray.io.w(
    valid   = isDirWb,
    data    = req.META,
    setIdx  = reqSet,
    waymask = UIntToOH(req.WAY),
    bitmask = dirWbBitmask.asUInt
  )

  // ══════════════════════════════════════════════════════════════════
  // S3: tag compare, way selection, PLRU update
  // ══════════════════════════════════════════════════════════════════

  val dirData_s3 = RegEnable(dirReadData_s2, 0.U.asTypeOf(dirReadData_s2), s2_valid)

  // Tag match and state-valid per way
  val tagMatchVec = VecInit(dirData_s3.map(_.tag === s3_tag))
  val validVec    = VecInit(dirData_s3.map(_.state =/= L2Directory.MetaState.I))
  val hitVec      = VecInit(tagMatchVec.zip(validVec).map { case (t, v) => t && v })
  val hit_s3      = hitVec.asUInt.orR
  val hitWay_s3   = PriorityEncoder(hitVec)

  // dirInFlight: a DirRd for the same set is in S2 or S3.  Its hit way is
  // not yet reflected in PLRU, so an in-flight ReplRd cannot safely pick
  // a victim → retry.
  // NOTE: compares against reqSet (S1 combinational).  If this path is
  //   timing-critical at 3 GHz, switch to the conservative variant:
  //     dirInFlight = (s2_valid && s2_isDirRd) || (s3_valid && s3_isDirRd)
  //   (no set comparison).
  val dirInFlight =
    (s2_valid && s2_isDirRd && (s2_set === reqSet)) ||
    (s3_valid && s3_isDirRd && (s3_set === reqSet))

  // Invalid way has priority (no eviction needed), else PLRU recommendation.
  val invalidVec    = VecInit(dirData_s3.map(_.state === L2Directory.MetaState.I))
  val hasInvalidWay = invalidVec.asUInt.orR
  val invalidWay    = PriorityEncoder(invalidVec)
  val plruWay       = PLRU4.getReplaceWay(s3_plru)
  val victimWay     = Mux(hasInvalidWay, invalidWay, plruWay)

  val replRetry = dirInFlight

  // Resolved way:
  //   ReplRd     → victimWay
  //   DirRd hit  → hitWay_s3
  //   DirRd miss → 0 (TSHR uses DirRdHit=false to detect miss)
  val way_s3   = Mux(s3_isRepl, victimWay,
                 Mux(hit_s3,    hitWay_s3, 0.U))
  val entry_s3 = dirData_s3(way_s3)

  // META is only meaningful on a DirRd hit or a successful ReplRd.
  val metaValid_s3 = (s3_isDirRd && hit_s3) || (s3_isRepl && !replRetry)

  // ── PLRU update at S3 ─────────────────────────────────────────────
  // Promote on DirRd hit; insert on successful ReplRd.  Use the actual
  // selected way so fallback selections are reflected in the tree.
  val plruWen = (s3_valid && s3_isDirRd && hit_s3) ||
                (s3_valid && s3_isRepl  && !replRetry)
  when(plruWen) {
    plruRegs(s3_set) := PLRU4.getNextState(s3_plru, way_s3)
  }

  // ══════════════════════════════════════════════════════════════════
  // Response: single data bus + one-hot valid
  // ══════════════════════════════════════════════════════════════════

  // Default data bus
  io.fromDir.TSHRADDR       := s3_tshr
  io.fromDir.META           := 0.U.asTypeOf(new L2Directory.Meta)
  io.fromDir.DirRdResp      := false.B
  io.fromDir.DirRdHit       := false.B
  io.fromDir.ReplRdResp     := false.B
  io.fromDir.ReplRdRetryAck := false.B

  when(s3_valid) {
    when(metaValid_s3) {
      io.fromDir.META.way     := way_s3
      io.fromDir.META.tag     := entry_s3.tag
      io.fromDir.META.state   := entry_s3.state
      io.fromDir.META.dirty   := entry_s3.dirty
      io.fromDir.META.clients := entry_s3.clients
    }
    when(s3_isRepl) {
      io.fromDir.ReplRdResp     := !replRetry
      io.fromDir.ReplRdRetryAck := replRetry
    }.otherwise {
      io.fromDir.DirRdResp := true.B
      io.fromDir.DirRdHit  := hit_s3
    }
  }

  // One-hot valid: route the response to the originating TSHR.
  val routeOH = UIntToOH(s3_tshr, paramL2.nMSHR)
  io.fromDirValid.zipWithIndex.foreach { case (v, i) =>
    v := s3_valid && routeOH(i)
  }

  // ══════════════════════════════════════════════════════════════════
  // Assertions
  // ══════════════════════════════════════════════════════════════════

  // At most one operation type asserted when valid (arbiter invariant)
  when(reqVld) {
    assert(PopCount(Seq(req.ReplRd, req.DirRd, req.DirWb)) === 1.U,
      "Directory: a valid request must assert exactly one operation type")
  }

  // Upstream arbiter must honour replStall: no ReplRd while one is in flight
  assert(!(isReplRd && io.replStall),
    "Directory: ReplRd issued while replStall asserted (arbiter contract violated)")

  // At most one way hits per DirRd (tag uniqueness invariant)
  when(s3_valid && s3_isDirRd) {
    assert(PopCount(hitVec) <= 1.U,
      "Directory: multiple ways hit for same address (tag uniqueness violation)")
  }

  // ══════════════════════════════════════════════════════════════════
  // Performance counters
  // ══════════════════════════════════════════════════════════════════

  XSPerfAccumulate("dirRd_cnt",          isDirRd)
  XSPerfAccumulate("replRd_cnt",         isReplRd)
  XSPerfAccumulate("dirWb_cnt",          isDirWb)
  XSPerfAccumulate("replStall_cnt",      io.replStall)
  XSPerfAccumulate("replRetry_cnt",      s3_valid && s3_isRepl && replRetry)
  XSPerfAccumulate("dirRd_hit_cnt",      s3_valid && s3_isDirRd && hit_s3)
  XSPerfAccumulate("dirRd_miss_cnt",     s3_valid && s3_isDirRd && !hit_s3)
  XSPerfAccumulate("plruUpdate_cnt",     plruWen)
}
