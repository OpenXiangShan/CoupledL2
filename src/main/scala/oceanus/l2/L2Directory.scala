package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import utility.sram.SRAMTemplate
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.{Field, Parameters}

case object L2DirectoryBlockRefillFaultModeKey extends Field[Int](0)

object L2DirectoryBlockRefillFaultMode {
  val NeverUnlock = 2
}

object L2Directory {

  class BlockRefillEntry(implicit val p: Parameters) extends Bundle with HasL2Params {
    val locked = Bool()
    val dirWbDone = Bool()
    val owner = UInt(mshrIndexWidth.W)
  }

  object MetaState {

    def apply() = UInt(2.W)

    def UU: UInt = 3.U(2.W)
    def US: UInt = 2.U(2.W)
    def S: UInt = 1.U(2.W)
    def I: UInt = 0.U(2.W)

    def atLeastS(state: UInt): Bool = state >= MetaState.S
    def atLeastU(state: UInt): Bool = state >= MetaState.US

    def isClientU(state: UInt, client: Bool) = {
      Mux(state === UU, client, false.B)
    }

    def isClientS(state: UInt, client: Bool) = {
      Mux(state === US || state === S, client, false.B)
    }

    def toLocal(state: UInt): L2CacheLocalState = {
      val value = Wire(new L2CacheLocalState)
      value.bits := ParallelLookUp(state, Seq(
        (UU, L2CacheStates.U),
        (US, L2CacheStates.U),
        (S , L2CacheStates.S),
        (I , L2CacheStates.I)
      ))
      value
    }
  }

  // Meta: coherence state (NO tag — tag is internal to Directory)
  class Meta(implicit val p: Parameters) extends Bundle with HasL2Params {
    val state = MetaState()
    val dirty = Bool()
    val clients = Vec(1, Bool()) // TODO: parameterize with coherent l2 client count
    val alias = UInt(2.W) // TODO: parameterize with L2 alias width
  }

  class MetaReadResult(implicit override val p: Parameters) extends Meta with HasL2Params {
    val way = UInt(4.W) // TODO: parameterize with l2 way count
    val hit = Bool() // DirRd: tag hit. ReplRd: unused.
  }

  class MetaWriteMask(implicit val p: Parameters) extends Bundle with HasL2Params {
    val state = Bool()
    val dirty = Bool()
    val clients = Vec(1, Bool()) // TODO: parameterize with coherent l2 client count
    val alias = Bool()

    def any = this.asUInt.orR

    def maskAndWrite(dst: Meta, src: Meta): Unit = {
      when (state) { dst.state := src.state }
      when (dirty) { dst.dirty := src.dirty }
      when (alias) { dst.alias := src.alias }
      clients.zip(dst.clients.zip(src.clients)).foreach { case (mask, (dst, src)) => when (mask) { dst := src } }
    }

    def maskAndWrite(dst: Meta, dstFlag: MetaWriteMask, src: Meta): Unit = {
      maskAndWrite(dst, src)
      when (state) { dstFlag.state := true.B }
      when (dirty) { dstFlag.dirty := true.B }
      when (alias) { dstFlag.alias := true.B }
      clients.zip(dstFlag.clients).foreach { case (mask, dst) => when (mask) { dst := true.B } }
    }

    def unmaskAndWrite(dst: Meta, src: Meta): Unit = {
      when (!state) { dst.state := src.state }
      when (!dirty) { dst.dirty := src.dirty }
      when (!alias) { dst.alias := src.alias }
      clients.zip(dst.clients.zip(src.clients)).foreach { case (mask, (dst, src)) => when (!mask) { dst := src } }
    }
  }

  object MetaWriteMask {
    def empty(implicit p: Parameters): MetaWriteMask = {
      val zero = new MetaWriteMask
      zero.elements.foreach(_._2 := 0.U)
      zero
    }
  }

  class ReplReadResult(implicit override val p: Parameters) extends Bundle with HasL2Params {
    val paddr = UInt(paramL2.physicalAddrWidth.W) // victim PA; valid when META.state =/= I
    val way = UInt(4.W) // TODO: parameterize with l2 way count
  }

  class PathToDirectoryUOPs extends Bundle {
    val DirRd = Bool()
    val DirWb = Bool()
    val ReplRd = Bool()
  }

  class PathToDirectory(implicit val p: Parameters) extends PathToDirectoryUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val PADDR = UInt(paramL2.physicalAddrWidth.W)
    val WAY = UInt(4.W) // TODO: parameterize with l2 way count // only applicable for DirWb
    val META = new L2Directory.Meta // only applicable for DirWb
    val META_WEN = new L2Directory.MetaWriteMask // only applicable for DirWb
    val TAG_WEN = Bool()
  }

  class PathFromDirectoryUOPs extends Bundle {
    val DirRdArbComp = Bool()
    val DirRdResp = Bool()
    val DirWbArbComp = Bool()
    val ReplRdArbComp = Bool()
    val ReplRdResp = Bool()
    val ReplRdRetryAck = Bool()
  }

  class PathFromDirectory(implicit val p: Parameters) extends PathFromDirectoryUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val META = new L2Directory.MetaReadResult
    val REPL = new L2Directory.ReplReadResult // only applicable for ReplRd
  }

  // Internal SRAM entry: tag + meta together
  class DirEntry(implicit val p: Parameters) extends Bundle with HasL2Params {
    val tag = UInt(tagWidth.W)
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
    val next = Wire(Vec(nBits, Bool()))
    next := state.asTypeOf(Vec(nBits, Bool()))
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
// replStall (internal): any in-flight PLRU writer blocks new ReplRd
//   grants, closing the PLRU register S3-write/S1-read RAW window.

class Directory(implicit val p: Parameters) extends Module with HasL2Params {
  val MAX_AGE = 15.U(4.W)
  val idxW    = log2Ceil(nMSHR)
  private val enableDirStateProbe = p(L2SliceDirStateProbeEnableKey)
  private val enableDirPlruProbe = p(L2SliceDirPlruProbeEnableKey)
  private val blockRefillFaultMode = p(L2DirectoryBlockRefillFaultModeKey).U(2.W)

  val io = IO(new Bundle {
    val toDir   = Flipped(Vec(nMSHR, new L2Directory.PathToDirectory))
    val fromDir = Vec(nMSHR, new L2Directory.PathFromDirectory)
    val tshrIdle = Input(Vec(nMSHR, Bool()))
    val debugStateWrite = if (enableDirStateProbe) {
      Some(Output(new Bundle {
        val valid = Bool()
        val set = UInt(setBits.W)
        val way = UInt(wayBits.W)
        val state = L2Directory.MetaState()
        val dirty = Bool()
        val client0 = Bool()
      }))
    } else {
      None
    }
    val debugPlru = if (enableDirPlruProbe) {
      Some(Output(new Bundle {
        val valid = Bool()
        val set = UInt(setBits.W)
        val state = UInt(PLRU4.nBits.W)
        val s3Valid = Bool()
        val s3Tshr = UInt(mshrIndexWidth.W)
        val s3IsDirRd = Bool()
        val s3IsRepl = Bool()
        val wayS3 = UInt(wayBits.W)
        val s3Plru = UInt(PLRU4.nBits.W)
        val currentSetPlru = UInt(PLRU4.nBits.W)
      }))
    } else {
      None
    }
    val debugRetry = if (enableDirPlruProbe) {
      Some(Output(new Bundle {
        val replStall = Bool()
        val s2Valid = Bool()
        val s2Tshr = UInt(log2Ceil(paramL2.mshrSize).W)
        val s2IsDirRd = Bool()
        val s2IsRepl = Bool()
        val s2Set = UInt(setBits.W)
        val s3Valid = Bool()
        val s3Tshr = UInt(log2Ceil(paramL2.mshrSize).W)
        val s3IsDirRd = Bool()
        val s3IsRepl = Bool()
        val s3Set = UInt(setBits.W)
        val hasInvalid = Bool()
        val invalidWay = UInt(wayBits.W)
        val hitWay = UInt(wayBits.W)
        val plruWay = UInt(wayBits.W)
        val chosenWay = UInt(wayBits.W)
        val victimWay = UInt(wayBits.W)
        val blockRefillMaskS2 = UInt(paramL2.ways.W)
        val freeWayMaskS3 = UInt(paramL2.ways.W)
        val dirWbDone = Bool()
        val dirWbDoneSet = UInt(setBits.W)
        val dirWbDoneWay = UInt(wayBits.W)
        val blockRefillLocked = Vec(paramL2.sets, UInt(paramL2.ways.W))
        val ageWb = UInt(4.W)
        val ageRd = UInt(4.W)
        val rrReplRd = UInt(log2Ceil(paramL2.mshrSize).W)
        val rrDirWb = UInt(log2Ceil(paramL2.mshrSize).W)
        val rrDirRd = UInt(log2Ceil(paramL2.mshrSize).W)
        val plruRegs = Vec(paramL2.sets, UInt(PLRU4.nBits.W))
      }))
    } else {
      None
    }
  })

  val sets = paramL2.sets
  val ways = paramL2.ways
  require(ways == 4, "Directory uses PLRU4 hardwired for 4 ways")

  // ── SRAM + PLRU ───────────────────────────────────────────────────
  val dirArray = Module(new SRAMTemplate(
    gen = new L2Directory.DirEntry, set = sets, way = ways,
    singlePort = true, useBitmask = true, hasMbist = paramL2.hasMbist
  ))
  val plruRegs = RegInit(VecInit(Seq.fill(sets)(0.U(PLRU4.nBits.W))))
  val blockRefill = RegInit(VecInit(Seq.fill(sets)(
    VecInit(Seq.fill(ways)(0.U.asTypeOf(new L2Directory.BlockRefillEntry)))
  )))
  val blockRefillAge = RegInit(VecInit(Seq.fill(sets)(
    VecInit(Seq.fill(ways)(0.U(6.W)))
  )))
  val initActive = RegInit(true.B)
  val initSet = RegInit(0.U(setBits.W))
  val initMeta = Wire(new L2Directory.Meta)
  initMeta.state := L2Directory.MetaState.I
  initMeta.dirty := false.B
  initMeta.alias := 0.U
  initMeta.clients.foreach(_ := false.B)
  val initEntry = Wire(new L2Directory.DirEntry)
  initEntry.tag := 0.U
  initEntry.meta := initMeta
  val initMask = Wire(new L2Directory.DirEntry)
  initMask.tag := Fill(tagWidth, true.B)
  initMask.meta.state := Fill(2, true.B)
  initMask.meta.dirty := true.B
  initMask.meta.alias := Fill(2, true.B)
  initMask.meta.clients.foreach(_ := true.B)
  val initWayMask = Fill(ways, true.B)
  when(initActive) {
    when(initSet === (sets - 1).U) {
      initActive := false.B
    }.otherwise {
      initSet := initSet + 1.U
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // Internal arbiter
  // ══════════════════════════════════════════════════════════════════
  val replRdVec = VecInit(io.toDir.map(_.ReplRd))
  val dirWbVec  = VecInit(io.toDir.map(_.DirWb))
  val dirRdVec  = VecInit(io.toDir.map(_.DirRd))

  val rawAnyReplRd = replRdVec.asUInt.orR
  val rawAnyDirWb  = dirWbVec.asUInt.orR
  val rawAnyDirRd  = dirRdVec.asUInt.orR
  val anyReplRd = rawAnyReplRd && !initActive
  val anyDirWb  = rawAnyDirWb && !initActive
  val anyDirRd  = rawAnyDirRd && !initActive
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

  // ══════════════════════════════════════════════════════════════════
  // DirWb: bitmask partial write at S1
  // ══════════════════════════════════════════════════════════════════
  val wbEntry = Wire(new L2Directory.DirEntry)
  wbEntry.tag  := reqTag
  wbEntry.meta := winReq.META

  val wbMask = Wire(new L2Directory.DirEntry)
  wbMask.tag        := Fill(tagWidth, winReq.TAG_WEN)
  wbMask.meta.state := Fill(2, winReq.META_WEN.state)
  wbMask.meta.dirty := winReq.META_WEN.dirty
  wbMask.meta.alias := Fill(2, winReq.META_WEN.alias)
  wbMask.meta.clients.zip(winReq.META_WEN.clients).foreach { case (m, w) => m := w }

  val dirWriteValid = initActive || isDirWb
  val dirWriteEntry = Mux(initActive, initEntry, wbEntry)
  val dirWriteSet = Mux(initActive, initSet, reqSet)
  val dirWriteWayMask = Mux(initActive, initWayMask, UIntToOH(winReq.WAY(wayBits - 1, 0), ways))
  val dirWriteBitMask = Mux(initActive, initMask.asUInt, wbMask.asUInt)

  dirArray.io.w(
    valid   = dirWriteValid,
    data    = dirWriteEntry,
    setIdx  = dirWriteSet,
    waymask = dirWriteWayMask,
    bitmask = dirWriteBitMask
  )

  if (enableDirStateProbe) {
    io.debugStateWrite.get.valid := isDirWb
    io.debugStateWrite.get.set := reqSet
    io.debugStateWrite.get.way := winReq.WAY(wayBits - 1, 0)
    io.debugStateWrite.get.state := winReq.META.state
    io.debugStateWrite.get.dirty := winReq.META.dirty
    io.debugStateWrite.get.client0 := winReq.META.clients(0)
  }

  // DirWb completion is observed one cycle after grant, when the write has landed.
  val dirWbArb_v    = RegNext(grantDirWb, false.B)
  val dirWbArb_tshr = RegNext(winReq.TSHRID, 0.U)
  val dirWbDoneSet  = RegEnable(reqSet, 0.U, grantDirWb)
  val dirWbDoneWay  = RegEnable(winReq.WAY(wayBits - 1, 0), 0.U, grantDirWb)

  // ══════════════════════════════════════════════════════════════════
  // S3: hit detect, way select, PLRU update
  // ══════════════════════════════════════════════════════════════════
  val dirData_s3 = RegEnable(dirReadData_s2, 0.U.asTypeOf(dirReadData_s2), s2_valid)

  val tagMatch = VecInit(dirData_s3.map(_.tag === s3_tag))
  val validVec = VecInit(dirData_s3.map(_.meta.state =/= L2Directory.MetaState.I))
  val hitVec   = VecInit(tagMatch.zip(validVec).map { case (t, v) => t && v })
  val hit_s3   = hitVec.asUInt.orR
  val hitWay   = PriorityEncoder(hitVec)
  val blockRefillMask_s2 = Wire(UInt(ways.W))
  blockRefillMask_s2 := VecInit(blockRefill(s2_set).map(_.locked)).asUInt
  val freeWayMask_s2 = Wire(UInt(ways.W))
  freeWayMask_s2 := (~blockRefillMask_s2)(ways - 1, 0)
  val freeWayMask_s3 = RegInit(((BigInt(1) << ways) - 1).U(ways.W))
  when(s2_isRepl) {
    freeWayMask_s3 := freeWayMask_s2
  }

  // replStall: any in-flight PLRU writer blocks new ReplRd grant.
  replStall :=
    (s2_valid && s2_isRepl) ||
    (s3_valid && s3_isRepl) ||
    (s2_valid && s2_isDirRd) ||
    (s3_valid && s3_isDirRd && hit_s3)

  val invalidVec = VecInit(dirData_s3.map(_.meta.state === L2Directory.MetaState.I))
  val hasInvalid = invalidVec.asUInt.orR
  val invalidWay = PriorityEncoder(invalidVec)
  val plruWay    = PLRU4.getReplaceWay(s3_plru)
  val chosenWay  = Mux(hasInvalid, invalidWay, plruWay)
  val victimWay  = Mux(freeWayMask_s3(chosenWay), chosenWay, PriorityEncoder(freeWayMask_s3))
  val replRetry  = s3_valid && s3_isRepl && !freeWayMask_s3.orR

  val way_s3   = Mux(s3_isRepl, victimWay, Mux(hit_s3, hitWay, 0.U))
  val entry_s3 = dirData_s3(way_s3)
  val metaValid = (s3_isDirRd && hit_s3) || (s3_isRepl && !replRetry)

  val plruWen = (s3_valid && s3_isDirRd && hit_s3) ||
                (s3_valid && s3_isRepl && !replRetry)
  when(plruWen) {
    plruRegs(s3_set) := PLRU4.getNextState(s3_plru, way_s3)
  }

  when(s3_valid && s3_isRepl && !hit_s3) {
    blockRefill(s3_set)(way_s3).locked := true.B
    blockRefill(s3_set)(way_s3).dirWbDone := false.B
    blockRefill(s3_set)(way_s3).owner := s3_tshr
    blockRefillAge(s3_set)(way_s3) := 0.U
  }

  when(dirWbArb_v) {
    val entry = blockRefill(dirWbDoneSet)(dirWbDoneWay)
    when(entry.locked) {
      entry.dirWbDone := true.B
    }
  }

  for (setIdx <- 0 until sets) {
    for (wayIdx <- 0 until ways) {
      val entry = blockRefill(setIdx)(wayIdx)
      val age = blockRefillAge(setIdx)(wayIdx)
      when(entry.locked) {
        age := age + 1.U
        when(entry.dirWbDone &&
             blockRefillFaultMode =/= L2DirectoryBlockRefillFaultMode.NeverUnlock.U) {
          entry := 0.U.asTypeOf(new L2Directory.BlockRefillEntry)
          age := 0.U
        }
      }.otherwise {
        age := 0.U
      }
      assert(!(entry.locked && io.tshrIdle(entry.owner(idxW - 1, 0))),
        s"Directory: blockRefill leaked after owner MSHR retired at set=$setIdx way=$wayIdx")
    }
  }

  if (enableDirPlruProbe) {
    io.debugPlru.get.valid := plruWen
    io.debugPlru.get.set := s3_set
    io.debugPlru.get.state := Mux(plruWen, PLRU4.getNextState(s3_plru, way_s3), s3_plru)
    io.debugPlru.get.s3Valid := s3_valid
    io.debugPlru.get.s3Tshr := s3_tshr
    io.debugPlru.get.s3IsDirRd := s3_isDirRd
    io.debugPlru.get.s3IsRepl := s3_isRepl
    io.debugPlru.get.wayS3 := way_s3
    io.debugPlru.get.s3Plru := s3_plru
    io.debugPlru.get.currentSetPlru := plruRegs(s3_set)
    io.debugRetry.get.replStall := replStall
    io.debugRetry.get.s2Valid := s2_valid
    io.debugRetry.get.s2Tshr := s2_tshr
    io.debugRetry.get.s2IsDirRd := s2_isDirRd
    io.debugRetry.get.s2IsRepl := s2_isRepl
    io.debugRetry.get.s2Set := s2_set
    io.debugRetry.get.s3Valid := s3_valid
    io.debugRetry.get.s3Tshr := s3_tshr
    io.debugRetry.get.s3IsDirRd := s3_isDirRd
    io.debugRetry.get.s3IsRepl := s3_isRepl
    io.debugRetry.get.s3Set := s3_set
    io.debugRetry.get.hasInvalid := hasInvalid
    io.debugRetry.get.invalidWay := invalidWay
    io.debugRetry.get.hitWay := hitWay
    io.debugRetry.get.plruWay := plruWay
    io.debugRetry.get.chosenWay := chosenWay
    io.debugRetry.get.victimWay := victimWay
    io.debugRetry.get.blockRefillMaskS2 := blockRefillMask_s2
    io.debugRetry.get.freeWayMaskS3 := freeWayMask_s3
    io.debugRetry.get.dirWbDone := dirWbArb_v
    io.debugRetry.get.dirWbDoneSet := dirWbDoneSet
    io.debugRetry.get.dirWbDoneWay := dirWbDoneWay
    io.debugRetry.get.blockRefillLocked := VecInit(blockRefill.map(setLocks =>
      VecInit(setLocks.map(_.locked)).asUInt
    ))
    io.debugRetry.get.ageWb := ageWb
    io.debugRetry.get.ageRd := ageRd
    io.debugRetry.get.rrReplRd := rrReplRd
    io.debugRetry.get.rrDirWb := rrDirWb
    io.debugRetry.get.rrDirRd := rrDirRd
    io.debugRetry.get.plruRegs := plruRegs
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
      o.ReplRdResp := !replRetry
      o.ReplRdRetryAck := replRetry
      when(!replRetry) {
        o.REPL.paddr := Cat(entry_s3.tag, s3_set, 0.U(offsetBits.W))
        o.REPL.way := way_s3
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
  assert(PopCount(blockRefill.flatten.map(_.locked)) <= nMSHR.U,
    "Directory: blockRefill lock count exceeds TSHR count")
  when(initActive) {
    assert(!rawAnyReplRd && !rawAnyDirWb && !rawAnyDirRd,
      "Directory: functional traffic must be blocked while directory init is in progress")
  }
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
  XSPerfAccumulate("replRetry",     replRetry)
  XSPerfAccumulate("blockRefill_locked", PopCount(blockRefill.flatten.map(_.locked)))
  XSPerfAccumulate("dirRd_hit",     s3_valid && s3_isDirRd && hit_s3)
  XSPerfAccumulate("dirRd_miss",    s3_valid && s3_isDirRd && !hit_s3)
  XSPerfAccumulate("plru_upd",      plruWen)
  XSPerfAccumulate("dir_init_active", initActive)
  XSPerfAccumulate("starve_wb",     wbStarved)
  XSPerfAccumulate("starve_rd",     rdStarved)
}
