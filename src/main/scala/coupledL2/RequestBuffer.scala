package coupledL2

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import chisel3._
import chisel3.util._
import coupledL2.utils._
import utility._

class ReqEntry(entries: Int = 4)(implicit p: Parameters) extends L2Bundle() {
  val valid    = Bool()
  val rdy      = Bool()
  val task     = new TaskBundle()

  /* blocked by MainPipe
  * [3] by stage1, a same-set entry just fired
  * [2] by stage2
  * [1] by stage3
  * [0] block release flag
  */
  val waitMP  = UInt(4.W)

  /* which MSHR the entry is waiting for
  * We need to compare req.tag AND dirResult.tag (if replacement)
  *
  * There are three occasions that we need to update it
  * (1) when new MSHR is allocated
  * (2) when MSHR finishes replacement(TODO)
  * (3) when MSHR is free
  * */
  val waitMS  = UInt(mshrsAll.W)

  /* buffer_dep_mask[i][j] => entry i should wait entry j
  *   this is used to make sure that same set requests will be sent
  *   to MSHR in order
  */
  val depMask = Vec(entries, Bool())

  /* ways in the set that are occupied by unfinished MSHR task */
  val occWays = UInt(cacheParams.ways.W)
}

class ChosenQBundle(idWIdth: Int = 2)(implicit p: Parameters) extends L2Bundle {
  val bits = new ReqEntry()
  val id = UInt(idWIdth.W)
}

class RequestBuffer(flow: Boolean = true, entries: Int = 4)(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val in          = Flipped(DecoupledIO(new TaskBundle))
    val out         = DecoupledIO(new TaskBundle)
    val mshrStatus  = Vec(mshrsAll, Flipped(ValidIO(new MSHRBlockAInfo)))
    val mainPipeBlock = Input(Vec(2, Bool()))

    val ATag        = Output(UInt(tagBits.W))
    val ASet        = Output(UInt(setBits.W))

    // when Probe enters MainPipe, we need also to block
    val probeEntrance = Flipped(ValidIO(new L2Bundle {
      val tag = UInt(tagBits.W)
      val set = UInt(setBits.W)
    }))
  })

  /* ======== Data Structure ======== */

  io.ATag := io.in.bits.tag
  io.ASet := io.in.bits.set

  val buffer = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new ReqEntry))))
  val issueArb = Module(new FastArbiter(new ReqEntry, entries))
  val chosenQ = Module(new Queue(new ChosenQBundle(log2Ceil(entries)), entries = 1, pipe = true, flow = false))
  val chosenQValid = chosenQ.io.deq.valid

  /* ======== Enchantment ======== */
  val NWay = cacheParams.ways
  // count conflict
  def sameAddr(a: TaskBundle, b: TaskBundle):     Bool = Cat(a.tag, a.set) === Cat(b.tag, b.set)
  def sameSet (a: TaskBundle, b: TaskBundle):     Bool = a.set === b.set
  def sameSet (a: TaskBundle, b: MSHRBlockAInfo): Bool = a.set === b.set
  def addrConflict(a: TaskBundle, s: MSHRBlockAInfo): Bool = {
    a.set === s.set && (a.tag === s.reqTag || a.tag === s.metaTag && s.needRelease)
  }
  def conflictMask(a: TaskBundle): UInt = VecInit(io.mshrStatus.map(s =>
    s.valid && addrConflict(a, s.bits) && !s.bits.willFree)).asUInt
  def conflict(a: TaskBundle): Bool = conflictMask(a).orR

  // count ways
  def countWaysOH(cond: (MSHRBlockAInfo => Bool)): UInt = {
    VecInit(io.mshrStatus.map(s =>
      Mux(
        s.valid && cond(s.bits),
        UIntToOH(s.bits.way, NWay),
        0.U(NWay.W)
      )
    )).reduceTree(_ | _)
  }
  def occWays     (a: TaskBundle): UInt = countWaysOH(s => !s.willFree && sameSet(a, s))
  def willFreeWays(a: TaskBundle): UInt = countWaysOH(s =>  s.willFree && sameSet(a, s))

  def noFreeWay(a: TaskBundle): Bool = !Cat(~occWays(a)).orR
  def noFreeWay(occWays: UInt): Bool = !Cat(~occWays).orR

  // other flags
  val in      = io.in.bits
  val full    = Cat(buffer.map(_.valid)).andR
  val canFlow = flow.B && !conflict(in) && !chosenQValid && !Cat(io.mainPipeBlock).orR && !noFreeWay(in)
  val doFlow  = canFlow && io.out.ready

  // TODO: remove depMatrix cuz not important
  val depMask    = buffer.map(e => e.valid && sameAddr(io.in.bits, e.task))
  val isPrefetch = io.in.bits.fromA && io.in.bits.opcode === Hint
  val dup        = io.in.valid && isPrefetch && Cat(depMask).orR // duplicate prefetch

  /* ======== Alloc ======== */
  io.in.ready   := !full || doFlow

  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val alloc = !full && io.in.valid && !doFlow && !dup
  when(alloc){
    val entry = buffer(insertIdx)
    val mpBlock = Cat(io.mainPipeBlock).orR
    val pipeBlockOut = io.out.valid && sameSet(in, io.out.bits)
    val probeBlock   = io.probeEntrance.valid && io.probeEntrance.bits.set === in.set
    val s1Block      = pipeBlockOut || probeBlock

    entry.valid   := true.B
    // when Addr-Conflict / Same-Addr-Dependent / MainPipe-Block / noFreeWay-in-Set, entry not ready
    entry.rdy     := !conflict(in) && !Cat(depMask).orR && !mpBlock && !noFreeWay(in) && !s1Block
    entry.task    := io.in.bits
    entry.waitMP  := Cat(
      s1Block,
      io.mainPipeBlock(0),
      io.mainPipeBlock(1),
      0.U(1.W))
    entry.waitMS  := conflictMask(in)
    entry.occWays := Mux(mpBlock, 0.U, occWays(in))

    entry.depMask := depMask //TODO
    assert(PopCount(conflictMask(in)) <= 2.U)
  }

  /* ======== chosenQ enq ======== */
  chosenQ.io.enq.valid := issueArb.io.out.valid
  chosenQ.io.enq.bits.bits := issueArb.io.out.bits
  chosenQ.io.enq.bits.id := issueArb.io.chosen
  issueArb.io.out.ready := chosenQ.io.enq.ready

  // once fired at issueArb, it is ok to enter MainPipe without conflict
  // however, it may be blocked for other reasons such as high-prior reqs or MSHRFull
  // in such case, we need a place to save it

  for (i <- 0 until entries) {
    issueArb.io.in(i).valid := buffer(i).valid && buffer(i).rdy
    issueArb.io.in(i).bits  := buffer(i)
  }

  //TODO: if i use occWays when update,
  // does this mean that every entry has occWays logic?
  // !TODO: do it for now, later consider using Queue2

  /* ======== Update rdy and masks ======== */
  for (e <- buffer) {
    when(e.valid) {
      val waitMSUpdate  = WireInit(e.waitMS)
      val depMaskUpdate = WireInit(e.depMask)
      val occWaysUpdate = WireInit(e.occWays)

      // when mshr will_free, clear it in other reqs' waitMS and occWays
      val willFreeMask = VecInit(io.mshrStatus.map(s => s.valid && s.bits.willFree)).asUInt
      waitMSUpdate  := e.waitMS  & (~willFreeMask).asUInt
      occWaysUpdate := e.occWays & (~willFreeWays(e.task)).asUInt

      // Initially,
      //    waitMP(2) = s2 blocking, wait 2 cycles
      //    waitMP(1) = s3 blocking, wait 1 cycle
      // Now that we shift right waitMP every cycle
      //    so when waitMP(1) is 0 and waitMP(0) is 1, desired cycleCnt reached
      //    we recalculate waitMS and occWays, overriding old mask
      //    to take new allocated MSHR into account
      e.waitMP := e.waitMP >> 1.U
      when(e.waitMP(1) === 0.U && e.waitMP(0) === 1.U) {
        waitMSUpdate  := conflictMask(e.task)
        occWaysUpdate := occWays(e.task)
      }

      // when request is sent, clear it in other reqs' depMask
      when(io.out.fire && !canFlow) {
        depMaskUpdate(chosenQ.io.deq.bits.id) := false.B
      }
      // if io.out is the same set, we also need to set waitMP
      when(
        io.out.fire && sameSet(e.task, io.out.bits) || //TODO: maybe io.out.valid is sufficient, like when(alloc)
        io.probeEntrance.valid && io.probeEntrance.bits.set === e.task.set // wait for same-set probe to enter MSHR
      ) {
        e.waitMP := e.waitMP | "b1000".U
      }

      // update info
      val pipeBlockOut = io.out.valid && sameSet(e.task, io.out.bits)
      e.waitMS  := waitMSUpdate
      e.depMask := depMaskUpdate
      e.occWays := occWaysUpdate
      e.rdy     := !waitMSUpdate.orR && !Cat(depMaskUpdate).orR && !e.waitMP && !noFreeWay(occWaysUpdate) && !pipeBlockOut
    }
  }

  /* ======== Output ======== */
  // when entry.rdy is no longer true,
  // we cancel chosenQ req, but the entry is still held in buffer to issue later
  val cancel = (canFlow && sameSet(chosenQ.io.deq.bits.bits.task, io.in.bits)) || !buffer(chosenQ.io.deq.bits.id).rdy

  chosenQ.io.deq.ready := io.out.ready || cancel
  io.out.valid := chosenQValid && !cancel || io.in.valid && canFlow
  io.out.bits  := Mux(canFlow, io.in.bits, chosenQ.io.deq.bits.bits.task)

  when(chosenQ.io.deq.fire && !cancel) {
    buffer(chosenQ.io.deq.bits.id).valid := false.B
  }

  // for Dir to choose a way not occupied by some unfinished MSHR task
  io.out.bits.wayMask := Mux(canFlow, ~occWays(io.in.bits), ~chosenQ.io.deq.bits.bits.occWays)

  // add XSPerf to see how many cycles the req is held in Buffer
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(entries)(0.U(16.W))))
    buffer zip bufferTimer map {
      case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid) && !e.valid) { t := 0.U }
        assert(t < 10000.U, "ReqBuf Leak")

        val enable = RegNext(e.valid) && !e.valid
        XSPerfHistogram(cacheParams, "reqBuf_timer", t, enable, 0, 400, 20)
        XSPerfMax(cacheParams, "max_reqBuf_timer", t, enable)
    }
  }
}
