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
//  val depMask = Vec(entries, Bool())

}

class ChosenQBundle(idWIdth: Int = 2)(implicit p: Parameters) extends L2Bundle {
  val bits = new ReqEntry()
  val id = UInt(idWIdth.W)
}

class AMergeTask(implicit p: Parameters) extends L2Bundle {
  val id = UInt(mshrBits.W)
  val task = new TaskBundle()
}

class RequestBuffer(flow: Boolean = true, entries: Int = 4)(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val in          = Flipped(DecoupledIO(new TaskBundle))
    val out         = DecoupledIO(new TaskBundle)
    val mshrInfo  = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
    val aMergeTask = ValidIO(new AMergeTask)
    val mainPipeBlock = Input(Vec(2, Bool()))

    val ATag        = Output(UInt(tagBits.W))
    val ASet        = Output(UInt(setBits.W))

    // when Probe/Release/MSHR enters MainPipe, we need also to block A req
    val s1Entrance = Flipped(ValidIO(new L2Bundle {
      val set = UInt(setBits.W)
    }))

    val hasLatePF = Output(Bool())
    val hasMergeA = Output(Bool())
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
  def sameAddr(a: TaskBundle, b: MSHRInfo): Bool = Cat(a.tag, a.set) === Cat(b.reqTag, b.set)
  def sameSet (a: TaskBundle, b: TaskBundle):     Bool = a.set === b.set
  def sameSet (a: TaskBundle, b: MSHRInfo): Bool = a.set === b.set
  def addrConflict(a: TaskBundle, s: MSHRInfo): Bool = {
    a.set === s.set && (a.tag === s.reqTag || a.tag === s.metaTag && s.needRelease)
  }
  def conflictMask(a: TaskBundle): UInt = VecInit(io.mshrInfo.map(s =>
    s.valid && addrConflict(a, s.bits) && !s.bits.willFree)).asUInt
  def conflict(a: TaskBundle): Bool = conflictMask(a).orR

  def latePrefetch(a: TaskBundle): Bool = VecInit(io.mshrInfo.map(s =>
    s.valid && s.bits.isPrefetch && sameAddr(a, s.bits) && !s.bits.willFree &&
    a.fromA && (a.opcode === AcquireBlock || a.opcode === AcquirePerm)
  )).asUInt.orR
  def mergeA_latepf(a: TaskBundle): Bool = VecInit(io.mshrInfo.map(s =>
    s.valid && s.bits.isPrefetch && sameAddr(a, s.bits) && !s.bits.willFree && !s.bits.dirHit && !s.bits.s_refill &&
    a.fromA && (a.opcode === AcquireBlock || a.opcode === AcquirePerm) && !s.bits.mergeA
  )).asUInt.orR

  // count ways
//  def countWaysOH(cond: (MSHRInfo => Bool)): UInt = {
//    VecInit(io.mshrInfo.map(s =>
//      Mux(
//        s.valid && cond(s.bits),
//        UIntToOH(s.bits.way, NWay),
//        0.U(NWay.W)
//      )
//    )).reduceTree(_ | _)
//  }

  // other flags
  val in      = io.in.bits
  val full    = Cat(buffer.map(_.valid)).andR

  // incoming Acquire can be merged with late_pf MSHR block
  val mergeAMask = VecInit(io.mshrInfo.map(s =>
    s.valid && s.bits.isPrefetch && sameAddr(in, s.bits) && !s.bits.willFree && !s.bits.dirHit && !s.bits.s_refill &&
      in.fromA && (in.opcode === AcquireBlock || in.opcode === AcquirePerm) && !s.bits.mergeA
  )).asUInt
  val mergeA = mergeAMask.orR
  val mergeAId = OHToUInt(mergeAMask)
  io.aMergeTask.valid := io.in.valid && mergeA
  io.aMergeTask.bits.id := mergeAId
  io.aMergeTask.bits.task := in

  // flow not allowed when full, or entries might starve
  val canFlow = flow.B && !full && !conflict(in) && !chosenQValid && !Cat(io.mainPipeBlock).orR
  val doFlow  = canFlow && io.out.ready
  io.hasLatePF := latePrefetch(in) && io.in.valid && !sameAddr(in, RegNext(in))
  io.hasMergeA := mergeA_latepf(in) && io.in.valid && !sameAddr(in, RegNext(in))

  //  val depMask    = buffer.map(e => e.valid && sameAddr(io.in.bits, e.task))
  // remove duplicate prefetch if same-addr A req in MSHR or ReqBuf
  val isPrefetch = in.fromA && in.opcode === Hint
  val dupMask    = VecInit(
    io.mshrInfo.map(s =>
      s.valid && s.bits.isAcqOrPrefetch && sameAddr(in, s.bits)) ++
    buffer.map(e =>
      e.valid && sameAddr(in, e.task)
    )
  ).asUInt
  val dup        = io.in.valid && isPrefetch && dupMask.orR

  //!! TODO: we can also remove those that duplicate with mainPipe

  /* ======== Alloc ======== */
  io.in.ready   := !full || doFlow || mergeA

  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val alloc = !full && io.in.valid && !doFlow && !dup && !mergeA
  when(alloc){
    val entry = buffer(insertIdx)
    val mpBlock = Cat(io.mainPipeBlock).orR
    val pipeBlockOut = io.out.fire && sameSet(in, io.out.bits)
    val probeBlock   = io.s1Entrance.valid && io.s1Entrance.bits.set === in.set // wait for same-addr req to enter MSHR
    val s1Block      = pipeBlockOut || probeBlock

    entry.valid   := true.B
    // when Addr-Conflict / Same-Addr-Dependent / MainPipe-Block / noFreeWay-in-Set, entry not ready
    entry.rdy     := !conflict(in) && !mpBlock && !s1Block // && !Cat(depMask).orR
    entry.task    := io.in.bits
    entry.waitMP  := Cat(
      s1Block,
      io.mainPipeBlock(0),
      io.mainPipeBlock(1),
      0.U(1.W))
    entry.waitMS  := conflictMask(in)

//    entry.depMask := depMask
    assert(PopCount(conflictMask(in)) <= 2.U)
  }

  /* ======== Issue ======== */
  issueArb.io.in zip buffer foreach {
    case(in, e) =>
      // when io.out.valid, we temporarily stall all entries of the same set
      val pipeBlockOut = io.out.valid && sameSet(e.task, io.out.bits)

      in.valid := e.valid && e.rdy && !pipeBlockOut
      in.bits  := e
  }

  /* ======== chosenQ enq ======== */
  // once fired at issueArb, it is ok to enter MainPipe without conflict
  // however, it may be blocked for other reasons such as high-prior reqs or MSHRFull
  // in such case, we need a place to save it
  chosenQ.io.enq.valid := issueArb.io.out.valid
  chosenQ.io.enq.bits.bits := issueArb.io.out.bits
  chosenQ.io.enq.bits.id := issueArb.io.chosen
  issueArb.io.out.ready := chosenQ.io.enq.ready

  /* ======== Update rdy and masks ======== */
  for (e <- buffer) {
    when(e.valid) {
      val waitMSUpdate  = WireInit(e.waitMS)
//      val depMaskUpdate = WireInit(e.depMask)

      // when mshr will_free, clear it in other reqs' waitMS
      val willFreeMask = VecInit(io.mshrInfo.map(s => s.valid && s.bits.willFree)).asUInt
      waitMSUpdate  := e.waitMS  & (~willFreeMask).asUInt

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
      }

      // when request is sent, clear it in other reqs' depMask
//      when(io.out.fire && !canFlow) {
//        depMaskUpdate(chosenQ.io.deq.bits.id) := false.B
//      }

      // set waitMP if fired-s1-req is the same set
      val s1A_Block = io.out.fire && sameSet(e.task, io.out.bits)
      val s1B_Block = io.s1Entrance.valid && io.s1Entrance.bits.set === e.task.set
      val s1_Block  = s1A_Block || s1B_Block
      when(s1_Block) {
        e.waitMP := e.waitMP | "b0100".U // fired-req at s2 next cycle
      }

      // update info
      e.waitMS  := waitMSUpdate
//      e.depMask := depMaskUpdate
      e.rdy     := !waitMSUpdate.orR && !e.waitMP && !s1_Block
    }
  }

  /* ======== Output ======== */
  // when entry.rdy is no longer true,
  // we cancel req in chosenQ, with the entry still held in buffer to issue later
//  val cancel = (canFlow && sameSet(chosenQ.io.deq.bits.bits.task, io.in.bits)) || !buffer(chosenQ.io.deq.bits.id).rdy
  val cancel = !buffer(chosenQ.io.deq.bits.id).rdy

  chosenQ.io.deq.ready := io.out.ready || cancel
  io.out.valid := chosenQValid && !cancel || io.in.valid && canFlow
  io.out.bits  := Mux(canFlow, io.in.bits, chosenQ.io.deq.bits.bits.task)

  when(chosenQ.io.deq.fire && !cancel) {
    buffer(chosenQ.io.deq.bits.id).valid := false.B
  }

  // for Dir to choose a free way
  io.out.bits.wayMask := Fill(cacheParams.ways, 1.U(1.W))

  // add XSPerf to see how many cycles the req is held in Buffer
  if(cacheParams.enablePerf) {
    XSPerfAccumulate(cacheParams, "drop_prefetch", dup) // this also serves as late prefetch
    if(flow){
      XSPerfAccumulate(cacheParams, "req_buffer_flow", doFlow)
    }
    XSPerfAccumulate(cacheParams, "req_buffer_alloc", alloc)
    XSPerfAccumulate(cacheParams, "req_buffer_full", full)
    XSPerfAccumulate(cacheParams, "recv_prefetch", io.in.fire && isPrefetch)
    XSPerfAccumulate(cacheParams, "recv_normal", io.in.fire && !isPrefetch)
    XSPerfAccumulate(cacheParams, "chosenQ_cancel", chosenQValid && cancel)
    XSPerfAccumulate(cacheParams, "req_buffer_mergeA", io.hasMergeA)
    // TODO: count conflict
    for(i <- 0 until entries){
      val cntEnable = PopCount(buffer.map(_.valid)) === i.U
      XSPerfAccumulate(cacheParams, s"req_buffer_util_$i", cntEnable)
    }
    val bufferTimer = RegInit(VecInit(Seq.fill(entries)(0.U(16.W))))
    buffer zip bufferTimer map {
      case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(RegNext(e.valid) && !e.valid)) { t := 0.U }
        assert(t < 20000.U, "ReqBuf Leak")

        val enable = RegNext(e.valid) && !e.valid
        XSPerfHistogram(cacheParams, "reqBuf_timer", t, enable, 0, 20, 1, right_strict = true)
        XSPerfHistogram(cacheParams, "reqBuf_timer", t, enable, 20, 400, 20, left_strict = true)
        XSPerfMax(cacheParams, "max_reqBuf_timer", t, enable)

        // assert !(all entries occupied for 100 cycles)
    }
  }
}
