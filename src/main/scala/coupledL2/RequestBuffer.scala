/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package coupledL2

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import chisel3._
import chisel3.util._
import coupledL2._
import coupledL2.prefetch._
import coupledL2.utils._
import utility._
import freechips.rocketchip.util.SeqToAugmentedSeq

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

class ChosenQBundle(idOHWIdth: Int = 2)(implicit p: Parameters) extends L2Bundle {
  val bits = new ReqEntry()
  val idOH = UInt(idOHWIdth.W)
}

class AMergeTask(implicit p: Parameters) extends L2Bundle {
  val idOH = UInt(mshrsAll.W)
  val task = new TaskBundle()
}

class RequestBuffer(flow: Boolean = true, entries: Int = 4)(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val in          = Flipped(DecoupledIO(new TaskBundle))
    val out         = DecoupledIO(new TaskBundle)
    val mshrInfo  = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
    val aMergeTask = ValidIO(new AMergeTask)
    val mainPipeBlock = Input(Vec(2, Bool()))
    /* Snoop task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new TaskBundle()))

    val ATag        = Output(UInt(tagBits.W))
    val ASet        = Output(UInt(setBits.W))

    // when Probe/Release/MSHR enters MainPipe, we need also to block A req
    val s1Entrance = Flipped(ValidIO(new L2Bundle {
      val set = UInt(setBits.W)
    }))

    val hasHitPfInMSHR = ValidIO(UInt(PfSource.pfSourceBits.W))
    val hasPfLateInMSHR = ValidIO(UInt(MemReqSource.reqSourceBits.W))
    val hasMergeA = Output(Bool())
  })

  /* ======== Data Structure ======== */

  io.ATag := io.in.bits.tag
  io.ASet := io.in.bits.set

  val buffer = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new ReqEntry))))
  val issueArb = Module(new HalfFastArbiter(new ReqEntry, entries))
  ArbPerf(issueArb, "issueArb")
  val chosenQ = Module(new Queue(new ChosenQBundle(entries), entries = 1, pipe = true, flow = false))
  val chosenQValid = chosenQ.io.deq.valid

  /* ======== Enchantment ======== */
  val NWay = cacheParams.ways
  // count conflict
  def sameAddr(a: TaskBundle, b: TaskBundle):     Bool = Cat(a.tag, a.set) === Cat(b.tag, b.set)
  def sameSet (a: TaskBundle, b: TaskBundle):     Bool = a.set === b.set
  val msValids = (io.mshrInfo.map(_.valid)).asUInt
  val msNeedRelease = (io.mshrInfo.map(_.bits.needRelease)).asUInt
  val msWillFree = (io.mshrInfo.map(_.bits.willFree)).asUInt
  val msFrmA = (io.mshrInfo.map(_.bits.fromA)).asUInt
  val msIsPft = (io.mshrInfo.map(_.bits.isPrefetch)).asUInt
  val msIsAcqOrPft = (io.mshrInfo.map(_.bits.isAcqOrPrefetch)).asUInt
  val msWGrantLast = (io.mshrInfo.map(s => s.bits.w_grantlast && s.bits.w_grant)).asUInt
  val msDirHit = (io.mshrInfo.map(_.bits.dirHit)).asUInt
  val msMergeA = (io.mshrInfo.map(_.bits.mergeA)).asUInt
  val msNtoB = (io.mshrInfo.map(_.bits.param === NtoB)).asUInt
  val task_s2 = io.taskFromArb_s2

  class Compare(a: TaskBundle) {
    val mshrSameSet = (io.mshrInfo.map(_.bits.set === a.set)).asUInt
    val mshrSameReqTag = (io.mshrInfo.map(_.bits.reqTag === a.tag)).asUInt
    val mshrSameMetaTag = (io.mshrInfo.map(_.bits.metaTag === a.tag)).asUInt
    val mshrSameAddr = mshrSameSet & mshrSameReqTag
    val mshrAddrConflict = mshrSameSet & (mshrSameReqTag | mshrSameMetaTag & msNeedRelease)
    val mshrConflictMask = msValids & mshrAddrConflict & ~msWillFree
    val mshrConflictMaskFromA = mshrConflictMask & msFrmA
    val mshrConflict = mshrConflictMask.orR

    val sameSet_s2 = task_s2.valid && task_s2.bits.fromA && !task_s2.bits.mshrTask && task_s2.bits.set === a.set
    val sameSet_s3 = RegNext(task_s2.valid && task_s2.bits.fromA && !task_s2.bits.mshrTask) &&
      RegEnable(task_s2.bits.set, task_s2.valid) === a.set
    val sameSetCnt = PopCount(VecInit((msValids & mshrSameSet & msFrmA).asBools :+ sameSet_s2 :+ sameSet_s3).asUInt)

    val noFreeWay = sameSetCnt(sameSetCnt.getWidth - 1, log2Ceil(cacheParams.ways)) =/= 0.U
    assert(noFreeWay === (sameSetCnt >= cacheParams.ways.U), "sameSetCnt calculation error")
    val hasFreeWay = sameSetCnt(sameSetCnt.getWidth - 1, log2Ceil(cacheParams.ways)) === 0.U
  }


  // other flags
  val in      = io.in.bits
  val full    = Cat(buffer.map(_.valid)).andR
  val cmpWithIn = new Compare(in)
  val cmpWithEntrys = buffer.map(e => new Compare(e.task))


  // val mshrConflictMask = conflictMask(in)
  // val mshrConflictMaskFromA = conflictMaskFromA(in)
  // dontTouch(mshrConflictMask)
  // dontTouch(mshrConflictMaskFromA)

  // incoming Acquire can be merged with late_pf MSHR block
  val mergeAMask = msValids & msIsPft & cmpWithIn.mshrSameAddr & ~msDirHit & !msWGrantLast &
    msMergeA & !(Seq.fill(mshrsAll)(in.param === NtoT).asUInt & msNtoB)
  val mergeA = mergeAMask.orR && in.fromA && (in.opcode === AcquireBlock || in.opcode === AcquirePerm)
  io.aMergeTask.valid := io.in.valid && mergeA
  io.aMergeTask.bits.idOH := mergeAMask
  io.aMergeTask.bits.task := in

  /*
   noFreeWay check: s2 + s3 + mshrs >= ways(L2)
   */

  // flow not allowed when full, or entries might starve
  val canFlow = flow.B && !full && !cmpWithIn.mshrConflict.orR && !chosenQValid && !Cat(io.mainPipeBlock).orR && cmpWithIn.hasFreeWay
  val doFlow  = canFlow && io.out.ready
  //  val depMask    = buffer.map(e => e.valid && sameAddr(io.in.bits, e.task))
  // remove duplicate prefetch if same-addr A req in MSHR or ReqBuf
  val isPrefetch = in.fromA && in.opcode === Hint
  val dupMask    = VecInit(
    (msValids & msIsAcqOrPft & cmpWithIn.mshrSameAddr).asBools ++
    buffer.map(e =>
      e.valid && sameAddr(in, e.task)
    )
  ).asUInt
  val dup        = isPrefetch && dupMask.orR

  // statistics io
  val latePfMatchVec = msValids & msIsPft & cmpWithIn.mshrSameAddr & ~msWillFree
  val latePfMatched = latePfMatchVec.orR && (in.opcode === AcquireBlock || in.opcode === AcquirePerm) && in.fromA
  val latePfMatchSrc = OHMux(latePfMatchVec, io.mshrInfo.map(_.bits.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U)))
  io.hasHitPfInMSHR.valid := latePfMatched && io.in.valid && !sameAddr(in, RegNext(in))
  io.hasHitPfInMSHR.bits := latePfMatchSrc
  io.hasPfLateInMSHR.valid := io.in.valid && dup
  io.hasPfLateInMSHR.bits := io.in.bits.reqSource
  io.hasMergeA := mergeA && io.in.valid && !sameAddr(in, RegNext(in))

  //!! TODO: we can also remove those that duplicate with mainPipe

  /* ======== Alloc ======== */
  io.in.ready   := !full || doFlow || mergeA || dup

  val insertOH = MaskToOH(buffer.map(!_.valid))
  val alloc = !full && io.in.valid && !doFlow && !dup && !mergeA
  buffer.zip(insertOH.asBools).zip(cmpWithEntrys).foreach { case ((entry, sel), cmpWithE) =>
    when(alloc && sel){
      val mpBlock = Cat(io.mainPipeBlock).orR
      val pipeBlockOut = io.out.fire && sameSet(in, io.out.bits)
      val probeBlock   = io.s1Entrance.valid && io.s1Entrance.bits.set === in.set // wait for same-addr req to enter MSHR
      val s1Block      = pipeBlockOut || probeBlock

      entry.valid   := true.B
      // when Addr-Conflict / Same-Addr-Dependent / MainPipe-Block / noFreeWay-in-Set, entry not ready
      entry.rdy     := !cmpWithIn.mshrConflict && !mpBlock && !s1Block && cmpWithIn.hasFreeWay
      entry.task    := io.in.bits
      entry.waitMP  := Cat(
        s1Block,
        io.mainPipeBlock(0),
        io.mainPipeBlock(1),
        0.U(1.W))
      entry.waitMS  := cmpWithIn.mshrConflictMask
      assert(PopCount(cmpWithIn.mshrConflictMaskFromA) <= 2.U)
    }
  }

  /* ======== Issue ======== */
  issueArb.io.in zip buffer foreach {
    case(in, e) =>
      in.valid := e.valid && e.rdy
      in.bits  := e
  }

  /* ======== chosenQ enq ======== */
  // once fired at issueArb, it is ok to enter MainPipe without conflict
  // however, it may be blocked for other reasons such as high-prior reqs or MSHRFull
  // in such case, we need a place to save it
  chosenQ.io.enq.valid := issueArb.io.out.valid
  chosenQ.io.enq.bits.bits := issueArb.io.out.bits
  chosenQ.io.enq.bits.idOH := issueArb.io.chosenOH
  issueArb.io.out.ready := chosenQ.io.enq.ready

  /* ======== Update rdy and masks ======== */
  buffer.zip(cmpWithEntrys).zipWithIndex.foreach { case ((e, cmpWithE), i) =>
    when(e.valid) {
      val waitMSUpdate  = WireInit(e.waitMS)
//      val depMaskUpdate = WireInit(e.depMask)

      // when mshr will_free, clear it in other reqs' waitMS
      val willFreeMask = msValids & msWillFree
      waitMSUpdate  := e.waitMS  & (~willFreeMask).asUInt

      // Initially,
      //    waitMP(2) = s2 blocking, wait 2 cycles
      //    waitMP(1) = s3 blocking, wait 1 cycle
      // Now that we shift right waitMP every cycle
      //    so when waitMP(1) is 0 and waitMP(0) is 1, desired cycleCnt reached
      //    we recalculate waitMS and occWays, overriding old mask
      //    to take new allocated MSHR into account
      e.waitMP := e.waitMP >> 1
      when(e.waitMP(1) === 0.U && e.waitMP(0) === 1.U) {
        waitMSUpdate  := cmpWithE.mshrConflictMask
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
        e.waitMP := (e.waitMP >> 1) | "b0100".U // fired-req at s2 next cycle
      }

      // update info
      e.waitMS  := waitMSUpdate
      e.rdy     := !waitMSUpdate.orR && !e.waitMP && !s1_Block && cmpWithE.hasFreeWay
    }
  }

  /* ======== Output ======== */
  // when entry.rdy is no longer true,
  // we cancel req in chosenQ, with the entry still held in buffer to issue later
//  val cancel = (canFlow && sameSet(chosenQ.io.deq.bits.bits.task, io.in.bits)) || !buffer(chosenQ.io.deq.bits.id).rdy
  val cancel = !OHMux(chosenQ.io.deq.bits.idOH, buffer.map(_.rdy))

  chosenQ.io.deq.ready := io.out.ready || cancel
  io.out.valid := chosenQValid && !cancel || io.in.valid && canFlow
  io.out.bits := {
    if (!flow) chosenQ.io.deq.bits.bits.task
    else Mux(chosenQValid, chosenQ.io.deq.bits.bits.task, io.in.bits)
  }

  buffer.zip(chosenQ.io.deq.bits.idOH.asBools).foreach {
    case (e, y) =>
      when(chosenQ.io.deq.fire && y && !cancel) {
        e.valid := false.B
      }
  }

  // for Dir to choose a free way
  io.out.bits.wayMask := Fill(cacheParams.ways, 1.U(1.W))

  // add XSPerf to see how many cycles the req is held in Buffer
  if(cacheParams.enablePerf) {
    XSPerfAccumulate("drop_prefetch", io.in.valid && dup)
    if(flow){
      XSPerfAccumulate("req_buffer_flow", io.in.valid && doFlow)
    }
    XSPerfAccumulate("req_buffer_alloc", alloc)
    XSPerfAccumulate("req_buffer_full", full)
    XSPerfAccumulate("recv_prefetch", io.in.fire && isPrefetch)
    XSPerfAccumulate("recv_normal", io.in.fire && !isPrefetch)
    XSPerfAccumulate("chosenQ_cancel", chosenQValid && cancel)
    XSPerfAccumulate("req_buffer_mergeA", io.hasMergeA)
    // TODO: count conflict
    for(i <- 0 until entries){
      val cntEnable = PopCount(buffer.map(_.valid)) === i.U
      XSPerfAccumulate(s"req_buffer_util_$i", cntEnable)
    }
    val bufferTimer = RegInit(VecInit(Seq.fill(entries)(0.U(16.W))))
    buffer zip bufferTimer map {
      case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(RegNext(e.valid) && !e.valid)) { t := 0.U }
        assert(t < 20000.U, "ReqBuf Leak")

        val enable = RegNext(e.valid) && !e.valid
        XSPerfHistogram("reqBuf_timer", t, enable, 0, 20, 1, right_strict = true)
        XSPerfHistogram("reqBuf_timer", t, enable, 20, 400, 20, left_strict = true)
        XSPerfMax("max_reqBuf_timer", t, enable)

        // assert !(all entries occupied for 100 cycles)
    }
  }
}
