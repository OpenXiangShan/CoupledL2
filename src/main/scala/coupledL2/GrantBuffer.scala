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

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.prefetch.PrefetchResp

// record info of those with Grant sent, yet GrantAck not received
// used to block Probe upwards
class InflightGrantEntry(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
}

class TaskWithData(implicit p: Parameters) extends L2Bundle {
  val task = new TaskBundle()
  val data = new DSBlock()
}

/*class GrantQueueTask(implicit p: Parameters) extends L2Bundle {
  val task = new TaskBundle()
  val data = new DSBlock()
  val grantid = UInt(mshrBits.W)
}*/
class GrantQueueTask(implicit p: Parameters) extends L2Bundle {
  val task = new TaskBundle()
  val grantid = UInt(mshrBits.W)
}
class GrantQueueData(implicit p: Parameters) extends L2Bundle {
  val data = new DSBeat()
}

// 1. Communicate with L1
//   1.1 Send Grant/GrantData/ReleaseAck/AccessAckData from d and
//   1.2 Receive GrantAck through e
// 2. Send response to Prefetcher
// 3. Block MainPipe enterance when there is not enough space
// 4. Generate Hint signal for L1 early wake-up
class GrantBuffer(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // receive task from MainPipe
    val d_task = Flipped(DecoupledIO(new TaskWithData()))

    // interact with channels to L1
    val d = DecoupledIO(new TLBundleD(edgeIn.bundle))
    val e = Flipped(DecoupledIO(new TLBundleE(edgeIn.bundle)))

    // for MainPipe entrance blocking
    val fromReqArb = Input(new Bundle() {
      val status_s1 = new PipeEntranceStatus
    })
    val pipeStatusVec = Flipped(Vec(5, ValidIO(new PipeStatus)))
    val toReqArb = Output(new Bundle() {
      val blockSinkReqEntrance = new BlockInfo()
      val blockMSHRReqEntrance = Bool()
    })

    // response to prefetcher
    val prefetchResp = prefetchOpt.map(_ => DecoupledIO(new PrefetchResp))

    // to block sourceB from sending same-addr probe until GrantAck received
    val grantStatus = Output(Vec(grantBufInflightSize, new GrantStatus))
  })

  // =========== functions ===========
  def toTLBundleD(task: TaskBundle, data: UInt = 0.U, grant_id: UInt = 0.U) = {
    val d = Wire(new TLBundleD(edgeIn.bundle))
    d.opcode := task.opcode
    d.param := task.param
    d.size := offsetBits.U
    d.source := task.sourceId
    d.sink := grant_id
    d.denied := task.denied
    d.data := data
    d.corrupt := task.corrupt
    d.echo.lift(IsKeywordKey).foreach(_ := false.B)
    d
  }

//  def getBeat(data: UInt, beatsOH: UInt): (UInt, UInt) = {
//    // get one beat from data according to beatsOH
//    require(data.getWidth == (blockBytes * 8))
//    require(beatsOH.getWidth == beatSize)
//    // next beat
//    val next_beat = ParallelPriorityMux(beatsOH, data.asTypeOf(Vec(beatSize, UInt((beatBytes * 8).W))))
//    val selOH = PriorityEncoderOH(beatsOH)
//    // remaining beats that haven't been sent out
//    val next_beatsOH = beatsOH & ~selOH
//    (next_beat, next_beatsOH)
//  }

  // val grantQueue = Module(new Queue(new GrantQueueTask(), entries = mshrsAll))
  // Use customized SRAM: dual_port, max 256bits:
  val grantQueue = Module(new Queue(new GrantQueueTask(), entries = mshrsAll))
  val grantQueueData0 = Module(new Queue(new GrantQueueData(), entries = mshrsAll))
  val grantQueueData1 = Module(new Queue(new GrantQueueData(), entries = mshrsAll))

  val inflightGrant = RegInit(VecInit(Seq.fill(grantBufInflightSize) {
    0.U.asTypeOf(Valid(new InflightGrantEntry))
  }))

  val dtaskOpcode = io.d_task.bits.task.opcode
  val mergeAtask = WireInit(0.U.asTypeOf(new TaskBundle()))
  mergeAtask.channel := io.d_task.bits.task.channel
  mergeAtask.txChannel := io.d_task.bits.task.txChannel
  mergeAtask.off := io.d_task.bits.task.aMergeTask.off
  mergeAtask.alias.foreach(_ := io.d_task.bits.task.aMergeTask.alias.getOrElse(0.U))
  mergeAtask.opcode := io.d_task.bits.task.aMergeTask.opcode
  mergeAtask.param := io.d_task.bits.task.aMergeTask.param
  mergeAtask.sourceId := io.d_task.bits.task.aMergeTask.sourceId
  mergeAtask.meta := io.d_task.bits.task.aMergeTask.meta
  mergeAtask.set := io.d_task.bits.task.set
  mergeAtask.tag := io.d_task.bits.task.tag
  mergeAtask.vaddr.foreach(_ := io.d_task.bits.task.vaddr.getOrElse(0.U))
  mergeAtask.isKeyword.foreach(_ := io.d_task.bits.task.aMergeTask.isKeyword.getOrElse(false.B))
  mergeAtask.size := io.d_task.bits.task.size
  mergeAtask.bufIdx := io.d_task.bits.task.bufIdx
  mergeAtask.needProbeAckData := io.d_task.bits.task.needProbeAckData
  mergeAtask.mshrTask := io.d_task.bits.task.mshrTask
  mergeAtask.mshrId := io.d_task.bits.task.mshrId
  mergeAtask.aliasTask.foreach(_ := io.d_task.bits.task.aliasTask.getOrElse(0.U))
  mergeAtask.useProbeData := false.B
  mergeAtask.mshrRetry := false.B
  mergeAtask.fromL2pft.foreach(_ := false.B)
  mergeAtask.needHint.foreach(_ := false.B)
  mergeAtask.dirty := io.d_task.bits.task.dirty
  mergeAtask.way := io.d_task.bits.task.way
  mergeAtask.metaWen := io.d_task.bits.task.metaWen
  mergeAtask.tagWen := io.d_task.bits.task.tagWen
  mergeAtask.dsWen := io.d_task.bits.task.dsWen
  mergeAtask.wayMask := io.d_task.bits.task.wayMask
  mergeAtask.replTask := io.d_task.bits.task.replTask
  mergeAtask.reqSource := io.d_task.bits.task.reqSource
  mergeAtask.mergeA := false.B
  mergeAtask.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
  val inflight_insertIdx = PriorityEncoder(inflightGrant.map(!_.valid))
  val grantQueue_enq_isKeyword = Mux(
    io.d_task.bits.task.mergeA,
    mergeAtask.isKeyword.getOrElse(false.B),
    io.d_task.bits.task.isKeyword.getOrElse(false.B)
  )
  // The following is organized in the order of data flow
  // =========== save d_task in queue[FIFO] ===========
  grantQueue.io.enq.valid := io.d_task.valid && (dtaskOpcode =/= HintAck || io.d_task.bits.task.mergeA)
  grantQueue.io.enq.bits.task := Mux(io.d_task.bits.task.mergeA, mergeAtask, io.d_task.bits.task)
  grantQueue.io.enq.bits.task.isKeyword.foreach(_ := grantQueue_enq_isKeyword)
  // grantQueue.io.enq.bits.task.isKeyword.foreach(_ := io.d_task.bits.task.isKeyword.getOrElse(false.B))
  grantQueue.io.enq.bits.grantid := inflight_insertIdx
  val enqData = io.d_task.bits.data.asTypeOf(Vec(beatSize, new DSBeat))
  grantQueueData0.io.enq.valid := grantQueue.io.enq.valid
  grantQueueData0.io.enq.bits.data := enqData(0)
  grantQueueData1.io.enq.valid := grantQueue.io.enq.valid
  grantQueueData1.io.enq.bits.data := enqData(1)
  io.d_task.ready := true.B // GrantBuf should always be ready

  val grantQueueCnt = grantQueue.io.count
  val full = !grantQueue.io.enq.ready
  assert(!(full && io.d_task.valid), "GrantBuf full and RECEIVE new task, back pressure failed")

  // =========== dequeue entry and fire ===========
  require(beatSize == 2)
  val deqValid = grantQueue.io.deq.valid
  val deqTask = grantQueue.io.deq.bits.task
  // val deqTask.isKeyword.foreach(_ := grantQueue.io.deq.bits.task.isKeyword)
  val deqId = grantQueue.io.deq.bits.grantid
  val deqData = VecInit(Seq(grantQueueData0.io.deq.bits.data, grantQueueData1.io.deq.bits.data))

  // grantBuf: to keep the remaining unsent beat of GrantData
  val grantBufValid = RegInit(false.B)
  val grantBuf = RegInit(0.U.asTypeOf(new Bundle() {
    val task = new TaskBundle()
    val data = new DSBeat()
    val grantid = UInt(mshrBits.W)
  }))

  grantQueue.io.deq.ready := io.d.ready && !grantBufValid
  grantQueueData0.io.deq.ready := grantQueue.io.deq.ready
  grantQueueData1.io.deq.ready := grantQueue.io.deq.ready

  // if deqTask has data, send the first beat directly and save the remaining beat in grantBuf
  when(deqValid && io.d.ready && !grantBufValid && deqTask.opcode(0)) {
    grantBufValid := true.B
    grantBuf.task := deqTask
    grantBuf.task.isKeyword.foreach(_ := deqTask.isKeyword.getOrElse(false.B))
    // grantBuf.data := deqData(1)
    grantBuf.data := Mux(deqTask.isKeyword.getOrElse(false.B), deqData(0), deqData(1))
    grantBuf.grantid := deqId
  }
  when(grantBufValid && io.d.ready) {
    grantBufValid := false.B
  }

  io.d.valid := grantBufValid || deqValid
  io.d.bits := Mux(
    grantBufValid,
    toTLBundleD(grantBuf.task, grantBuf.data.data, grantBuf.grantid),
    // toTLBundleD(deqTask, deqData(0).data, deqId)
    toTLBundleD(deqTask, Mux(deqTask.isKeyword.getOrElse(false.B), deqData(1).data, deqData(0).data), deqId)
  )

  XSPerfAccumulate("toTLBundleD_valid", deqValid)
  XSPerfAccumulate("toTLBundleD_valid_isKeyword", deqValid && deqTask.isKeyword.getOrElse(false.B))
  XSPerfAccumulate("toTLBundleD_fire", deqValid && io.d.ready)
  XSPerfAccumulate("toTLBundleD_fire_isKeyword", deqValid && io.d.ready && deqTask.isKeyword.getOrElse(false.B))
  /* val d_isKeyword = Mux(
    grantBufValid,
    grantBuf.task.isKeyword,
    deqTask.isKeyword
  )*/
  io.d.bits.echo.lift(IsKeywordKey).foreach(_ := Mux(
    grantBufValid,
    grantBuf.task.isKeyword.getOrElse(false.B),
    deqTask.isKeyword.getOrElse(false.B)
  ))

  // =========== send response to prefetcher ===========
  val pftRespEntry = new Bundle() {
    val tag = UInt(tagBits.W)
    val set = UInt(setBits.W)
    val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
    val pfSource = UInt(MemReqSource.reqSourceBits.W)
  }
  // TODO: this may not need 10 entries, but this does not take much space
  val pftQueueLen = 10
  val pftRespQueue = prefetchOpt.map(_ => Module(new Queue(pftRespEntry, entries = pftQueueLen, flow = true)))
  prefetchOpt.map { _ =>
    pftRespQueue.get.io.enq.valid := io.d_task.valid && dtaskOpcode === HintAck &&
      io.d_task.bits.task.fromL2pft.getOrElse(false.B)
    pftRespQueue.get.io.enq.bits.tag := io.d_task.bits.task.tag
    pftRespQueue.get.io.enq.bits.set := io.d_task.bits.task.set
    pftRespQueue.get.io.enq.bits.vaddr.foreach(_ := io.d_task.bits.task.vaddr.getOrElse(0.U))
    pftRespQueue.get.io.enq.bits.pfSource := io.d_task.bits.task.reqSource

    val resp = io.prefetchResp.get
    resp.valid := pftRespQueue.get.io.deq.valid
    resp.bits.tag := pftRespQueue.get.io.deq.bits.tag
    resp.bits.set := pftRespQueue.get.io.deq.bits.set
    resp.bits.vaddr.foreach(_ := pftRespQueue.get.io.deq.bits.vaddr.getOrElse(0.U))
    resp.bits.pfSource := pftRespQueue.get.io.deq.bits.pfSource
    pftRespQueue.get.io.deq.ready := resp.ready

    assert(pftRespQueue.get.io.enq.ready, "pftRespQueue should never be full, no back pressure logic")
  }
  // If no prefetch, there never should be HintAck
  // assert(prefetchOpt.nonEmpty.B || !io.d_task.valid || dtaskOpcode =/= HintAck)

  // =========== record unreceived GrantAck ===========
  // Addrs with Grant sent and GrantAck not received
  when(io.d_task.fire && (dtaskOpcode === Grant || dtaskOpcode === GrantData || io.d_task.bits.task.mergeA)) {
    // choose an empty entry
    val entry = inflightGrant(inflight_insertIdx)
    entry.valid := true.B
    entry.bits.set := io.d_task.bits.task.set
    entry.bits.tag := io.d_task.bits.task.tag
  }
  val inflight_full = Cat(inflightGrant.map(_.valid)).andR
  assert(
    !(inflight_full & (io.d_task.fire && (dtaskOpcode === Grant || dtaskOpcode === GrantData || io.d_task.bits.task.mergeA))),
    "inflightGrant entries overflow"
  )

  // report status to SourceB to block same-addr Probe
  io.grantStatus zip inflightGrant foreach {
    case (g, i) =>
      g.valid := i.valid
      g.tag := i.bits.tag
      g.set := i.bits.set
  }

  when(io.e.fire) {
    assert(io.e.bits.sink < grantBufInflightSize.U, "GrantBuf: e.sink overflow inflightGrant size")
    inflightGrant(io.e.bits.sink).valid := false.B
  }

  io.e.ready := true.B

  // =========== handle blocking - capacity conflict ===========
  // count the number of valid blocks + those in pipe that might use GrantBuf
  // so that GrantBuffer will not exceed capacity [back pressure]
  val noSpaceForSinkReq = PopCount(VecInit(io.pipeStatusVec.tail.map { case s =>
    s.valid && (s.bits.fromA || s.bits.fromC)
  }).asUInt) + grantQueueCnt >= mshrsAll.U
  val noSpaceWaitSinkEForSinkReq = PopCount(VecInit(io.pipeStatusVec.tail.map { case s =>
    s.valid && s.bits.fromA
  }).asUInt) + PopCount(VecInit(inflightGrant.map(x => x.valid))) >= mshrsAll.U
  // for timing consideration, drop s1 info, so always reserve one entry for it
  val noSpaceForMSHRReq = PopCount(VecInit(io.pipeStatusVec.tail.map { case s =>
    s.valid && (s.bits.fromA || s.bits.fromC)
  }).asUInt) + grantQueueCnt >= (mshrsAll - 1).U
  val noSpaceWaitSinkEForMSHRReq = PopCount(VecInit(io.pipeStatusVec.tail.map { case s =>
    s.valid && s.bits.fromA
  }).asUInt) + PopCount(VecInit(inflightGrant.map(x => x.valid))) >= (mshrsAll - 1).U
  // pftRespQueue also requires back pressure to ensure that it will not exceed capacity
  // Ideally, it should only block Prefetch from entering MainPipe
  // But since it is extremely rare that pftRespQueue of 10 would be full, we just block all Entrance here, simpler logic
  // TODO: consider optimize this
  val noSpaceForSinkPft = prefetchOpt.map(_ =>
    PopCount(VecInit(io.pipeStatusVec.tail.map { case s =>
      s.valid && s.bits.fromA
    }).asUInt) + pftRespQueue.get.io.count >= pftQueueLen.U
  )
  val noSpaceForMSHRPft = prefetchOpt.map(_ =>
    PopCount(VecInit(io.pipeStatusVec.tail.map { case s =>
      s.valid && s.bits.fromA
    }).asUInt) + pftRespQueue.get.io.count >= (pftQueueLen - 1).U
  )

  io.toReqArb.blockSinkReqEntrance.blockA_s1 := noSpaceForSinkReq || noSpaceWaitSinkEForSinkReq || noSpaceForSinkPft.getOrElse(
    false.B
  )
  io.toReqArb.blockSinkReqEntrance.blockB_s1 := Cat(inflightGrant.map(g =>
    g.valid &&
      g.bits.set === io.fromReqArb.status_s1.b_set && g.bits.tag === io.fromReqArb.status_s1.b_tag
  )).orR
  // TODO: or should we still Stall B req?
  // A-replace related rprobe is handled in SourceB
  io.toReqArb.blockSinkReqEntrance.blockC_s1 := noSpaceForSinkReq
  io.toReqArb.blockSinkReqEntrance.blockG_s1 := false.B // this is not used
  io.toReqArb.blockMSHRReqEntrance := noSpaceForMSHRReq || noSpaceWaitSinkEForMSHRReq || noSpaceForMSHRPft.getOrElse(
    false.B
  )

  // =========== XSPerf ===========
  if (cacheParams.enablePerf) {
    val timers = RegInit(VecInit(Seq.fill(grantBufInflightSize) { 0.U(64.W) }))
    inflightGrant zip timers map {
      case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid) && !e.valid) { t := 0.U }
        assert(t < 10000.U, "Inflight Grant Leak")

        val enable = RegNext(e.valid) && !e.valid
        XSPerfHistogram("grant_grantack_period", t, enable, 0, 12, 1)
        XSPerfMax("max_grant_grantack_period", t, enable)
    }
    // pftRespQueue is about to be full, and using back pressure to block All MainPipe Entrance
    // which can SERIOUSLY affect performance, should consider less drastic prefetch policy
    XSPerfAccumulate("pftRespQueue_about_to_full", noSpaceForMSHRPft.getOrElse(false.B))
  }
}
