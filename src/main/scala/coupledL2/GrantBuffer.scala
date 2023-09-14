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
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.prefetch.PrefetchResp
import coupledL2.utils.{XSPerfAccumulate, XSPerfHistogram, XSPerfMax}

// record info of those with Grant sent, yet GrantAck not received
// used to block Probe upwards
class InflightGrantEntry(implicit p: Parameters) extends L2Bundle {
  val set   = UInt(setBits.W)
  val tag   = UInt(tagBits.W)
  val sink  = UInt(mshrBits.W)
}

class TaskWithData(implicit p: Parameters) extends L2Bundle {
  val task = new TaskBundle()
  val data = new DSBlock()
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

    // response to MSHR
    val e_resp = Output(new RespBundle)

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

    // generate hint signal for L1
    val l1Hint = ValidIO(new L2ToL1Hint())
    val globalCounter = Output(UInt((log2Ceil(mshrsAll) + 1).W))
  })

  // =========== functions ===========
  def toTLBundleD(task: TaskBundle, data: UInt = 0.U) = {
    val d = Wire(new TLBundleD(edgeIn.bundle))
    d.opcode := task.opcode
    d.param := task.param
    d.size := offsetBits.U
    d.source := task.sourceId
    d.sink := task.mshrId
    d.denied := false.B
    d.data := data
    d.corrupt := false.B
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

  val dtaskOpcode = io.d_task.bits.task.opcode
  // The following is organized in the order of data flow
  // =========== save d_task in queue[FIFO] ===========
  val grantQueue = Module(new Queue(new TaskWithData(), entries = mshrsAll))
  grantQueue.io.enq.valid := io.d_task.valid && dtaskOpcode =/= HintAck
  grantQueue.io.enq.bits := io.d_task.bits
  io.d_task.ready := true.B // GrantBuf should always be ready

  val grantQueueCnt = grantQueue.io.count
  val full = !grantQueue.io.enq.ready
  assert(!(full && io.d_task.valid), "GrantBuf full and RECEIVE new task, back pressure failed")

  // =========== dequeue entry and fire ===========
  require(beatSize == 2)
  val deqValid = grantQueue.io.deq.valid
  val deqTask = grantQueue.io.deq.bits.task
  val deqData = grantQueue.io.deq.bits.data.asTypeOf(Vec(beatSize, new DSBeat))

  // grantBuf: to keep the remaining unsent beat of GrantData
  val grantBufValid = RegInit(false.B)
  val grantBuf =  RegInit(0.U.asTypeOf(new Bundle() {
    val task = new TaskBundle()
    val data = new DSBeat()
  }))

  grantQueue.io.deq.ready := io.d.ready && !grantBufValid

  // if deqTask has data, send the first beat directly and save the remaining beat in grantBuf
  when(deqValid && io.d.ready && !grantBufValid && deqTask.opcode(0)) {
    grantBufValid := true.B
    grantBuf.task := deqTask
    grantBuf.data := deqData(1)
  }
  when(grantBufValid && io.d.ready) {
    grantBufValid := false.B
  }

  io.d.valid := grantBufValid || deqValid
  io.d.bits := Mux(
    grantBufValid,
    toTLBundleD(grantBuf.task, grantBuf.data.data),
    toTLBundleD(deqTask, deqData(0).data)
  )

  // =========== send response to prefetcher ===========
  val pftRespEntry = new Bundle() {
    val tag = UInt(tagBits.W)
    val set = UInt(setBits.W)
  }
  // TODO: this may not need 10 entries, but this does not take much space
  val pftQueueLen = 10
  val pftRespQueue = prefetchOpt.map(_ => Module(new Queue(pftRespEntry, entries = pftQueueLen, flow = true)))
  prefetchOpt.map { _ =>
    pftRespQueue.get.io.enq.valid := io.d_task.valid && dtaskOpcode === HintAck &&
      io.d_task.bits.task.fromL2pft.getOrElse(false.B)
    pftRespQueue.get.io.enq.bits.tag := io.d_task.bits.task.tag
    pftRespQueue.get.io.enq.bits.set := io.d_task.bits.task.set

    val resp = io.prefetchResp.get
    resp.valid := pftRespQueue.get.io.deq.valid
    resp.bits.tag := pftRespQueue.get.io.deq.bits.tag
    resp.bits.set := pftRespQueue.get.io.deq.bits.set
    pftRespQueue.get.io.deq.ready := resp.ready

    assert(pftRespQueue.get.io.enq.ready, "pftRespQueue should never be full, no back pressure logic")
  }
  // If no prefetch, there never should be HintAck
  assert(prefetchOpt.nonEmpty.B || !io.d_task.valid || dtaskOpcode =/= HintAck)

  // =========== record unreceived GrantAck ===========
  // Addrs with Grant sent and GrantAck not received
  val inflight_grant = RegInit(VecInit(Seq.fill(grantBufInflightSize){
    0.U.asTypeOf(Valid(new InflightGrantEntry))
  }))
  when (io.d_task.fire && dtaskOpcode(2, 1) === Grant(2, 1)) {
    // choose an empty entry
    val insertIdx = PriorityEncoder(inflight_grant.map(!_.valid))
    val entry = inflight_grant(insertIdx)
    entry.valid := true.B
    entry.bits.set    := io.d_task.bits.task.set
    entry.bits.tag    := io.d_task.bits.task.tag
    entry.bits.sink   := io.d_task.bits.task.mshrId
  }
  val inflight_full = Cat(inflight_grant.map(_.valid)).andR
  assert(!inflight_full, "inflight_grant entries should not be full")

  // report status to SourceB to block same-addr Probe
  io.grantStatus zip inflight_grant foreach {
    case (g, i) =>
      g.valid := i.valid
      g.tag   := i.bits.tag
      g.set   := i.bits.set
  }

  when (io.e.fire) {
    // compare sink to clear buffer
    val sinkMatchVec = inflight_grant.map(g => g.valid && g.bits.sink === io.e.bits.sink)
    assert(PopCount(sinkMatchVec) === 1.U, "GrantBuf: there must be one and only one match")
    val bufIdx = OHToUInt(sinkMatchVec)
    inflight_grant(bufIdx).valid := false.B
  }

  // =========== send e resp to MSHRs ===========
  io.e.ready := true.B
  io.e_resp.valid := io.e.valid
  io.e_resp.mshrId := io.e.bits.sink
  io.e_resp.set := 0.U(setBits.W)
  io.e_resp.tag := 0.U(tagBits.W)
  io.e_resp.respInfo.opcode := GrantAck
  io.e_resp.respInfo.param := 0.U(3.W)
  io.e_resp.respInfo.last := true.B
  io.e_resp.respInfo.dirty := false.B
  io.e_resp.respInfo.isHit := false.B

  // =========== handle blocking - capacity conflict ===========
  // count the number of valid blocks + those in pipe that might use GrantBuf
  // so that GrantBuffer will not exceed capacity [back pressure]
  val noSpaceForSinkReq = PopCount(VecInit(io.pipeStatusVec.tail.map { case s =>
    s.valid && (s.bits.fromA || s.bits.fromC)
  }).asUInt) + grantQueueCnt >= mshrsAll.U
  val noSpaceForMSHRReq = PopCount(VecInit(io.pipeStatusVec.map { case s =>
    s.valid && s.bits.fromA
  }).asUInt) + grantQueueCnt >= mshrsAll.U
  // pftRespQueue also requires back pressure to ensure that it will not exceed capacity
  // Ideally, it should only block Prefetch from entering MainPipe
  // But since it is extremely rare that pftRespQueue of 10 would be full, we just block all Entrance here, simpler logic
  // TODO: consider optimize this
  val noSpaceForSinkPft = prefetchOpt.map(_ => PopCount(VecInit(io.pipeStatusVec.tail.map { case s =>
    s.valid && s.bits.fromA
  }).asUInt) + pftRespQueue.get.io.count >= pftQueueLen.U)
  val noSpaceForMSHRPft = prefetchOpt.map(_ => PopCount(VecInit(io.pipeStatusVec.map { case s =>
    s.valid && s.bits.fromA
  }).asUInt) + pftRespQueue.get.io.count >= pftQueueLen.U)

  io.toReqArb.blockSinkReqEntrance.blockA_s1 := noSpaceForSinkReq || noSpaceForSinkPft.getOrElse(false.B)
  io.toReqArb.blockSinkReqEntrance.blockB_s1 := Cat(inflight_grant.map(g => g.valid &&
    g.bits.set === io.fromReqArb.status_s1.b_set && g.bits.tag === io.fromReqArb.status_s1.b_tag)).orR
  //TODO: or should we still Stall B req?
  // A-replace related rprobe is handled in SourceB
  io.toReqArb.blockSinkReqEntrance.blockC_s1 := noSpaceForSinkReq
  io.toReqArb.blockSinkReqEntrance.blockG_s1 := false.B // this is not used
  io.toReqArb.blockMSHRReqEntrance := noSpaceForMSHRReq || noSpaceForMSHRPft.getOrElse(false.B)

  // =========== generating Hint to L1 ===========
  // TODO: the following keeps the exact same logic as before, but it needs serious optimization
  val hintQueue = Module(new Queue(UInt(sourceIdBits.W), entries = mshrsAll))
  // Total number of beats left to send in GrantBuf
  // [This is better]
  // val globalCounter = (grantQueue.io.count << 1.U).asUInt + grantBufValid.asUInt // entries * 2 + grantBufValid
  val globalCounter = RegInit(0.U((log2Ceil(grantBufSize) + 1).W))
  when(io.d_task.fire()) {
    val hasData = io.d_task.bits.task.opcode(0)
    when(hasData) {
      globalCounter := globalCounter + 1.U // counter = counter + 2 - 1
    }.otherwise {
      globalCounter := globalCounter // counter = counter + 1 - 1
    }
  }.otherwise {
    globalCounter := Mux(globalCounter === 0.U, 0.U, globalCounter - 1.U) // counter = counter - 1
  }

  // if globalCounter >= 3, it means the hint that should be sent is in GrantBuf
  when(globalCounter >= 3.U) {
    hintQueue.io.enq.valid := true.B
    hintQueue.io.enq.bits := io.d_task.bits.task.sourceId
  }.otherwise {
    hintQueue.io.enq.valid := false.B
    hintQueue.io.enq.bits := 0.U(sourceIdBits.W)
  }
  hintQueue.io.deq.ready := true.B

  // tell CustomL1Hint about the delay in GrantBuf
  io.globalCounter := globalCounter

  io.l1Hint.valid := hintQueue.io.deq.valid
  io.l1Hint.bits.sourceId := hintQueue.io.deq.bits

  // =========== XSPerf ===========
  if (cacheParams.enablePerf) {
    val timers = RegInit(VecInit(Seq.fill(grantBufInflightSize){0.U(64.W)}))
    inflight_grant zip timers map {
      case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid) && !e.valid) { t := 0.U }
        assert(t < 10000.U, "Inflight Grant Leak")

        val enable = RegNext(e.valid) && !e.valid
        XSPerfHistogram(cacheParams, "grant_grantack_period", t, enable, 0, 12, 1)
        XSPerfMax(cacheParams, "max_grant_grantack_period", t, enable)
    }
    // pftRespQueue is about to be full, and using back pressure to block All MainPipe Entrance
    // which can SERIOUSLY affect performance, should consider less drastic prefetch policy
    XSPerfAccumulate(cacheParams, "WARNING_pftRespQueue_about_to_full", noSpaceForMSHRPft.getOrElse(false.B))
  }
}
