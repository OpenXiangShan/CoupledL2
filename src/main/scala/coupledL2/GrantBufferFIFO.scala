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
import coupledL2.utils._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._

// Send out Grant/GrantData/ReleaseAck through d and
// receive GrantAck through e
// FIFO version of GrantBuffer
class GrantBufferFIFO(implicit p: Parameters) extends BaseGrantBuffer with HasCircularQueuePtrHelper{

  class GrantBufferPtr(implicit p: Parameters) extends CircularQueuePtr[GrantBufferPtr](mshrsAll){ }

  object GrantBufferPtr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): GrantBufferPtr = {
      val ptr = Wire(new GrantBufferPtr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val enqPtrExt = RegInit(0.U.asTypeOf(new GrantBufferPtr))
  val deqPtrExt = RegInit(0.U.asTypeOf(new GrantBufferPtr))

  val enqPtr = enqPtrExt.value
  val deqPtr = deqPtrExt.value

  val beat_valids = RegInit(VecInit(Seq.fill(mshrsAll) {
    VecInit(Seq.fill(beatSize)(false.B))
  }))
  val block_valids = VecInit(beat_valids.map(_.asUInt.orR)).asUInt
  val tasks = Reg(Vec(mshrsAll, new TaskBundle))
  val datas = Reg(Vec(mshrsAll, new DSBlock))
  val full = block_valids.andR

  // hint interface: l2 will send hint to l1 before sending grantData (2 cycle ahead)
  val globalCounter = RegInit(0.U(log2Ceil(mshrsAll).W))
  val beat_counters = RegInit(VecInit(Seq.fill(mshrsAll) {
    0.U(log2Ceil(mshrsAll).W)
  }))
  io.globalCounter := globalCounter

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

  // GrantData
  val hint_valid_vec = beat_counters.zip(tasks).map{case (counter, task) => { counter === hintCycleAhead.U && task.opcode(0) }}
  val sourceid_vec = tasks.map{case task => task.sourceId}

  io.l1Hint.valid := VecInit(hint_valid_vec).asUInt.orR
  io.l1Hint.sourceId := ParallelMux(hint_valid_vec zip sourceid_vec)
  assert(PopCount(VecInit(hint_valid_vec)) <= 1.U)

  beat_counters.foreach {
    case (counter) => {
      counter := Mux(counter === 0.U, 0.U, counter - 1.U)
    }
  }

  // used to block Probe upwards
  val inflight_grant_set = Reg(Vec(sourceIdAll, UInt(setBits.W)))
  val inflight_grant_tag = Reg(Vec(sourceIdAll, UInt(tagBits.W)))
  val inflight_grant_valid = RegInit(VecInit(Seq.fill(sourceIdAll)(false.B)))
  val inflight_grant = inflight_grant_valid zip (inflight_grant_set zip inflight_grant_tag)

  when (io.d_task.fire() && io.d_task.bits.task.opcode(2, 1) === Grant(2, 1)) {
    val id = io.d_task.bits.task.mshrId(sourceIdBits-1, 0)
    inflight_grant_set(id) := io.d_task.bits.task.set
    inflight_grant_tag(id) := io.d_task.bits.task.tag
    inflight_grant_valid(id) := true.B
  }
  when (io.e.fire) {
    val id = io.e.bits.sink(sourceIdBits-1, 0)
    inflight_grant_valid(id) := false.B
  }

  // handle capacity conflict
  val noSpaceForSinkReq = PopCount(Cat(VecInit(io.pipeStatusVec.tail.map { case s =>
    s.valid && (s.bits.fromA || s.bits.fromC)
  }).asUInt, block_valids)) >= mshrsAll.U
  val noSpaceForMSHRReq = PopCount(Cat(VecInit(io.pipeStatusVec.map { case s =>
    s.valid && s.bits.fromA
  }).asUInt, block_valids)) >= mshrsAll.U

  io.toReqArb.blockSinkReqEntrance.blockA_s1 := Cat(inflight_grant.map { case (v, (set, _)) =>
    v && set === io.fromReqArb.status_s1.a_set
  }).orR || noSpaceForSinkReq
  io.toReqArb.blockSinkReqEntrance.blockB_s1 := Cat(inflight_grant.map { case (v, (set, tag)) =>
    v && set === io.fromReqArb.status_s1.b_set && tag === io.fromReqArb.status_s1.b_tag
  }).orR
  io.toReqArb.blockSinkReqEntrance.blockC_s1 := noSpaceForSinkReq
  io.toReqArb.blockMSHRReqEntrance := noSpaceForMSHRReq

  when(io.d_task.fire()) {
    beat_valids(enqPtr).foreach(_ := true.B)
    tasks(enqPtr) := io.d_task.bits.task
    datas(enqPtr) := io.d_task.bits.data
    beat_counters(enqPtr) := globalCounter
    enqPtrExt := enqPtrExt + 1.U
  }

  def toTLBundleD(task: TaskBundle, data: UInt = 0.U) = {
    val d = Wire(new TLBundleD(edgeIn.bundle))
    d := DontCare
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

  def getBeat(data: UInt, beatsOH: UInt): (UInt, UInt) = {
    // get one beat from data according to beatsOH
    require(data.getWidth == (blockBytes * 8))
    require(beatsOH.getWidth == beatSize)
    // next beat
    val next_beat = ParallelPriorityMux(beatsOH, data.asTypeOf(Vec(beatSize, UInt((beatBytes * 8).W))))
    val selOH = PriorityEncoderOH(beatsOH)
    // remaining beats that haven't been sent out
    val next_beatsOH = beatsOH & ~selOH
    (next_beat, next_beatsOH)
  }

  io.d := DontCare
  for(idx <- (0 until mshrsAll)) {
    when(deqPtr === idx.U) {
      io.d.valid := block_valids(idx)
      val data = datas(idx).data
      val beatsOH = beat_valids(idx).asUInt
      val (beat, next_beatsOH) = getBeat(data, beatsOH)
      io.d.bits := toTLBundleD(tasks(idx), beat)
      val hasData = io.d.bits.opcode(0)

      when (io.d.fire()) {
        when (hasData) {
          beat_valids(idx) := VecInit(next_beatsOH.asBools)
          // only when all beats fire, inc deqPtrExt
          when(next_beatsOH === 0.U) {
            deqPtrExt := deqPtrExt + 1.U
          }
        }.otherwise {
          beat_valids(idx).foreach(_ := false.B)
          deqPtrExt := deqPtrExt + 1.U
        }
      }
    }
  }

  io.d_task.ready := !full

  // GrantBuf should always be ready.
  // If not, block reqs at the entrance of the pipeline when GrantBuf is about to be full.
  assert(!io.d_task.valid || io.d_task.ready) 

  io.e.ready := true.B
  io.e_resp := DontCare
  io.e_resp.valid := io.e.valid
  io.e_resp.mshrId := io.e.bits.sink
  io.e_resp.respInfo := DontCare
  io.e_resp.respInfo.opcode := GrantAck
  io.e_resp.respInfo.last := true.B

}