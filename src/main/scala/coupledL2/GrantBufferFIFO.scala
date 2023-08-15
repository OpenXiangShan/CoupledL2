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
import coupledL2.prefetch.PrefetchResp
import coupledL2.utils.{XSPerfAccumulate, XSPerfHistogram, XSPerfMax}
import utility._

// Communicate with L1
// Send out Grant/GrantData/ReleaseAck from d and
// receive GrantAck through e
// ** L1 is non-blocking for Grant
// ** FIFO version of GrantBuffer
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

  // used by prefetch, update deqPtrExt
  val flush = RegInit(VecInit(Seq.fill(mshrsAll) {
    false.B
  }))

  // =========== Counting Hint Cycles ===========
  // hint interface: l2 will send hint to l1 before sending grantData (3 cycle ahead)
  val globalCounter = RegInit(0.U((log2Ceil(mshrsAll) + 1).W))
  val beat_counters = RegInit(VecInit(Seq.fill(mshrsAll) {
    0.U((log2Ceil(mshrsAll) + 1).W)
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
  val hint_valid_vec = beat_counters.zip(taskAll).map{case (counter, task) => { counter === hintCycleAhead.U && task.opcode === GrantData && !task.fromL2pft.getOrElse(false.B) }}
  val sourceid_vec = taskAll.map{case task => task.sourceId}

  io.l1Hint.valid := VecInit(hint_valid_vec).asUInt.orR
  io.l1Hint.bits.sourceId := ParallelMux(hint_valid_vec zip sourceid_vec)
  assert(PopCount(VecInit(hint_valid_vec)) <= 1.U)

  beat_counters.foreach {
    case (counter) => {
      counter := Mux(counter === 0.U, 0.U, counter - 1.U)
    }
  }

  // =========== save d_task in entries, waiting to fire ===========
  when(io.d_task.fire() && !(io.d_task.bits.task.opcode === HintAck && !io.d_task.bits.task.fromL2pft.getOrElse(false.B))) {
    beat_valids(enqPtr).foreach(_ := true.B)
    taskAll(enqPtr) := io.d_task.bits.task
    dataAll(enqPtr) := io.d_task.bits.data
    beat_counters(enqPtr) := globalCounter
    enqPtrExt := enqPtrExt + 1.U
  }

  // =========== fire at D channel ===========
  io.d.valid := false.B
  io.d.bits := 0.U.asTypeOf(io.d.bits)
  for(idx <- (0 until mshrsAll)) {
    when(deqPtr === idx.U) {
      io.d.valid := block_valids(idx) && taskAll(idx).opcode =/= HintAck // L1 does not need HintAck (for now)
      val data = dataAll(idx).data
      val beatsOH = beat_valids(idx).asUInt
      val (beat, next_beatsOH) = getBeat(data, beatsOH)
      io.d.bits := toTLBundleD(taskAll(idx), beat)
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

  io.prefetchResp.zip(pft_resps).foreach {
    case (out, ins) =>
      ins.zipWithIndex.foreach {
        case (in, i) =>
          in.valid := block_valids(i) && taskAll(i).opcode === HintAck
          in.bits.tag := taskAll(i).tag
          in.bits.set := taskAll(i).set
          when (in.fire()) {
            beat_valids(i).foreach(_ := false.B)
            flush(i) := true.B
          }
      }
      fastArb(ins, out, Some("pft_resp_arb"))
  }

  when(flush(deqPtr)) {
    flush(deqPtr) := false.B
    deqPtrExt := deqPtrExt + 1.U
  }
}
