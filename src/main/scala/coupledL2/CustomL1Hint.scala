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
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.utils._

class HintQueueEntry(implicit p: Parameters) extends L2Bundle {
  val source = UInt(sourceIdBits.W)
  val opcode = UInt(4.W)
  val isKeyword = Bool()
}

class CustomL1HintIOBundle(implicit p: Parameters) extends L2Bundle {
  // input information
  val s1 = Flipped(ValidIO(new TaskBundle()))
  val s3 = new L2Bundle {
      val task      = Flipped(ValidIO(new TaskBundle()))
      val need_mshr = Input(Bool())
  }

  // output shadow D token stream
  val l1Hint = DecoupledIO(new L2HintShadowToken())
}

// Build a per-slice shadow D stream from early pipeline predictions.
class CustomL1Hint(implicit p: Parameters) extends L2Module {
  val io = IO(new CustomL1HintIOBundle)

  val task_s1 = io.s1
  val task_s3 = io.s3.task
  val mshrReq_s1 = task_s1.bits.mshrTask
  val mshrReq_s3 = task_s3.bits.mshrTask
  val need_mshr_s3 = io.s3.need_mshr

  def isRelease(t: TaskBundle): Bool = t.fromC && (t.opcode === Release || t.opcode === ReleaseData)
  def effOpcode(t: TaskBundle): UInt = Mux(t.mergeA, t.aMergeTask.opcode, t.opcode)
  def effSource(t: TaskBundle): UInt = Mux(t.mergeA, t.aMergeTask.sourceId, t.sourceId)
  def effIsKeyword(t: TaskBundle): Bool = Mux(
    t.mergeA,
    t.aMergeTask.isKeyword.getOrElse(false.B),
    t.isKeyword.getOrElse(false.B)
  )
  def usesSourceD(t: TaskBundle): Bool = t.fromA && (t.opcode =/= AccessAck) && (t.opcode =/= HintAck || t.mergeA)
  def toShadowToken(entry: HintQueueEntry, firstBeat: Bool, beats1: UInt): L2HintShadowToken = {
    val token = Wire(new L2HintShadowToken)
    token.sourceId := entry.source
    token.opcode := entry.opcode
    token.isKeyword := entry.isKeyword
    token.firstBeat := firstBeat
    token.beats1 := beats1
    token
  }

  // ==================== Shadow Event Generation ====================
  val mshr_dResp_s1 = task_s1.valid && mshrReq_s1 && usesSourceD(task_s1.bits)
  val chn_Release_s1 = task_s1.valid && !mshrReq_s1 && isRelease(task_s1.bits)

  val enqValid_s1 = mshr_dResp_s1 || chn_Release_s1
  val enqSource_s1 = Mux(chn_Release_s1, task_s1.bits.sourceId, effSource(task_s1.bits))
  val enqKeyWord_s1 = Mux(chn_Release_s1, task_s1.bits.isKeyword.getOrElse(false.B), effIsKeyword(task_s1.bits))
  val enqOpcode_s1 = Mux(chn_Release_s1, ReleaseAck, effOpcode(task_s1.bits))

  val chn_dResp_s3 = task_s3.valid && !mshrReq_s3 && !need_mshr_s3 && usesSourceD(task_s3.bits)
  val enqValid_s3 = chn_dResp_s3
  val enqSource_s3 = effSource(task_s3.bits)
  val enqKeyWord_s3 = effIsKeyword(task_s3.bits)
  val enqOpcode_s3 = effOpcode(task_s3.bits)

  // ==================== Shadow Event Queue ====================
  val hintEntries = mshrsAll * 2
  val hintQueue = Module(new Queue(new HintQueueEntry, hintEntries))

  // this will have at most 2 entries
  val hint_s1Queue = Module(new Queue(new HintQueueEntry, 8, flow = true))
  hint_s1Queue.io.enq.valid := enqValid_s1
  hint_s1Queue.io.enq.bits.opcode := enqOpcode_s1
  hint_s1Queue.io.enq.bits.source := enqSource_s1
  hint_s1Queue.io.enq.bits.isKeyword := enqKeyWord_s1
  hint_s1Queue.io.deq.ready := hintQueue.io.enq.ready && !enqValid_s3
  // WARNING:TODO: ensure queue will never overflow
  assert(hint_s1Queue.io.enq.ready, "hint_s1Queue should never be full")
  assert(hintQueue.io.enq.ready, "hintQueue should never be full")

  hintQueue.io.enq.valid := enqValid_s3 || hint_s1Queue.io.deq.valid
  hintQueue.io.enq.bits.opcode := Mux(enqValid_s3, enqOpcode_s3, hint_s1Queue.io.deq.bits.opcode)
  hintQueue.io.enq.bits.source := Mux(enqValid_s3, enqSource_s3, hint_s1Queue.io.deq.bits.source)
  hintQueue.io.enq.bits.isKeyword := Mux(enqValid_s3, enqKeyWord_s3, hint_s1Queue.io.deq.bits.isKeyword)

  val shadowTokenBufValid = RegInit(false.B)
  val shadowTokenBuf = RegInit(0.U.asTypeOf(new L2HintShadowToken))
  val firstToken = toShadowToken(
    hintQueue.io.deq.bits,
    firstBeat = true.B,
    beats1 = Mux(
      hintQueue.io.deq.bits.opcode(0),
      (beatSize - 1).U(scala.math.max(1, log2Ceil(beatSize)).W),
      0.U
    )
  )
  val secondToken = toShadowToken(
    hintQueue.io.deq.bits,
    firstBeat = false.B,
    beats1 = 0.U
  )

  hintQueue.io.deq.ready := io.l1Hint.ready && !shadowTokenBufValid

  when (hintQueue.io.deq.valid && io.l1Hint.ready && !shadowTokenBufValid && hintQueue.io.deq.bits.opcode(0)) {
    shadowTokenBufValid := true.B
    shadowTokenBuf := secondToken
  }
  when (shadowTokenBufValid && io.l1Hint.ready) {
    shadowTokenBufValid := false.B
  }

  io.l1Hint.valid := shadowTokenBufValid || hintQueue.io.deq.valid
  io.l1Hint.bits := Mux(shadowTokenBufValid, shadowTokenBuf, firstToken)
}
