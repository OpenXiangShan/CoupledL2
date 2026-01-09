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
  val s2 = new L2Bundle {
      val task  = Flipped(ValidIO(new TaskBundle()))
      val retry = Input(Bool())
  }

  val s3 = new L2Bundle {
      val task      = Flipped(ValidIO(new TaskBundle()))
      val need_mshr = Input(Bool())
  }

  // output hint
  val l1Hint = DecoupledIO(new L2ToL1HintInsideL2())
}

// grantData hint interface
// use this interface to give a hint to l1 before actually sending a GrantData
class CustomL1Hint(implicit p: Parameters) extends L2Module {
  val io = IO(new CustomL1HintIOBundle)

  val task_s1 = io.s1
  val task_s2 = io.s2.task
  val task_s3 = io.s3.task
  val mshrReq_s1 = task_s1.bits.mshrTask
  val mshrReq_s2 = task_s2.bits.mshrTask
  val mshrReq_s3 = task_s3.bits.mshrTask
  val mergeA_s1  = task_s1.bits.mergeA
  val need_mshr_s3 = io.s3.need_mshr

  def isGrantData(t: TaskBundle):  Bool = t.fromA && t.opcode === GrantData
  def isGrant(t: TaskBundle):      Bool = t.fromA && t.opcode === Grant
  def isHintAck(t: TaskBundle):    Bool = t.fromA && t.opcode === HintAck // HintAck has no effect on Hint
  def isRelease(t: TaskBundle):    Bool = t.fromC && (t.opcode === Release || t.opcode === ReleaseData)
  def isMergeGrantData(t: TaskBundle): Bool = t.fromA && t.mergeA && t.aMergeTask.opcode === GrantData
  def isMergeGrant(t: TaskBundle):     Bool = t.fromA && t.mergeA && t.aMergeTask.opcode === Grant
  def isAccessAckData(t: TaskBundle):  Bool = t.fromA && t.opcode === AccessAckData
  def isCBOAck(t: TaskBundle):         Bool = t.fromA && t.opcode === CBOAck

  // ==================== Hint Generation ====================
  // Hint for "MSHRTask and ReleaseAck" will fire@s1
  val mshr_GrantData_s1     = task_s1.valid &&  mshrReq_s1 && (isGrantData(task_s1.bits) || isMergeGrantData(task_s1.bits))
  val mshr_Grant_s1         = task_s1.valid &&  mshrReq_s1 && (isGrant(task_s1.bits) || isMergeGrant(task_s1.bits))
  val mshr_AccessAckData_s1 = task_s1.valid &&  mshrReq_s1 && isAccessAckData(task_s1.bits)
  val mshr_CBOAck_s1        = task_s1.valid &&  mshrReq_s1 && isCBOAck(task_s1.bits)
  val chn_Release_s1        = task_s1.valid && !mshrReq_s1 && isRelease(task_s1.bits)

  val enqValid_s1 = mshr_GrantData_s1 || mshr_Grant_s1 || mshr_AccessAckData_s1 || mshr_CBOAck_s1 || chn_Release_s1
  val enqSource_s1 = Mux(task_s1.bits.mergeA, task_s1.bits.aMergeTask.sourceId, task_s1.bits.sourceId)
  val enqKeyWord_s1 = Mux(task_s1.bits.mergeA,
    task_s1.bits.aMergeTask.isKeyword.getOrElse(false.B),
    task_s1.bits.isKeyword.getOrElse(false.B)
  )
  val enqOpcode_s1 = ParallelPriorityMux(
    Seq(
      mshr_Grant_s1 -> Grant,
      mshr_GrantData_s1 -> GrantData,
      chn_Release_s1 -> ReleaseAck,
      mshr_AccessAckData_s1 -> AccessAckData,
      mshr_CBOAck_s1 -> CBOAck
    )
  )

  val mshr_GrantDataRetry_s2 = task_s2.valid && mshrReq_s2 &&
    (isGrantData(task_s2.bits) || isMergeGrantData(task_s2.bits)) && io.s2.retry && task_s2.bits.replTask
  val mshr_GrantRetry_s2 = task_s2.valid && mshrReq_s2 &&
    (isGrant(task_s2.bits) || isMergeGrant(task_s2.bits)) && io.s2.retry && task_s2.bits.replTask
  val mshr_AccessRetry_s2 = task_s2.valid && mshrReq_s2 &&
    isAccessAckData(task_s2.bits) && io.s2.retry && task_s2.bits.replTask
  val enqValid_s2 = mshr_GrantRetry_s2 || mshr_GrantDataRetry_s2 || mshr_AccessRetry_s2
  val enqSource_s2 = Mux(task_s2.bits.mergeA, task_s2.bits.aMergeTask.sourceId, task_s2.bits.sourceId)
  val enqKeyWord_s2 = Mux(task_s2.bits.mergeA,
    task_s2.bits.aMergeTask.isKeyword.getOrElse(false.B),
    task_s2.bits.isKeyword.getOrElse(false.B)
  )
  val enqOpcode_s2 = ParallelPriorityMux(
    Seq(
      mshr_GrantDataRetry_s2 -> GrantData,
      mshr_GrantRetry_s2 -> Grant,
      mshr_AccessRetry_s2 -> AccessAckData
    )
  )


  // Hint for "chnTask Hit" will fire@s3
  val respWithDataFired = io.l1Hint.fire && io.l1Hint.bits.hasData
  val grantCanFlow = Wire(Bool())
  val chn_Grant_s3     = task_s3.valid && !mshrReq_s3 && !need_mshr_s3 && isGrant(task_s3.bits)
  val chn_GrantData_s3 = task_s3.valid && !mshrReq_s3 && !need_mshr_s3 && isGrantData(task_s3.bits)
  val chn_AccessAckData_s3 = task_s3.valid && !mshrReq_s3 && !need_mshr_s3 && isAccessAckData(task_s3.bits)
  val enqValid_s3 = (chn_Grant_s3 && (!grantCanFlow || RegNext(respWithDataFired, false.B) || !io.l1Hint.ready)) || chn_GrantData_s3 || chn_AccessAckData_s3
  val enqSource_s3 = task_s3.bits.sourceId
  val enqKeyWord_s3 = task_s3.bits.isKeyword.getOrElse(false.B)
  val enqOpcode_s3 = ParallelPriorityMux(
    Seq(
      chn_Grant_s3 -> Grant,
      chn_GrantData_s3 -> GrantData,
      chn_AccessAckData_s3 -> AccessAckData
    )
  )

  // ==================== Hint Queue ====================
  val hintEntries = mshrsAll
  val hintEntriesWidth = log2Ceil(hintEntries)
  val hintQueue = Module(new Queue(new HintQueueEntry, hintEntries))
  val hintNeedCancel = Wire(Bool())
  grantCanFlow := !hintQueue.io.deq.valid

  // this will have at most 2 entries
  val hint_s1Queue = Module(new Queue(new HintQueueEntry, 4, flow = true))
  hint_s1Queue.io.enq.valid := enqValid_s1
  hint_s1Queue.io.enq.bits.opcode := enqOpcode_s1
  hint_s1Queue.io.enq.bits.source := enqSource_s1
  hint_s1Queue.io.enq.bits.isKeyword := enqKeyWord_s1
  hint_s1Queue.io.deq.ready := hintQueue.io.enq.ready && !enqValid_s3
  // WARNING:TODO: ensure queue will never overflow
  assert(hint_s1Queue.io.enq.ready, "hint_s1Queue should never be full")
  assert(hintQueue.io.enq.ready || !hintQueue.io.enq.valid, "hintQueue should never be full")

  val hint_s2Queue = Module(new Queue(new HintQueueEntry, 4, flow = true))
  val s2Deq = hint_s2Queue.io.deq
  hint_s2Queue.io.enq.valid := enqValid_s2
  hint_s2Queue.io.enq.bits.opcode := enqOpcode_s2
  hint_s2Queue.io.enq.bits.source := enqSource_s2
  hint_s2Queue.io.enq.bits.isKeyword := enqKeyWord_s2
  hint_s2Queue.io.deq.ready := hintQueue.io.deq.fire && hintQueue.io.deq.bits === s2Deq.bits
  assert(hint_s2Queue.io.enq.ready, "hint_s2Queue should never be full")

  // For AccessAckData that would never go to LSU, insert a dequing bubble for channel delay accuracy

  hintQueue.io.enq.valid := enqValid_s3 || hint_s1Queue.io.deq.valid
  hintQueue.io.enq.bits.opcode := Mux(enqValid_s3, enqOpcode_s3, hint_s1Queue.io.deq.bits.opcode)
  hintQueue.io.enq.bits.source := Mux(enqValid_s3, enqSource_s3, hint_s1Queue.io.deq.bits.source)
  hintQueue.io.enq.bits.isKeyword := Mux(enqValid_s3, enqKeyWord_s3, hint_s1Queue.io.deq.bits.isKeyword)
  hintQueue.io.deq.ready := io.l1Hint.ready && !RegNext(respWithDataFired, false.B) || hintNeedCancel

  hintNeedCancel := hintQueue.io.deq.valid && s2Deq.valid && hintQueue.io.deq.bits === s2Deq.bits

  io.l1Hint.valid := (hintQueue.io.deq.valid && !hintNeedCancel || grantCanFlow && chn_Grant_s3) && !RegNext(respWithDataFired, false.B)
  io.l1Hint.bits.sourceId := Mux(grantCanFlow && chn_Grant_s3, task_s3.bits.sourceId, hintQueue.io.deq.bits.source)
  io.l1Hint.bits.isKeyword := Mux(grantCanFlow && chn_Grant_s3, task_s3.bits.isKeyword.getOrElse(false.B), hintQueue.io.deq.bits.isKeyword)
  io.l1Hint.bits.hasData := Mux(grantCanFlow && chn_Grant_s3, false.B, Seq(GrantData, AccessAckData).map(hintQueue.io.deq.bits.opcode === _).reduce(_||_))
}