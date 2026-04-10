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
  val retry_s2 = Input(Bool())

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
  val task_s3 = io.s3.task
  val mshrReq_s1 = task_s1.bits.mshrTask
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

  val enqBits_s1 = Wire(new HintQueueEntry)
  enqBits_s1.source := Mux(task_s1.bits.mergeA, task_s1.bits.aMergeTask.sourceId, task_s1.bits.sourceId)
  enqBits_s1.isKeyword := Mux(task_s1.bits.mergeA,
    task_s1.bits.aMergeTask.isKeyword.getOrElse(false.B),
    task_s1.bits.isKeyword.getOrElse(false.B)
  )
  enqBits_s1.opcode := ParallelPriorityMux(
    Seq(
      mshr_Grant_s1 -> Grant,
      mshr_GrantData_s1 -> GrantData,
      chn_Release_s1 -> ReleaseAck,
      mshr_AccessAckData_s1 -> AccessAckData,
      mshr_CBOAck_s1 -> CBOAck
    )
  )

  // Hint for "chnTask Hit" will fire@s3
  val chn_Grant_s3     = task_s3.valid && !mshrReq_s3 && !need_mshr_s3 && isGrant(task_s3.bits)
  val chn_GrantData_s3 = task_s3.valid && !mshrReq_s3 && !need_mshr_s3 && isGrantData(task_s3.bits)
  val chn_AccessAckData_s3 = task_s3.valid && !mshrReq_s3 && !need_mshr_s3 && isAccessAckData(task_s3.bits)
  val enqBits_s3 = Wire(new HintQueueEntry)
  val enqValid_s3 = chn_Grant_s3 || chn_GrantData_s3 || chn_AccessAckData_s3
  enqBits_s3.source := task_s3.bits.sourceId
  enqBits_s3.isKeyword := task_s3.bits.isKeyword.getOrElse(false.B)
  enqBits_s3.opcode := ParallelPriorityMux(
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
  val canFlow_s1 = !hintQueue.io.deq.valid || hintQueue.io.count === 1.U && hintQueue.io.deq.fire
  val valid_s1 = mshr_GrantData_s1 || mshr_Grant_s1 || mshr_AccessAckData_s1 || mshr_CBOAck_s1 || chn_Release_s1
  val flow_s1, arbOut_s1, deqDrop_s1, enq_s3 = Wire(Decoupled(new HintQueueEntry))
  // noSpaceForSinkReq in GrantBuffer may ensure that these queues will not overflow
  assert(enq_s3.ready || !enq_s3.valid)

  // this will have at most 2 entries
  val hint_s1Queue = Module(new Queue(new HintQueueEntry, 1, pipe = true))
  hint_s1Queue.io.enq.valid := valid_s1 && (!canFlow_s1 || !flow_s1.ready)
  hint_s1Queue.io.enq.bits  := enqBits_s1
  assert(!valid_s1 || hint_s1Queue.io.enq.ready || flow_s1.ready)

  flow_s1.valid := valid_s1 && canFlow_s1
  flow_s1.bits := enqBits_s1

  deqDrop_s1.valid := hint_s1Queue.io.deq.valid && !io.retry_s2
  deqDrop_s1.bits := hint_s1Queue.io.deq.bits
  hint_s1Queue.io.deq.ready := deqDrop_s1.ready || io.retry_s2
  arb(Seq(deqDrop_s1, flow_s1), arbOut_s1, Some("s1"))

  val respWithDataFired = io.l1Hint.fire && io.l1Hint.bits.hasData
  enq_s3.valid := enqValid_s3
  enq_s3.bits := enqBits_s3
  arb(Seq(enq_s3,  arbOut_s1), hintQueue.io.enq, Some("Hint"))
  hintQueue.io.deq.ready := io.l1Hint.ready && !RegNext(respWithDataFired, false.B) ||
    io.retry_s2 && !hint_s1Queue.io.deq.valid

  io.l1Hint.valid := hintQueue.io.deq.valid && !(io.retry_s2 && !hint_s1Queue.io.deq.valid) && !RegNext(respWithDataFired, false.B)
  io.l1Hint.bits.sourceId := hintQueue.io.deq.bits.source
  io.l1Hint.bits.isKeyword := hintQueue.io.deq.bits.isKeyword
  io.l1Hint.bits.hasData := Seq(GrantData, AccessAckData).map(hintQueue.io.deq.bits.opcode === _).reduce(_||_)
}