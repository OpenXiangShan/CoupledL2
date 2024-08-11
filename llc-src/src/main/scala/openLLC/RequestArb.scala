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

package openLLC

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2chi.HasCHIOpcodes
import scala.math.min

class RequestArb(implicit p: Parameters) extends LLCModule with HasClientInfo with HasCHIOpcodes {
  val io = IO(new Bundle() {
    /* receive incoming tasks from s1 */
    val busTask_s1 = Flipped(DecoupledIO(new Task()))
    val refillTask_s1 = Flipped(DecoupledIO(new Task()))

    /* read local/client directory */
    val dirRead_s1 = DecoupledIO(new DirRead())

    /* send task to mainPipe */
    val taskToPipe_s2 = ValidIO(new Task())

    /* send refillBuf read request */
    val refillBufRead_s2 = ValidIO(new RefillBufRead())

    /* handle set conflict, capacity conflict and coherency conflict */
    val pipeInfo = Input(new PipeStatus())
    val refillInfo = Flipped(Vec(mshrs.refill, ValidIO(new BlockInfo())))
    val respInfo = Flipped(Vec(mshrs.response, ValidIO(new ResponseInfo())))
    val snpInfo = Flipped(Vec(mshrs.snoop, ValidIO(new BlockInfo)))
    val memInfo = Flipped(Vec(mshrs.memory, ValidIO(new BlockInfo)))
  })

  val pipeInfo   = io.pipeInfo
  val refillInfo = io.refillInfo
  val respInfo   = io.respInfo
  val snpInfo    = io.snpInfo
  val memInfo    = io.memInfo

  val task_s1 = Wire(Valid(new Task()))
  val task_s2 = Wire(Valid(new Task()))

  /* Stage 1 */
  def minSetBits = min(setBits, clientSetBits)

  val tag_s1   = task_s1.bits.tag
  val set_s1   = task_s1.bits.set
  val reqID_s1 = task_s1.bits.reqID

  val isCleanInvalid_s1 = !task_s1.bits.refillTask && task_s1.bits.chiOpcode === CleanInvalid
  val isCleanShared_s1 = !task_s1.bits.refillTask && task_s1.bits.chiOpcode === CleanShared
  val isClean_s1 = isCleanInvalid_s1 || isCleanShared_s1

  // To prevent data hazards caused by read-after-write conflicts in the directory,
  // blocking is required when the set of s1 is the same as that of s2 or s3
  val sameSet_s2 = pipeInfo.s2_valid && pipeInfo.s2_set(minSetBits - 1, 0) === set_s1(minSetBits - 1, 0)
  val sameSet_s3 = pipeInfo.s3_valid && pipeInfo.s3_set(minSetBits - 1, 0) === set_s1(minSetBits - 1, 0)
  // Snoop tasks caused by replacements may be issued at S4 stage,
  // so blocking is required when the set of S1 matches S4.
  val sameSet_s4  = pipeInfo.s4_valid && pipeInfo.s4_set(minSetBits - 1, 0) === set_s1(minSetBits - 1, 0)
  val sameAddr_s5 = pipeInfo.s5_valid && Cat(pipeInfo.s5_tag, pipeInfo.s5_set) === Cat(tag_s1, set_s1)
  val sameAddr_s6 = pipeInfo.s6_valid && Cat(pipeInfo.s6_tag, pipeInfo.s6_set) === Cat(tag_s1, set_s1)

  // Since the stages within the MainPipe are non-blocking, when the sum of the requests being processed
  // in the buffer and the potential requests that might occupy the buffer in the MainPipe exceeds the 
  // total number of buffer entries, new requests need to be blocked from entering the MainPipe.
  val inflight_refill    = PopCount(refillInfo.map(e => e.valid))
  val inflight_snoop     = PopCount(snpInfo.map(e => e.valid))
  val inflight_response  = PopCount(respInfo.map(e => e.valid))
  val inflight_memAccess = PopCount(memInfo.map(e => e.valid))
  val potential_refill, potential_snoop = PopCount(Seq(pipeInfo.s2_valid, pipeInfo.s3_valid, pipeInfo.s4_valid))
  val potential_response, potential_memAccess = PopCount(pipeInfo.valids)

  val blockByMainPipe = sameSet_s2 || sameSet_s3 || sameSet_s4 || sameAddr_s5 || sameAddr_s6
  val blockByRefill = !task_s1.bits.refillTask && (
    Cat(refillInfo.map(e => e.valid && Cat(e.bits.tag, e.bits.set) === Cat(tag_s1, set_s1))).orR ||
    Cat(refillInfo.map(e => e.valid && e.bits.reqID === reqID_s1)).orR ||
    (inflight_refill +& potential_refill) >= mshrs.refill.U
  )
  val blockByResp = !task_s1.bits.refillTask && (
    Cat(respInfo.map(e => e.valid && Cat(e.bits.tag, e.bits.set) === Cat(tag_s1, set_s1))).orR ||
    Cat(respInfo.map(e => e.valid && e.bits.reqID === reqID_s1)).orR ||
    (inflight_response +& potential_response) >= mshrs.response.U
  )
  val blockBySnp = !task_s1.bits.refillTask && (
    Cat(snpInfo.map(e => e.valid && e.bits.reqID === reqID_s1)).orR ||
    (inflight_snoop +& potential_snoop) >= mshrs.snoop.U
  )
  val blockByMem = Cat(memInfo.map(e => e.valid && Cat(e.bits.tag, e.bits.set) === Cat(tag_s1, set_s1) &&
    e.bits.opcode === WriteNoSnpFull && (task_s1.bits.refillTask  || isClean_s1))).orR ||
    Cat(memInfo.map(e => e.valid && e.bits.reqID === reqID_s1 && !task_s1.bits.refillTask)).orR ||
    (inflight_memAccess +& potential_memAccess) >= mshrs.memory.U

  val blockEntrance = blockByMainPipe || blockByRefill || blockByResp || blockByMem

  task_s1.valid := io.dirRead_s1.ready && (io.busTask_s1.valid || io.refillTask_s1.valid) && !blockEntrance
  task_s1.bits := Mux(io.refillTask_s1.valid, io.refillTask_s1.bits, io.busTask_s1.bits)

  io.busTask_s1.ready := io.dirRead_s1.ready && !io.refillTask_s1.valid && !blockEntrance
  io.refillTask_s1.ready := io.dirRead_s1.ready && !blockEntrance

  def addrConnect(lset: UInt, ltag: UInt, rset: UInt, rtag: UInt) = {
    assert(lset.getWidth + ltag.getWidth == rset.getWidth + rtag.getWidth)
    val addr = Cat(rtag, rset)
    lset := addr.tail(ltag.getWidth)
    ltag := addr.head(ltag.getWidth)
  }

  // Meta read request
  io.dirRead_s1.valid := task_s1.valid
  val rports = Seq(io.dirRead_s1.bits.self, io.dirRead_s1.bits.clients)
  rports.foreach { p =>
    addrConnect(p.set, p.tag, set_s1, tag_s1)
    p.replacerInfo.opcode := task_s1.bits.chiOpcode
    p.replacerInfo.refill := task_s1.bits.refillTask
  }

  /* Stage 2 */
  task_s2.valid := RegNext(task_s1.valid, false.B)
  task_s2.bits := RegEnable(task_s1.bits, task_s1.valid)

  io.taskToPipe_s2 := task_s2

  io.refillBufRead_s2.valid := task_s2.valid && task_s2.bits.refillTask
  io.refillBufRead_s2.bits.id := task_s2.bits.bufID

}
