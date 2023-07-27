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
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import chipsalliance.rocketchip.config.Parameters
import huancun.{PreferCacheKey}

class AcquireUnit(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val sourceA = DecoupledIO(new TLBundleA(edgeOut.bundle))
    val task = Flipped(DecoupledIO(new SourceAReq))
    val pbRead = DecoupledIO(new PutBufferRead)
    val pbResp = Flipped(ValidIO(new PutBufferEntry))
  })

  val a = io.sourceA
  val a_out = Wire(a.cloneType)
  val a_acquire = Wire(a.cloneType)
  val a_put = Wire(a.cloneType)
  val task = io.task.bits
  val put = task.opcode === PutFullData || task.opcode === PutPartialData
  val busy = RegInit(false.B)

  when (io.task.fire() && put) {
    busy := true.B
  }

  val s1_ready = Wire(Bool())
  val s1_valid = RegInit(false.B)

  // S0: read putBuffer
  val s0_task = RegEnable(task, 0.U.asTypeOf(task), io.task.fire() && put)
  val s0_count = RegInit(0.U(beatBits.W))
  val s0_last = s0_count === Mux(s0_task.size === log2Ceil(blockBytes).U, (beatSize-1).U, 0.U) // TODO
  val s0_valid = io.pbRead.fire()

  when(io.pbRead.fire()) {
    s0_count := s0_count + 1.U
    when (s0_last) {
      busy := false.B
      s0_count := 0.U
    }
  }

  // S1: get putBuffer and transfer to outer A
  val s1_latch = s0_valid && s1_ready
  val s1_task = RegEnable(s0_task, 0.U.asTypeOf(s0_task), s1_latch)
  val s1_cango = a_put.ready
  val s1_pb_latch = HoldUnless(io.pbResp.bits, RegNext(s1_latch, false.B))

  s1_ready := s1_cango || !s1_valid

  when (s1_valid && s1_cango) { s1_valid := false.B }
  when (s1_latch) { s1_valid := true.B }

  a_acquire.valid := io.task.valid && !put
  a_acquire.bits.opcode := task.opcode
  a_acquire.bits.param := task.param
  a_acquire.bits.size := offsetBits.U
  a_acquire.bits.source := task.source
  a_acquire.bits.address := Cat(task.tag, task.set, 0.U(offsetBits.W))
  a_acquire.bits.mask := Fill(edgeOut.manager.beatBytes, 1.U(1.W))
  a_acquire.bits.data := 0.U((edgeOut.manager.beatBytes * 8).W)
  a_acquire.bits.echo.lift(DirtyKey).foreach(_ := true.B)
  a_acquire.bits.user.lift(PreferCacheKey).foreach(_ := false.B)
  a_acquire.bits.user.lift(utility.ReqSourceKey).foreach(_ := task.reqSource)
  a_acquire.bits.corrupt := false.B

  a_put.valid := s1_valid
  a_put.bits.opcode := s1_task.opcode
  a_put.bits.param := s1_task.param
  a_put.bits.size := s1_task.size // TODO
  a_put.bits.source := s1_task.source
  a_put.bits.address := Cat(s1_task.tag, s1_task.set, s1_task.off)
  a_put.bits.echo.lift(DirtyKey).foreach(_ := true.B)
  a_put.bits.user.lift(PreferCacheKey).foreach(_ := false.B)
  a_put.bits.user.lift(utility.ReqSourceKey).foreach(_ := MemReqSource.NoWhere.id.U) //Ignore: where does Put comes from
  a_put.bits.mask := s1_pb_latch.mask
  a_put.bits.data := s1_pb_latch.data.data
  a_put.bits.corrupt := false.B

  TLArbiter.lowest(edgeOut, a_out, a_put, a_acquire)
  io.sourceA <> a_out
  io.sourceA.valid := a_out.valid && !(a_acquire.valid && !a_put.valid && busy)

  io.task.ready := a_acquire.ready && !busy

  io.pbRead.valid := busy && s1_ready
  io.pbRead.bits.idx := s0_task.pbIdx
  io.pbRead.bits.count := s0_count

  dontTouch(io)
}
