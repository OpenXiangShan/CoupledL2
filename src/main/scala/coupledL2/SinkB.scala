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
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.utils.XSPerfAccumulate

class BMergeTask(implicit p: Parameters) extends L2Bundle {
  val id = UInt(mshrBits.W)
  val task = new TaskBundle()
}

class SinkB(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val b = Flipped(DecoupledIO(new TLBundleB(edgeIn.bundle)))
    val task = DecoupledIO(new TaskBundle)
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
    val bMergeTask = ValidIO(new BMergeTask)
  })

  def fromTLBtoTaskBundle(b: TLBundleB): TaskBundle = {
    val task = Wire(new TaskBundle)
    task := DontCare
    task.channel := "b010".U
    task.tag := parseAddress(b.address)._1
    task.set := parseAddress(b.address)._2
    task.off := parseAddress(b.address)._3
    task.alias.foreach(_ := 0.U)
    task.opcode := b.opcode
    task.param := b.param
    task.size := b.size
    task.needProbeAckData := b.data(0) // TODO: parameterize this
    task.mshrTask := false.B
    task.fromL2pft.foreach(_ := false.B)
    task.needHint.foreach(_ := false.B)
    task.wayMask := Fill(cacheParams.ways, "b1".U)
    task
  }
  val task = fromTLBtoTaskBundle(io.b.bits)

  /* ======== Merge Nested-B req ======== */
  // unable to accept incoming B req because same-addr as some MSHR REQ
  val addrConflict = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.reqTag === task.tag && !s.bits.willFree
  )).asUInt.orR

  // unable to accept incoming B req because same-addr as some MSHR replaced block and cannot nest
  val replaceConflictMask = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && !s.bits.nestB
  )).asUInt
  val replaceConflict = replaceConflictMask.orR

  // incoming B is nested with some MSHR replaced block and able to be accepted
  val nestBMask = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && s.bits.nestB
  )).asUInt

  assert(PopCount(replaceConflictMask) <= 1.U)
  assert(PopCount(nestBMask) <= 1.U)

  val nestB = nestBMask.orR
  val nestBId = OHToUInt(nestBMask)

  // when conflict, we block B req from entering SinkB
  // when !conflict and nestB , we merge B req to MSHR
  // when !conflict and !nestB, we let B req enter MainPipe
  io.task.valid := io.b.valid && !addrConflict && !replaceConflict && !nestB
  io.task.bits  := task
  io.b.ready :=  nestB || (io.task.ready && !addrConflict && !replaceConflict)

  io.bMergeTask.valid := io.b.valid && nestB
  io.bMergeTask.bits.id := nestBId
  io.bMergeTask.bits.task := task

  // TODO: add conflict XSPerf counter
}
