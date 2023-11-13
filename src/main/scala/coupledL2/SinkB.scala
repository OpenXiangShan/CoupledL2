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
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import coupledL2.utils.XSPerfAccumulate
import utility.MemReqSource

class SinkB(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val b = Flipped(DecoupledIO(new TLBundleB(edgeIn.bundle)))
    val task = DecoupledIO(new TaskBundle)
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
  })

  def fromTLBtoTaskBundle(b: TLBundleB): TaskBundle = {
    val task = Wire(new TaskBundle)
    task.channel := "b010".U
    task.tag := parseAddress(b.address)._1
    task.set := parseAddress(b.address)._2
    task.off := parseAddress(b.address)._3
    task.alias.foreach(_ := 0.U)
    task.vaddr.foreach(_ := 0.U)
    task.opcode := b.opcode
    task.param := b.param
    task.size := b.size
    task.sourceId := 0.U(sourceIdBits.W)
    task.bufIdx := 0.U(bufIdxBits.W)
    task.needProbeAckData := b.data(0) // TODO: parameterize this
    task.mshrTask := false.B
    task.mshrId := 0.U(mshrBits.W)
    task.aliasTask.foreach(_ := false.B)
    task.useProbeData := false.B
    task.fromL2pft.foreach(_ := false.B)
    task.needHint.foreach(_ := false.B)
    task.dirty := false.B
    task.way := 0.U(wayBits.W)
    task.meta := 0.U.asTypeOf(new MetaEntry)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dsWen := false.B
    task.wayMask := Fill(cacheParams.ways, "b1".U)
    task.reqSource := MemReqSource.NoWhere.id.U // Ignore
    task.replTask := false.B
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    task
  }
  val task = fromTLBtoTaskBundle(io.b.bits)

  // unable to accept incoming B req because same-addr as some MSHR REQ
  val addrConflict = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.reqTag === task.tag && !s.bits.willFree && !s.bits.nestB
  )).asUInt.orR

  // unable to accept incoming B req because same-addr as some MSHR replaced block and cannot nest
  val replaceConflictMask = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && s.bits.releaseNotSent
  )).asUInt
  val replaceConflict = replaceConflictMask.orR

  // when conflict, we block B req from entering SinkB
  io.task.valid := io.b.valid && !addrConflict && !replaceConflict
  io.task.bits  := task
  io.b.ready := io.task.ready && !addrConflict && !replaceConflict
}
