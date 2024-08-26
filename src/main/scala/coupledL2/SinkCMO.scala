/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
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
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import org.chipsalliance.cde.config.Parameters
import utility.MemReqSource


// SinkCMO receives upwards CMO_Inst Req, and send it to RequestArb directly
class SinkCMO(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val cmoReq = Flipped(DecoupledIO(new RVA23CMOReq()))
    val task = DecoupledIO(new TaskBundle)
  })

  val task = Wire(new TaskBundle)
  task := 0.U.asTypeOf(new TaskBundle)
  task.cmoTask := true.B
  task.channel := "b000".U
  task.txChannel := 0.U
  task.tag := parseAddress(io.cmoReq.bits.address)._1
  task.set := parseAddress(io.cmoReq.bits.address)._2
  task.off := parseAddress(io.cmoReq.bits.address)._3
  task.alias.foreach(_ := 0.U)
  task.vaddr.foreach(_ := 0.U)
  task.isKeyword.foreach(_ := false.B)
  task.opcode := io.cmoReq.bits.opcode
  task.param := 0.U
  task.size := 0.U
  task.sourceId := 0.U(sourceIdBits.W)
  task.bufIdx := 0.U(bufIdxBits.W)
  task.needProbeAckData := false.B
  task.mshrTask := false.B
  task.mshrId := 0.U(mshrBits.W)
  task.aliasTask.foreach(_ := false.B)
  task.useProbeData := false.B
  task.mshrRetry := false.B
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

  io.task.valid := io.cmoReq.valid
  io.task.bits := task
  io.cmoReq.ready := io.task.ready
}
