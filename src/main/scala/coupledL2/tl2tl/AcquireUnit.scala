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

package coupledL2.tl2tl

import chisel3._
import chisel3.util._
import utility._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import org.chipsalliance.cde.config.Parameters
import huancun.{DirtyKey, PreferCacheKey}
import coupledL2._

class AcquireUnit(implicit p: Parameters) extends TL2TLL2Module {
  val io = IO(new Bundle() {
    val sourceA = DecoupledIO(new TLBundleA(edgeOut.bundle))
    val task = Flipped(DecoupledIO(new SourceAReq))
  })

  val a = io.sourceA
  val task = io.task.bits

  a.bits.opcode := task.opcode
  a.bits.param := task.param
  a.bits.size := offsetBits.U
  a.bits.source := task.source
  a.bits.address := Cat(task.tag, task.set, 0.U(offsetBits.W))
  a.bits.mask := Fill(edgeOut.manager.beatBytes, 1.U(1.W))
  a.bits.data := DontCare
  a.bits.echo.lift(DirtyKey).foreach(_ := true.B)
  a.bits.user.lift(PreferCacheKey).foreach(_ := false.B)
  a.bits.user.lift(utility.ReqSourceKey).foreach(_ := task.reqSource)
  a.bits.corrupt := false.B

  a.valid := io.task.valid
  io.task.ready := a.ready

  dontTouch(io)
}
