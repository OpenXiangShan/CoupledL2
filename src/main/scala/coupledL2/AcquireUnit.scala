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
import chipsalliance.rocketchip.config.Parameters

class AcquireUnit(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val sourceA = DecoupledIO(new TLBundleA(edgeOut.bundle))
    val task = Flipped(DecoupledIO(new SourceAReq))
  })

  val a = io.sourceA
  io.task.ready := a.ready

  a.valid := io.task.valid
  a.bits.opcode := io.task.bits.opcode
  a.bits.param := io.task.bits.param
  a.bits.size := offsetBits.U
  a.bits.source := io.task.bits.source
  a.bits.address := Cat(io.task.bits.tag, io.task.bits.set, 0.U(offsetBits.W))
  a.bits.mask := Fill(edgeOut.manager.beatBytes, 1.U(1.W))
  a.bits.data := DontCare
  a.bits.echo.lift(DirtyKey).foreach(_ := true.B)
  a.bits.corrupt := false.B

  dontTouch(io)
}
