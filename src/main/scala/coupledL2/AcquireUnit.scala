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
import coupledL2.utils._
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config.Parameters

class AcquireUnit(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val sourceA = DecoupledIO(new TLBundleA(edgeOut.bundle))
    val tasks = Vec(mshrsAll, Flipped(DecoupledIO(new SourceAReq)))
  })

  val arbiter = Module(new FastArbiter(chiselTypeOf(io.tasks(0).bits), mshrsAll))
  for ((arb, req) <- arbiter.io.in.zip(io.tasks)) {
    arb <> req
  }
  val task = arbiter.io.out
  task.ready := io.sourceA.ready

  io.sourceA.valid := task.valid
  io.sourceA.bits.opcode := task.bits.opcode
  io.sourceA.bits.param := task.bits.param
  io.sourceA.bits.size := offsetBits.U
  io.sourceA.bits.source := task.bits.source
  io.sourceA.bits.address := Cat(task.bits.tag, task.bits.set, 0.U(offsetBits.W))
  io.sourceA.bits.mask := Fill(edgeOut.manager.beatBytes, 1.U(1.W))
  io.sourceA.bits.data := DontCare
  io.sourceA.bits.echo.lift(DirtyKey).foreach(_ := true.B)
  io.sourceA.bits.corrupt := false.B

  dontTouch(io)
}
