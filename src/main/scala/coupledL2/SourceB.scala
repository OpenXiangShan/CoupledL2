
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

class SourceB(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val sourceB = DecoupledIO(new TLBundleB(edgeIn.bundle))
    val task = Flipped(DecoupledIO(new SourceBReq))
  })

  val b = io.sourceB
  io.task.ready := b.ready

  b.valid := io.task.valid
  b.bits.opcode := io.task.bits.opcode
  b.bits.param := io.task.bits.param
  b.bits.size := offsetBits.U
  b.bits.source := 0.U // make sure there are only 1 client
  b.bits.address := Cat(io.task.bits.tag, io.task.bits.set, 0.U(offsetBits.W))
  b.bits.mask := Fill(beatBytes, 1.U(1.W))
  b.bits.data := Cat(io.task.bits.alias, 0.U(1.W)) // TODO: this is the same as HuanCun
  b.bits.corrupt := false.B
}