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
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._

class SourceMD(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // receive task from MainPipe
    val d_task = Flipped(DecoupledIO(new TaskWithData()))
    // send Release/Grant/ProbeAck via D channels
    val toSourceD = DecoupledIO(new TaskWithData())
    // send GrantMatrixData via  MD channels
    val toMatrixD = DecoupledIO(new MatrixDataBundle())
  })

  io.toSourceD <> io.d_task

  when (io.d_task.bits.task.opcode === AccessAckData && io.d_task.bits.task.matrixTask) {
    io.toMatrixD.valid := io.d_task.valid
    io.toMatrixD.bits.data := io.d_task.bits.data
    io.toMatrixD.bits.sourceId := io.d_task.bits.task.sourceId
    io.toSourceD.valid := false.B
  }.otherwise {
    io.toMatrixD.valid := false.B
    io.toMatrixD.bits.data.data := 0.U
    io.toMatrixD.bits.sourceId := 0.U
    io.toSourceD.valid := io.d_task.valid
  }

  io.d_task.ready := io.toSourceD.ready || io.toMatrixD.ready
}
