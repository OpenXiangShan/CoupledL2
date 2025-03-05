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
import firrtl.Utils.False

class MatrixSourceD(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // receive task from MainPipe
    val d_task = Flipped(DecoupledIO(new TaskWithData()))
    // send Release/Grant/ProbeAck via D channels
    val toSourceD = DecoupledIO(new TaskWithData())
    val toMatrixD = DecoupledIO(new MatrixDataBundle())
  })
  // 如果io.d_task.bits.task.matrixTask=/=true,
  // io.toSourceD:=io.d_task
  // 否则io.toMatrixD.bits.data:=io.d_task.bits.data
  // 除此之外不变
  // 默认情况下，toSourceD 连接到 d_task
  io.toSourceD <> io.d_task

  when (!io.d_task.bits.task.matrixTask) {
    // 当 matrixTask 为 false 时，将 io.toSourceD 设为有效，并传递 d_task 信号
    io.toSourceD.valid := io.d_task.valid
    io.toMatrixD.valid := false.B  // 禁用 toMatrixD
    io.toMatrixD.bits.data.data := 0.U  // 清除数据
    io.toMatrixD.bits.sourceId := 0.U
  }.otherwise {
    // 当 matrixTask 为 true 时，设置 toMatrixD 的数据，并将 toSourceD 无效
    io.toMatrixD.bits.data := io.d_task.bits.data
    io.toMatrixD.bits.sourceId := io.d_task.bits.task.sourceId
    io.toMatrixD.valid := io.d_task.valid
    io.toSourceD.valid := false.B  // 禁用 toSourceD
  }

  // 只在 d_task 有效时允许准备好
  io.d_task.ready := io.toSourceD.ready || io.toMatrixD.ready
}
