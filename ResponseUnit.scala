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

package openLLC

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class ResponseUnit(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val fromMainPipe = new Bundle() {
      val task_s4 = Flipped(ValidIO(new Task()))
      val task_s6 = Flipped(ValidIO(new TaskWithData()))
    }
    val taskFromRXDAT = Flipped(DecoupledIO(new TaskWithData()))
    val rspFromRXRSP = Flipped(ValidIO(new Resp()))

    /* generate responses sent to the Request Node. */
    val taskToTXRSP = DecoupledIO(new Task())
    val taskToTXDAT = DecoupledIO(new TaskWithData())
  })
  io.fromMainPipe := DontCare
  io.taskFromRXDAT := DontCare

  io.taskToTXRSP := DontCare
  io.taskToTXDAT := DontCare
}
