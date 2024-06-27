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

class RequestArb(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /* receive incoming tasks from s1 */
    val busTask_s1 = Flipped(DecoupledIO(new Task()))
    val mshrTask_s1 = Flipped(DecoupledIO(new Task()))

    /* read local/client directory */
    val dirRead_s1 = DecoupledIO(new DirRead())

    /* send task to mainPipe */
    val taskToPipe_s2 = ValidIO(new Task())
  })

  val task_s1 = Wire(Valid(new Task()))
  val task_s2 = Wire(Valid(new Task()))

  /* Stage 1 */
  task_s1.valid := io.dirRead_s1.ready && (io.busTask_s1.valid || io.mshrTask_s1.valid)
  task_s1.bits := Mux(io.mshrTask_s1.valid, io.mshrTask_s1.bits, io.busTask_s1.bits)

  def addrConnect(lset: UInt, ltag: UInt, rset: UInt, rtag: UInt) = {
    assert(lset.getWidth + ltag.getWidth == rset.getWidth + rtag.getWidth)
    val addr = Cat(rtag, rset)
    lset := addr.tail(ltag.getWidth)
    ltag := addr.head(ltag.getWidth)
  }

  // Meta read request
  io.dirRead_s1.valid := task_s1.valid
  val rports = Seq(io.dirRead_s1.bits.self, io.dirRead_s1.bits.clients)
  rports.foreach { p =>
    p := DontCare
    addrConnect(p.set, p.tag, task_s1.bits.set, task_s1.bits.tag)
  }

  /* Stage 2 */
  task_s2.valid := RegNext(task_s1.valid, false.B)
  task_s2.bits := RegEnable(task_s1.bits, task_s1.valid)

  io.busTask_s1.ready := io.dirRead_s1.ready && !io.mshrTask_s1.valid
  io.mshrTask_s1.ready := io.dirRead_s1.ready

  io.taskToPipe_s2 := task_s2

}
