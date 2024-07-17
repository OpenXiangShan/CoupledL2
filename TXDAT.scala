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
import coupledL2.tl2chi.CHIDAT

class TXDAT (implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val dat = DecoupledIO(new CHIDAT())
    val task = Flipped(DecoupledIO(new TaskWithData()))

  })

  val beatValids = RegInit(VecInit(Seq.fill(beatSize)(false.B)))
  val buffer = RegEnable(io.task.bits, 0.U.asTypeOf(new TaskWithData()), io.task.fire)
  val bufferValid = Cat(beatValids).orR
  val beat_id = PriorityEncoder(beatValids)
  val last_beat = if(beatSize == 1) beatValids.toSeq.last else
    !Cat(beatValids.toSeq.init).orR && beatValids.toSeq.last

  when(io.task.fire) {
    beatValids := VecInit(Seq.fill(beatSize)(true.B))
  }.elsewhen(io.dat.fire) {
    beatValids(beat_id) := false.B
  }

  io.dat.valid := bufferValid
  io.dat.bits := 0.U.asTypeOf(new CHIDAT())
  for (i <- 0 until beatSize) {
    when(beat_id === i.U) {
      io.dat.bits := buffer.toCHIDATBundle(i)
    }
  }
  io.task.ready := !bufferValid || last_beat && io.dat.fire

}