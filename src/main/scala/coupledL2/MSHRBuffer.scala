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
import chipsalliance.rocketchip.config.Parameters
import coupledL2.utils._
import java.util.ResourceBundle

// read with block granularity
class MSHRBufRead(implicit p: Parameters) extends L2Bundle {
  val valid = Input(Bool())
  val id = Input(UInt(mshrBits.W))
  val ready = Output(Bool())
  val data = Output(new DSBlock)
}

// write with beat granularity
class MSHRBufWrite(implicit p: Parameters) extends L2Bundle {
  val valid = Input(Bool())
  val beat = Input(UInt(beatBits.W))
  val data = Input(new DSData)
  val id = Input(UInt(mshrBits.W))
  val ready = Output(Bool())
}

// TODO: should it have both r/w port?
// MSHR Buffer is used when MSHR needs to save data, so each buffer entry corresponds to an MSHR
class MSHRBuffer(wPorts: Int = 1)(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val r = new MSHRBufRead()
    val w = Vec(wPorts, new MSHRBufWrite)
  })

  val buffer = Seq.fill(mshrsAll) {
    Module(new SRAMTemplate(new DSData(), set = 1, way = beatSize, singlePort = true))
  }
  val valids = RegInit(VecInit(Seq.fill(mshrsAll) {
    VecInit(Seq.fill(beatSize)(false.B))
  }))
  io.w.foreach {
    case w =>
      when (w.valid) { valids(w.id)(w.beat) := true.B }
  }
  when (io.r.valid) {
    assert(valids(io.r.id).asUInt.andR, "[%d] attempt to read an invalid entry", io.r.id)
    valids(io.r.id).foreach(_ := false.B)
  }

  buffer.zipWithIndex.foreach {
    case (buf, i) =>
      assert(!buf.io.r.req.valid || buf.io.r.req.ready, "avoid rw hazard manually")
      val w_sel = VecInit(io.w.map(w => w.valid && w.id === i.U)).asUInt
      val w_id = ParallelPriorityMux(w_sel, io.w.map(_.id))
      val w_beat = ParallelPriorityMux(w_sel, io.w.map(_.beat))
      val w_data = ParallelPriorityMux(w_sel, io.w.map(_.data))
      buf.io.w.req.valid := w_sel.orR
      buf.io.w.req.bits.apply(w_data, 0.U, UIntToOH(w_beat))
      assert(PopCount(w_sel) <= 1.U)

      buf.io.r.req.valid := io.r.valid && io.r.id === i.U
      buf.io.r.req.bits.apply(0.U)
  }

  io.r.ready := true.B
  io.w.foreach(_.ready := true.B)

  val ridReg = RegNext(io.r.id)
  io.r.data.data := VecInit(buffer.map(_.io.r.resp.data.asUInt))(ridReg)
}
