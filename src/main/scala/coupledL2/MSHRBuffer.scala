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
import utility._
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
  val beat_sel = UInt(beatSize.W)
  val data = new DSBlock
  val id = UInt(mshrBits.W)
}

// TODO: should it have both r/w port?
// MSHR Buffer is used when MSHR needs to save data, so each buffer entry corresponds to an MSHR
class MSHRBuffer(wPorts: Int = 1)(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val r = new MSHRBufRead()
    val w = Vec(wPorts, Flipped(DecoupledIO(new MSHRBufWrite)))
  })

  val buffer = Module(new SRAMTemplate(new DSBeat(), set = mshrsAll, way = beatSize, singlePort = false))
  val valids = RegInit(VecInit(Seq.fill(mshrsAll) {
    VecInit(Seq.fill(beatSize)(false.B))
  }))

  val w = ParallelMux(io.w.map(_.valid), io.w.map(_.bits))
  val w_valid = Cat(io.w.map(_.valid)).orR

  when (w_valid) {
    w.beat_sel.asBools.zipWithIndex.foreach {
      case (sel, i) =>
        when (sel) { valids(w.id)(i) := true.B }
    }
  }

  when (io.r.valid) {
    assert(valids(io.r.id).asUInt.andR, "[%d] attempt to read an invalid entry", io.r.id)
    valids(io.r.id).foreach(_ := false.B)
  }

  buffer.io.w.req.valid := w_valid
  buffer.io.w.req.bits.apply(
    data = w.data.asTypeOf(buffer.io.w.req.bits.data),
    setIdx = w.id,
    waymask = w.beat_sel
  )
  buffer.io.r.req.valid := io.r.valid
  buffer.io.r.req.bits.apply(io.r.id)

  io.r.ready := true.B
  // io.w.foreach(_.ready := true.B)
  io.w.zipWithIndex.foreach {
    case (x, i) =>
      i match {
        case 0 => x.ready := true.B
        case _ => x.ready := !Cat(io.w.take(i).map(_.valid)).orR
      }
  }

  io.r.data.data := buffer.io.r.resp.data.asUInt
}
