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
import org.chipsalliance.cde.config.Parameters
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
  val beat_sel = Input(UInt(beatSize.W))
  val data = Input(new DSBlock)
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

  val valids = RegInit(VecInit(Seq.fill(mshrsAll) {
    VecInit(Seq.fill(beatSize)(false.B))
  }))
  val buffer = Reg(Vec(mshrsAll, Vec(beatSize, new DSBeat())))

  io.w.foreach {
    case w =>
      when (w.valid) {
        w.beat_sel.asBools.zipWithIndex.foreach {
          case (sel, i) =>
            when (sel) { valids(w.id)(i) := true.B }
        }
      }
  }

  when (io.r.valid) {
    // TODO: When the acquireperm is sent and grant is received, refillBuf does not contain data.
    //  Therefore, refill buffer should be blocked from being read.

    // assert(valids(io.r.id).asUInt.andR, "[%d] attempt to read an invalid entry", io.r.id)
    valids(io.r.id).foreach(_ := false.B)
  }

  buffer.zipWithIndex.foreach {
    case (block, i) =>
      val wens = VecInit(io.w.map(w => w.valid && w.id === i.U)).asUInt
      assert(PopCount(wens) <= 2.U, "triple write to the same MSHR buffer entry")

      val w_beat_sel = PriorityMux(wens, io.w.map(_.beat_sel))
      val w_data = PriorityMux(wens, io.w.map(_.data))

      when(wens.orR) {
        (0 until beatSize).map { i =>
          when(w_beat_sel(i)) {
            block(i) := w_data.data(beatBytes * 8 * (i+1) - 1, beatBytes * 8 * i).asTypeOf(new DSBeat)
          }
        }
      }
  }

  io.r.ready := true.B
  io.w.foreach(_.ready := true.B)

  val ridReg = RegNext(io.r.id, 0.U.asTypeOf(io.r.id))
  io.r.data.data := buffer(ridReg).asUInt
}
