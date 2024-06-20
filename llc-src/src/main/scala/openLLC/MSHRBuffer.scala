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

class MSHRBufRead(implicit p: Parameters) extends LLCBundle {
  val id = Output(UInt(mshrBits.W))
}

class MSHRBufResp(implicit p: Parameters) extends LLCBundle {
  val data = Output(new DSBlock())
}

class MSHRBufWrite(implicit p: Parameters) extends LLCBundle {
  val id = Output(UInt(mshrBits.W))
  val data = Output(new DSBlock())
  val beatMask = Output(UInt(beatSize.W))
}

// Each buffer entry corresponds to an MSHR
class MSHRBuffer(wPorts: Int = 1)(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val r = Flipped(ValidIO(new MSHRBufRead()))
    val resp = new MSHRBufResp()
    val w = Flipped(ValidIO(new MSHRBufWrite()))
  })

  val wen = io.w.valid
  val w_data = io.w.bits.data
  val w_beatSel = io.w.bits.beatMask
  val w_id = io.w.bits.id

  val buffer = Reg(Vec(mshrs, Vec(beatSize, UInt((beatBytes * 8).W))))

  when (wen) {
    buffer(w_id).zip(w_beatSel.asBools).zipWithIndex.foreach { case ((beat, sel), i) =>
      when (sel) {
        beat := w_data.data((i + 1) * beatBytes * 8 - 1, i * beatBytes * 8)
      }
    }
  }

  val ridReg = RegEnable(io.r.bits.id, 0.U(mshrBits.W), io.r.valid)
  io.resp.data.data := buffer(ridReg).asUInt
}
