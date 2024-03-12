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

class MSHRBufRead(implicit p: Parameters) extends L2Bundle {
  val id = Output(UInt(mshrBits.W))
}

class MSHRBufResp(implicit p: Parameters) extends L2Bundle {
  val data = Output(new DSBlock)
}

class MSHRBufWrite(implicit p: Parameters) extends L2Bundle {
  val id = Output(UInt(mshrBits.W))
  val data = Output(new DSBlock)
}

// MSHR Buffer is used when MSHR needs to save data, so each buffer entry corresponds to an MSHR
class MSHRBuffer(wPorts: Int = 1)(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val r = Flipped(ValidIO(new MSHRBufRead))
    val resp = new MSHRBufResp
    val w = Vec(wPorts, Flipped(ValidIO(new MSHRBufWrite)))
  })

  val buffer = Reg(Vec(mshrsAll, new DSBlock))

  buffer.zipWithIndex.foreach {
    case (block, i) =>
      val wens = VecInit(io.w.map(w => w.valid && w.bits.id === i.U)).asUInt
      assert(PopCount(wens) <= 2.U, "triple write to the same MSHR buffer entry")

      val w_data = PriorityMux(wens, io.w.map(_.bits.data))
      when(wens.orR) {
        block := w_data
      }
  }

  val ridReg = RegEnable(io.r.bits.id, 0.U(mshrBits.W), io.r.valid)
  io.resp.data := buffer(ridReg)
}

// may consider just choose an empty entry to insert
// instead of using MSHRId to index
// by this, we can reduce the size of MSHRBuffer
// yet another problem, MSHR may be unable to insert because of FULL
