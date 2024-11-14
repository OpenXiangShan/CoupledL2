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

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import coupledL2.{RespBundle, MSHRBufWrite}

class RXDAT(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val out = Flipped(DecoupledIO(new CHIDAT()))
    val in = Output(new RespBundle())
    val refillBufWrite = ValidIO(new MSHRBufWrite())
  })

  /* RXDAT for Transactions: CompData */

  // TODO: parameterize this
  // For bus width is 256-bit
  val first = (io.out.bits.dataID === "b00".U)
  val last  = (io.out.bits.dataID === "b10".U)

  /* Write Refill Buffer*/
  io.refillBufWrite.valid := io.out.valid
  io.refillBufWrite.bits.id := io.out.bits.txnID
  io.refillBufWrite.bits.data.data := Fill(beatSize, io.out.bits.data)
  io.refillBufWrite.bits.beatMask := Cat(last, first)

  /* Response to MSHR */
  io.in.valid := (first || last) && io.out.valid
  io.in.mshrId := io.out.bits.txnID
  io.in.set := 0.U(setBits.W)
  io.in.tag := 0.U(tagBits.W)

  io.in.respInfo.opcode        := DontCare
  io.in.respInfo.param         := DontCare
  io.in.respInfo.last          := last
  io.in.respInfo.dirty         := DontCare
  io.in.respInfo.isHit         := DontCare
  io.in.respInfo.chiOpcode.get := io.out.bits.opcode
  io.in.respInfo.txnID.get     := io.out.bits.txnID
  io.in.respInfo.srcID.get     := io.out.bits.srcID
  io.in.respInfo.homeNID.get   := io.out.bits.homeNID
  io.in.respInfo.dbID.get      := io.out.bits.dbID
  io.in.respInfo.resp.get      := io.out.bits.resp
  io.in.respInfo.pCrdType.get  := DontCare // RXDAT Channel does not have a pCrdType field
  io.in.respInfo.respErr.get   := io.out.bits.respErr
  io.in.respInfo.traceTag.get  := io.out.bits.traceTag

  io.out.ready := true.B

}
