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
    val out = Flipped(ChannelIO(new CHIDAT()))
    val in = Output(new RespBundle())
    val refillBufWrite = ValidIO(new MSHRBufWrite())
  })

  /* RXDAT for Transactions: CompData */

  //For bus width is 256-bit
  val first = (io.out.bits.dataID === "b00".U)
  val last  = (io.out.bits.dataID === "b10".U)

  //GrantBuf used to keep first half 256 bit data
  val grantDataBuf = RegEnable(io.out.bits.data, 0.U((beatBytes * 8).W), io.out.valid && first)

  /* Write Refill Buffer*/
  io.refillBufWrite.valid := io.out.valid && first
  io.refillBufWrite.bits.id := io.out.bits.txnID
  io.refillBufWrite.bits.data.data := Cat(io.out.bits.data, grantDataBuf)

  /* Response to MSHR */
  io.in.valid := (first || last) && io.out.valid 
  io.in.mshrId := io.out.bits.txnID
  io.in.set := 0.U(setBits.W)
  io.in.tag := 0.U(tagBits.W)
//  io.in.respInfo.opcode := io.out.bits.opcode
  io.in.respInfo.last := last

  io.in.respInfo.chiOpcode.get := io.out.bits.opcode
  io.in.respInfo.homeNID.get := io.out.bits.homeNID
  io.in.respInfo.dbID.get := io.out.bits.dbID


  // TODO
  io <> DontCare
}
