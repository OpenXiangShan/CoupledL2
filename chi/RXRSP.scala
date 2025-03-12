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
import coupledL2.tl2chi.CHIRSP

class RXRSP (implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new CHIRSP()))
    val out = ValidIO(new Resp())
  })

  io.out.valid := io.in.valid
  io.in.ready := true.B

  def fromCHIRSPtoResp(r: CHIRSP): Resp = {
    val rsp = Wire(new Resp())
    rsp.txnID := r.txnID
    rsp.dbID := r.dbID
    rsp.opcode := r.opcode
    rsp.resp := r.resp
    rsp.srcID := r.srcID
    rsp
  }
  io.out.bits := fromCHIRSPtoResp(io.in.bits)

}
