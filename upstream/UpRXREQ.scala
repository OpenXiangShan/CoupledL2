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
import coupledL2.tl2chi.CHIREQ

// receive task from upwards and convert to inner task
class UpRXREQ (implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new CHIREQ()))
    val task = DecoupledIO(new Task())
  })

  io.task.valid := io.req.valid
  io.req.ready := io.task.ready

  def fromCHIREQtoTaskBundle(r: CHIREQ): Task = {
    val task = Wire(new Task)
    task := 0.U.asTypeOf(new Task)
    task.tag := parseAddress(r.addr)._1
    task.set := parseAddress(r.addr)._2
    task.off := parseAddress(r.addr)._3
    task.size := r.size
    task.mshrTask := false.B
    task.mshrId := 0.U(mshrBits.W)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dataWen := false.B
    // this follows coupledL2.tl2chi.TaskBundle.toCHIReqBundle
    task.tgtID := r.tgtID
    task.srcID := r.srcID
    task.txnID := r.txnID
    task.chiOpcode := r.opcode // chiOpcode-width is larger than req-opcode-width
    task.allowRetry := r.allowRetry
    task.pCrdType := r.pCrdType
    task.expCompAck := r.expCompAck
    task.memAttr := r.memAttr
    task.snpAttr := r.snpAttr
    task.order := r.order
    task
  }
  io.task.bits := fromCHIREQtoTaskBundle(io.req.bits)
}
