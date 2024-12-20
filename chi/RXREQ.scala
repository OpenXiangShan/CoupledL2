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
class RXREQ (implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new CHIREQ()))
    val task = DecoupledIO(new Task())
  })

  // Outer interface connection
  /**
    * TxnID space arrangement:
    * If this is a cacheable request:
    * +----------------+-----------+---------------+
    * |    0.U(1.W)    |  SliceID  |  Inner TxnID  |
    * +----------------+-----------+---------------+
    * Otherwise this is an MMIO request:
    * +----------------+-----------+---------------+
    * |    1.U(1.W)    |        Inner TxnID        |
    * +----------------+---------------------------+  
    *
    */
  val id_pool = RegInit(0.U((TXNID_WIDTH - bankBits - 1).W))
  when(io.task.fire) {
    id_pool := id_pool + 1.U // maybe fail if in-flight transactions exceed (256/banks) ?
  }

  io.task.valid := io.req.valid
  io.req.ready := io.task.ready

  def fromCHIREQtoTaskBundle(r: CHIREQ): Task = {
    val task = Wire(new Task)
    val (tag, set, bank, off) = parseAddress(r.addr)
    task := 0.U.asTypeOf(new Task)
    task.tag := tag
    task.set := set
    task.bank := bank
    task.off := off
    task.size := r.size
    task.refillTask := false.B
    task.reqID := Cat(0.U(1.W), bank, id_pool)
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
