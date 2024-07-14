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
import coupledL2.tl2chi._
import utility.{FastArbiter}

class RefillBufRead(implicit p: Parameters) extends LLCBundle {
  val id = Output(UInt(mshrBits.W))
}

class RefillEntry(implicit p: Parameters) extends TaskEntry {
  val data = new DSBlock()
}

class RefillUnit(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /* receive refill requests from mainpipe */
    val task_in = Flipped(ValidIO(new Task()))
  
    /* send refill task to request arbiter */
    val task_out = DecoupledIO(new Task())

    // respond from upstream RXDAT channel
    val resp = Flipped(ValidIO(new RespWithData()))

    /* refill data read */
    val read = Flipped(ValidIO(new RefillBufRead()))
    val data = Output(new DSBlock())
  })

  val rsp = io.resp

  /* Data Structure */
  val buffer   = RegInit(VecInit(Seq.fill(mshrs)(0.U.asTypeOf(new RefillEntry()))))
  val issueArb = Module(new FastArbiter(new Task(), mshrs))

  val full = Cat(buffer.map(_.valid)).andR

  /* Alloc */
  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val alloc = !full && io.task_in.valid
  when(alloc) {
    val entry = buffer(insertIdx)
    entry.valid := true.B
    entry.ready := false.B
    entry.task := io.task_in.bits
    entry.task.bufID := insertIdx
  }
  assert(!full || !io.task_in.valid, "RefillBuf overflow")

  /* Update ready */
  when(rsp.valid) {
    val id_match_vec = buffer.map(e => (e.task.reqID === rsp.bits.txnID) && e.valid && !e.ready)
    assert(PopCount(id_match_vec) < 2.U, "Refill task repeated")
    val idMatch = Cat(id_match_vec).orR
    when(idMatch) {
      val pendingId = PriorityEncoder(id_match_vec)
      buffer(pendingId).ready := true.B
      buffer(pendingId).data := rsp.bits.data
      buffer(pendingId).task.resp := rsp.bits.resp
    }
  }

  /* Issue */
  issueArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.ready
    in.bits := e.task
  }
  issueArb.io.out.ready := io.task_out.ready

  io.task_out.valid := issueArb.io.out.valid
  io.task_out.bits := issueArb.io.out.bits

  /* Dealloc */
  when(io.task_out.fire) {
    val entry = buffer(issueArb.io.chosen)
    entry.valid := false.B
    entry.ready := false.B
  }

  /* Data read */
  val ridReg = RegEnable(io.read.bits.id, 0.U(mshrBits.W), io.read.valid)
  io.data := buffer(ridReg).data

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs)(0.U(16.W))))
    buffer.zip(bufferTimer).map { case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < 20000.U, "RefillBuf Leak")
    }
  }

}
