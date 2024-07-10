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
import coupledL2.tl2chi.CHIOpcode.REQOpcodes._
import coupledL2.tl2chi.CHIOpcode.DATOpcodes._
import coupledL2.tl2chi.CHIOpcode.RSPOpcodes._
import utility.{FastArbiter}

class ResponseEntry(implicit p: Parameters) extends TaskEntry {
  val inflight = Bool() // Only use in CompData
}

class ResponseUnit(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /* Comp(DBIDResp/Data) task from MainPipe */
    val fromMainPipe = new Bundle() {
      val task_s4 = Flipped(ValidIO(new Task()))
      val task_s6 = Flipped(ValidIO(new TaskWithData()))
    }

    /* read respond from downstream memory */
    val memResp = Flipped(ValidIO(new RespWithData()))

    /* snoop respond from upper-level cache */
    val snpResp = Flipped(ValidIO(new RespWithData()))

    /* CompAck from upstream RXRSP channel */
    val compack = Flipped(ValidIO(new Resp()))

    /* generate responses sent to the Request Node. */
    val txrsp = DecoupledIO(new Task()) // Comp(DBIDResp)
    val txdat = DecoupledIO(new TaskWithData()) // CompData
  })

  val task1  = io.fromMainPipe.task_s6
  val task2  = io.fromMainPipe.task_s4
  val memRsp = io.memResp
  val snpRsp = io.snpResp
  val ack    = io.compack
  val txrsp  = io.txrsp
  val txdat  = io.txdat

  /* Data Structure */
  val buffer = RegInit(VecInit(Seq.fill(mshrs)(0.U.asTypeOf(new ResponseEntry()))))
  val txrspArb = Module(new FastArbiter(new Task(), mshrs))
  val txdatArb = Module(new FastArbiter(new TaskWithData(), mshrs))

  /* Alloc */
  val freeVec1   = buffer.map(!_.valid)
  val idOH1      = PriorityEncoderOH(freeVec1)
  val freeVec2   = Mux(task1.valid, Cat(freeVec1).asUInt & ~Cat(idOH1), Cat(freeVec1))
  val idOH2      = PriorityEncoderOH(freeVec2)
  val insertIdx1 = OHToUInt(idOH1)
  val insertIdx2 = OHToUInt(idOH2)

  val full1  = !(Cat(freeVec1).orR)
  val full2  = !freeVec2.orR
  val alloc1 = task1.valid && !full1
  val alloc2 = task2.valid && !full2

  when(alloc1) {
    val entry = buffer(insertIdx1)
    entry.valid := true.B
    entry.ready := true.B
    entry.inflight := false.B
    entry.task := task1.bits.task
    entry.data := task1.bits.data
  }
  
  when(alloc2) {
    val entry = buffer(insertIdx2)
    entry.valid := true.B
    entry.ready := !(task2.bits.chiOpcode === ReadNotSharedDirty || task2.bits.chiOpcode === ReadUnique)
    entry.inflight := false.B
    entry.task := task2.bits
  }

  assert(!(full1 && task1.valid || full2 && task2.valid) , "ResponseBuf overflow")

  /* Update ready */
  def handleResponse(response: Valid[RespWithData]): Unit = {
    when(response.valid) {
      val id_match_vec = buffer.map(e =>
        (e.task.reqId === response.bits.txnId) && e.valid && !e.ready && !e.inflight
      )
      assert(PopCount(id_match_vec) < 2.U, "Response task repeated")
      val idMatch = Cat(id_match_vec).orR
      when(idMatch) {
        val pendingId = PriorityEncoder(id_match_vec)
        buffer(pendingId).ready := true.B
        buffer(pendingId).data := response.bits.data
      }
    }
  }

  handleResponse(memRsp)
  handleResponse(snpRsp)

  /* Issue */
  txdatArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.ready && !e.inflight &&
      (e.task.chiOpcode === ReadNotSharedDirty || e.task.chiOpcode === ReadUnique)
    in.bits.task := e.task
    in.bits.data := e.data
  }
  txdatArb.io.out.ready := txdat.ready
  txdat.valid := txdatArb.io.out.valid
  txdat.bits := txdatArb.io.out.bits
  txdat.bits.task.chiOpcode := CompData

  txrspArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.ready &&
      !(e.task.chiOpcode === ReadNotSharedDirty || e.task.chiOpcode === ReadUnique)
    in.bits := e.task
  }
  txrspArb.io.out.ready := txrsp.ready
  txrsp.valid := txrspArb.io.out.valid
  txrsp.bits := txrspArb.io.out.bits
  txrsp.bits.chiOpcode := Mux(
    txrspArb.io.out.bits.chiOpcode === WriteBackFull,
    CompDBIDResp,
    Comp
  )

  when(txdat.fire) {
    val entry = buffer(txdatArb.io.chosen)
    entry.inflight := true.B
  }

  /* Dealloc */
  when(txrsp.fire) {
    val entry = buffer(txrspArb.io.chosen)
    entry.valid := false.B
    entry.ready := false.B
    entry.inflight := false.B
  }

  when(ack.valid) {
    val id_match_vec = buffer.map(e =>
      (e.task.reqId === ack.bits.txnId) && e.valid && e.ready && e.inflight
    )
    assert(PopCount(id_match_vec) < 2.U, "Response task repeated")
    val idMatch = Cat(id_match_vec).orR
    when(idMatch) {
      val pendingId = PriorityEncoder(id_match_vec)
      buffer(pendingId).valid := false.B
      buffer(pendingId).ready := false.B
      buffer(pendingId).inflight := false.B
    }
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs)(0.U(16.W))))
    buffer.zip(bufferTimer).map { case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid) && !e.valid) { t := 0.U }
        assert(t < 20000.U, "ResponseBuf Leak")
    }
  }

}
