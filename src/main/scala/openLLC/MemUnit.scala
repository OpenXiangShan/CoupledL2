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

class MemEntry(implicit p: Parameters) extends TaskEntry {
  val s_issueReq = Bool()
  val s_issueDat = Bool()
  val w_dbid     = Bool()
  val w_comp     = Bool()
  val data       = new DSBlock()
}

object MemEntry {
  def apply(valid: Bool, task: Task, data: DSBlock)(implicit p: Parameters) = {
    val entry = WireInit(0.U.asTypeOf(new MemEntry()))
    entry.valid := valid
    entry.task := task
    entry.data := data
    entry
  }
}

class MemUnit(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /* ReadNoSnp/WriteNoSnp task from MainPipe */
    val fromMainPipe = new Bundle() {
      val task_s4 = Flipped(ValidIO(new Task()))
      val task_s6 = Flipped(ValidIO(new TaskWithData()))
    }

    // urgent indicating high priority
    // used when all snoops to the upper-level cache fail to retrieve data
    val urgentRead = Flipped(DecoupledIO(new Task()))

    /* response from downstream RXREQ channel */
    val resp = Flipped(ValidIO(new Resp()))

    /* generate requests sent to the Slave Node. */
    val txreq = DecoupledIO(new Task())
    val txdat = DecoupledIO(new TaskWithData())

    /* bypass rdata when RAW conflict occurs */
    val bypassData = ValidIO(new RespWithData)
  })

  val rsp        = io.resp
  val task_1     = io.fromMainPipe.task_s6
  val task_2     = io.fromMainPipe.task_s4
  val txreq      = io.txreq
  val txdat      = io.txdat
  val bypassData = io.bypassData
  val urgentRead = io.urgentRead

  /* Data Structure */
  val buffer   = RegInit(VecInit(Seq.fill(mshrs)(0.U.asTypeOf(new MemEntry()))))
  val txreqArb = Module(new FastArbiter(new Task(), mshrs))
  val txdatArb = Module(new FastArbiter(new TaskWithData(), mshrs))

  /* Bypass Read */
  def sameAddr(a: Task, b: Task): Bool = Cat(a.tag, a.set) === Cat(b.tag, b.set)
  def conflict(r: Task, w: Task): Bool = sameAddr(r, w) &&
    r.chiOpcode === ReadNoSnp && w.chiOpcode === WriteBackFull

  val entries = VecInit(buffer.toSeq :+ MemEntry(task_1.valid, task_1.bits.task, task_1.bits.data))
  val conflictMask_1 = entries.map(e => e.valid && urgentRead.valid && conflict(urgentRead.bits, e.task))
  val conflictMask_2 = entries.map(e => e.valid && task_2.valid && conflict(task_2.bits, e.task))
  val conflictIdx_1 = PriorityEncoder(conflictMask_1)
  val conflictIdx_2 = PriorityEncoder(conflictMask_2)
  val bypass_en_1 = urgentRead.valid && Cat(conflictMask_1).orR
  val bypass_en_2 = task_2.valid && Cat(conflictMask_2).orR
  val fakeRsp = RegInit(0.U.asTypeOf(new RespWithData()))

  when(bypass_en_1 || bypass_en_2) {
    fakeRsp.txnID := Mux(bypass_en_2, task_2.bits.txnID, urgentRead.bits.txnID)
    fakeRsp.dbID := 0.U
    fakeRsp.opcode := CompData
    fakeRsp.resp := 0.U
    fakeRsp.data := Mux(bypass_en_2, entries(conflictIdx_2).data, entries(conflictIdx_1).data)
  }
  bypassData.valid := RegNext(bypass_en_1 || bypass_en_2, false.B)
  bypassData.bits := fakeRsp

  urgentRead.ready := txreq.ready && !bypass_en_1 || bypass_en_1 && !bypass_en_2

  /* Alloc */
  val freeVec_1   = buffer.map(!_.valid)
  val idOH_1      = PriorityEncoderOH(freeVec_1)
  val freeVec_2   = Mux(task_1.valid, Cat(freeVec_1).asUInt & ~Cat(idOH_1), Cat(freeVec_1))
  val idOH_2      = PriorityEncoderOH(freeVec_2)
  val insertIdx_1 = OHToUInt(idOH_1)
  val insertIdx_2 = OHToUInt(idOH_2)

  val full_1  = !(Cat(freeVec_1).orR)
  val full_2  = !freeVec_2.orR
  val alloc_1 = task_1.valid && !full_1
  val alloc_2 = task_2.valid && !full_2 && !bypass_en_2

  when(alloc_1) {
    val entry = buffer(insertIdx_1)
    entry.valid := true.B
    entry.s_issueReq := false.B
    entry.s_issueDat := false.B
    entry.w_dbid := false.B
    entry.w_comp := false.B
    entry.task := task_1.bits.task
    entry.data := task_1.bits.data
  }

  when(alloc_2) {
    val entry = buffer(insertIdx_2)
    entry.valid := true.B
    entry.s_issueReq := false.B
    entry.s_issueDat := true.B
    entry.w_dbid := true.B
    entry.w_comp := true.B
    entry.task := task_2.bits
  }

  assert(!(full_1 && task_1.valid || full_2 && task_2.valid && !bypass_en_2) , "MemBuf overflow")

  /* Issue */
  txreqArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && !e.s_issueReq
    in.bits := e.task
  }
  txreqArb.io.out.ready := txreq.ready && (!urgentRead.valid || bypass_en_1)
  txreq.valid := txdatArb.io.out.valid || urgentRead.valid && !bypass_en_1
  txreq.bits := Mux(urgentRead.valid && !bypass_en_1, urgentRead.bits, txreqArb.io.out.bits)

  txdatArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.task.chiOpcode === WriteNoSnpFull &&
      e.s_issueReq && e.w_dbid && !e.s_issueDat
    in.bits.task := e.task
    in.bits.data := e.data
  }
  txdatArb.io.out.ready := txdat.ready
  txdat.valid := txdatArb.io.out.valid
  txdat.bits := txdatArb.io.out.bits
  txdat.bits.task.chiOpcode := NonCopyBackWrData

  /* Update state */
  when(txreq.fire && (!urgentRead.valid || bypass_en_1)) {
    val entry = buffer(txreqArb.io.chosen)
    when(entry.task.chiOpcode === WriteNoSnpFull) {
      entry.s_issueReq := true.B
    }.otherwise {
      entry.valid := false.B
    }
  }

  when(rsp.valid) {
    val match_vec = buffer.map(e => e.valid && e.s_issueReq && e.task.reqID === rsp.bits.txnID)
    assert(PopCount(match_vec) < 2.U, "Mem task repeated")
    val update = Cat(match_vec).orR
    val bufID = PriorityEncoder(match_vec)
    when(update) {
      val entry = buffer(bufID)
      when(rsp.bits.opcode === CompDBIDResp) {
        entry.w_dbid := true.B
        entry.w_comp := true.B
        entry.task.txnID := rsp.bits.dbID
      }.elsewhen(rsp.bits.opcode === DBIDResp) {
        entry.w_dbid := true.B
        entry.task.txnID := rsp.bits.dbID
      }.elsewhen(rsp.bits.opcode === Comp) {
        entry.w_comp := true.B
      }
    }
  }

  when(txdat.fire) {
    val entry = buffer(txdatArb.io.chosen)
    entry.s_issueDat := true.B
  }

  buffer.foreach { e =>
    val will_free = e.valid && e.task.chiOpcode === WriteNoSnpFull &&
      e.s_issueReq && e.s_issueDat && e.w_dbid && e.w_comp
    when(will_free) {
      e.valid := false.B
    }
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs)(0.U(16.W))))
    buffer.zip(bufferTimer).map { case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < 20000.U, "MemBuf Leak")
    }
  }

}
