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
import coupledL2.tl2chi.HasCHIOpcodes
import utility.{FastArbiter}

class MemState(implicit p: Parameters) extends LLCBundle {
  val s_issueReq = Bool()
  val s_issueDat = Bool()
  val w_datRsp   = Bool()
  val w_dbid     = Bool()
  val w_comp     = Bool()
}

class MemRequest(withData: Boolean)(implicit p: Parameters) extends LLCBundle {
  val state = new MemState()
  val task  = new Task()
  val data  = if (withData) Some(new DSBlock()) else None
}

class MemInfo(implicit p: Parameters) extends BlockInfo {
  val w_datRsp = Bool()
}

class MemEntry(implicit p: Parameters) extends TaskEntry {
  val state      = new MemState()
  val data       = new DSBlock()
  val beatValids = Vec(beatSize, Bool())
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

class MemUnit(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    /* ReadNoSnp/WriteNoSnp task from MainPipe */
    val fromMainPipe = new Bundle() {
      val alloc_s4 = Flipped(ValidIO(new MemRequest(withData = false)))
      val alloc_s6 = Flipped(ValidIO(new MemRequest(withData = true)))
    }

    // urgent indicating high priority
    // used when all snoops to the upper-level cache fail to retrieve data
    val urgentRead = Flipped(DecoupledIO(new Task()))

    /* response from downstream RXRSP channel */
    val snRxrsp = Flipped(ValidIO(new Resp()))

    /* response from upper-level cache */
    val rnRxdat = Flipped(ValidIO(new RespWithData()))
    val rnRxrsp = Flipped(ValidIO(new Resp()))

    /* generate requests sent to the Slave Node. */
    val txreq = DecoupledIO(new Task())
    val txdat = DecoupledIO(new TaskWithData())

    /* bypass rdata when RAW conflict occurs */
    val bypassData = Vec(beatSize, ValidIO(new RespWithData()))

    /* memory access buffers info */
    val memInfo = Vec(mshrs.memory, ValidIO(new MemInfo()))

    /* block info from ResponseUnit */
    val respInfo = Flipped(Vec(mshrs.response, ValidIO(new ResponseInfo())))
  })

  val memResp    = io.snRxrsp
  val coreData   = io.rnRxdat
  val coreResp   = io.rnRxrsp
  val txreq      = io.txreq
  val txdat      = io.txdat
  val alloc_s6   = io.fromMainPipe.alloc_s6
  val alloc_s4   = io.fromMainPipe.alloc_s4
  val bypassData = io.bypassData
  val urgentRead = io.urgentRead

  /* Data Structure */
  val buffer   = RegInit(VecInit(Seq.fill(mshrs.memory)(0.U.asTypeOf(new MemEntry()))))
  val txreqArb = Module(new FastArbiter(new Task(), mshrs.memory))
  val txdatArb = Module(new FastArbiter(new TaskWithData(), mshrs.memory))

  /* Bypass Read */
  // If a new read task entering the MemUnit has the same target address as one of the write tasks in the buffer,
  // then this request will not be sent to the memory. Instead, a fake CompData will be returned to the responseUnit,
  // reducing processing time
  def sameAddr(a: Task, b: Task): Bool = Cat(a.tag, a.set) === Cat(b.tag, b.set)
  def conflict(r: Task, w: Task): Bool = sameAddr(r, w) &&
    r.chiOpcode === ReadNoSnp && w.chiOpcode === WriteNoSnpFull

  val entries = VecInit(buffer.toSeq :+ MemEntry(alloc_s6.valid, alloc_s6.bits.task, alloc_s6.bits.data.get))
  val conflictMask_ur = entries.map(e => e.valid && urgentRead.valid && conflict(urgentRead.bits, e.task))
  val conflictMask_s4 = entries.map(e => e.valid && alloc_s4.valid && conflict(alloc_s4.bits.task, e.task))
  val conflictIdx_ur = PriorityEncoder(conflictMask_ur)
  val conflictIdx_s4 = PriorityEncoder(conflictMask_s4)
  val bypass_ur = urgentRead.valid && Cat(conflictMask_ur).orR
  val bypass_s4 = alloc_s4.valid && Cat(conflictMask_s4).orR
  val fakeRsp = RegInit(VecInit(Seq.fill(beatSize)(0.U.asTypeOf(new RespWithData()))))

  when(bypass_ur || bypass_s4) {
    for (i <- 0 until beatSize) {
      fakeRsp(i).txnID := Mux(bypass_s4, alloc_s4.bits.task.txnID, urgentRead.bits.txnID)
      fakeRsp(i).dbID := 0.U
      fakeRsp(i).opcode := CompData
      fakeRsp(i).resp := 0.U
      fakeRsp(i).data := Mux(bypass_s4, entries(conflictIdx_s4).data.data(i), entries(conflictIdx_ur).data.data(i))
      fakeRsp(i).dataID := (beatBytes * i * 8).U(log2Ceil(blockBytes * 8) - 1, log2Ceil(blockBytes * 8) - 2)
    }
  }

  for (i <- 0 until beatSize) {
    bypassData(i).valid := RegNext(bypass_ur || bypass_s4, false.B)
    bypassData(i).bits := fakeRsp(i)
  }

  urgentRead.ready := txreq.ready && !bypass_ur || bypass_ur && !bypass_s4

  /* Alloc */
  val freeVec_s6   = VecInit(buffer.map(!_.valid))
  val idOH_s6      = PriorityEncoderOH(freeVec_s6)
  val freeVec_s4   = 
    Mux(
      alloc_s6.valid,
      VecInit(freeVec_s6.zip(idOH_s6).map{ case (x, y) => x && ~y }).asUInt,
      freeVec_s6.asUInt
    )
  val idOH_s4      = PriorityEncoderOH(freeVec_s4)
  val insertIdx_s6 = OHToUInt(idOH_s6)
  val insertIdx_s4 = OHToUInt(idOH_s4)

  val full_s6     = !(Cat(freeVec_s6).orR)
  val full_s4     = !freeVec_s4.orR
  val canAlloc_s6 = alloc_s6.valid && !full_s6
  val canAlloc_s4 = alloc_s4.valid && !full_s4 && !bypass_s4

  when(canAlloc_s6) {
    val entry = buffer(insertIdx_s6)
    entry.valid := true.B
    entry.state := alloc_s6.bits.state
    entry.task := alloc_s6.bits.task
    entry.data := alloc_s6.bits.data.get
    entry.beatValids := VecInit(Seq.fill(beatSize)(true.B))
  }

  when(canAlloc_s4) {
    val entry = buffer(insertIdx_s4)
    entry.valid := true.B
    entry.state := alloc_s4.bits.state
    entry.task := alloc_s4.bits.task
    entry.beatValids := VecInit(Seq.fill(beatSize)(false.B))
  }

  assert(!(full_s6 && alloc_s6.valid || full_s4 && alloc_s4.valid && !bypass_s4) , "MemBuf overflow")

  /* Issue */
  txreqArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.state.w_datRsp && !e.state.s_issueReq
    in.bits := e.task
  }
  txreqArb.io.out.ready := true.B

  // This blocking mechanism might be triggered when a request needs to read memory and snoop other cache blocks
  // due to an SF capacity conflict. In this case, when the snooped block is refilled, it may result a writeback
  // if a dirty block is evicted. This can result in two memory access requests with the same TxnID. Therefore, 
  // it is necessary to control the issuance of write request, ensuring that the write request is sent only after
  // the read request has received a response.
  val blockByResp = Cat(
    io.respInfo.map(e => e.valid && !e.bits.w_compdata && e.bits.w_snpRsp &&
    (e.bits.opcode === ReadUnique || e.bits.opcode === ReadNotSharedDirty) &&
    e.bits.reqID === txreqArb.io.out.bits.reqID && txreqArb.io.out.bits.chiOpcode === WriteNoSnpFull)
  ).orR
  txreq.valid := txreqArb.io.out.valid && !blockByResp || urgentRead.valid && !bypass_ur
  txreq.bits := Mux(urgentRead.valid && !bypass_ur, urgentRead.bits, txreqArb.io.out.bits)

  txdatArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.state.w_datRsp && e.task.chiOpcode === WriteNoSnpFull &&
      e.state.s_issueReq && e.state.w_dbid && !e.state.s_issueDat
    in.bits.task := e.task
    in.bits.data := e.data
  }
  txdatArb.io.out.ready := txdat.ready
  txdat.valid := txdatArb.io.out.valid
  txdat.bits := txdatArb.io.out.bits
  txdat.bits.task.chiOpcode := NonCopyBackWrData

  /* Update state */
  when(txreq.fire && (!urgentRead.valid || bypass_ur)) {
    val entry = buffer(txreqArb.io.chosen)
    when(entry.task.chiOpcode === WriteNoSnpFull) {
      entry.state.s_issueReq := true.B
    }.otherwise {
      entry.valid := false.B
    }
  }

  when(coreData.valid) {
    val match_vec = buffer.map(e => e.valid && !e.state.w_datRsp && e.task.chiOpcode === WriteNoSnpFull &&
      e.task.reqID === coreData.bits.txnID)
    assert(PopCount(match_vec) < 2.U, "Mem task repeated")
    val update = Cat(match_vec).orR
    val bufID = PriorityEncoder(match_vec)
    when(update) {
      val entry = buffer(bufID)
      when(!coreData.bits.resp(2)) {
        entry.valid := false.B
      }.otherwise {
        val beatId = coreData.bits.dataID >> log2Ceil(beatBytes / 16)
        val newBeatValids = entry.beatValids.asUInt | UIntToOH(beatId)
        entry.beatValids := VecInit(newBeatValids.asBools)
        entry.state.w_datRsp := newBeatValids.andR
        entry.data.data(beatId) := coreData.bits.data
      }
    }
  }

  when(coreResp.valid) {
    val match_vec = buffer.map(e => e.valid && !e.state.w_datRsp && e.task.chiOpcode === WriteNoSnpFull &&
      e.task.reqID === coreResp.bits.txnID)
    assert(PopCount(match_vec) < 2.U, "Mem task repeated")
    val update = Cat(match_vec).orR
    val bufID = PriorityEncoder(match_vec)
    when(update) {
      val entry = buffer(bufID)
      entry.valid := false.B
    }
  }

  when(memResp.valid) {
    val match_vec = buffer.map(e => e.valid && e.state.s_issueReq && e.task.reqID === memResp.bits.txnID)
    assert(PopCount(match_vec) < 2.U, "Mem task repeated")
    val update = Cat(match_vec).orR
    val bufID = PriorityEncoder(match_vec)
    when(update) {
      val entry = buffer(bufID)
      when(memResp.bits.opcode === CompDBIDResp) {
        entry.state.w_dbid := true.B
        entry.state.w_comp := true.B
        entry.task.txnID := memResp.bits.dbID
      }.elsewhen(memResp.bits.opcode === DBIDResp) {
        entry.state.w_dbid := true.B
        entry.task.txnID := memResp.bits.dbID
      }.elsewhen(memResp.bits.opcode === Comp) {
        entry.state.w_comp := true.B
      }
    }
  }

  when(txdat.fire) {
    val entry = buffer(txdatArb.io.chosen)
    entry.state.s_issueDat := true.B
  }

  buffer.foreach { e =>
    val will_free = e.valid && e.task.chiOpcode === WriteNoSnpFull && e.state.s_issueReq && e.state.s_issueDat &&
      e.state.w_dbid && e.state.w_comp && e.state.w_datRsp
    when(will_free) {
      e.valid := false.B
    }
  }

  /* block info */
  io.memInfo.zipWithIndex.foreach { case (m, i) =>
    m.valid := buffer(i).valid
    m.bits.tag := buffer(i).task.tag
    m.bits.set := buffer(i).task.set
    m.bits.opcode := buffer(i).task.chiOpcode
    m.bits.reqID := buffer(i).task.reqID
    m.bits.w_datRsp := buffer(i).state.w_datRsp
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs.memory)(0.U(16.W))))
    buffer.zip(bufferTimer).zipWithIndex.map { case ((e, t), i) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < timeoutThreshold.U, "MemBuf Leak(id: %d)", i.U)
    }
  }

}
