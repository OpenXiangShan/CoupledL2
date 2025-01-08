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
import coupledL2.tl2chi.CHICohStates._
import utility.{FastArbiter}

class RefillBufRead(implicit p: Parameters) extends LLCBundle {
  val id = Output(UInt(log2Ceil(mshrs.refill).W))
}

class RefillState(implicit p: Parameters) extends LLCBundle {
  val s_refill = Bool()
  val w_datRsp = Bool()
  val w_snpRsp = Bool()
}

class RefillRequest(implicit p: Parameters) extends LLCBundle {
  val state = new RefillState()
  val task = new Task()
  val dirResult = new DirResult()
}

class RefillEntry(implicit p: Parameters) extends TaskEntry {
  val state = new RefillState()
  val data = new DSBlock()
  val beatValids = Vec(beatSize, Bool())
  val dirResult = new DirResult()
}

class RefillUnit(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    /* receive refill requests from mainpipe */
    val alloc = Flipped(ValidIO(new RefillRequest()))
  
    /* send refill task to request arbiter */
    val task = DecoupledIO(new Task())

    /* response from upstream RXDAT/RXRSP channel */ 
    val respData = Flipped(ValidIO(new RespWithData()))
    val resp = Flipped(ValidIO(new Resp()))

    /* refill data read */
    val read = Flipped(ValidIO(new RefillBufRead()))
    val data = Output(new DSBlock())

    /* refill buffers info */
    val refillInfo = Vec(mshrs.refill, ValidIO(new BlockInfo()))
  })

  val rsp     = io.resp
  val rspData = io.respData

  /* Data Structure */
  val buffer   = RegInit(VecInit(Seq.fill(mshrs.refill)(0.U.asTypeOf(new RefillEntry()))))
  val issueArb = Module(new FastArbiter(new Task(), mshrs.refill))

  val full = Cat(buffer.map(_.valid)).andR

  /* Alloc */
  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val canAlloc = !full && io.alloc.valid
  when(canAlloc) {
    val entry = buffer(insertIdx)
    entry.valid := true.B
    entry.state := io.alloc.bits.state
    entry.task := io.alloc.bits.task
    entry.task.bufID := insertIdx
    entry.dirResult := io.alloc.bits.dirResult
    entry.beatValids := VecInit(Seq.fill(beatSize)(false.B))
  }
  assert(!full || !io.alloc.valid, "RefillBuf overflow")

  /* Update state */
  when(rspData.valid) {
    val update_vec = buffer.map(e => (e.task.reqID === rspData.bits.txnID) && e.valid)
    assert(PopCount(update_vec) < 2.U, "Refill task repeated")
    val canUpdate = Cat(update_vec).orR
    val update_id = PriorityEncoder(update_vec)
    when(canUpdate) {
      val entry = buffer(update_id)
      val isWriteBackFull = entry.task.chiOpcode === WriteBackFull
      val isWriteEvictOrEvict = onIssueEbOrElse(entry.task.chiOpcode === WriteEvictOrEvict, false.B)
      val inv_CBWrData = rspData.bits.resp === I
      val cancel = isWriteBackFull && inv_CBWrData
      val clients_hit = entry.dirResult.clients.hit
      val clients_meta = entry.dirResult.clients.meta

      assert(
        !isWriteBackFull && !isWriteEvictOrEvict || inv_CBWrData || clients_hit && clients_meta(rspData.bits.srcID).valid,
        "Non-exist block release?(addr: 0x%x)",
        Cat(entry.task.tag, entry.task.set, entry.task.bank, entry.task.off)
      )

      val beatId = rspData.bits.dataID >> log2Ceil(beatBytes / 16)
      val newBeatValids = entry.beatValids.asUInt | UIntToOH(beatId)
      entry.valid := !cancel
      entry.beatValids := VecInit(newBeatValids.asBools)
      entry.state.w_datRsp := newBeatValids.andR
      entry.data.data(beatId) := rspData.bits.data
      entry.task.resp := rspData.bits.resp
      when(rspData.bits.opcode === SnpRespData) {
        val src_idOH  = UIntToOH(rspData.bits.srcID)(numRNs - 1, 0)
        val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH).asBools)
        entry.task.snpVec := newSnpVec
        entry.state.w_snpRsp := !Cat(newSnpVec).orR
      }
    }
  }

  when(rsp.valid) {
    val update_vec = buffer.map(e =>
      e.task.reqID === rsp.bits.txnID && e.valid && !e.state.w_snpRsp && rsp.bits.opcode === SnpResp
    )
    assert(PopCount(update_vec) < 2.U, "Refill task repeated")
    val canUpdate = Cat(update_vec).orR
    val update_id = PriorityEncoder(update_vec)
    when(canUpdate) {
      val entry = buffer(update_id)
      val src_idOH = UIntToOH(rsp.bits.srcID)(numRNs - 1, 0)
      val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH).asBools)
      entry.task.snpVec := newSnpVec
      entry.state.w_snpRsp := !Cat(newSnpVec).orR
    }
  }

  when(rspData.valid && rsp.valid) {
    when(rspData.bits.opcode === SnpRespData && rsp.bits.opcode === SnpResp) {
      when(rspData.bits.txnID === rsp.bits.txnID) {
        val update_vec = buffer.map(e => e.task.reqID === rsp.bits.txnID && e.valid && !e.state.w_snpRsp)
        assert(PopCount(update_vec) < 2.U, "Refill task repeated")
        val update_id = PriorityEncoder(update_vec)
        val entry = buffer(update_id)
        val canUpdate = Cat(update_vec).orR
        when(canUpdate) {
          val src_idOH_dat = UIntToOH(rspData.bits.srcID)(numRNs - 1, 0)
          val src_idOH_rsp = UIntToOH(rsp.bits.srcID)(numRNs - 1, 0)
          val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH_dat & ~src_idOH_rsp).asBools)
          entry.task.snpVec := newSnpVec
          entry.state.w_snpRsp := !Cat(newSnpVec).orR
        }
      }
    }
  }

  /* Issue */
  issueArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.state.w_datRsp && !e.state.s_refill && (!e.task.replSnp || e.task.replSnp && e.state.w_snpRsp)
    in.bits := e.task
  }
  issueArb.io.out.ready := true.B
  when(io.task.fire) {
    val entry = buffer(issueArb.io.chosen)
    entry.state.s_refill := true.B
  }

  io.task.valid := issueArb.io.out.valid
  io.task.bits := issueArb.io.out.bits

  /* Data read */
  val ridReg = RegEnable(io.read.bits.id, 0.U(log2Ceil(mshrs.refill).W), io.read.valid)
  io.data := buffer(ridReg).data

  /* Dealloc */
  buffer.foreach {e =>
    val cancel = e.valid && !e.state.w_datRsp && !(e.task.chiOpcode === WriteBackFull) && e.state.w_snpRsp &&
      !e.beatValids.asUInt.orR
    when(cancel) {
      e.valid := false.B
    }
  }

  when(io.read.valid) {
    val entry = buffer(io.read.bits.id)
    entry.valid := false.B
    entry.state.w_datRsp := false.B
  }

  /* block info */
  io.refillInfo.zipWithIndex.foreach { case (m, i) =>
    m.valid := buffer(i).valid
    m.bits.tag := buffer(i).task.tag
    m.bits.set := buffer(i).task.set
    m.bits.opcode := buffer(i).task.chiOpcode
    m.bits.reqID := buffer(i).task.reqID
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs.refill)(0.U(16.W))))
    buffer.zip(bufferTimer).zipWithIndex.map { case ((e, t), i) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < timeoutThreshold.U, "RefillBuf Leak(id: %d)", i.U)
    }
  }

}
