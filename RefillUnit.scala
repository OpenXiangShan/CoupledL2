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
import coupledL2.tl2chi.CHIOpcode.REQOpcodes._
import coupledL2.tl2chi.CHIOpcode.DATOpcodes._
import coupledL2.tl2chi.CHIOpcode.RSPOpcodes._
import utility.{FastArbiter}

class RefillBufRead(implicit p: Parameters) extends LLCBundle {
  val id = Output(UInt(mshrBits.W))
}

class RefillTask(implicit p: Parameters) extends LLCBundle {
  val task = new Task()
  val dirResult = new DirResult()
}

class RefillEntry(implicit p: Parameters) extends TaskEntry {
  val s_refill = Bool()
  val w_snpRsp = Bool()
  val data = new DSBlock()
  val beatValids = Vec(beatSize, Bool())
  val dirResult = new DirResult()
}

class RefillUnit(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /* receive refill requests from mainpipe */
    val task_in = Flipped(ValidIO(new RefillTask()))
  
    /* send refill task to request arbiter */
    val task_out = DecoupledIO(new Task())

    /* response from upstream RXDAT/RXRSP channel */ 
    val respData = Flipped(ValidIO(new RespWithData()))
    val resp = Flipped(ValidIO(new Resp()))

    /* refill data read */
    val read = Flipped(ValidIO(new RefillBufRead()))
    val data = Output(new DSBlock())

    /* refill buffers info */
    val refillInfo = Vec(mshrs, ValidIO(new BlockInfo()))
  })

  val rsp     = io.resp
  val rspData = io.respData

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
    entry.s_refill := false.B
    entry.w_snpRsp := !Cat(io.task_in.bits.task.snpVec).orR
    entry.task := io.task_in.bits.task
    entry.task.bufID := insertIdx
    entry.dirResult := io.task_in.bits.dirResult
    entry.beatValids := VecInit(Seq.fill(beatSize)(false.B))
  }
  assert(!full || !io.task_in.valid, "RefillBuf overflow")

  /* Update state */
  when(rspData.valid) {
    val update_vec = buffer.map(e => (e.task.reqID === rspData.bits.txnID) && e.valid && !e.ready)
    assert(PopCount(update_vec) < 2.U, "Refill task repeated")
    val canUpdate = Cat(update_vec).orR
    val update_id = PriorityEncoder(update_vec)
    when(canUpdate) {
      val entry = buffer(update_id)
      val isWriteBackFull = entry.task.chiOpcode === WriteBackFull
      val inv_CBWrData = rspData.bits.resp === I
      val cancel = isWriteBackFull && inv_CBWrData
      val clients_hit = entry.dirResult.clients.hit
      val clients_meta = entry.dirResult.clients.meta
      val beatId = rspData.bits.dataID >> log2Ceil(beatBytes / 16)
      val newBeatValids = entry.beatValids.asUInt | UIntToOH(beatId)

      assert(
        !isWriteBackFull || inv_CBWrData || clients_hit && clients_meta(rspData.bits.srcID).valid,
        "Non-exist block release?(addr: 0x%x)",
        Cat(entry.task.tag, entry.task.set, entry.task.bank, entry.task.off)
      )

      entry.valid := !cancel
      entry.beatValids := VecInit(newBeatValids.asBools)
      when(newBeatValids.andR) {
        entry.ready := true.B
        when(rspData.bits.opcode === SnpRespData) {
          val src_idOH = UIntToOH(rspData.bits.srcID)(numRNs - 1, 0)
          val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH).asBools)
          entry.task.snpVec := newSnpVec
          when(!Cat(newSnpVec).orR) {
            entry.w_snpRsp := true.B
          }
        }
      }
      entry.data.data(beatId) := rspData.bits.data
      entry.task.resp := rspData.bits.resp
    }
  }

  when(rsp.valid) {
    val update_vec = buffer.map(e =>
      e.task.reqID === rsp.bits.txnID && e.valid && !e.w_snpRsp && rsp.bits.opcode === SnpResp
    )
    assert(PopCount(update_vec) < 2.U, "Refill task repeated")
    val canUpdate = Cat(update_vec).orR
    val update_id = PriorityEncoder(update_vec)
    when(canUpdate) {
      val entry = buffer(update_id)
      val src_idOH = UIntToOH(rsp.bits.srcID)(numRNs - 1, 0)
      val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH).asBools)
      entry.task.snpVec := newSnpVec
      when(!Cat(newSnpVec).orR) {
        entry.w_snpRsp := true.B
      }
    }
  }

  when(rspData.valid && rsp.valid) {
    when(rspData.bits.opcode === SnpRespData && rsp.bits.opcode === SnpResp) {
      when(rspData.bits.txnID === rsp.bits.txnID) {
        val update_vec = buffer.map(e => e.task.reqID === rsp.bits.txnID && e.valid && !e.w_snpRsp)
        assert(PopCount(update_vec) < 2.U, "Refill task repeated")
        val update_id = PriorityEncoder(update_vec)
        val entry = buffer(update_id)
        val waitLastBeat = PopCount(~entry.beatValids.asUInt) === 1.U
        val canUpdate = Cat(update_vec).orR && waitLastBeat
        when(canUpdate) {
          val src_idOH_1 = UIntToOH(rspData.bits.srcID)(numRNs - 1, 0)
          val src_idOH_2 = UIntToOH(rsp.bits.srcID)(numRNs - 1, 0)
          val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH_1 & ~src_idOH_2).asBools)
          entry.task.snpVec := newSnpVec
          when(!Cat(newSnpVec).orR) {
            entry.w_snpRsp := true.B
          }
        }
      }
    }
  }

  /* Issue */
  issueArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.ready && !e.s_refill && (!e.task.replSnp || e.task.replSnp && e.w_snpRsp)
    in.bits := e.task
  }
  issueArb.io.out.ready := io.task_out.ready
  when(io.task_out.fire) {
    val entry = buffer(issueArb.io.chosen)
    entry.s_refill := true.B
  }

  io.task_out.valid := issueArb.io.out.valid
  io.task_out.bits := issueArb.io.out.bits

  /* Data read */
  val ridReg = RegEnable(io.read.bits.id, 0.U(mshrBits.W), io.read.valid)
  io.data := buffer(ridReg).data

  /* Dealloc */
  buffer.foreach {e =>
    val cancel = e.valid && !e.ready && !(e.task.chiOpcode === WriteBackFull) && e.w_snpRsp
    when(cancel) {
      e.valid := false.B
    }
  }

  when(io.read.valid) {
    val entry = buffer(io.read.bits.id)
    entry.valid := false.B
    entry.ready := false.B
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
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs)(0.U(16.W))))
    buffer.zip(bufferTimer).zipWithIndex.map { case ((e, t), i) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < timeoutThreshold.U, "RefillBuf Leak(id: %d)", i.U)
    }
  }

}
