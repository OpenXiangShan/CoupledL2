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
import coupledL2.tl2chi.CHIOpcode.REQOpcodes._
import coupledL2.tl2chi.CHIOpcode.DATOpcodes._
import coupledL2.tl2chi.CHIOpcode.RSPOpcodes._
import utility.{FastArbiter}

class ResponseEntry(implicit p: Parameters) extends TaskEntry {
  val data = new DSBlock()
  val beatValids = Vec(beatSize, Bool())
  val s_comp = Bool()
  val s_urgentRead = Bool()
  val w_snpRsp = Bool()
  val w_compack = Bool()
}

class ResponseInfo(implicit p: Parameters) extends BlockInfo {
  val w_compack = Bool()
}

class ResponseUnit(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /* Comp(DBIDResp/Data) task from MainPipe */
    val fromMainPipe = new Bundle() {
      val task_s4 = Flipped(ValidIO(new Task()))
      val task_s6 = Flipped(ValidIO(new TaskWithData()))
    }

    /* read response data from downstream memory */
    val memData = Flipped(ValidIO(new RespWithData()))

    /* snoop response data from upper-level cache */
    val snpData = Flipped(ValidIO(new RespWithData()))

    /* fake CompData response from MemUnit */
    val bypassData = Flipped(Vec(beatSize, ValidIO(new RespWithData())))

    /* CompAck/SnpResp from upstream RXRSP channel */
    val response = Flipped(ValidIO(new Resp()))

    /* generate responses sent to the Request Node. */
    val txrsp = DecoupledIO(new Task()) // Comp(DBIDResp)
    val txdat = DecoupledIO(new TaskWithData()) // CompData

    /* Response buffers info */
    val respInfo = Vec(mshrs, ValidIO(new ResponseInfo()))

    /* instruct MemUnit to immediately send a read request */
    val urgentRead = DecoupledIO(new Task())
  })

  val task_1     = io.fromMainPipe.task_s6
  val task_2     = io.fromMainPipe.task_s4
  val memData    = io.memData
  val snpData    = io.snpData
  val bypassData = io.bypassData
  val rsp        = io.response
  val txrsp      = io.txrsp
  val txdat      = io.txdat
  val urgentRead = io.urgentRead

  /* Data Structure */
  val buffer = RegInit(VecInit(Seq.fill(mshrs)(0.U.asTypeOf(new ResponseEntry()))))
  val txrspArb = Module(new FastArbiter(new Task(), mshrs))
  val txdatArb = Module(new FastArbiter(new TaskWithData(), mshrs))

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
  val alloc_2 = task_2.valid && !full_2

  // Task_1 can only be CompData
  when(alloc_1) {
    val entry = buffer(insertIdx_1)
    entry.valid := true.B
    entry.ready := true.B
    entry.s_comp := false.B
    entry.s_urgentRead := true.B
    entry.w_snpRsp := true.B
    entry.w_compack := false.B
    entry.task := task_1.bits.task
    entry.data := task_1.bits.data
    entry.beatValids := VecInit(Seq.fill(beatSize)(true.B))
  }

  when(alloc_2) {
    val entry = buffer(insertIdx_2)
    val isReadNotSharedDirty = task_2.bits.chiOpcode === ReadNotSharedDirty
    val isReadUnique = task_2.bits.chiOpcode === ReadUnique
    val isMakeUnique = task_2.bits.chiOpcode === MakeUnique
    entry.valid := true.B
    entry.ready := !(isReadNotSharedDirty || isReadUnique)
    entry.beatValids := VecInit(Seq.fill(beatSize)(!(isReadNotSharedDirty || isReadUnique)))
    entry.s_comp := false.B
    entry.s_urgentRead := true.B
    entry.w_snpRsp := !Cat(task_2.bits.snpVec).orR
    entry.w_compack := !(isReadUnique || isReadNotSharedDirty || isMakeUnique)
    entry.task := task_2.bits
  }

  assert(!(full_1 && task_1.valid || full_2 && task_2.valid) , "ResponseBuf overflow")

  /* Update state */
  def handleMemResp(response: Valid[RespWithData]): Unit = {
    when(response.valid) {
      val update_vec = buffer.map(e =>
        e.task.reqID === response.bits.txnID && e.valid && !e.ready && !e.s_comp &&
        response.bits.opcode === CompData
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        val beatId = response.bits.dataID >> (beatBytes / 16)
        val newBeatValids = Cat(entry.beatValids) | UIntToOH(beatId)
        entry.beatValids := VecInit(newBeatValids.asBools)
        when(newBeatValids.andR) {
          entry.ready := true.B
        }
        entry.data.data(beatId) := response.bits.data
      }
    }
  }

  def handleSnpResp(snpRsp: Valid[Resp], snpData: Valid[RespWithData]): Unit = {
    when(snpData.valid) {
      val update_vec = buffer.map(e =>
        (e.task.reqID === snpData.bits.txnID) && e.valid && !e.ready && !e.s_comp && !e.w_snpRsp &&
        snpData.bits.opcode === SnpRespData
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        val beatId = snpData.bits.dataID >> (beatBytes / 16)
        val newBeatValids = Cat(entry.beatValids) | UIntToOH(beatId)
        entry.beatValids := VecInit(newBeatValids.asBools)
        when(newBeatValids.andR) {
          val src_idOH = UIntToOH(snpData.bits.srcID)(clientBits - 1, 0)
          val newSnpVec = VecInit((Cat(entry.task.snpVec) & ~src_idOH).asBools)
          entry.task.snpVec := newSnpVec
          entry.ready := true.B
          when(!Cat(newSnpVec).orR) {
            entry.w_snpRsp := true.B
          }
        }
        entry.data.data(beatId) := snpData.bits.data
      }
    }

    when(snpRsp.valid) {
      val update_vec = buffer.map(e =>
        e.task.reqID === snpRsp.bits.txnID && e.valid && !e.w_snpRsp && snpRsp.bits.opcode === SnpResp
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        val src_idOH = UIntToOH(snpRsp.bits.srcID)(clientBits - 1, 0)
        val newSnpVec = VecInit((Cat(entry.task.snpVec) & ~src_idOH).asBools)
        entry.task.snpVec := newSnpVec
        when(!Cat(newSnpVec).orR) {
          entry.w_snpRsp := true.B
          entry.s_urgentRead := entry.ready
        }
      }
    }

    when(snpData.valid && snpRsp.valid) {
      when(snpData.bits.opcode === SnpRespData && snpRsp.bits.opcode === SnpResp) {
        when(snpData.bits.txnID === snpRsp.bits.txnID) {
          val update_vec = buffer.map(e => e.task.reqID === snpRsp.bits.txnID && e.valid && !e.w_snpRsp)
          assert(PopCount(update_vec) < 2.U, "Response task repeated")
          val update_id = PriorityEncoder(update_vec)
          val entry = buffer(update_id)
          val waitLastBeat = PopCount(~Cat(entry.beatValids)) === 1.U
          val canUpdate = Cat(update_vec).orR && waitLastBeat
          when(canUpdate) {
            val src_idOH_1 = UIntToOH(snpData.bits.srcID)(clientBits - 1, 0)
            val src_idOH_2 = UIntToOH(snpRsp.bits.srcID)(clientBits - 1, 0)
            val newSnpVec = VecInit((Cat(entry.task.snpVec) & ~src_idOH_1 & ~src_idOH_2).asBools)
            entry.task.snpVec := newSnpVec
            when(!Cat(newSnpVec).orR) {
              entry.w_snpRsp := true.B
            }
          }
        }
      }
    }
  }

  def handleCompAck(ack: Valid[Resp]): Unit = {
    when(ack.valid) {
      val update_vec = buffer.map(e =>
        e.task.reqID === ack.bits.txnID && e.valid && e.ready && e.s_comp && !e.w_compack &&
        ack.bits.opcode === CompAck
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        entry.w_compack := true.B
      }
    }
  }

  for (i <- 0 until beatSize) {
    handleMemResp(bypassData(i))
  } 
  handleMemResp(memData)
  handleSnpResp(rsp, snpData)
  handleCompAck(rsp)

  /* Issue */
  txdatArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.ready && e.w_snpRsp && e.s_urgentRead && !e.s_comp
    in.bits.task := e.task
    in.bits.data := e.data
  }
  txdatArb.io.out.ready := txdat.ready
  txdat.valid := txdatArb.io.out.valid
  txdat.bits := txdatArb.io.out.bits
  txdat.bits.task.chiOpcode := CompData
  when(txdat.fire) {
    val entry = buffer(txdatArb.io.chosen)
    entry.s_comp := true.B
  }

  txrspArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.ready && e.w_snpRsp && e.s_urgentRead && !e.s_comp
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
  when(txrsp.fire) {
    val entry = buffer(txrspArb.io.chosen)
    entry.s_comp := true.B
  }

  // if none of the snoops return the required data,
  // then a ReadNoSnp needs to be sent to memory
  val urgent_vec = buffer.map(e => e.valid && !e.s_urgentRead)
  val urgentIdx = PriorityEncoder(urgent_vec)
  val readUrgently = Cat(urgent_vec).orR
  val urgentTask = Wire(new Task())
  when(urgentRead.fire) {
    buffer(urgentIdx).s_urgentRead := true.B
  }
  urgentTask := buffer(urgentIdx).task
  urgentTask.chiOpcode := ReadNoSnp
  urgentTask.txnID := buffer(urgentIdx).task.reqID
  urgentTask.size := log2Ceil(64).U
  urgentTask.allowRetry := false.B
  urgentTask.order := OrderEncodings.None
  urgentTask.memAttr := MemAttr()
  urgentTask.snpAttr := false.B
  urgentTask.expCompAck := false.B

  urgentRead.valid := readUrgently
  urgentRead.bits := urgentTask

  /* Dealloc */
  val will_free_vec = buffer.map(e => e.valid && e.ready && e.s_comp && e.w_compack && e.w_snpRsp && e.s_urgentRead)
  buffer.zip(will_free_vec).foreach { case (e, v) =>
    when(v) {
      e.valid := false.B
      e.ready := false.B
    }
  }

  /* block info */
  io.respInfo.zipWithIndex.foreach { case (m, i) =>
    val will_free = will_free_vec(i)
    m.valid := buffer(i).valid && !will_free
    m.bits.tag := buffer(i).task.tag
    m.bits.set := buffer(i).task.set
    m.bits.opcode := buffer(i).task.chiOpcode
    m.bits.reqID := buffer(i).task.reqID
    m.bits.w_compack := buffer(i).w_compack || rsp.valid && rsp.bits.opcode === CompAck &&
      rsp.bits.txnID === buffer(i).task.reqID
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs)(0.U(16.W))))
    buffer.zip(bufferTimer).map { case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < 20000.U, "ResponseBuf Leak")
    }
  }

}
