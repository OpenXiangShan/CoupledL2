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

class ResponseState(implicit p: Parameters) extends LLCBundle {
  val s_comp = Bool()
  val s_urgentRead = Bool()
  val w_datRsp = Bool()
  val w_snpRsp = Bool()
  val w_compack = Bool()
  val w_comp = Bool()
}

class ResponseRequest(withData: Boolean)(implicit p: Parameters) extends LLCBundle {
  val state = new ResponseState()
  val task  = new Task()
  val data  = if (withData) Some(new DSBlock()) else None
}

class ResponseEntry(implicit p: Parameters) extends TaskEntry {
  val state = new ResponseState()
  val data = new DSBlock()
  val beatValids = Vec(beatSize, Bool())
}

class ResponseInfo(implicit p: Parameters) extends BlockInfo {
  val w_snpRsp = Bool()
  val w_compdata = Bool()
  val w_compack = Bool()
}

class ResponseUnit(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    /* Comp(DBIDResp/Data) request from MainPipe */
    val fromMainPipe = new Bundle() {
      val alloc_s4 = Flipped(ValidIO(new ResponseRequest(withData = false)))
      val alloc_s6 = Flipped(ValidIO(new ResponseRequest(withData = true)))
    }

    /* response from downstream memory */
    val snRxdat = Flipped(ValidIO(new RespWithData()))
    val snRxrsp = Flipped(ValidIO(new Resp()))

    /* fake CompData response from MemUnit */
    val bypassData = Flipped(Vec(beatSize, ValidIO(new RespWithData())))

    /* snoop response data from upper-level cache */
    val rnRxdat = Flipped(ValidIO(new RespWithData()))
  
    /* CompAck/SnpResp from upstream RXRSP channel */
    val rnRxrsp = Flipped(ValidIO(new Resp()))

    /* generate responses sent to the Request Node. */
    val txrsp = DecoupledIO(new Task()) // Comp(DBIDResp)
    val txdat = DecoupledIO(new TaskWithData()) // CompData

    /* Response buffers info */
    val respInfo = Vec(mshrs.response, ValidIO(new ResponseInfo()))

    /* instruct MemUnit to immediately send a read request */
    val urgentRead = DecoupledIO(new Task())
  })

  val alloc_s6   = io.fromMainPipe.alloc_s6
  val alloc_s4   = io.fromMainPipe.alloc_s4
  val memData    = io.snRxdat
  val memResp    = io.snRxrsp
  val snpData    = io.rnRxdat
  val bypassData = io.bypassData
  val txrsp      = io.txrsp
  val txdat      = io.txdat
  val urgentRead = io.urgentRead

  /* Data Structure */
  val buffer = RegInit(VecInit(Seq.fill(mshrs.response)(0.U.asTypeOf(new ResponseEntry()))))
  val txrspArb = Module(new FastArbiter(new Task(), mshrs.response))
  val txdatArb = Module(new FastArbiter(new TaskWithData(), mshrs.response))

  /* Alloc */
  val freeVec_s6 = VecInit(buffer.map(!_.valid))
  val idOH_s6    = PriorityEncoderOH(freeVec_s6)
  val freeVec_s4 = 
    Mux(
      alloc_s6.valid,
      VecInit(freeVec_s6.zip(idOH_s6).map{ case (x, y) => x && ~y }).asUInt,
      freeVec_s6.asUInt
    )
  val idOH_s4      = PriorityEncoderOH(freeVec_s4)
  val insertIdx_s6 = OHToUInt(idOH_s6)
  val insertIdx_s4 = OHToUInt(idOH_s4)

  val full_s6  = !(Cat(freeVec_s6).orR)
  val full_s4  = !freeVec_s4.orR
  val canAlloc_s6 = alloc_s6.valid && !full_s6
  val canAlloc_s4 = alloc_s4.valid && !full_s4

  // Task_s6 can only be CompData
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

  assert(!(full_s6 && alloc_s6.valid || full_s4 && alloc_s4.valid) , "ResponseBuf overflow")

  /* Update state */
  def handleMemResp(response: Valid[RespWithData], isBypass: Boolean): Unit = {
    when(response.valid) {
      val update_vec = buffer.map(e =>
        e.task.reqID === response.bits.txnID && e.valid && response.bits.opcode === CompData
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        val beatId = response.bits.dataID >> log2Ceil(beatBytes / 16)
        entry.state.w_datRsp := { if (!isBypass) PopCount(entry.beatValids) === (beatSize - 1).U else true.B }
        entry.beatValids(beatId) := true.B
        entry.data.data(beatId) := response.bits.data
      }
    }
  }

  def handleMemResp(response: Valid[Resp]): Unit = {
    when(response.valid) {
      val update_vec = buffer.map(e =>
        e.task.reqID === response.bits.txnID && e.valid && (response.bits.opcode === CompDBIDResp ||
        response.bits.opcode === Comp)
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        entry.state.w_comp := true.B
      }
    }
  }

  def handleSnpResp(snpRsp: Valid[Resp], snpData: Valid[RespWithData]): Unit = {
    when(snpData.valid) {
      val update_vec = buffer.map(e =>
        e.task.reqID === snpData.bits.txnID && e.valid && !e.task.replSnp && snpData.bits.opcode === SnpRespData
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        val beatId = snpData.bits.dataID >> log2Ceil(beatBytes / 16)
        val newBeatValids = entry.beatValids.asUInt | UIntToOH(beatId)
        val src_idOH = UIntToOH(snpData.bits.srcID)(numRNs - 1, 0)
        val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH).asBools)
        val isReadUnique = entry.task.chiOpcode === ReadUnique
        val isCleanInvalid = entry.task.chiOpcode === CleanInvalid
        val isCleanShared = entry.task.chiOpcode === CleanShared
        entry.beatValids := VecInit(newBeatValids.asBools)
        entry.task.snpVec := newSnpVec
        entry.state.w_datRsp := newBeatValids.andR
        entry.state.w_snpRsp := !Cat(newSnpVec).orR
        entry.data.data(beatId) := snpData.bits.data
        when(snpData.bits.resp(2)) {
          when(isReadUnique) {
            entry.task.resp := setPD(entry.task.resp)
          }.elsewhen(isCleanInvalid || isCleanShared) {
            entry.state.w_comp := false.B
          }
        }
      }
    }

    when(snpRsp.valid) {
      val update_vec = buffer.map(e =>
        e.task.reqID === snpRsp.bits.txnID && e.valid && !e.state.w_snpRsp && snpRsp.bits.opcode === SnpResp
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        val src_idOH = UIntToOH(snpRsp.bits.srcID)(numRNs - 1, 0)
        val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH).asBools)
        entry.task.snpVec := newSnpVec
        entry.state.w_snpRsp := !Cat(newSnpVec).orR
        entry.state.s_urgentRead := Cat(newSnpVec).orR || entry.beatValids.asUInt.orR || entry.state.w_datRsp
      }
    }

    when(snpData.valid && snpRsp.valid) {
      when(snpData.bits.opcode === SnpRespData && snpRsp.bits.opcode === SnpResp) {
        when(snpData.bits.txnID === snpRsp.bits.txnID) {
          val update_vec = buffer.map(e => e.task.reqID === snpRsp.bits.txnID && e.valid && !e.state.w_snpRsp)
          assert(PopCount(update_vec) < 2.U, "Response task repeated")
          val update_id = PriorityEncoder(update_vec)
          val entry = buffer(update_id)
          val canUpdate = Cat(update_vec).orR
          when(canUpdate) {
            val src_idOH_dat = UIntToOH(snpData.bits.srcID)(numRNs - 1, 0)
            val src_idOH_rsp = UIntToOH(snpRsp.bits.srcID)(numRNs - 1, 0)
            val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH_dat & ~src_idOH_rsp).asBools)
            entry.task.snpVec := newSnpVec
            entry.state.w_snpRsp := !Cat(newSnpVec).orR
          }
        }
      }
    }
  }

  def handleCompAck(ack: Valid[Resp]): Unit = {
    when(ack.valid) {
      val update_vec = buffer.map(e =>
        e.task.reqID === ack.bits.txnID && e.valid && e.state.w_datRsp && e.state.s_comp && !e.state.w_compack &&
        ack.bits.opcode === CompAck
      )
      assert(PopCount(update_vec) < 2.U, "Response task repeated")
      val canUpdate = Cat(update_vec).orR
      val update_id = PriorityEncoder(update_vec)
      when(canUpdate) {
        val entry = buffer(update_id)
        entry.state.w_compack := true.B
      }
    }
  }

  handleMemResp(memData, isBypass = false)
  handleMemResp(memResp)
  handleSnpResp(io.rnRxrsp, snpData)
  handleCompAck(io.rnRxrsp)
  for (i <- 0 until beatSize) {
    handleMemResp(bypassData(i), isBypass = true)
  }

  /* Issue */
  val isRead = buffer.map(e => e.task.chiOpcode === ReadUnique || e.task.chiOpcode === ReadNotSharedDirty ||
    e.task.chiOpcode === ReadNoSnp)
  txdatArb.io.in.zip(buffer).zip(isRead).foreach { case ((in, e), r) =>
    in.valid := e.valid && e.state.w_datRsp && e.state.w_snpRsp && e.state.s_urgentRead && !e.state.s_comp && r
    in.bits.task := e.task
    in.bits.data := e.data
  }
  txdatArb.io.out.ready := txdat.ready
  txdat.valid := txdatArb.io.out.valid
  txdat.bits := txdatArb.io.out.bits
  txdat.bits.task.chiOpcode := CompData
  when(txdat.fire) {
    val entry = buffer(txdatArb.io.chosen)
    entry.state.s_comp := true.B
  }

  txrspArb.io.in.zip(buffer).zip(isRead).foreach { case ((in, e), r) =>
    in.valid := e.valid && e.state.w_datRsp && e.state.w_snpRsp && e.state.w_comp && e.state.s_urgentRead &&
      !e.state.s_comp && !r
    in.bits := e.task
  }
  txrspArb.io.out.ready := txrsp.ready
  txrsp.valid := txrspArb.io.out.valid
  txrsp.bits := txrspArb.io.out.bits
  txrsp.bits.chiOpcode := Mux(
    txrspArb.io.out.bits.chiOpcode === WriteBackFull || txrspArb.io.out.bits.chiOpcode === WriteCleanFull,
    CompDBIDResp,
    Comp
  )
  when(txrsp.fire) {
    val entry = buffer(txrspArb.io.chosen)
    entry.state.s_comp := true.B
  }

  // if none of the snoops return the required data,
  // then a ReadNoSnp needs to be sent to memory
  val urgent_vec = buffer.map(e => e.valid && !e.state.s_urgentRead)
  val urgentIdx = PriorityEncoder(urgent_vec)
  val readUrgently = Cat(urgent_vec).orR
  val urgentTask = Wire(new Task())
  when(urgentRead.fire) {
    buffer(urgentIdx).state.s_urgentRead := true.B
  }
  urgentTask := buffer(urgentIdx).task
  urgentTask.chiOpcode := ReadNoSnp
  urgentTask.txnID := buffer(urgentIdx).task.reqID
  urgentTask.size := log2Ceil(64).U
  urgentTask.allowRetry := true.B
  urgentTask.order := OrderEncodings.None
  urgentTask.memAttr := MemAttr()
  urgentTask.snpAttr := false.B
  urgentTask.expCompAck := false.B

  urgentRead.valid := readUrgently
  urgentRead.bits := urgentTask

  /* Dealloc */
  val will_free_vec = buffer.map(e => e.valid && e.state.w_datRsp && e.state.s_comp && e.state.w_compack &&
    e.state.w_snpRsp && e.state.w_comp && e.state.s_urgentRead)
  buffer.zip(will_free_vec).foreach { case (e, v) =>
    when(v) {
      e.valid := false.B
      e.state.w_datRsp := false.B
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
    m.bits.w_compdata := buffer(i).state.w_datRsp
    m.bits.w_snpRsp := buffer(i).state.w_snpRsp
    m.bits.w_compack := buffer(i).state.w_compack || io.rnRxrsp.valid && io.rnRxrsp.bits.opcode === CompAck &&
      io.rnRxrsp.bits.txnID === buffer(i).task.reqID
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs.response)(0.U(16.W))))
    buffer.zip(bufferTimer).zipWithIndex.map { case ((e, t), i) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < timeoutThreshold.U, "ResponseBuf Leak(id: %d)", i.U)
    }
  }

}
