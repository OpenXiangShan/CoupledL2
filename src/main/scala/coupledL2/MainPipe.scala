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

package coupledL2

import chisel3._
import chisel3.util._
import coupledL2.utils._
import coupledL2.MetaData._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._

class MainPipe(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    /* receive task from arbiter */
    val taskFromArb_s2 = Flipped(ValidIO(new TaskBundle()))

    /* get dir result at stage 3 */
    val dirResp_s3 = Flipped(ValidIO(new DirResult))

    val toMSHRCtl = new Bundle() {
      val need_acquire_s3 = Output(Bool())
      val infoA_s3 = Output(new Bundle() {
        val addr = UInt(addressBits.W)
        val opcode = UInt(3.W)
        val param = UInt(3.W)
        val source = UInt(sourceIdBits.W)
      })
      val mshr_alloc_s3 = ValidIO(new MSHRRequest())
    }

    val fromMSHRCtl = new Bundle() {
      val mshr_alloc_ptr = Input(UInt(mshrBits.W))
    }

    /* DONT pass data in pipeline, use a buffer to save data */
    val bufRead = Output(ValidIO(new PipeBufferRead))
    val bufResp = Input(new PipeBufferResp)

    /* get ReleaseBuffer and RefillBuffer read result */
    val refillBufResp_s3 = ValidIO(new DSBlock)
    val releaseBufResp_s3 = ValidIO(new DSBlock)

    /* read or write data storage */
    val toDS = new Bundle() {
      val req_s3 = ValidIO(new DSRequest)
      val rdata_s5 = Input(new DSBlock)
      val wdata_s3 = Output(new DSBlock) 
    }

    /* send Release/Grant/ProbeAck via SourceC/D channels */
    val toSourceC = DecoupledIO(new TLBundleC(edgeOut.bundle))
    val toSourceD = DecoupledIO(new TLBundleD(edgeOut.bundle))

    /* write dir, including reset dir */
    val metaWReq = ValidIO(new MetaWrite)
    val tagWReq = ValidIO(new TagWrite)
  })

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)

  val c_s2, c_s3 = Wire(io.toSourceC.cloneType)
  val d_s2, d_s3 = Wire(io.toSourceD.cloneType)

  def toTLBundleC(task: TaskBundle, data: UInt = 0.U) = {
    val c = Wire(new TLBundleC(edgeOut.bundle))
    c := DontCare
    c.opcode := task.opcode
    c.param := task.param
    c.size := offsetBits.U
    c.source := task.mshrId
    c.address := Cat(task.tag, task.set, task.off)
    c.data := data
    c.corrupt := false.B
    c
  }

  def toTLBundleD(task: TaskBundle, data: UInt = 0.U) = {
    val d = Wire(new TLBundleD(edgeOut.bundle))
    d := DontCare
    d.opcode := task.opcode
    d.param := task.param
    d.size := offsetBits.U
    d.source := task.sourceId
    d.sink := task.mshrId
    d.denied := false.B
    d.data := data
    d.corrupt := false.B
    d
  }

  /* ======== Stage 2 ======== */
  // send out MSHR task if data is not needed
  val task_s2 = io.taskFromArb_s2
  val hasData_s2 = task_s2.bits.opcode(0)
  val isGrant_s2 = task_s2.bits.fromA && task_s2.bits.opcode === Grant
  val task_ready_s2 = task_s2.bits.mshrTask && !hasData_s2 // this task is ready to leave pipeline, but channels might be not ready
  val chnl_ready_s2 = Mux(isGrant_s2, d_s2.ready, c_s2.ready) // channel is ready
  val chnl_fire_s2 = task_ready_s2 && chnl_ready_s2 // both task and channel are ready, this task actually leave pipeline
  c_s2.valid := io.taskFromArb_s2.valid && task_ready_s2 && !isGrant_s2
  d_s2.valid := io.taskFromArb_s2.valid && task_ready_s2 && isGrant_s2
  c_s2.bits := toTLBundleC(task_s2.bits)
  d_s2.bits := toTLBundleD(task_s2.bits)

  io.bufRead.valid := task_s2.valid && task_s2.bits.fromC && task_s2.bits.opcode(0)
  io.bufRead.bits.bufIdx := task_s2.bits.bufIdx

  /* ======== Stage 3 ======== */
  val task_s3 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  task_s3.valid := io.taskFromArb_s2.valid && !chnl_fire_s2
  when(io.taskFromArb_s2.valid) {
    task_s3.bits := io.taskFromArb_s2.bits
  }
  // val need_retry_s3 = WireInit(false.B)
  // val hazard_s3 = WireInit(false.B)
  // val ls_op_done_m3 = WireInit(false.B)

  val dirResult_s3 = io.dirResp_s3.bits
  val meta_s3 = dirResult_s3.meta
  val meta_has_clients_s3 = meta_s3.clients.orR

  val req_s3 = task_s3.bits
  val mshr_req_s3 = req_s3.mshrTask
  val req_acquire_s3 = req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm
  val req_prefetch_s3 = req_s3.opcode === Hint
  val req_needT_s3 = needT(req_s3.opcode, req_s3.param)

  val acquire_on_miss_s3 = req_acquire_s3 || req_prefetch_s3
  val acquire_on_hit_s3 = meta_s3.state === BRANCH && req_needT_s3
  // For channel A reqs, alloc mshr when acquire downwards is needed
  val need_mshr_s3_a = //task_s3.valid && !mshr_req_s3 &&
    ((dirResult_s3.hit && acquire_on_hit_s3) || (!dirResult_s3.hit && acquire_on_miss_s3))
  // For channel B reqs, alloc mshr when Probe hits in both self and client dir
  val need_mshr_s3_b = dirResult_s3.hit && req_s3.fromB &&
    !(meta_s3.state === BRANCH && req_s3.param === toB) &&
    meta_has_clients_s3
  // For channel C reqs, Release will always hit on MainPipe.
  val need_mshr_s3 = need_mshr_s3_a || need_mshr_s3_b

  val alloc_on_hit_s3 = false.B  // TODO
  val alloc_on_miss_s3 = true.B  // TODO

  /* Signals to MSHR Ctl */

  // Acquire downwards at MainPipe
  io.toMSHRCtl.need_acquire_s3 := false.B //need_acquire_s3 TODO: fast acquire
  io.toMSHRCtl.infoA_s3.addr := 0.U // TODO: fast acquire
  io.toMSHRCtl.infoA_s3.opcode := Mux(dirResult_s3.hit, AcquirePerm, AcquireBlock)
  io.toMSHRCtl.infoA_s3.param := Mux(req_needT_s3, Mux(dirResult_s3.hit, BtoT, NtoT), NtoB)
  io.toMSHRCtl.infoA_s3.source := io.fromMSHRCtl.mshr_alloc_ptr // TODO

  // Allocation of MSHR: new request only
  val alloc_state = WireInit(0.U.asTypeOf(new FSMState()))
  alloc_state.elements.foreach(_._2 := true.B)
  io.toMSHRCtl.mshr_alloc_s3.valid := task_s3.valid && !mshr_req_s3 && need_mshr_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.set := req_s3.set
  io.toMSHRCtl.mshr_alloc_s3.bits.tag := req_s3.tag
  io.toMSHRCtl.mshr_alloc_s3.bits.off := req_s3.off
  io.toMSHRCtl.mshr_alloc_s3.bits.way := dirResult_s3.way
  io.toMSHRCtl.mshr_alloc_s3.bits.opcode := req_s3.opcode
  io.toMSHRCtl.mshr_alloc_s3.bits.param := req_s3.param
  io.toMSHRCtl.mshr_alloc_s3.bits.source := req_s3.sourceId
  io.toMSHRCtl.mshr_alloc_s3.bits.dirResult := dirResult_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.state := alloc_state

  val sink_resp_s3 = WireInit(0.U.asTypeOf(Valid(new TaskBundle))) // resp for sinkA/B/C request that does not need to alloc mshr
  val sink_resp_s3_opcode_a = Mux(req_s3.opcode === AcquirePerm && req_s3.param === BtoT, Grant, GrantData)
  val sink_resp_s3_opcode_b = Mux(meta_s3.state === TIP && meta_s3.dirty, ProbeAckData, ProbeAck)
  val sink_resp_s3_param_a = Mux(req_s3.param === NtoB, toB, toT)
  val sink_resp_s3_param_b = Mux(
    !dirResult_s3.hit,
    NtoN,
    MuxLookup(Cat(req_s3.param, meta_s3.state), BtoB, Seq(
      Cat(toN, BRANCH) -> BtoN,
      Cat(toN, TIP)    -> TtoN,
      Cat(toB, TIP)    -> TtoB,
      Cat(toT, TIP)    -> TtoT
    )) // other combinations should miss or have mshr allocated
  )
  sink_resp_s3.valid := task_s3.valid && !mshr_req_s3 && !need_mshr_s3
  sink_resp_s3.bits := task_s3.bits
  sink_resp_s3.bits.opcode := Mux(
    req_s3.fromA,
    sink_resp_s3_opcode_a,
    Mux(
      req_s3.fromB,
      sink_resp_s3_opcode_b,
      ReleaseAck
    )
  )
  sink_resp_s3.bits.param := Mux(
    req_s3.fromA,
    sink_resp_s3_param_a,
    Mux(
      req_s3.fromB,
      sink_resp_s3_param_b,
      0.U // param of ReleaseAck must be 0
    )
  )

  val source_req_s3 = Wire(new TaskBundle)
  // source_req_s3.valid := task_s3.valid && mshr_req_s3 || sink_resp_s3.valid
  source_req_s3 := Mux(sink_resp_s3.valid, sink_resp_s3.bits, req_s3)
  val data_s3 = Mux(io.refillBufResp_s3.valid, io.refillBufResp_s3.bits.data, io.releaseBufResp_s3.bits.data)
  val hasData_s3 = source_req_s3.opcode(0)
  val mshr_release_s3 = mshr_req_s3 && req_s3.opcode(2, 1) === Release(2, 1) // Release or ReleaseData from mshr
  val mshr_grant_s3 = mshr_req_s3 && req_s3.opcode(2, 1) === Grant(2, 1) && req_s3.fromA // Grant or GrantData from mshr
  val mshr_grantdata_s3 = mshr_req_s3 && req_s3.opcode === GrantData && req_s3.fromA
  val mshr_probeack_s3 = mshr_req_s3 && req_s3.opcode(2, 1) === ProbeAck(2, 1) && req_s3.fromB // ProbeAck or ProbeAckData from mshr
  val mshr_probeackdata_s3 = mshr_req_s3 && req_s3.opcode === ProbeAckData && req_s3.fromB
  assert(!(io.refillBufResp_s3.valid && io.releaseBufResp_s3.valid))

  def getBeat(data: UInt, beatsOH: UInt): (UInt, UInt) = {
    // get one beat from data according to beatsOH
    require(data.getWidth == (blockBytes * 8))
    require(beatsOH.getWidth == beatSize)
    // next beat
    val next_beat = ParallelPriorityMux(beatsOH, data.asTypeOf(Vec(beatSize, UInt((beatBytes * 8).W))))
    // remaining beats that haven't been sent out
    val next_beatsOH = beatsOH & ~PriorityEncoderOH(beatsOH)
    (next_beat, next_beatsOH)
  }

  val beatsOH_s3 = Fill(beatSize, hasData_s3) // beats that needs to send out
  val beatsOH_ready_s3 = Fill(beatSize, mshr_req_s3) // beats that are already ready
  val beats_unready_s3 = (beatsOH_s3 & ~beatsOH_ready_s3).orR
  val (beat_s3, next_beatsOH_s3) = getBeat(data_s3, beatsOH_s3)

  c_s3.valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_release_s3 || mshr_probeack_s3,
    req_s3.fromB && !need_mshr_s3 && !beats_unready_s3
  )
  c_s3.bits := toTLBundleC(source_req_s3, beat_s3)
  d_s3.valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_grant_s3,
    req_s3.fromC || req_s3.fromA && !need_mshr_s3 && !beats_unready_s3
  )
  d_s3.bits := toTLBundleD(source_req_s3, beat_s3)
  val task_ready_s3 = !hasData_s3 || (!mshr_req_s3 && (req_s3.fromC || need_mshr_s3))
  val mshr_fire_s3 = !mshr_req_s3 && need_mshr_s3
  val chnl_fire_s3 = task_ready_s3 && (c_s3.fire() || d_s3.fire()) && !next_beatsOH_s3.orR

  // write/read data storage
  val wen_c = !mshr_req_s3 && req_s3.fromC && isParamFromT(req_s3.param) && req_s3.opcode(0)
  val wen_mshr_grant = mshr_grantdata_s3
  val wen_mshr_probeack = mshr_probeack_s3
  val wen = wen_c || wen_mshr_grant || wen_mshr_probeack
  val need_data_on_hit_a = req_s3.fromA && !mshr_req_s3 && req_s3.opcode === AcquireBlock && (isT(meta_s3.state) || req_s3.param === NtoB)
  // read data ahead of time to prepare for ReleaseData later 
  val need_data_on_miss_a = req_s3.fromA && !mshr_req_s3 && !dirResult_s3.hit && (meta_s3.state === TRUNK || meta_s3.state === TIP && meta_s3.dirty)
  val need_data_b = req_s3.fromB && !mshr_req_s3 && dirResult_s3.hit && (meta_s3.state === TRUNK || meta_s3.state === TIP && meta_s3.dirty)
  val ren = Mux(dirResult_s3.hit, need_data_on_hit_a, need_data_on_miss_a) || need_data_b
  io.toDS.req_s3.valid := task_s3.valid && (ren || wen)
  io.toDS.req_s3.bits.way := Mux(mshr_req_s3, req_s3.way, dirResult_s3.way)
  io.toDS.req_s3.bits.set := dirResult_s3.set
  io.toDS.req_s3.bits.wen := wen
  io.toDS.wdata_s3.data := Mux(
    !mshr_req_s3,
    RegNext(io.bufResp.data.asUInt),
    Mux(
      req_s3.fromA,
      io.refillBufResp_s3.bits.data,
      io.releaseBufResp_s3.bits.data
    )
  )

  // write dir
  when (!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when (resetIdx === 0.U) {
    resetFinish := true.B
  }
  val metaW_valid_s3_a = !mshr_req_s3 && req_s3.fromA && !need_mshr_s3_a && !meta_s3.clients.orR
  val metaW_valid_s3_b = !mshr_req_s3 && req_s3.fromB && !need_mshr_s3_b && (meta_s3.state === TIP || meta_s3.state === BRANCH && req_s3.param === toN)
  val metaW_valid_s3_c = !mshr_req_s3 && req_s3.fromC
  val metaW_valid_s3_mshr = mshr_req_s3 && req_s3.metaWrite.valid
  require(clientBits == 1)
  val metaW_s3_a = MetaEntry(meta_s3.dirty, meta_s3.state, Fill(clientBits, true.B))
  val metaW_s3_b = Mux(req_s3.param === toN, MetaEntry(), MetaEntry(false.B, BRANCH, 0.U))
  val metaW_s3_c_dirty = meta_s3.dirty || wen_c
  val metaW_s3_c_state = Mux(
    isParamFromT(req_s3.param),
    TIP,
    meta_s3.state
  )
  val metaW_s3_c_clients = Fill(clientBits, !isToN(req_s3.param))
  val metaW_s3_c = MetaEntry(metaW_s3_c_dirty, metaW_s3_c_state, metaW_s3_c_clients)
  val metaW_s3_mshr = req_s3.metaWrite.bits
  io.metaWReq.valid := !resetFinish || task_s3.valid && (metaW_valid_s3_a || metaW_valid_s3_b || metaW_valid_s3_c || metaW_valid_s3_mshr)
  io.metaWReq.bits.set := Mux(resetFinish, req_s3.set, resetIdx)
  io.metaWReq.bits.wayOH := Mux(resetFinish, UIntToOH(Mux(mshr_req_s3, req_s3.way, dirResult_s3.way)), Fill(cacheParams.ways, true.B))
  io.metaWReq.bits.wmeta := Mux(
    resetFinish,
    ParallelPriorityMux(
      Seq(metaW_valid_s3_a, metaW_valid_s3_b, metaW_valid_s3_c, metaW_valid_s3_mshr),
      Seq(metaW_s3_a, metaW_s3_b, metaW_s3_c, metaW_s3_mshr)
    ),
    MetaEntry()
  )

  io.tagWReq.valid := task_s3.valid && mshr_grant_s3 && req_s3.tagWrite
  io.tagWReq.bits.set := req_s3.set
  io.tagWReq.bits.way := req_s3.way
  io.tagWReq.bits.wtag := req_s3.tag

  /* ======== Stage 4 ======== */
  val task_s4 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  task_s4.valid := task_s3.valid && !mshr_fire_s3 && !chnl_fire_s3
  when (task_s3.valid) {
    task_s4.bits := task_s3.bits
  }




  /* ======== Other Signals Assignment ======== */
  val meta_no_client = !meta_has_clients_s3
  val req_needT = needT(req_s3.opcode, req_s3.param)

  // Initial state assignment
  when(req_s3.fromA) {
    alloc_state.s_refill := req_s3.opcode === Hint // Q: Hint also needs refill?
    alloc_state.w_grantack := req_s3.opcode === Hint
    // need replacement
    when(!dirResult_s3.hit && meta_s3.state =/= INVALID) {
      alloc_state.s_release := false.B
      alloc_state.w_releaseack := false.B
      // need rprobe for release
      when(!meta_no_client) {
        alloc_state.s_rprobe := false.B
        alloc_state.w_rprobeackfirst := false.B
        alloc_state.w_rprobeacklast := false.B
      }
    }
    // need Acquire downwards
    when(!dirResult_s3.hit || meta_s3.state === BRANCH && req_needT) {
      alloc_state.s_acquire := false.B
      alloc_state.s_grantack := false.B
      alloc_state.w_grantfirst := false.B
      alloc_state.w_grantlast := false.B
      alloc_state.w_grant := false.B
    }
  }
  when(req_s3.fromB) {
    // Only consider the situation when mshr needs to be allocated
    alloc_state.s_pprobe := false.B
    alloc_state.w_pprobeackfirst := false.B
    alloc_state.w_pprobeacklast := false.B
    alloc_state.w_pprobeack := false.B
    alloc_state.s_probeack := false.B
  }

  assert(io.toSourceD.ready) // SourceD should always be ready
  assert(io.toSourceC.ready) // SourceC/wbq should be large enough to avoid blocking pipeline
  TLArbiter.lowest(edgeOut, io.toSourceC, c_s3, c_s2)
  TLArbiter.lowest(edgeIn, io.toSourceD, d_s3, d_s2)
}
