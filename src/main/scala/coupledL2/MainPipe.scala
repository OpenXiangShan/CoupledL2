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

    /* read or write data storage */
    val toDS = new Bundle() {
      val req_s3 = ValidIO(new DSRequest)
      val rdata_s5 = Input(new DSBlock)
      val wdata_s3 = Output(new DSBlock) 
    }

    /* send Release/Grant/ProbeAck via SourceC/D channels */
    val toSourceC = DecoupledIO(new TLBundleC(edgeOut.bundle))
    val toSourceD = DecoupledIO(new TLBundleD(edgeOut.bundle))

    // TODO: reset Directory
  })

  val c_s2 = Wire(io.toSourceC.cloneType)
  val d_s2 = Wire(io.toSourceD.cloneType)

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
  val fire_s2 = task_ready_s2 && chnl_ready_s2 // both task and channel are ready, this task actually leave pipeline
  c_s2.valid := io.taskFromArb_s2.valid && task_ready_s2 && !isGrant_s2
  d_s2.valid := io.taskFromArb_s2.valid && task_ready_s2 && isGrant_s2
  c_s2.bits := toTLBundleC(task_s2.bits)
  d_s2.bits := toTLBundleD(task_s2.bits)

  /* ======== Stage 3 ======== */
  val task_s3 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  task_s3.valid := io.taskFromArb_s2.valid && !fire_s2
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
  io.toMSHRCtl.mshr_alloc_s3.bits.set := task_s3.bits.set
  io.toMSHRCtl.mshr_alloc_s3.bits.tag := task_s3.bits.tag
  io.toMSHRCtl.mshr_alloc_s3.bits.off := task_s3.bits.off
  io.toMSHRCtl.mshr_alloc_s3.bits.way := dirResult_s3.way
  io.toMSHRCtl.mshr_alloc_s3.bits.opcode := task_s3.bits.opcode
  io.toMSHRCtl.mshr_alloc_s3.bits.param := task_s3.bits.param
  io.toMSHRCtl.mshr_alloc_s3.bits.source := task_s3.bits.sourceId
  io.toMSHRCtl.mshr_alloc_s3.bits.dirResult := dirResult_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.state := alloc_state

  /* ======== Stage 4 ======== */




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
  TLArbiter.lowest(edgeOut, io.toSourceC,/* s6, s5, ...*/ c_s2)
  TLArbiter.lowest(edgeIn, io.toSourceD, d_s2)
}
