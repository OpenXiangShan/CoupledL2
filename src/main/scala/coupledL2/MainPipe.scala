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

    val wdata_en_s3 = Output(Bool())

    val fromMSHRCtl = new Bundle() {
      val mshr_alloc_ptr = Input(UInt(mshrBits.W))
    }
  })

  /* ======== Stage 3 ======== */
  val task_s3 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  task_s3.valid := io.taskFromArb_s2.valid
  when(io.taskFromArb_s2.valid) {
    task_s3.bits := io.taskFromArb_s2.bits
  }
  // val need_retry_s3 = WireInit(false.B)
  // val hazard_s3 = WireInit(false.B)
  // val ls_op_done_m3 = WireInit(false.B)

  val dirResult_s3 = io.dirResp_s3.bits
  val meta_s3 = dirResult_s3.meta
  val req_s3 = task_s3.bits
  val mshr_req_s3 = req_s3.mshrOpType =/= 0.U
  val req_acquire_s3 = req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm
  val req_prefetch_s3 = req_s3.opcode === Hint
  val req_needT_s3 = needT(req_s3.opcode, req_s3.param)

  val acquire_on_miss_s3 = req_acquire_s3 || req_prefetch_s3
  val acquire_on_hit_s3 = dirResult_s3.meta.state === BRANCH && req_needT_s3
  val need_acquire_s3 = task_s3.valid && !mshr_req_s3 &&
    ((dirResult_s3.hit && acquire_on_hit_s3) || (!dirResult_s3.hit && acquire_on_miss_s3))

  val alloc_on_hit_s3 = false.B  // TODO
  val alloc_on_miss_s3 = true.B  // TODO

  io.wdata_en_s3 := task_s3.valid && req_s3.opcode === ReleaseData

  /* ======== Stage 4 ======== */
  val need_acquire_s4 = RegEnable(need_acquire_s3, task_s3.valid)


  /* Signals to MSHR Ctl */

  // Acquire downwards at MainPipe
  io.toMSHRCtl.need_acquire_s3 := false.B //need_acquire_s3 TODO: fast acquire
  io.toMSHRCtl.infoA_s3.addr := req_s3.addr
  io.toMSHRCtl.infoA_s3.opcode := Mux(dirResult_s3.hit, AcquirePerm, AcquireBlock)
  io.toMSHRCtl.infoA_s3.param := Mux(req_needT_s3, Mux(dirResult_s3.hit, BtoT, NtoT), NtoB)
  io.toMSHRCtl.infoA_s3.source := io.fromMSHRCtl.mshr_alloc_ptr  // TODO

  // Allocation of MSHR: new request only
  val alloc_state = WireInit(0.U.asTypeOf(new FSMState()))
  alloc_state.elements.foreach(_._2 := true.B)
  io.toMSHRCtl.mshr_alloc_s3.valid := task_s3.valid && !mshr_req_s3 &&
    ((dirResult_s3.hit && alloc_on_hit_s3) || (!dirResult_s3.hit && alloc_on_miss_s3))
  io.toMSHRCtl.mshr_alloc_s3.bits.addr := task_s3.bits.addr
  io.toMSHRCtl.mshr_alloc_s3.bits.way := dirResult_s3.way
  io.toMSHRCtl.mshr_alloc_s3.bits.opcode := task_s3.bits.opcode
  io.toMSHRCtl.mshr_alloc_s3.bits.param := task_s3.bits.param
  io.toMSHRCtl.mshr_alloc_s3.bits.dirResult := dirResult_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.state := alloc_state

  val meta_no_client = !meta_s3.clients.orR
  val req_needT = needT(req_s3.opcode, req_s3.param)

  when(req_s3.fromA) {
    alloc_state.s_execute := req_s3.opcode === Hint
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
    // TODO
  }
  when(req_s3.fromC) { // May not allocate MSHR
  }
}
