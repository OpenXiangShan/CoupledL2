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
    val fromMSHRCtl = new Bundle() {
      val mshr_alloc_ptr = Input(UInt(mshrBits.W))
    }
  })

  /* Stage 3 */
  val task_s3 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  task_s3.valid := io.taskFromArb_s2.valid
  when(io.taskFromArb_s2.valid) {
    task_s3.bits := io.taskFromArb_s2.bits
  }

  val dirResult_s3 = io.dirResp_s3.bits
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

  /* Stage 4 */
  val need_acquire_s4 = RegEnable(need_acquire_s3, task_s3.valid)


  /* Signals to MSHR Ctl */

  // Acquire downwards at MainPipe
  io.toMSHRCtl.need_acquire_s3 := need_acquire_s3
  io.toMSHRCtl.infoA_s3.addr := req_s3.addr
  io.toMSHRCtl.infoA_s3.opcode := Mux(dirResult_s3.hit, AcquirePerm, AcquireBlock)
  io.toMSHRCtl.infoA_s3.param := Mux(req_needT_s3, Mux(dirResult_s3.hit, BtoT, NtoT), NtoB)
  io.toMSHRCtl.infoA_s3.source := io.fromMSHRCtl.mshr_alloc_ptr  // TODO

  // Allocation of MSHR
  io.toMSHRCtl.mshr_alloc_s3.valid := task_s3.valid && !mshr_req_s3 &&
    ((dirResult_s3.hit && alloc_on_hit_s3) || (!dirResult_s3.hit && alloc_on_miss_s3))
  io.toMSHRCtl.mshr_alloc_s3.bits.addr := task_s3.bits.addr
  io.toMSHRCtl.mshr_alloc_s3.bits.way := dirResult_s3.way
  io.toMSHRCtl.mshr_alloc_s3.bits.state := 0.U.asTypeOf(new FSMState())
}
