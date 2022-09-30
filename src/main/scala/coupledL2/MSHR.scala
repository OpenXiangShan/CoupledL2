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
import coupledL2.MetaData._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import chipsalliance.rocketchip.config.Parameters

class MSHRTasks(implicit p: Parameters) extends L2Bundle {
  // outer
  val source_a = DecoupledIO(new SourceAReq) // TODO: no need to use decoupled handshake
}

class MSHR(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val id = Input(UInt(mshrBits.W))
    val status = ValidIO(new MSHRStatus)
    val alloc = Flipped(ValidIO(new MSHRRequest))
    val tasks = new MSHRTasks()
    val resp_refillUnit = Flipped(ValidIO(new RefillUnitResp()))
  })

  val initState = Wire(new FSMState())
  val state = RegInit(new FSMState(), initState)
  initState.elements.foreach(_._2 := true.B)
  val meta = RegInit(0.U.asTypeOf(new DirResult()))

  /* MSHR Allocation */
  val (alloc_tag, alloc_set, alloc_offset) = parseAddress(io.alloc.bits.addr)
  val status_reg = RegInit(0.U.asTypeOf(Valid(new MSHRStatus())))
  when(io.alloc.valid) {
    status_reg.valid := true.B
    status_reg.bits.tag := alloc_tag
    status_reg.bits.set := alloc_set
    status_reg.bits.off := alloc_offset
    status_reg.bits.way := io.alloc.bits.way
    status_reg.bits.opcode := io.alloc.bits.opcode
    status_reg.bits.param := io.alloc.bits.param
    state := io.alloc.bits.state
    meta := io.alloc.bits.dirResult
  }

  /* Intermediate logic */
  val req = status_reg.bits
  val req_needT = needT(req.opcode, req.param)

  /* Task allocation */
  io.tasks.source_a.valid := !state.s_acquire && state.s_release && state.s_pprobe

  val oa = io.tasks.source_a.bits
  oa.tag := status_reg.bits.tag
  oa.set := status_reg.bits.set
  oa.off := status_reg.bits.off
  oa.source := io.id
  oa.opcode := Mux(meta.hit, AcquirePerm, AcquireBlock)
  oa.param := Mux(req_needT, Mux(meta.hit, BtoT, NtoT), NtoB)

  /* Task update */
  when(io.tasks.source_a.fire) {
    state.s_acquire := true.B
  }

  /* Refill response */
  when(io.resp_refillUnit.valid) {
    when(io.resp_refillUnit.bits.opcode === Grant || io.resp_refillUnit.bits.opcode === GrantData) {
      state.w_grantfirst := true.B
      state.w_grantlast := io.resp_refillUnit.bits.last
      state.w_grant := status_reg.bits.off === 0.U || io.resp_refillUnit.bits.last  // TODO? why offset?
    }
    when(io.resp_refillUnit.bits.opcode === ReleaseAck) {
      state.w_releaseack := true.B
    }
  }

  io.status.valid := status_reg.valid
  io.status.bits <> status_reg.bits

}
