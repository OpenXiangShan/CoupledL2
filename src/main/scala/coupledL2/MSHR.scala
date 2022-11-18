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
  val source_a = DecoupledIO(new SourceAReq) // To AcquireUnit  // TODO: no need to use decoupled handshake
  val mainpipe = DecoupledIO(new TaskBundle) // To Mainpipe (SourceC or SourceD)
}

class MSHRResps(implicit p: Parameters) extends L2Bundle {
  val sink_c = Flipped(ValidIO(new RespInfoBundle))
  val sink_d = Flipped(ValidIO(new RespInfoBundle))
  val sink_e = Flipped(ValidIO(new RespInfoBundle))
}

class MSHR(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val id = Input(UInt(mshrBits.W))
    val status = ValidIO(new MSHRStatus)
    val alloc = Flipped(ValidIO(new MSHRRequest))
    val tasks = new MSHRTasks()
    val resps = new MSHRResps()
  })

  def odOpGen(r: UInt) = {
    val grantOp = GrantData
    val opSeq = Seq(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, grantOp, Grant)
    val opToA = VecInit(opSeq)(r)
    opToA
  }

  val initState = Wire(new FSMState())
  val state = RegInit(new FSMState(), initState)
  initState.elements.foreach(_._2 := true.B)
  val dirResult = RegInit(0.U.asTypeOf(new DirResult()))

  /* MSHR Allocation */
  val status_reg = RegInit(0.U.asTypeOf(Valid(new MSHRStatus())))
  when(io.alloc.valid) {
    status_reg.valid := true.B
    state := io.alloc.bits.state
    dirResult := io.alloc.bits.dirResult
    val ms_task = io.alloc.bits.task
    status_reg.bits.channel := ms_task.channel
    status_reg.bits.tag := ms_task.tag
    status_reg.bits.set := ms_task.set
    status_reg.bits.off := ms_task.off
    status_reg.bits.way := ms_task.way
    status_reg.bits.opcode := ms_task.opcode
    status_reg.bits.param := ms_task.param
    status_reg.bits.source := ms_task.sourceId
  }

  /* Intermediate logic */
  val req = status_reg.bits
  val meta = dirResult.meta
  val gotT = RegInit(false.B) // L3 might return T even though L2 wants B
  val meta_no_client = !meta.clients.orR

  val req_needT = needT(req.opcode, req.param)
  val req_acquire = req.opcode === AcquireBlock || req.opcode === AcquirePerm
  val req_promoteT = req_acquire && Mux(dirResult.hit, meta_no_client && meta.state === TIP, gotT)

  /* Task allocation */
  io.tasks.source_a.valid := !state.s_acquire && state.s_release && state.s_pprobe
  io.tasks.mainpipe.valid := !state.s_refill && state.w_grantlast && state.w_pprobeack  // refill when grantlast, TODO: opt?

  val oa = io.tasks.source_a.bits
  oa := DontCare
  oa.tag := req.tag
  oa.set := req.set
  oa.off := req.off
  oa.source := io.id
  oa.opcode := Mux(dirResult.hit, AcquirePerm, AcquireBlock)
  oa.param := Mux(req_needT, Mux(dirResult.hit, BtoT, NtoT), NtoB)

  val od = io.tasks.mainpipe.bits
  od := DontCare
  od.channel := req.channel
  od.tag := req.tag
  od.set := req.set
  od.off := req.off
  od.sourceId := req.source
  od.opcode := odOpGen(req.opcode)
  od.param :=
    MuxLookup(req.param, req.param, Seq(NtoB -> Mux(req_promoteT, toT, toB), BtoT -> toT, NtoT -> toT))
  // TODO: write tag/meta
  od.mshrTask := true.B
  od.mshrId := io.id

  /* Task update */
  when(io.tasks.source_a.fire) {
    state.s_acquire := true.B
  }
  when(io.tasks.mainpipe.fire) {
    state.s_refill := true.B
  }

  /* Refill response */
  val d_resp = io.resps.sink_d
  when(d_resp.valid) {
    when(d_resp.bits.opcode === Grant || d_resp.bits.opcode === GrantData) {
      state.w_grantfirst := true.B
      state.w_grantlast := d_resp.bits.last
      state.w_grant := status_reg.bits.off === 0.U || d_resp.bits.last  // TODO? why offset?
    }
    when(d_resp.bits.opcode === ReleaseAck) {
      state.w_releaseack := true.B
    }
  }

  when (io.resps.sink_e.valid) {
    state.w_grantack := true.B
  }

  io.status.valid := status_reg.valid
  io.status.bits <> status_reg.bits

  dontTouch(state)
}
