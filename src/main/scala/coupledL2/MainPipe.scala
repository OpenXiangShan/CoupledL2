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
    /* receive task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new TaskBundle()))

    /* handle set conflict in req arb */
    val fromReqArb = Input(new Bundle() {
      val status_s1 = new PipeEntranceStatus
    })
    val toReqArb = Output(new BlockInfo())

    /* get dir result at stage 3 */
    val dirResp_s3 = Flipped(ValidIO(new DirResult))

    /* send task to MSHRCtl at stage 3 */
    val toMSHRCtl = new Bundle() {
      val mshr_alloc_s3 = ValidIO(new MSHRRequest())
    }

    val fromMSHRCtl = new Bundle() {
      val mshr_alloc_ptr = Input(UInt(mshrBits.W))
    }

    /* DONT pass data in pipeline, use a buffer to save data */ //TODO
    val bufRead = Output(ValidIO(new PipeBufferRead))
    val bufResp = Input(new PipeBufferResp)

    /* get ReleaseBuffer and RefillBuffer read result */
    val refillBufResp_s3 = Flipped(ValidIO(new DSBlock))
    val releaseBufResp_s3 = Flipped(ValidIO(new DSBlock))

    /* read or write data storage */
    val toDS = new Bundle() {
      val req_s3 = ValidIO(new DSRequest)
      val rdata_s5 = Input(new DSBlock)
      val wdata_s3 = Output(new DSBlock) 
    }

    /* send Release/Grant/ProbeAck via SourceC/D channels */
    val toSourceC, toSourceD = DecoupledIO(new Bundle() {
      val task = new TaskBundle
      val data = new DSBlock
    })

    /* write dir, including reset dir */
    val metaWReq = ValidIO(new MetaWrite)
    val tagWReq = ValidIO(new TagWrite)

    /* read DS and write data into ReleaseBuf when the task needs to replace */
    val releaseBufWrite = Flipped(new MSHRBufWrite()) // s5 & s6

    val nestedwb = Output(new NestedWriteback)
    val nestedwbData = Output(new DSBlock)
  })

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  /* block reqs when reset */
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }

  val c_s3, c_s4, c_s5 = Wire(io.toSourceC.cloneType)
  val d_s3, d_s4, d_s5 = Wire(io.toSourceD.cloneType)

  /* ======== Stage 2 ======== */
  // send out MSHR task if data is not needed
  val task_s2 = io.taskFromArb_s2
  val hasData_s2 = task_s2.bits.opcode(0)
  val isGrant_s2 = task_s2.bits.fromA && task_s2.bits.opcode === Grant

  io.bufRead.valid := task_s2.valid && task_s2.bits.fromC && task_s2.bits.opcode(0)
  io.bufRead.bits.bufIdx := task_s2.bits.bufIdx

  /* ======== Stage 3 ======== */
  val task_s3 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  task_s3.valid := task_s2.valid// && !chnl_fire_s2
  when(task_s2.valid) {
    task_s3.bits := task_s2.bits
  }

  val dirResult_s3 = io.dirResp_s3.bits
  val meta_s3 = dirResult_s3.meta
  val req_s3 = task_s3.bits

  val mshr_req_s3 = req_s3.mshrTask
  val req_acquire_s3 = (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm) && req_s3.fromA
  val req_prefetch_s3 = req_s3.opcode === Hint

  val meta_has_clients_s3 = meta_s3.clients.orR
  val req_needT_s3 = needT(req_s3.opcode, req_s3.param) // require T status to handle req
  //[Alias] TODO: consider 1 client for now
  val cache_alias = req_acquire_s3 && dirResult_s3.hit && meta_s3.clients(0) && meta_s3.alias(0) =/= req_s3.alias

  val acquire_on_miss_s3 = req_acquire_s3 || req_prefetch_s3 // TODO: remove this cause always acquire on miss?
  val acquire_on_hit_s3 = meta_s3.state === BRANCH && req_needT_s3
  // For channel A reqs, alloc mshr when: acquire downwards is needed || alias
  val need_acquire_s3_a = ((dirResult_s3.hit && acquire_on_hit_s3) || (!dirResult_s3.hit && acquire_on_miss_s3))
  val need_mshr_s3_a = need_acquire_s3_a || cache_alias
  // For channel B reqs, alloc mshr when Probe hits in both self and client dir
  val need_mshr_s3_b = dirResult_s3.hit && req_s3.fromB &&
    !(meta_s3.state === BRANCH && req_s3.param === toB) &&
    meta_has_clients_s3
  // For channel C reqs, Release will always hit on MainPipe, no need for MSHR
  val need_mshr_s3 = need_mshr_s3_a || need_mshr_s3_b

  /* Signals to MSHR Ctl */
  // Allocation of MSHR: new request only
  val alloc_state = WireInit(0.U.asTypeOf(new FSMState()))
  alloc_state.elements.foreach(_._2 := true.B)
  io.toMSHRCtl.mshr_alloc_s3 := DontCare
  io.toMSHRCtl.mshr_alloc_s3.valid := task_s3.valid && !mshr_req_s3 && need_mshr_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.dirResult := dirResult_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.state := alloc_state
  val ms_task = io.toMSHRCtl.mshr_alloc_s3.bits.task
  ms_task.channel := req_s3.channel
  ms_task.set := req_s3.set
  ms_task.tag := req_s3.tag
  ms_task.off := req_s3.off
  ms_task.alias := req_s3.alias
  ms_task.opcode := req_s3.opcode
  ms_task.param := req_s3.param
  ms_task.sourceId := req_s3.sourceId
  ms_task.aliasTask := cache_alias
  //
  ms_task.way := dirResult_s3.way
  //

  val sink_resp_s3 = WireInit(0.U.asTypeOf(Valid(new TaskBundle))) // resp for sinkA/B/C request that does not need to alloc mshr
  sink_resp_s3.valid := task_s3.valid && !mshr_req_s3 && !need_mshr_s3
  sink_resp_s3.bits := task_s3.bits

  sink_resp_s3.bits.mshrId := (1 << (mshrBits-1)).U + sink_resp_s3.bits.sourceId // extra id for reqs that do not enter mshr
  when(req_s3.fromA) {
    sink_resp_s3.bits.opcode := Mux(req_s3.opcode === AcquirePerm, Grant, GrantData)
    sink_resp_s3.bits.param  := Mux(req_s3.param === NtoB, toB, toT)
  }.elsewhen(req_s3.fromB) {
    sink_resp_s3.bits.opcode := Mux(meta_s3.state === TIP && meta_s3.dirty, ProbeAckData, ProbeAck)
    sink_resp_s3.bits.param  := Mux(!dirResult_s3.hit, NtoN,
      MuxLookup(Cat(req_s3.param, meta_s3.state), BtoB, Seq(
        Cat(toN, BRANCH) -> BtoN,
        Cat(toN, TIP)    -> TtoN,
        Cat(toB, TIP)    -> TtoB,
        Cat(toT, TIP)    -> TtoT
      )) // other combinations should miss or have mshr allocated
    )
  }.otherwise { // req_s3.fromC
    sink_resp_s3.bits.opcode := ReleaseAck
    sink_resp_s3.bits.param  := 0.U // param of ReleaseAck must be 0
  }

  val source_req_s3 = Wire(new TaskBundle)
  source_req_s3 := Mux(sink_resp_s3.valid, sink_resp_s3.bits, req_s3)
  val data_s3 = Mux(io.refillBufResp_s3.valid, io.refillBufResp_s3.bits.data, io.releaseBufResp_s3.bits.data)
  val hasData_s3 = source_req_s3.opcode(0)
  val mshr_grant_s3        = mshr_req_s3 && req_s3.fromA && req_s3.opcode(2, 1) === Grant(2, 1) // Grant or GrantData from mshr
  val mshr_grantdata_s3    = mshr_req_s3 && req_s3.fromA && req_s3.opcode === GrantData
  val mshr_probeack_s3     = mshr_req_s3 && req_s3.fromB && req_s3.opcode(2, 1) === ProbeAck(2, 1) // ProbeAck or ProbeAckData from mshr
  val mshr_probeackdata_s3 = mshr_req_s3 && req_s3.fromB && req_s3.opcode === ProbeAckData
  val mshr_release_s3      = mshr_req_s3 && req_s3.opcode(2, 1) === Release(2, 1) // voluntary Release or ReleaseData from mshr
  assert(!(io.refillBufResp_s3.valid && io.releaseBufResp_s3.valid))

  // def getBeat(data: UInt, beatsOH: UInt): (UInt, UInt) = {
  //   // get one beat from data according to beatsOH
  //   require(data.getWidth == (blockBytes * 8))
  //   require(beatsOH.getWidth == beatSize)
  //   // next beat
  //   val next_beat = ParallelPriorityMux(beatsOH, data.asTypeOf(Vec(beatSize, UInt((beatBytes * 8).W))))
  //   val selOH = PriorityEncoderOH(beatsOH)
  //   // remaining beats that haven't been sent out
  //   val next_beatsOH = beatsOH & ~selOH
  //   (next_beat, next_beatsOH)
  // }

  // val beatsOH_s3 = Fill(beatSize, hasData_s3) // beats that needs to send out
  // val beatsOH_ready_s3 = Fill(beatSize, mshr_req_s3) // beats that are already ready
  // val beats_unready_s3 = (beatsOH_s3 & ~beatsOH_ready_s3).orR
  // val (beat_s3, next_beatsOH_s3) = getBeat(data_s3, beatsOH_s3)

  // write/read data storage
  val wen_c = !mshr_req_s3 && req_s3.fromC && isParamFromT(req_s3.param) && req_s3.opcode(0)
  val wen_mshr_grant = mshr_grantdata_s3
  val wen_mshr_probeack = mshr_probeackdata_s3
  val wen = wen_c || wen_mshr_grant || wen_mshr_probeack
  val need_data_on_hit_a = req_s3.fromA && !mshr_req_s3 && req_s3.opcode === AcquireBlock && (isT(meta_s3.state) || req_s3.param === NtoB)
  // read data ahead of time to prepare for ReleaseData later 
  val need_data_on_miss_a = req_s3.fromA && !mshr_req_s3 && !dirResult_s3.hit && (meta_s3.state === TRUNK || meta_s3.state === TIP && meta_s3.dirty)
  val need_data_b = req_s3.fromB && !mshr_req_s3 && dirResult_s3.hit && (meta_s3.state === TRUNK || meta_s3.state === TIP && meta_s3.dirty)
  val need_data_alias = mshr_grantdata_s3 && req_s3.aliasTask

  val ren = Mux(dirResult_s3.hit, need_data_on_hit_a, need_data_on_miss_a) || need_data_b || need_data_alias
  val bufResp_s3 = RegNext(io.bufResp.data.asUInt)
  val need_write_releaseBuf = need_data_on_miss_a || need_data_b
  io.toDS.req_s3.valid := task_s3.valid && (ren || wen)
  io.toDS.req_s3.bits.way := Mux(mshr_req_s3, req_s3.way, dirResult_s3.way)
  io.toDS.req_s3.bits.set := Mux(mshr_req_s3, req_s3.set, dirResult_s3.set)
  io.toDS.req_s3.bits.wen := wen
  io.toDS.wdata_s3.data := Mux(
    !mshr_req_s3,
    bufResp_s3,
    Mux(
      req_s3.fromA,
      io.refillBufResp_s3.bits.data,
      io.releaseBufResp_s3.bits.data
    )
  )

  val metaW_valid_s3_a = !mshr_req_s3 && req_s3.fromA && !need_mshr_s3_a// && !meta_s3.clients.orR
  val metaW_valid_s3_b = !mshr_req_s3 && req_s3.fromB && !need_mshr_s3_b && (meta_s3.state === TIP || meta_s3.state === BRANCH && req_s3.param === toN)
  val metaW_valid_s3_c = !mshr_req_s3 && req_s3.fromC
  val metaW_valid_s3_mshr = mshr_req_s3 && req_s3.metaWen
  require(clientBits == 1)
  //[Alias] TODO: consider 1 client for now
  val metaW_s3_a = MetaEntry(meta_s3.dirty, Mux(req_needT_s3, TRUNK, meta_s3.state), Fill(clientBits, true.B), meta_s3.alias)
  val metaW_s3_b = Mux(req_s3.param === toN, MetaEntry(), MetaEntry(false.B, BRANCH, Fill(clientBits, true.B), meta_s3.alias))

  val metaW_s3_c_dirty = meta_s3.dirty || wen_c
  val metaW_s3_c_state = Mux(isParamFromT(req_s3.param), TIP, meta_s3.state)
  val metaW_s3_c_clients = Fill(clientBits, !isToN(req_s3.param))
  val metaW_s3_c = MetaEntry(metaW_s3_c_dirty, metaW_s3_c_state, metaW_s3_c_clients, meta_s3.alias)

  val metaW_s3_mshr = req_s3.meta

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

  io.tagWReq.valid := task_s3.valid && mshr_grant_s3 && req_s3.tagWen
  io.tagWReq.bits.set := req_s3.set
  io.tagWReq.bits.way := req_s3.way
  io.tagWReq.bits.wtag := req_s3.tag

  val alias_grantdata_s3 = mshr_grantdata_s3 && req_s3.aliasTask
  val task_ready_s3 = !hasData_s3 || req_s3.fromC || need_mshr_s3 || (mshr_req_s3 && !alias_grantdata_s3)
  val mshr_fire_s3 = !mshr_req_s3 && need_mshr_s3 && !need_write_releaseBuf
  val chnl_fire_s3 = task_ready_s3 && (c_s3.fire() || d_s3.fire())// && !next_beatsOH_s3.orR

  //[Alias] TODO: may change this to ren?
  val data_unready_s3 = hasData_s3 && (!mshr_req_s3 || alias_grantdata_s3)
  c_s3.valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_release_s3 || mshr_probeack_s3,
    req_s3.fromB && !need_mshr_s3 && !data_unready_s3
  )
  c_s3.bits.task := source_req_s3
  c_s3.bits.data.data := data_s3
  d_s3.valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_grant_s3 && !alias_grantdata_s3, // grant under alias is ok to send
    req_s3.fromC || req_s3.fromA && !need_mshr_s3 && !data_unready_s3
  )
  d_s3.bits.task := source_req_s3
  d_s3.bits.data.data := data_s3

  // val block_s1 = Seq(io.toReqArb.blockC_s1, io.toReqArb.blockB_s1, io.toReqArb.blockA_s1)
  // (block_s1 zip io.fromReqArb.status_s1.sets).foreach {
  //   case (block, set) =>
  //     block := 
  //       task_s2.valid && !task_s2.bits.mshrTask && task_s2.bits.set === set ||
  //       io.toMSHRCtl.mshr_alloc_s3.valid && task_s3.bits.set === set
  // }

  io.nestedwb.set := req_s3.set
  io.nestedwb.tag := req_s3.tag
  io.nestedwb.b_toN := task_s3.valid && metaW_valid_s3_b && req_s3.param === toN
  io.nestedwb.b_toB := task_s3.valid && metaW_valid_s3_b && req_s3.param =/= toB // assume L3 won't send Probe toT
  io.nestedwb.b_clr_dirty := task_s3.valid && metaW_valid_s3_b && meta_s3.dirty
  io.nestedwb.c_set_dirty := task_s3.valid && metaW_valid_s3_c && metaW_s3_c_dirty

  io.nestedwbData := bufResp_s3.asTypeOf(new DSBlock)

  /* ======== Stage 4 ======== */
  val task_s4 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  // val beatsOH_s4 = RegInit(0.U(beatSize.W))
  // val beatsOH_ready_s4 = RegInit(0.U(beatSize.W))
  val data_unready_s4 = Reg(Bool())
  val data_s4 = Reg(UInt((blockBytes * 8).W))
  val ren_s4 = RegInit(false.B)
  val need_write_releaseBuf_s4 = Reg(Bool())
  task_s4.valid := task_s3.valid && !mshr_fire_s3 && !chnl_fire_s3
  when (task_s3.valid) {
    task_s4.bits := source_req_s3
    task_s4.bits.mshrId := Mux(!task_s3.bits.mshrTask && need_mshr_s3, io.fromMSHRCtl.mshr_alloc_ptr, source_req_s3.mshrId)
    // beatsOH_s4 := Mux(c_s3.fire() || d_s3.fire(), next_beatsOH_s3, beatsOH_s3)
    // beatsOH_ready_s4 := beatsOH_ready_s3
    data_unready_s4 := data_unready_s3
    data_s4 := data_s3
    ren_s4 := ren
    need_write_releaseBuf_s4 := need_write_releaseBuf
  }
  // val beats_unready_s4 = (beatsOH_s4 & ~beatsOH_ready_s4).orR
  // val (beat_s4, next_beatsOH_s4) = getBeat(data_s4, beatsOH_s4)
  val isC_s4 = task_s4.bits.opcode(2, 1) === Release(2, 1) && !task_s4.bits.fromB || task_s4.bits.opcode(2, 1) === ProbeAck(2, 1) && task_s4.bits.fromB
  val isD_s4 = task_s4.bits.opcode(2, 1) === Grant(2, 1) && task_s4.bits.fromA || task_s4.bits.fromC
  val chnl_fire_s4 = (c_s4.fire() || d_s4.fire())// && !next_beatsOH_s4.orR

  c_s4.valid := task_s4.valid && !data_unready_s4 && isC_s4 && !need_write_releaseBuf_s4
  d_s4.valid := task_s4.valid && !data_unready_s4 && isD_s4 && !need_write_releaseBuf_s4
  c_s4.bits.task := task_s4.bits
  c_s4.bits.data.data := data_s4
  d_s4.bits.task := task_s4.bits
  d_s4.bits.data.data := data_s4

  /* ======== Stage 5 ======== */
  val task_s5 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  val ren_s5 = RegInit(false.B)
  // val beatsOH_s5 = RegInit(0.U(beatSize.W))
  // val beatsOH_ready_s5 = RegNext(beatsOH_ready_s4) | Fill(beatSize, ren_s5)
  val data_s5 = Reg(UInt((blockBytes * 8).W))
  val need_write_releaseBuf_s5 = Reg(Bool())
  val isC_s5, isD_s5 = Reg(Bool())
  task_s5.valid := task_s4.valid && !chnl_fire_s4
  when (task_s4.valid) {
    task_s5.bits := task_s4.bits
    // beatsOH_s5 := Mux(c_s4.fire() || d_s4.fire(), next_beatsOH_s4, beatsOH_s4)
    ren_s5 := ren_s4
    data_s5 := data_s4
    need_write_releaseBuf_s5 := need_write_releaseBuf_s4
    isC_s5 := isC_s4
    isD_s5 := isD_s4
  }
  // assert(!task_s5.valid || !(beatsOH_s5 & ~beatsOH_ready_s5).orR) // data should always be ready by s5
  val rdata_s5 = io.toDS.rdata_s5.data
  val merged_data_s5 = Mux(ren_s5, rdata_s5, data_s5)
  // val (beat_s5, next_beatsOH_s5) = getBeat(merged_data_s5, beatsOH_s5)
  val chnl_fire_s5 = (c_s5.fire() || d_s5.fire())// && !next_beatsOH_s5

  io.releaseBufWrite.valid := task_s5.valid && need_write_releaseBuf_s5
  io.releaseBufWrite.beat_sel := Fill(beatSize, 1.U(1.W)) //PriorityEncoder(beatsOH_s5)
  io.releaseBufWrite.data.data := merged_data_s5
  io.releaseBufWrite.id := task_s5.bits.mshrId
  assert(!io.releaseBufWrite.valid || io.releaseBufWrite.ready)

  c_s5.valid := task_s5.valid && isC_s5 && !need_write_releaseBuf_s5
  d_s5.valid := task_s5.valid && isD_s5 && !need_write_releaseBuf_s5
  c_s5.bits.task := task_s5.bits
  c_s5.bits.data.data := merged_data_s5
  d_s5.bits.task := task_s5.bits
  d_s5.bits.data.data := merged_data_s5

  io.toReqArb.blockC_s1 :=
    task_s2.valid && !task_s2.bits.mshrTask && task_s2.bits.set === io.fromReqArb.status_s1.c_set ||
    io.toMSHRCtl.mshr_alloc_s3.valid && task_s3.bits.set === io.fromReqArb.status_s1.c_set
  io.toReqArb.blockB_s1 :=
    task_s2.valid && !task_s2.bits.mshrTask && task_s2.bits.set === io.fromReqArb.status_s1.b_set ||
    task_s3.valid && !task_s3.bits.mshrTask && task_s3.bits.set === io.fromReqArb.status_s1.b_set ||
    task_s4.valid && !task_s4.bits.mshrTask && task_s4.bits.set === io.fromReqArb.status_s1.b_set && task_s4.bits.opcode(2, 1) === Grant(2, 1) ||
    task_s5.valid && !task_s5.bits.mshrTask && task_s5.bits.set === io.fromReqArb.status_s1.b_set && task_s5.bits.opcode(2, 1) === Grant(2, 1)
  io.toReqArb.blockA_s1 :=
    task_s2.valid && !task_s2.bits.mshrTask && task_s2.bits.set === io.fromReqArb.status_s1.a_set ||
    task_s3.valid && !task_s3.bits.mshrTask && task_s3.bits.set === io.fromReqArb.status_s1.a_set ||
    task_s4.valid && !task_s4.bits.mshrTask && task_s4.bits.set === io.fromReqArb.status_s1.a_set && task_s4.bits.opcode(2, 1) === Grant(2, 1) ||
    task_s5.valid && !task_s5.bits.mshrTask && task_s5.bits.set === io.fromReqArb.status_s1.a_set && task_s5.bits.opcode(2, 1) === Grant(2, 1)
  // /* ======== Stage 6 ======== */
  // val task_s6 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  // val beatsOH_s6 = RegInit(0.U(beatSize.W))
  // val data_s6 = Reg(UInt((blockBytes * 8).W))
  // val need_write_releaseBuf_s6 = Reg(Bool())
  // val isC_s6, isD_s6 = Reg(Bool())
  // task_s6.valid := task_s5.valid && !chnl_fire_s5
  // when (task_s5.valid) {
  //   task_s6.bits := task_s5.bits
  //   beatsOH_s6 := Mux(c_s5.fire() || d_s5.fire(), next_beatsOH_s5, beatsOH_s5)
  //   data_s6 := merged_data_s5
  //   need_write_releaseBuf_s6 := need_write_releaseBuf_s5
  //   isC_s6 := isC_s5
  //   isD_s6 := isD_s5
  // }
  // val (beat_s6, next_beatsOH_s6) = getBeat(data_s6, beatsOH_s6)
  // assert(!task_s6.valid || !next_beatsOH_s6) // all the beats should be sent out by s6
  // // assert(c_s6.ready && d_s6.ready) // s6 has the highest priority for SourceC/D

  // val releaseBufWrite_s6 = io.releaseBufWrite(0)
  // releaseBufWrite_s6.valid := task_s6.valid && need_write_releaseBuf_s6
  // releaseBufWrite_s6.beat := PriorityEncoder(beatsOH_s6)
  // releaseBufWrite_s6.data.data := beat_s6
  // releaseBufWrite_s6.id := task_s6.bits.mshrId
  // assert(!releaseBufWrite_s6.valid || releaseBufWrite_s6.ready)

  /* ======== Other Signals Assignment ======== */
  // Initial state assignment
  // ! Caution: s_ and w_ are false valid
  when(req_s3.fromA) {
    alloc_state.s_refill := req_prefetch_s3   // no need to refill upwards for prefetch
    alloc_state.w_grantack := req_prefetch_s3 // TODO: Hint also needs refill?
    // need replacement
    when(!dirResult_s3.hit && meta_s3.state =/= INVALID) {
      alloc_state.s_release := false.B
      alloc_state.w_releaseack := false.B
      // need rprobe for release
      when(meta_has_clients_s3) {
        alloc_state.s_rprobe := false.B
        alloc_state.w_rprobeackfirst := false.B
        alloc_state.w_rprobeacklast := false.B
      }
    }
    // need Acquire downwards
    when(need_acquire_s3_a) {
      alloc_state.s_acquire := false.B
      alloc_state.w_grantfirst := false.B
      alloc_state.w_grantlast := false.B
      alloc_state.w_grant := false.B
    }
    // need Probe for alias
    when(cache_alias) {
      alloc_state.s_rprobe := false.B
      alloc_state.w_rprobeackfirst := false.B
      alloc_state.w_rprobeacklast := false.B
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

  // assert(io.toSourceD.ready) // SourceD should always be ready
  // assert(io.toSourceC.ready) // SourceC/wbq should be large enough to avoid blocking pipeline
  // val c_s4_block_s3 = hasData_s3 && task_s4.valid && beats_unready_s4 && isC_s4 && !need_write_releaseBuf_s4
  // val d_s4_block_s3 = hasData_s3 && task_s4.valid && beats_unready_s4 && isD_s4 && !need_write_releaseBuf_s4
  // c_s3.valid := task_s3.valid && Mux(
  //   mshr_req_s3,
  //   mshr_release_s3 || mshr_probeack_s3,
  //   req_s3.fromB && !need_mshr_s3 && !beats_unready_s3
  // )// && !c_s4_block_s3
  // c_s3.bits := toTLBundleC(source_req_s3, beat_s3)
  // d_s3.valid := task_s3.valid && Mux(
  //   mshr_req_s3,
  //   mshr_grant_s3,
  //   req_s3.fromC || req_s3.fromA && !need_mshr_s3 && !beats_unready_s3
  // )// && !d_s4_block_s3
  // d_s3.bits := toTLBundleD(source_req_s3, beat_s3)

  // c_s4.valid := task_s4.valid && !beats_unready_s4 && isC_s4 && !need_write_releaseBuf_s4
  // d_s4.valid := task_s4.valid && !beats_unready_s4 && isD_s4 && !need_write_releaseBuf_s4
  // c_s4.bits := toTLBundleC(task_s4.bits, beat_s4)
  // d_s4.bits := toTLBundleD(task_s4.bits, beat_s4)

  // c_s5.valid := task_s5.valid && isC_s5 && !need_write_releaseBuf_s5
  // d_s5.valid := task_s5.valid && isD_s5 && !need_write_releaseBuf_s5
  // c_s5.bits := toTLBundleC(task_s5.bits, beat_s5)
  // d_s5.bits := toTLBundleD(task_s5.bits, beat_s5)

  // c_s6.valid := task_s6.valid && isC_s6 && !need_write_releaseBuf_s6
  // d_s6.valid := task_s6.valid && isD_s6 && !need_write_releaseBuf_s6
  // c_s6.bits := toTLBundleC(task_s6.bits, beat_s6)
  // d_s6.bits := toTLBundleD(task_s6.bits, beat_s6)

  val c = Seq(c_s5, c_s4, c_s3)
  val d = Seq(d_s5, d_s4, d_s3)
  // DO NOT use TLArbiter because TLArbiter will send continuous beats for the same source
  val c_arb = Module(new Arbiter(io.toSourceC.bits.cloneType, c.size))
  val d_arb = Module(new Arbiter(io.toSourceD.bits.cloneType, d.size))
  c_arb.io.in <> c
  d_arb.io.in <> d

  io.toSourceC <> c_arb.io.out
  io.toSourceD <> d_arb.io.out
}
