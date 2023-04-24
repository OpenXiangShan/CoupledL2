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
import utility._
import coupledL2.MetaData._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import coupledL2.utils.{XSPerfAccumulate, XSPerfHistogram}
import coupledL2.prefetch.PrefetchTrain

class MainPipe(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    /* receive task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new TaskBundle()))
    /* status from arbiter at stage1  */
    val taskInfo_s1 = Flipped(ValidIO(new TaskBundle()))

    /* handle set conflict in req arb */
    val fromReqArb = Input(new Bundle() {
      val status_s1 = new PipeEntranceStatus
    })
    val toReqArb = Output(new BlockInfo())

    /* handle capacity conflict of GrantBuffer */
    val status_vec = Vec(3, ValidIO(new PipeStatus))

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

    /* read DS and write data into RefillBuf when Acquire toT hits on B */
    val refillBufWrite = Flipped(new MSHRBufWrite())

    val nestedwb = Output(new NestedWriteback)
    val nestedwbData = Output(new DSBlock)

    val l1Hint = ValidIO(new L2ToL1Hint())
    val grantBufferHint = Flipped(ValidIO(new L2ToL1Hint()))
    val globalCounter = Input(UInt(log2Ceil(mshrsAll).W))
    /* send prefetchTrain to Prefetch to trigger a prefetch req */
    val prefetchTrain = prefetchOpt.map(_ => DecoupledIO(new PrefetchTrain))
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
  val hint_s1, hint_s2, hint_s3, hint_s4, hint_s5 = Wire(io.l1Hint.cloneType)

  /* ======== Stage 2 ======== */
  // send out MSHR task if data is not needed
  val task_s2 = io.taskFromArb_s2
  val hasData_s2 = task_s2.bits.opcode(0)

  io.bufRead.valid := task_s2.valid && task_s2.bits.fromC && task_s2.bits.opcode(0)
  io.bufRead.bits.bufIdx := task_s2.bits.bufIdx

  /* ======== Stage 3 ======== */
  val task_s3 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  task_s3.valid := task_s2.valid
  when(task_s2.valid) {
    task_s3.bits := task_s2.bits
  }

  val dirResult_s3 = io.dirResp_s3.bits
  val meta_s3 = dirResult_s3.meta
  val req_s3 = task_s3.bits

  val mshr_req_s3 = req_s3.mshrTask
  val req_acquire_s3 = (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm) && req_s3.fromA && !mshr_req_s3
  val req_prefetch_s3 = req_s3.opcode === Hint && req_s3.fromA && !mshr_req_s3
  val req_get_s3 = req_s3.opcode === Get && req_s3.fromA && !mshr_req_s3
  val req_put_s3 = (req_s3.opcode === PutFullData || req_s3.opcode === PutPartialData) && req_s3.fromA && !mshr_req_s3

  val meta_has_clients_s3 = meta_s3.clients.orR
  val req_needT_s3 = needT(req_s3.opcode, req_s3.param) // require T status to handle req
  //[Alias] TODO: consider 1 client for now
  val cache_alias = (req_acquire_s3 || req_get_s3) && dirResult_s3.hit && meta_s3.clients(0) &&
    meta_s3.alias.getOrElse(0.U) =/= req_s3.alias.getOrElse(0.U)

  val acquire_on_miss_s3 = req_acquire_s3 || req_prefetch_s3 || req_get_s3 // TODO: remove this cause always acquire on miss?
  val acquire_on_hit_s3 = meta_s3.state === BRANCH && req_needT_s3
  // For channel A reqs, alloc mshr when: acquire downwards is needed || alias
  val need_acquire_s3_a = req_s3.fromA && Mux(
    dirResult_s3.hit,
    acquire_on_hit_s3,
    acquire_on_miss_s3
  )
  val need_probe_s3_a = req_s3.fromA && req_get_s3 && dirResult_s3.hit && meta_s3.state === TRUNK
  assert(RegNext(!(task_s3.valid && !mshr_req_s3 && dirResult_s3.hit && meta_s3.state === TRUNK && !meta_s3.clients.orR)))
  val need_mshr_s3_a = need_acquire_s3_a || need_probe_s3_a || cache_alias || req_put_s3
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
  ms_task.alias.foreach(_ := req_s3.alias.getOrElse(0.U))
  ms_task.opcode := req_s3.opcode
  ms_task.param := req_s3.param
  ms_task.size := req_s3.size
  ms_task.sourceId := req_s3.sourceId
  ms_task.needProbeAckData := req_s3.needProbeAckData
  ms_task.aliasTask.foreach(_ := cache_alias)
  ms_task.useProbeData := false.B
  ms_task.pbIdx := req_s3.pbIdx
  ms_task.fromL2pft.foreach(_ := req_s3.fromL2pft.get)
  ms_task.needHint.foreach(_ := req_s3.needHint.get)
  //
  ms_task.way := dirResult_s3.way
  //

  val sink_resp_s3 = WireInit(0.U.asTypeOf(Valid(new TaskBundle))) // resp for sinkA/B/C request that does not need to alloc mshr
  sink_resp_s3.valid := task_s3.valid && !mshr_req_s3 && !need_mshr_s3
  sink_resp_s3.bits := task_s3.bits

  sink_resp_s3.bits.mshrId := (1 << (mshrBits-1)).U + sink_resp_s3.bits.sourceId // extra id for reqs that do not enter mshr
  when(req_s3.fromA) {
    sink_resp_s3.bits.opcode := odOpGen(req_s3.opcode)
    sink_resp_s3.bits.param := Mux(
      req_acquire_s3,
      Mux(req_s3.param === NtoB, toB, toT),
      0.U // reserved
    )
  }.elsewhen(req_s3.fromB) {
    sink_resp_s3.bits.opcode := Mux(
      dirResult_s3.hit && (meta_s3.state === TIP && meta_s3.dirty || req_s3.needProbeAckData),
      ProbeAckData,
      ProbeAck
    )
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
  val mshr_grant_s3         = mshr_req_s3 && req_s3.fromA && req_s3.opcode(2, 1) === Grant(2, 1) // Grant or GrantData from mshr
  val mshr_grantdata_s3     = mshr_req_s3 && req_s3.fromA && req_s3.opcode === GrantData
  val mshr_accessackdata_s3 = mshr_req_s3 && req_s3.fromA && req_s3.opcode === AccessAckData
  val mshr_accessack_s3     = mshr_req_s3 && req_s3.fromA && req_s3.opcode === AccessAck
  val mshr_hintack_s3       = mshr_req_s3 && req_s3.fromA && req_s3.opcode === HintAck
  val mshr_probeack_s3      = mshr_req_s3 && req_s3.fromB && req_s3.opcode(2, 1) === ProbeAck(2, 1) // ProbeAck or ProbeAckData from mshr
  val mshr_probeackdata_s3  = mshr_req_s3 && req_s3.fromB && req_s3.opcode === ProbeAckData
  val mshr_release_s3       = mshr_req_s3 && req_s3.opcode(2, 1) === Release(2, 1) // voluntary Release or ReleaseData from mshr
  assert(!(io.refillBufResp_s3.valid && io.releaseBufResp_s3.valid))

  // write/read data storage
  val wen_c = !mshr_req_s3 && req_s3.fromC && isParamFromT(req_s3.param) && req_s3.opcode(0)
  val wen = wen_c || req_s3.dsWen && (mshr_grant_s3 || mshr_accessackdata_s3 || mshr_probeack_s3 || mshr_hintack_s3)

  val need_data_on_hit_a = req_s3.fromA && !mshr_req_s3 && (req_get_s3 || req_s3.opcode === AcquireBlock)
  // read data ahead of time to prepare for ReleaseData later 
  val need_data_on_miss_a = req_s3.fromA && !mshr_req_s3 && !dirResult_s3.hit && (meta_s3.state === TRUNK || (meta_s3.state === TIP || meta_s3.state === BRANCH) && meta_s3.dirty)
  val need_data_b = req_s3.fromB && !mshr_req_s3 && dirResult_s3.hit &&
    (meta_s3.state === TRUNK || meta_s3.state === TIP && meta_s3.dirty || req_s3.needProbeAckData)

  val ren = Mux(dirResult_s3.hit, need_data_on_hit_a, need_data_on_miss_a) || need_data_b
  val bufResp_s3 = RegNext(io.bufResp.data.asUInt) // for Release from C
  // need_write_releaseBuf indicates that DS should be read and the data will be written into ReleaseBuffer
  // need_write_releaseBuf is assigned true when:
  // 1. inner clients' data is needed, but whether the client will ack data is uncertain, so DS data is also needed, or
  // 2. replacement occurs, so DS data should be read for replacement later
  val need_write_releaseBuf = need_probe_s3_a ||
    cache_alias ||
    need_data_on_miss_a ||
    need_data_b && need_mshr_s3_b
  val need_write_refillBuf = req_s3.fromA && !mshr_req_s3 && req_needT_s3 && dirResult_s3.hit && meta_s3.state === BRANCH && !req_put_s3 && !req_prefetch_s3
  io.toDS.req_s3.valid := task_s3.valid && (ren || wen)
  io.toDS.req_s3.bits.way := Mux(mshr_req_s3, req_s3.way, dirResult_s3.way)
  io.toDS.req_s3.bits.set := Mux(mshr_req_s3, req_s3.set, dirResult_s3.set)
  io.toDS.req_s3.bits.wen := wen
  //[Alias] TODO: may change this according to four || signals of wen, use ParallelPriorityMux
  io.toDS.wdata_s3.data := Mux(
    !mshr_req_s3,
    bufResp_s3,
    Mux(
      req_s3.useProbeData,
      io.releaseBufResp_s3.bits.data,
      io.refillBufResp_s3.bits.data
    )
  )

  val metaW_valid_s3_a = !mshr_req_s3 && req_s3.fromA && !need_mshr_s3_a && !req_get_s3 && !req_prefetch_s3
  val metaW_valid_s3_b = !mshr_req_s3 && req_s3.fromB && !need_mshr_s3_b && (meta_s3.state === TIP || meta_s3.state === BRANCH && req_s3.param === toN)
  val metaW_valid_s3_c = !mshr_req_s3 && req_s3.fromC
  val metaW_valid_s3_mshr = mshr_req_s3 && req_s3.metaWen
  require(clientBits == 1)

  val metaW_s3_a = MetaEntry(meta_s3.dirty, Mux(req_needT_s3, TRUNK, meta_s3.state), Fill(clientBits, true.B), req_s3.alias)
  val metaW_s3_b = Mux(req_s3.param === toN, MetaEntry(), MetaEntry(false.B, BRANCH, meta_s3.clients, meta_s3.alias))

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

  io.tagWReq.valid := task_s3.valid && (mshr_grant_s3 || mshr_accessack_s3 || mshr_accessackdata_s3 || mshr_hintack_s3) && req_s3.tagWen
  io.tagWReq.bits.set := req_s3.set
  io.tagWReq.bits.way := req_s3.way
  io.tagWReq.bits.wtag := req_s3.tag

  val task_ready_s3 = !hasData_s3 || req_s3.fromC || need_mshr_s3 || mshr_req_s3
  val mshr_fire_s3 = !mshr_req_s3 && need_mshr_s3 && !need_write_releaseBuf && !need_write_refillBuf
  val chnl_fire_s3 = task_ready_s3 && (c_s3.fire() || d_s3.fire())// && !next_beatsOH_s3.orR

  //[Alias] TODO: may change this to ren?
  val data_unready_s3 = hasData_s3 && !mshr_req_s3
  c_s3.valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_release_s3 || mshr_probeack_s3,
    req_s3.fromB && !need_mshr_s3 && !data_unready_s3
  )
  c_s3.bits.task := source_req_s3
  c_s3.bits.data.data := data_s3
  d_s3.valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_grant_s3 || mshr_accessackdata_s3 || mshr_accessack_s3 || mshr_hintack_s3,
    req_s3.fromC || req_s3.fromA && !need_mshr_s3 && !data_unready_s3
  )
  d_s3.bits.task := source_req_s3
  d_s3.bits.data.data := data_s3

  io.nestedwb.set := req_s3.set
  io.nestedwb.tag := req_s3.tag
  io.nestedwb.b_toN := task_s3.valid && metaW_valid_s3_b && req_s3.param === toN
  io.nestedwb.b_toB := task_s3.valid && metaW_valid_s3_b && req_s3.param =/= toB // assume L3 won't send Probe toT
  io.nestedwb.b_clr_dirty := task_s3.valid && metaW_valid_s3_b && meta_s3.dirty
  io.nestedwb.c_set_dirty := task_s3.valid && metaW_valid_s3_c && metaW_s3_c_dirty

  io.nestedwbData := bufResp_s3.asTypeOf(new DSBlock)

  io.prefetchTrain.foreach {
    train =>
      train.valid := task_s3.valid && (req_acquire_s3 || req_get_s3) && req_s3.needHint.getOrElse(false.B) &&
        (!dirResult_s3.hit || meta_s3.prefetch.get)
      train.bits.tag := req_s3.tag
      train.bits.set := req_s3.set
      train.bits.needT := req_needT_s3
      train.bits.source := req_s3.sourceId
  }

  /* ======== Stage 4 ======== */
  val task_s4 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  val data_unready_s4 = Reg(Bool())
  val data_s4 = Reg(UInt((blockBytes * 8).W))
  val ren_s4 = RegInit(false.B)
  val need_write_releaseBuf_s4 = Reg(Bool())
  val need_write_refillBuf_s4 = Reg(Bool())
  task_s4.valid := task_s3.valid && !mshr_fire_s3 && !chnl_fire_s3
  when (task_s3.valid) {
    task_s4.bits := source_req_s3
    task_s4.bits.mshrId := Mux(!task_s3.bits.mshrTask && need_mshr_s3, io.fromMSHRCtl.mshr_alloc_ptr, source_req_s3.mshrId)
    data_unready_s4 := data_unready_s3
    data_s4 := data_s3
    ren_s4 := ren
    need_write_releaseBuf_s4 := need_write_releaseBuf
    need_write_refillBuf_s4 := need_write_refillBuf
  }
  val isC_s4 = task_s4.bits.opcode(2, 1) === Release(2, 1) && task_s4.bits.fromA || task_s4.bits.opcode(2, 1) === ProbeAck(2, 1) && task_s4.bits.fromB
  val isD_s4 = task_s4.bits.fromA && (
    task_s4.bits.opcode(2, 1) === Grant(2, 1) ||
    task_s4.bits.opcode(2, 1) === AccessAck(2, 1) ||
    task_s4.bits.opcode === HintAck
  ) || task_s4.bits.fromC
  val chnl_fire_s4 = c_s4.fire() || d_s4.fire()

  c_s4.valid := task_s4.valid && !data_unready_s4 && isC_s4 && !need_write_releaseBuf_s4 && !need_write_refillBuf_s4
  d_s4.valid := task_s4.valid && !data_unready_s4 && isD_s4 && !need_write_releaseBuf_s4 && !need_write_refillBuf_s4
  c_s4.bits.task := task_s4.bits
  c_s4.bits.data.data := data_s4
  d_s4.bits.task := task_s4.bits
  d_s4.bits.data.data := data_s4

  /* ======== Stage 5 ======== */
  val task_s5 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  val ren_s5 = RegInit(false.B)
  val data_s5 = Reg(UInt((blockBytes * 8).W))
  val need_write_releaseBuf_s5 = Reg(Bool())
  val need_write_refillBuf_s5 = Reg(Bool())
  val isC_s5, isD_s5 = Reg(Bool())
  task_s5.valid := task_s4.valid && !chnl_fire_s4
  when (task_s4.valid) {
    task_s5.bits := task_s4.bits
    ren_s5 := ren_s4
    data_s5 := data_s4
    need_write_releaseBuf_s5 := need_write_releaseBuf_s4
    need_write_refillBuf_s5 := need_write_refillBuf_s4
    isC_s5 := isC_s4
    isD_s5 := isD_s4
  }
  val rdata_s5 = io.toDS.rdata_s5.data
  val merged_data_s5 = Mux(ren_s5, rdata_s5, data_s5)
  val chnl_fire_s5 = c_s5.fire() || d_s5.fire()


  // grantData hint interface
  // use this interface to give a hint to l1 before actually sending a GrantData
  // l2 hit: the GrantData task is from sinkA rather than mshr, and this request dose not need mshr or writing refillBuffer
  // l2 refill: the GrantData task is from mshr rather than sinkA
  // priority: s3, s4, s5 might generate request to grantBuffer, but only one will be accepted, priority is s5 > s4 > s3
  // NOTE: l2's hardware prefetcher should not generate a hint
  // NOTE: if l2 hit, it will always send a request to grantBuffer in s5. if l2 refill, sending requests to grantBuffer might happen in s3, s4, s5

  val s3_l2_hit_grant_data = task_s3.valid && !mshr_req_s3 && !need_mshr_s3 && req_s3.fromA && task_s3.bits.opcode === AcquireBlock && !task_s3.bits.fromL2pft.getOrElse(false.B)
  //  req_grantbuffer_next_cycle_s4: this **hit** req will request grantBuffer in S5
  val req_grantbuffer_next_cycle_s4 = !need_write_releaseBuf_s4 && !need_write_refillBuf_s4
  val s4_l2_hit_grant_data = task_s4.valid && req_grantbuffer_next_cycle_s4 && task_s4.bits.opcode === GrantData && task_s4.bits.fromA && !task_s4.bits.mshrTask && !task_s4.bits.fromL2pft.getOrElse(false.B)
  
  // S1 hint
  //    * l1 acquire and l2 miss situation, **no hit situation**
  val task_s1 = io.taskInfo_s1
  val s1_l2_miss_refill_grant_data = task_s1.valid && task_s1.bits.fromA && task_s1.bits.opcode === GrantData
  val s1_l2_miss_refill_counter_match = Wire(Bool())

  // TODO: generalization, for now, only fit hintCycleAhead == 3
  s1_l2_miss_refill_counter_match := PopCount(Seq(d_s3.valid, d_s4.valid, d_s5.valid, s3_l2_hit_grant_data, s4_l2_hit_grant_data, task_s2.valid && task_s2.bits.fromA)) === 0.U && io.globalCounter <= 2.U

  hint_s1.valid := s1_l2_miss_refill_grant_data && s1_l2_miss_refill_counter_match
  hint_s1.bits.sourceId := task_s1.bits.sourceId

  // S2 hint
  //    * l1 acquire and l2 miss situation, **no hit situation**
  val s2_l2_miss_refill_grant_data = task_s2.valid && task_s2.bits.fromA && task_s2.bits.opcode === GrantData && task_s2.bits.mshrTask
  val s2_l2_miss_refill_counter_match = Wire(Bool())

  // TODO: generalization, for now, only fit hintCycleAhead == 2
  //  s2_l2_miss_refill_counter_match := PopCount(Seq(d_s3.valid, d_s4.valid, d_s5.valid, s3_l2_hit_grant_data, s4_l2_hit_grant_data)) === 0.U && io.globalCounter === 0.U

  s2_l2_miss_refill_counter_match := MuxLookup(Cat(d_s3.valid, d_s4.valid, d_s5.valid), false.B, Seq(
    Cat(true.B, true.B, true.B) -> ((io.globalCounter + 4.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U),
    Cat(true.B, true.B, false.B) -> ((io.globalCounter + 3.U + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U),
    Cat(true.B, false.B, true.B) -> Mux(s4_l2_hit_grant_data, (io.globalCounter + 4.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                                                (io.globalCounter + 3.U + task_s5.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U),
    Cat(false.B, true.B, true.B) -> Mux(s3_l2_hit_grant_data, (io.globalCounter + 4.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                                                (io.globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U),
    Cat(true.B, false.B, false.B) -> Mux(s4_l2_hit_grant_data, (io.globalCounter + 3.U + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
      (io.globalCounter + 2.U + task_s3.bits.opcode(0)) === hintCycleAhead.U),
    Cat(false.B, true.B, false.B) -> ((io.globalCounter + 2.U + task_s4.bits.opcode(0)) === hintCycleAhead.U),
    Cat(false.B, false.B, true.B) -> Mux(s4_l2_hit_grant_data,
                                          Mux(s3_l2_hit_grant_data, (io.globalCounter + 4.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                            (io.globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U),
                                              (io.globalCounter + 2.U + task_s5.bits.opcode(0)) === hintCycleAhead.U),
    Cat(false.B, false.B, false.B)-> Mux(s4_l2_hit_grant_data,
                                          Mux(s3_l2_hit_grant_data, (io.globalCounter + 4.U + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                            (io.globalCounter + 3.U + task_s4.bits.opcode(0)) === hintCycleAhead.U),
                                              (io.globalCounter + 2.U) === hintCycleAhead.U)
  ))

  hint_s2.valid := s2_l2_miss_refill_grant_data && s2_l2_miss_refill_counter_match
  hint_s2.bits.sourceId := task_s2.bits.sourceId

  // S3 hint
  //    * l1 acquire and l2 hit situation
  val s3_l2_hit_counter_match = Wire(Bool())
  when(d_s5.valid && d_s4.valid) {
    s3_l2_hit_counter_match := (io.globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U
  }.elsewhen(d_s4.valid) {
    s3_l2_hit_counter_match := (io.globalCounter + 3.U + task_s4.bits.opcode(0)) === hintCycleAhead.U
  }.elsewhen(d_s5.valid) {
    // NOTE: if s4 is a hit grantData, it will not request grantBuffer in s4, but in s5
    when(s4_l2_hit_grant_data) {
      s3_l2_hit_counter_match := (io.globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U
    }.otherwise {
      s3_l2_hit_counter_match := (io.globalCounter + 3.U + task_s5.bits.opcode(0)) === hintCycleAhead.U
    }
  }.otherwise {
    when(s4_l2_hit_grant_data) {
      s3_l2_hit_counter_match := (io.globalCounter + 3.U + task_s4.bits.opcode(0)) === hintCycleAhead.U
    }.otherwise {
      s3_l2_hit_counter_match :=  io.globalCounter + 3.U === hintCycleAhead.U
    }
  }
  val validHint_s3 = s3_l2_hit_grant_data && s3_l2_hit_counter_match

  // S3 hint
  //    * l1 acquire and l2 miss situation
  val s3_l2_miss_refill_grant_data = d_s3.valid && mshr_req_s3 && req_s3.fromA && task_s3.bits.opcode === GrantData && !task_s3.bits.fromL2pft.getOrElse(false.B)
  val s3_l2_miss_refill_counter_match = Wire(Bool())
  when(d_s5.valid && d_s4.valid) {
    s3_l2_miss_refill_counter_match := (io.globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U
  }.elsewhen(d_s4.valid) {
    s3_l2_miss_refill_counter_match := (io.globalCounter + 2.U + task_s4.bits.opcode(0)) === hintCycleAhead.U
  }.elsewhen(d_s5.valid) {
    when(s4_l2_hit_grant_data) {
      s3_l2_miss_refill_counter_match := (io.globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U
    }.otherwise {
      s3_l2_miss_refill_counter_match := (io.globalCounter + 2.U + task_s5.bits.opcode(0)) === hintCycleAhead.U
    }
  }.otherwise {
    s3_l2_miss_refill_counter_match :=  io.globalCounter + 1.U === hintCycleAhead.U
  }
  val validHintMiss_s3 = s3_l2_miss_refill_grant_data && s3_l2_miss_refill_counter_match
  hint_s3.valid := validHint_s3 || validHintMiss_s3
  hint_s3.bits.sourceId := task_s3.bits.sourceId

  // S4 hint
  //    * l1 acquire and l2 hit situation
  val s4_l2_hit_counter_match = Mux(d_s5.valid && task_s5.bits.opcode(0), (io.globalCounter + 3.U) === hintCycleAhead.U,
                                      (io.globalCounter + 2.U) === hintCycleAhead.U )
  val validHint_s4 = s4_l2_hit_grant_data && s4_l2_hit_counter_match
  // S4 hint
  //    * l1 acquire and l2 miss situation
  val s4_l2_miss_refill_grant_data = d_s4.valid && task_s4.bits.opcode === GrantData && task_s4.bits.fromA && task_s4.bits.mshrTask && !task_s4.bits.fromL2pft.getOrElse(false.B)
  val s4_l2_miss_refill_counter_match = Mux(d_s5.valid && task_s5.bits.opcode(0), (io.globalCounter + 3.U) === hintCycleAhead.U, 
                                            Mux(d_s5.valid && !task_s5.bits.opcode(0), (io.globalCounter + 2.U) === hintCycleAhead.U, 
                                                (io.globalCounter + 1.U) === hintCycleAhead.U ))
  val validHintMiss_s4 = s4_l2_miss_refill_grant_data && s4_l2_miss_refill_counter_match
  hint_s4.valid := validHint_s4 || validHintMiss_s4
  hint_s4.bits.sourceId := task_s4.bits.sourceId

  // S5 hint
  //    * l1 acquire and l2 hit situation
  val validHint_s5 = d_s5.valid && task_s5.bits.opcode === GrantData && task_s5.bits.fromA && !task_s5.bits.mshrTask && ((io.globalCounter + 1.U) === hintCycleAhead.U) && !task_s5.bits.fromL2pft.getOrElse(false.B)
  // S5 hint
  //    * l1 acquire and l2 miss situation
  val validHintMiss_s5 = d_s5.valid && task_s5.bits.opcode === GrantData && task_s5.bits.fromA && task_s5.bits.mshrTask && ((io.globalCounter + 1.U) === hintCycleAhead.U) && !task_s5.bits.fromL2pft.getOrElse(false.B)
  hint_s5.valid := validHint_s5 || validHintMiss_s5
  hint_s5.bits.sourceId := task_s5.bits.sourceId

  val hint_valid = Seq(io.grantBufferHint.valid, hint_s1.valid, hint_s2.valid, hint_s3.valid, hint_s4.valid, hint_s5.valid)
  val hint_sourceId = Seq(io.grantBufferHint.bits.sourceId, hint_s1.bits.sourceId, hint_s2.bits.sourceId, hint_s3.bits.sourceId, hint_s4.bits.sourceId, hint_s5.bits.sourceId)

  io.l1Hint.valid := VecInit(hint_valid).asUInt.orR
  io.l1Hint.bits.sourceId := ParallelMux(hint_valid zip hint_sourceId)
  assert(PopCount(VecInit(hint_valid)) <= 1.U)

  XSPerfAccumulate(cacheParams, "hint_grantBufferHint_valid", io.grantBufferHint.valid)
  XSPerfAccumulate(cacheParams, "hint_s1_valid", hint_s1.valid)
  XSPerfAccumulate(cacheParams, "hint_s2_valid", hint_s2.valid)
  XSPerfAccumulate(cacheParams, "hint_s3_valid", hint_s3.valid)
  XSPerfAccumulate(cacheParams, "hint_s4_valid", hint_s4.valid)
  XSPerfAccumulate(cacheParams, "hint_s5_valid", hint_s5.valid)

  io.releaseBufWrite.valid := task_s5.valid && need_write_releaseBuf_s5
  io.releaseBufWrite.beat_sel := Fill(beatSize, 1.U(1.W))
  io.releaseBufWrite.data.data := merged_data_s5
  io.releaseBufWrite.id := task_s5.bits.mshrId
  assert(!io.releaseBufWrite.valid || io.releaseBufWrite.ready)

  io.refillBufWrite.valid := task_s5.valid && need_write_refillBuf_s5
  io.refillBufWrite.beat_sel := Fill(beatSize, 1.U(1.W))
  io.refillBufWrite.data.data := merged_data_s5
  io.refillBufWrite.id := task_s5.bits.mshrId
  assert(!io.refillBufWrite.valid || io.refillBufWrite.ready)

  c_s5.valid := task_s5.valid && isC_s5 && !need_write_releaseBuf_s5 && !need_write_refillBuf_s5
  d_s5.valid := task_s5.valid && isD_s5 && !need_write_releaseBuf_s5 && !need_write_refillBuf_s5
  c_s5.bits.task := task_s5.bits
  c_s5.bits.data.data := merged_data_s5
  d_s5.bits.task := task_s5.bits
  d_s5.bits.data.data := merged_data_s5

  io.toReqArb.blockC_s1 :=
    task_s2.valid && task_s2.bits.set === io.fromReqArb.status_s1.c_set ||
    io.toMSHRCtl.mshr_alloc_s3.valid && task_s3.bits.set === io.fromReqArb.status_s1.c_set
  io.toReqArb.blockB_s1 :=
    task_s2.valid && task_s2.bits.set === io.fromReqArb.status_s1.b_set ||
    task_s3.valid && !task_s3.bits.mshrTask && task_s3.bits.set === io.fromReqArb.status_s1.b_set ||
    task_s4.valid && !task_s4.bits.mshrTask && task_s4.bits.set === io.fromReqArb.status_s1.b_set ||
    task_s5.valid && !task_s5.bits.mshrTask && task_s5.bits.set === io.fromReqArb.status_s1.b_set
  io.toReqArb.blockA_s1 :=
    task_s2.valid && task_s2.bits.set === io.fromReqArb.status_s1.a_set ||
    task_s3.valid && !task_s3.bits.mshrTask && task_s3.bits.set === io.fromReqArb.status_s1.a_set ||
    task_s4.valid && !task_s4.bits.mshrTask && task_s4.bits.set === io.fromReqArb.status_s1.a_set ||
    task_s5.valid && !task_s5.bits.mshrTask && task_s5.bits.set === io.fromReqArb.status_s1.a_set

  require(io.status_vec.size == 3)
  io.status_vec(0).valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_grant_s3 || mshr_accessackdata_s3 || mshr_accessack_s3,
    true.B
    // TODO: To consider grantBuffer capacity conflict,
    // only " req_s3.fromC || req_s3.fromA && !need_mshr_s3 " is needed
    // But to consider mshrFull, all channel_reqs are needed
  )
  io.status_vec(0).bits.channel := task_s3.bits.channel
  io.status_vec(1).valid := task_s4.valid && isD_s4 && !need_write_releaseBuf_s4 && !need_write_refillBuf_s4
  io.status_vec(1).bits.channel := task_s4.bits.channel
  io.status_vec(2).valid := d_s5.valid
  io.status_vec(2).bits.channel := task_s5.bits.channel

  /* ======== Other Signals Assignment ======== */
  // Initial state assignment
  // ! Caution: s_ and w_ are false-as-valid
  when(req_s3.fromA) {
    alloc_state.s_refill := false.B
    alloc_state.w_grantack := req_prefetch_s3 || req_get_s3 || req_put_s3
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
    when(need_acquire_s3_a || req_put_s3) {
      alloc_state.s_acquire := false.B
      alloc_state.w_grantfirst := false.B
      alloc_state.w_grantlast := false.B
      alloc_state.w_grant := false.B
    }
    alloc_state.w_release_sent := alloc_state.s_acquire || alloc_state.s_release
    // need Probe for alias
    // need Probe when Get hits on a TRUNK block
    when(cache_alias || need_probe_s3_a) {
      alloc_state.s_rprobe := false.B
      alloc_state.w_rprobeackfirst := false.B
      alloc_state.w_rprobeacklast := false.B
    }
    // need trigger a prefetch, send PrefetchTrain msg to Prefetcher
    // prefetchOpt.foreach {_ =>
    //   when (req_s3.fromA && req_s3.needHint.getOrElse(false.B) && (!dirResult_s3.hit || meta_s3.prefetch.get)) {
    //     alloc_state.s_triggerprefetch.foreach(_ := false.B)
    //   }
    // }
  }
  when(req_s3.fromB) {
    // Only consider the situation when mshr needs to be allocated
    alloc_state.s_pprobe := false.B
    alloc_state.w_pprobeackfirst := false.B
    alloc_state.w_pprobeacklast := false.B
    alloc_state.w_pprobeack := false.B
    alloc_state.s_probeack := false.B
  }

  val c = Seq(c_s5, c_s4, c_s3)
  val d = Seq(d_s5, d_s4, d_s3)
  // DO NOT use TLArbiter because TLArbiter will send continuous beats for the same source
  val c_arb = Module(new Arbiter(io.toSourceC.bits.cloneType, c.size))
  val d_arb = Module(new Arbiter(io.toSourceD.bits.cloneType, d.size))
  c_arb.io.in <> c
  d_arb.io.in <> d

  io.toSourceC <> c_arb.io.out
  io.toSourceD <> d_arb.io.out

  // Performance counters
  // num of mshr req
  XSPerfAccumulate(cacheParams, "mshr_grant_req", task_s3.valid && mshr_grant_s3)
  XSPerfAccumulate(cacheParams, "mshr_grantdata_req", task_s3.valid && mshr_grantdata_s3)
  XSPerfAccumulate(cacheParams, "mshr_accessackdata_req", task_s3.valid && mshr_accessackdata_s3)
  XSPerfAccumulate(cacheParams, "mshr_accessack_req", task_s3.valid && mshr_accessack_s3)
  XSPerfAccumulate(cacheParams, "mshr_hintack_req", task_s3.valid && mshr_hintack_s3)
  XSPerfAccumulate(cacheParams, "mshr_probeack_req", task_s3.valid && mshr_probeack_s3)
  XSPerfAccumulate(cacheParams, "mshr_probeackdata_req", task_s3.valid && mshr_probeackdata_s3)
  XSPerfAccumulate(cacheParams, "mshr_release_req", task_s3.valid && mshr_release_s3)

  // directory access result
  val hit_s3 = task_s3.valid && !mshr_req_s3 && dirResult_s3.hit
  val miss_s3 = task_s3.valid && !mshr_req_s3 && !dirResult_s3.hit
  XSPerfAccumulate(cacheParams, "a_req_hit", hit_s3 && req_s3.fromA)
  XSPerfAccumulate(cacheParams, "acquire_hit", hit_s3 && req_s3.fromA &&
    (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm))
  XSPerfAccumulate(cacheParams, "get_hit", hit_s3 && req_s3.fromA && req_s3.opcode === Get)

  XSPerfAccumulate(cacheParams, "a_req_miss", miss_s3 && req_s3.fromA)
  XSPerfAccumulate(cacheParams, "acquire_miss", miss_s3 && req_s3.fromA &&
    (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm))
  XSPerfAccumulate(cacheParams, "get_miss", miss_s3 && req_s3.fromA && req_s3.opcode === Get)

  XSPerfAccumulate(cacheParams, "a_req_need_replacement", io.toMSHRCtl.mshr_alloc_s3.valid && !alloc_state.s_release)

  XSPerfAccumulate(cacheParams, "b_req_hit", hit_s3 && req_s3.fromB)
  XSPerfAccumulate(cacheParams, "b_req_miss", miss_s3 && req_s3.fromB)

  XSPerfHistogram(cacheParams, "a_req_access_way", perfCnt = dirResult_s3.way,
    enable = task_s3.valid && !mshr_req_s3 && req_s3.fromA && !req_put_s3, start = 0, stop = cacheParams.ways, step = 1)
  XSPerfHistogram(cacheParams, "a_req_hit_way", perfCnt = dirResult_s3.way,
    enable = hit_s3 && req_s3.fromA && !req_put_s3, start = 0, stop = cacheParams.ways, step = 1)
  XSPerfHistogram(cacheParams, "a_req_miss_way_choice", perfCnt = dirResult_s3.way,
    enable = miss_s3 && req_s3.fromA && !req_put_s3, start = 0, stop = cacheParams.ways, step = 1)
  
  // pipeline stages for sourceC and sourceD reqs
  val sourceC_pipe_len = ParallelMux(Seq(
    c_s5.fire() -> 5.U,
    c_s4.fire() -> 4.U,
    c_s3.fire() -> 3.U
  ))
  val sourceD_pipe_len = ParallelMux(Seq(
    d_s5.fire() -> 5.U,
    d_s4.fire() -> 4.U,
    d_s3.fire() -> 3.U
  ))
  XSPerfHistogram(cacheParams, "sourceC_pipeline_stages", sourceC_pipe_len,
    enable = io.toSourceC.fire(), start = 3, stop = 5+1, step = 1)
  XSPerfHistogram(cacheParams, "sourceD_pipeline_stages", sourceD_pipe_len,
    enable = io.toSourceD.fire(), start = 3, stop = 5+1, step = 1)

  // XSPerfAccumulate(cacheParams, "a_req_tigger_prefetch", io.prefetchTrain.)
  prefetchOpt.foreach {
    _ =>
      XSPerfAccumulate(cacheParams, "a_req_trigger_prefetch", io.prefetchTrain.get.fire())
      XSPerfAccumulate(cacheParams, "a_req_trigger_prefetch_not_ready", io.prefetchTrain.get.valid && !io.prefetchTrain.get.ready)
      XSPerfAccumulate(cacheParams, "acquire_trigger_prefetch_on_miss", io.prefetchTrain.get.fire() && req_acquire_s3 && !dirResult_s3.hit)
      XSPerfAccumulate(cacheParams, "acquire_trigger_prefetch_on_hit_pft", io.prefetchTrain.get.fire() && req_acquire_s3 && dirResult_s3.hit && meta_s3.prefetch.get)
      XSPerfAccumulate(cacheParams, "get_trigger_prefetch_on_miss", io.prefetchTrain.get.fire() && req_get_s3 && !dirResult_s3.hit)
      XSPerfAccumulate(cacheParams, "get_trigger_prefetch_on_hit_pft", io.prefetchTrain.get.fire() && req_get_s3 && dirResult_s3.hit && meta_s3.prefetch.get)
  }
}
