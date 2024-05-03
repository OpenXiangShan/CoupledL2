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

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import utility._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters
import coupledL2._
import coupledL2.prefetch.{PrefetchTrain, PfSource}
import coupledL2.tl2chi.CHIOpcode._
import coupledL2.tl2chi.CHICohStates._
import coupledL2.MetaData._
import coupledL2.utils.{XSPerfAccumulate, XSPerfHistogram}

class MainPipe(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    /* receive task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new TaskBundle()))
    /* status from arbiter at stage1  */
    val taskInfo_s1 = Flipped(ValidIO(new TaskBundle()))

    /* handle set conflict in req arb */
    val fromReqArb = Input(new Bundle() {
      val status_s1 = new PipeEntranceStatus
    })
    /* block B and C at Entrance */
    val toReqArb = Output(new BlockInfo())

    /* block A at Entrance */
    val toReqBuf = Output(Vec(2, Bool()))

    /* handle capacity conflict of GrantBuffer */
    val status_vec_toD = Vec(3, ValidIO(new PipeStatus))
    /* handle capacity conflict of TX channels */
    val status_vec_toTX = Vec(3, ValidIO(new PipeStatusWithCHI))

    /* get dir result at stage 3 */
    val dirResp_s3 = Input(new DirResult())
    val replResp = Flipped(ValidIO(new ReplacerResult()))

    /* send task to MSHRCtl at stage 3 */
    val toMSHRCtl = new Bundle() {
      val mshr_alloc_s3 = ValidIO(new MSHRRequest())
    }

    val fromMSHRCtl = new Bundle() {
      val mshr_alloc_ptr = Input(UInt(mshrBits.W))
    }

    /* read C-channel Release Data and write into DS */
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

    /* send Grant via SourceD channel */
    val toSourceD = DecoupledIO(new TaskWithData())

    /* send req/Comp/CompData via TXREQ/TXRSP/TXDAT channel */
    val toTXREQ = DecoupledIO(new CHIREQ())
    val toTXRSP = DecoupledIO(new TaskBundle())
    val toTXDAT = DecoupledIO(new TaskWithData())

    /* write dir, including reset dir */
    val metaWReq = ValidIO(new MetaWrite)
    val tagWReq = ValidIO(new TagWrite)

    /* read DS and write data into ReleaseBuf when the task needs to replace */
    val releaseBufWrite = ValidIO(new MSHRBufWrite())

    /* nested writeback */
    val nestedwb = Output(new NestedWriteback())
    val nestedwbData = Output(new DSBlock())

    /* l2 refill hint */
    val l1Hint = DecoupledIO(new L2ToL1Hint())
    // val grantBufferHint = Flipped(ValidIO(new L2ToL1Hint()))
    // val globalCounter = Input(UInt((log2Ceil(mshrsAll) + 1).W))

    /* send prefetchTrain to Prefetch to trigger a prefetch req */
    val prefetchTrain = prefetchOpt.map(_ => DecoupledIO(new PrefetchTrain))

    /* top-down monitor */
    // TODO
  })

  require(chiOpt.isDefined)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  /* block reqs when reset */
  when (!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when (resetIdx === 0.U) {
    resetFinish := true.B
  }

  val txreq_s3, txreq_s4, txreq_s5 = WireInit(0.U.asTypeOf(io.toTXREQ.cloneType))
  val txrsp_s3, txrsp_s4, txrsp_s5 = Wire(io.toTXRSP.cloneType)
  val txdat_s3, txdat_s4, txdat_s5 = Wire(io.toTXDAT.cloneType)
  val d_s3, d_s4, d_s5 = Wire(io.toSourceD.cloneType)

  /* ======== Stage 2 ======== */
  val task_s2 = io.taskFromArb_s2

  /* ======== Stage 3 ======== */
  val task_s3 = RegInit(0.U.asTypeOf(Valid(new TaskBundle)))
  task_s3.valid := task_s2.valid
  when (task_s2.valid) {
    task_s3.bits := task_s2.bits
  }

  /* ======== Enchantment ======== */
  val dirResult_s3    = io.dirResp_s3
  val meta_s3         = dirResult_s3.meta
  val req_s3          = task_s3.bits

  val mshr_req_s3     = req_s3.mshrTask
  val sink_req_s3     = !mshr_req_s3
  val sinkA_req_s3    = !mshr_req_s3 && req_s3.fromA
  val sinkB_req_s3    = !mshr_req_s3 && req_s3.fromB
  val sinkC_req_s3    = !mshr_req_s3 && req_s3.fromC

  val req_acquire_s3            = sinkA_req_s3 && (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm)
  val req_acquireBlock_s3       = sinkA_req_s3 && req_s3.opcode === AcquireBlock
  val req_prefetch_s3           = sinkA_req_s3 && req_s3.opcode === Hint
  val req_get_s3                = sinkA_req_s3 && req_s3.opcode === Get

  val mshr_grant_s3             = mshr_req_s3 && req_s3.fromA && req_s3.opcode(2, 1) === Grant(2, 1) // Grant or GrantData from mshr
  val mshr_grantdata_s3         = mshr_req_s3 && req_s3.fromA && req_s3.opcode === GrantData
  val mshr_accessackdata_s3     = mshr_req_s3 && req_s3.fromA && req_s3.opcode === AccessAckData
  val mshr_hintack_s3           = mshr_req_s3 && req_s3.fromA && req_s3.opcode === HintAck

  val mshr_snpResp_s3           = mshr_req_s3 && req_s3.toTXRSP && req_s3.chiOpcode.get === RSPOpcodes.SnpResp
  val mshr_snpRespFwded_s3      = mshr_req_s3 && req_s3.toTXRSP && req_s3.chiOpcode.get === RSPOpcodes.SnpRespFwded
  val mshr_snpRespData_s3       = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === DATOpcodes.SnpRespData
  val mshr_snpRespDataPtl_s3    = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === DATOpcodes.SnpRespDataPtl
  val mshr_snpRespDataFwded_s3  = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === DATOpcodes.SnpRespDataFwded
  val mshr_snpRespX_s3 = mshr_snpResp_s3 || mshr_snpRespFwded_s3
  val mshr_snpRespDataX_s3 = mshr_snpRespData_s3 || mshr_snpRespDataPtl_s3 || mshr_snpRespDataFwded_s3

  val mshr_dct_s3               = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === DATOpcodes.CompData

  val mshr_writeBackFull_s3     = mshr_req_s3 && req_s3.toTXREQ && req_s3.chiOpcode.get === REQOpcodes.WriteBackFull
  val mshr_evict_s3             = mshr_req_s3 && req_s3.toTXREQ && req_s3.chiOpcode.get === REQOpcodes.Evict
  
  val mshr_cbWrData_s3          = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === DATOpcodes.CopyBackWrData

  val meta_has_clients_s3       = meta_s3.clients.orR
  val req_needT_s3              = needT(req_s3.opcode, req_s3.param)

  val cache_alias               = req_acquire_s3 && dirResult_s3.hit && meta_s3.clients(0) &&
                              meta_s3.alias.getOrElse(0.U) =/= req_s3.alias.getOrElse(0.U)

  val mshr_refill_s3 = mshr_accessackdata_s3 || mshr_hintack_s3 || mshr_grant_s3 // needs refill to L2 DS
  val retry = io.replResp.valid && io.replResp.bits.retry
  val need_repl = io.replResp.valid && io.replResp.bits.meta.state =/= INVALID && req_s3.replTask

  /* ======== Interact with MSHR ======== */
  val acquire_on_miss_s3 = req_acquire_s3 || req_prefetch_s3 || req_get_s3
  val acquire_on_hit_s3 = meta_s3.state === BRANCH && req_needT_s3 && !req_prefetch_s3
  val need_acquire_s3_a = req_s3.fromA && Mux(
    dirResult_s3.hit,
    acquire_on_hit_s3,
    acquire_on_miss_s3
  )
  val need_probe_s3_a = req_get_s3 && dirResult_s3.hit && meta_s3.state === TRUNK

  val need_mshr_s3_a = need_acquire_s3_a || need_probe_s3_a || cache_alias
  
  /**
    * 1. For SnpOnce/SnpOnceFwd, only the latest copy of the cacheline is needed without changing the state of the
    *    cacheline at the snoopee. Therefore L2 should only send pProbe toT (to get the latest copy) when the state
    *    in L2 is TRUNK
    * 2. For SnpClean/SnpCleanFwd, SnpShared/SnpSharedFwd, SnpNotSharedDirty/SnpNotSharedDirtyFwd, and SnpCleanShared,
    *    the snooped cacheline should be degraded into BRANCH state because there is no SharedDirty state or Owner
    *    state (of MOESI) in CoupledL2. Therefore L2 should only send pProbe toB to degrade upper clients when the
    *    state in L2 is TRUNK
    * 3. For SnpUnique/SnpUniqueFwd/SnpUniqueStash, SnpCleanInvalid, SnpMakeInvalid/SnpMakeInvalidStash, the snooped
    *    cacheline should be degraded into INVALID state. Therefore L2 should only send pProbe toN to degrade upper
    *    clients when the state in L2 is TRUNK or BRANCH with clients.orR = 1
    * 
    */
  // whether L2 should do forwarding or not
  val expectFwd = SNPOpcodes.isSnpXFwd(req_s3.chiOpcode.get)
  val canFwd = dirResult_s3.hit
  val doFwd = expectFwd && canFwd
  val doFwdHitRelease = expectFwd && req_s3.snpHitRelease && req_s3.snpHitReleaseWithData
  val need_pprobe_s3_b_snpOnceX = req_s3.fromB && SNPOpcodes.isSnpOnceX(req_s3.chiOpcode.get) &&
    dirResult_s3.hit && meta_s3.state === TRUNK && meta_has_clients_s3
  val need_pprobe_s3_b_snpToB = req_s3.fromB && (
    SNPOpcodes.isSnpToB(req_s3.chiOpcode.get) ||
    req_s3.chiOpcode.get === SNPOpcodes.SnpCleanShared
  ) && dirResult_s3.hit && meta_s3.state === TRUNK && meta_has_clients_s3
  val need_pprobe_s3_b_snpToN = req_s3.fromB && (
    SNPOpcodes.isSnpUniqueX(req_s3.chiOpcode.get) ||
    req_s3.chiOpcode.get === SNPOpcodes.SnpCleanInvalid ||
    SNPOpcodes.isSnpMakeInvalidX(req_s3.chiOpcode.get)
  ) && dirResult_s3.hit && meta_s3.state =/= TIP && meta_has_clients_s3
  val need_pprobe_s3_b = need_pprobe_s3_b_snpOnceX || need_pprobe_s3_b_snpToB || need_pprobe_s3_b_snpToN
  val need_dct_s3_b = doFwd || doFwdHitRelease // DCT
  val need_mshr_s3_b = need_pprobe_s3_b || need_dct_s3_b

  val need_mshr_s3 = need_mshr_s3_a || need_mshr_s3_b

  /* Signals to MSHR Ctl */
  val alloc_state = WireInit(0.U.asTypeOf(new FSMState()))
  alloc_state.elements.foreach(_._2 := true.B)
  io.toMSHRCtl.mshr_alloc_s3.valid := task_s3.valid && !mshr_req_s3 && need_mshr_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.dirResult := dirResult_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.state := alloc_state
  io.toMSHRCtl.mshr_alloc_s3.bits.task match { case task =>
    task := req_s3
    task.bufIdx := 0.U(bufIdxBits.W)
    task.mshrTask := false.B
    task.aliasTask.foreach(_ := cache_alias)
    task.wayMask := 0.U(cacheParams.ways.W)
    // TODO
  }
  
  /* ======== Resps to SinkA/B/C Reqs ======== */
  val sink_resp_s3 = WireInit(0.U.asTypeOf(Valid(new TaskBundle)))
  val sink_resp_s3_a_promoteT = dirResult_s3.hit && isT(meta_s3.state)

  // whether L2 should respond data to HN or not
  val retToSrc = req_s3.retToSrc.getOrElse(false.B)
  val neverRespData = SNPOpcodes.isSnpMakeInvalidX(req_s3.chiOpcode.get) ||
    SNPOpcodes.isSnpStashX(req_s3.chiOpcode.get) ||
    req_s3.chiOpcode.get === SNPOpcodes.SnpOnceFwd ||
    req_s3.chiOpcode.get === SNPOpcodes.SnpUniqueFwd
  val shouldRespData_dirty = dirResult_s3.hit && (meta_s3.state === TIP || meta_s3.state === TRUNK) && meta_s3.dirty
  // For forwarding snoops, if the RetToSrc value is 1, must return a copy is the cache line is Dirty or Clean.
  val shouldRespData_retToSrc_fwd = dirResult_s3.hit && retToSrc && SNPOpcodes.isSnpXFwd(req_s3.chiOpcode.get)
  // For non-forwarding snoops, ig the RetToSrc value is 1, must return a copy if the cache line is Shared Clean and
  // snoopee retains a copy of the cache line.
  val shouldRespData_retToSrc_nonFwd = dirResult_s3.hit && retToSrc && meta_s3.state === BRANCH && (
    req_s3.chiOpcode.get === SNPOpcodes.SnpOnce ||
    req_s3.chiOpcode.get === SNPOpcodes.SnpUnique ||
    SNPOpcodes.isSnpToBNonFwd(req_s3.chiOpcode.get)
  )
  val shouldRespData = shouldRespData_dirty || shouldRespData_retToSrc_fwd || shouldRespData_retToSrc_nonFwd
  val doRespData = shouldRespData && !neverRespData
  val doRespDataHitRelease = req_s3.snpHitRelease && req_s3.snpHitReleaseWithData && !neverRespData
  dontTouch(doRespData)
  dontTouch(shouldRespData)
  dontTouch(neverRespData)
  
  // Resp[2: 0] = {PassDirty, CacheState[1: 0]}
  val respCacheState = WireInit(I)
  val respPassDirty = dirResult_s3.hit && meta_s3.state === TIP && meta_s3.dirty &&
    !(neverRespData || req_s3.chiOpcode.get === SNPOpcodes.SnpOnce)
  when (dirResult_s3.hit) {
    when (SNPOpcodes.isSnpToB(req_s3.chiOpcode.get)) {
      respCacheState := SC
    }
    when (SNPOpcodes.isSnpOnceX(req_s3.chiOpcode.get) || SNPOpcodes.isSnpStashX(req_s3.chiOpcode.get)) {
      respCacheState := Mux(
        meta_s3.state === BRANCH,
        SC,
        Mux(meta_s3.dirty, UD, UC)
      )
    }
    when (req_s3.chiOpcode.get === SNPOpcodes.SnpCleanShared) {
      respCacheState := Mux(meta_s3.state === BRANCH, SC, UC)
    }
  }

  // FwdState[2: 0] = {PassDirty, CacheState[1: 0]}
  val fwdCacheState = WireInit(I)
  val fwdPassDirty = WireInit(false.B)
  when (dirResult_s3.hit) {
    when (SNPOpcodes.isSnpToBFwd(req_s3.chiOpcode.get)) {
      fwdCacheState := SC
    }
    when (req_s3.chiOpcode.get === SNPOpcodes.SnpUniqueFwd) {
      when (meta_s3.state === TIP && meta_s3.dirty) {
        fwdCacheState := UD
        fwdPassDirty := true.B
      }.otherwise {
        fwdCacheState := UC
      }
    }
  }

  sink_resp_s3.valid := task_s3.valid && !mshr_req_s3 && !need_mshr_s3
  sink_resp_s3.bits := task_s3.bits
  sink_resp_s3.bits.mshrId := (1 << (mshrBits-1)).U + sink_resp_s3.bits.sourceId
  when (req_s3.fromA) {
    sink_resp_s3.bits.opcode := odOpGen(req_s3.opcode)
    sink_resp_s3.bits.param := Mux (
      req_acquire_s3,
      Mux(req_s3.param === NtoB && !sink_resp_s3_a_promoteT, toB, toT),
      0.U // reserved
    )
  }.elsewhen (req_s3.fromB) {

    sink_resp_s3.bits.opcode := 0.U
    sink_resp_s3.bits.param := 0.U

    sink_resp_s3.bits.tgtID.foreach(_ := task_s3.bits.srcID.get)
    sink_resp_s3.bits.srcID.foreach(_ := task_s3.bits.tgtID.get) // TODO: srcID should be fixed. FIX THIS!!!
    sink_resp_s3.bits.txnID.foreach(_ := task_s3.bits.txnID.get)
    sink_resp_s3.bits.dbID.foreach(_ := 0.U)
    sink_resp_s3.bits.pCrdType.foreach(_ := 0.U) // TODO
    sink_resp_s3.bits.chiOpcode.foreach(_ := MuxLookup(
      Cat(doFwd || doFwdHitRelease, doRespData || doRespDataHitRelease),
      RSPOpcodes.SnpResp
    )(Seq(
      Cat(false.B, false.B) -> RSPOpcodes.SnpResp,
      Cat(true.B, false.B)  -> RSPOpcodes.SnpRespFwded,
      Cat(false.B, true.B)  -> DATOpcodes.SnpRespData, // ignore SnpRespDataPtl for now
      Cat(true.B, true.B)   -> DATOpcodes.SnpRespDataFwded
    )))
    sink_resp_s3.bits.resp.foreach(_ := Mux(
      req_s3.snpHitRelease && !SNPOpcodes.isSnpStashX(req_s3.chiOpcode.get),
      setPD(I, req_s3.snpHitReleaseWithData && !SNPOpcodes.isSnpMakeInvalidX(req_s3.chiOpcode.get)),
      setPD(respCacheState, respPassDirty && (doRespData || doRespDataHitRelease))
    ))
    sink_resp_s3.bits.fwdState.foreach(_ := setPD(fwdCacheState, fwdPassDirty))
    sink_resp_s3.bits.txChannel := Cat(
      doRespData || doRespDataHitRelease,
      !(doRespData || doRespDataHitRelease),
      false.B
    ) // TODO: parameterize this
    sink_resp_s3.bits.size := log2Ceil(blockBytes).U

  }.otherwise { // req_s3.fromC
    sink_resp_s3.bits.opcode := ReleaseAck
    sink_resp_s3.bits.param := 0.U // param of ReleaseAck must be 0
  }

  val source_req_s3 = Wire(new TaskBundle)
  source_req_s3 := Mux(sink_resp_s3.valid, sink_resp_s3.bits, req_s3)
  source_req_s3.isKeyword.foreach(_ := req_s3.isKeyword.getOrElse(false.B))

  /* ======== Interact with DS ======== */
  val data_s3 = Mux(io.releaseBufResp_s3.valid, io.releaseBufResp_s3.bits.data, io.refillBufResp_s3.bits.data)
  val c_releaseData_s3 = io.bufResp.data.asUInt
  val hasData_s3_tl = source_req_s3.opcode(0) // whether to respond data to TileLink-side
  val hasData_s3_chi = source_req_s3.toTXDAT // whether to respond data to CHI-side
  val hasData_s3 = hasData_s3_tl || hasData_s3_chi

  val need_data_a = dirResult_s3.hit && (req_get_s3 || req_acquireBlock_s3)
  val need_data_b = sinkB_req_s3 && (doRespData || doFwd || dirResult_s3.hit && meta_s3.state === TRUNK)
  val need_data_mshr_repl = mshr_refill_s3 && need_repl && !retry
  val ren = need_data_a || need_data_b || need_data_mshr_repl

  val wen_c = sinkC_req_s3 && isParamFromT(req_s3.param) && req_s3.opcode(0) && dirResult_s3.hit
  val wen_mshr = req_s3.dsWen && (
    mshr_snpRespX_s3 || mshr_snpRespDataX_s3 || mshr_writeBackFull_s3 || mshr_evict_s3 ||
    mshr_refill_s3 && !need_repl && !retry
  )
  val wen = wen_c || wen_mshr

  io.toDS.req_s3.valid := task_s3.valid && (ren || wen)
  io.toDS.req_s3.bits.way := Mux(
    mshr_refill_s3 && req_s3.replTask,
    io.replResp.bits.way,
    Mux(mshr_req_s3, req_s3.way, dirResult_s3.way)
  )
  io.toDS.req_s3.bits.set := Mux(mshr_req_s3, req_s3.set, dirResult_s3.set)
  io.toDS.req_s3.bits.wen := wen
  io.toDS.wdata_s3.data := Mux(
    !mshr_req_s3,
    c_releaseData_s3,
    Mux(
      req_s3.useProbeData,
      io.releaseBufResp_s3.bits.data,
      io.refillBufResp_s3.bits.data
    )
  )

  /* ======== Read DS and store data in Buffer ======== */
  // A: need_write_releaseBuf indicates that DS should be read and the data will be written into ReleaseBuffer
  //    need_write_releaseBuf is assigned true when:
  //    inner clients' data is needed, but whether the client will ack data is uncertain, so DS data is also needed
  val need_write_releaseBuf = need_probe_s3_a ||
    cache_alias ||
    need_data_b && need_mshr_s3_b ||
    need_data_mshr_repl
  // B: need_write_refillBuf indicates that DS should be read and the data will be written into RefillBuffer
  //    when L1 AcquireBlock but L2 AcquirePerm to L3, we need to prepare data for L1
  //    but this will no longer happen, cuz we always AcquireBlock for L1 AcquireBlock
  val need_write_refillBuf = false.B

  /* ======== Write Directory ======== */
  val metaW_valid_s3_a = sinkA_req_s3 && !need_mshr_s3_a && !req_get_s3 && !req_prefetch_s3 // get & prefetch that hit will not write meta
  val metaW_valid_s3_b = sinkB_req_s3 && !need_mshr_s3_b && dirResult_s3.hit &&
    !SNPOpcodes.isSnpOnceX(req_s3.chiOpcode.get) && !SNPOpcodes.isSnpStashX(req_s3.chiOpcode.get) && (
      meta_s3.state === TIP || meta_s3.state === BRANCH && SNPOpcodes.isSnpToN(req_s3.chiOpcode.get)
    )
  val metaW_valid_s3_c = sinkC_req_s3 && dirResult_s3.hit
  val metaW_valid_s3_mshr = mshr_req_s3 && req_s3.metaWen && !(mshr_refill_s3 && retry)
  require(clientBits == 1)

  val metaW_s3_a_alias = Mux(
    req_get_s3 || req_prefetch_s3,
    meta_s3.alias.getOrElse(0.U),
    req_s3.alias.getOrElse(0.U)
  )
  val metaW_s3_a = MetaEntry(
    dirty = meta_s3.dirty,
    state = Mux(req_needT_s3 || sink_resp_s3_a_promoteT, TRUNK, meta_s3.state),
    clients = Fill(clientBits, true.B),
    alias = Some(metaW_s3_a_alias),
    accessed = true.B
  )
  val metaW_s3_b = Mux(SNPOpcodes.isSnpToN(req_s3.chiOpcode.get), MetaEntry(),
    MetaEntry(
      dirty = false.B,
      state = Mux(req_s3.chiOpcode.get === SNPOpcodes.SnpCleanShared, meta_s3.state, BRANCH),
      clients = meta_s3.clients,
      alias = meta_s3.alias,
      accessed = meta_s3.accessed
    )
  )
  val metaW_s3_c = MetaEntry(
    dirty = meta_s3.dirty || wen_c,
    state = Mux(isParamFromT(req_s3.param), TIP, meta_s3.state),
    clients = Fill(clientBits, !isToN(req_s3.param)),
    alias = meta_s3.alias,
    accessed = meta_s3.accessed
  )
  // use merge_meta if mergeA
  val metaW_s3_mshr = Mux(req_s3.mergeA, req_s3.aMergeTask.meta, req_s3.meta)

  val metaW_way = Mux(
    mshr_refill_s3 && req_s3.replTask,
    io.replResp.bits.way, // grant always use replResp way
    Mux(mshr_req_s3, req_s3.way, dirResult_s3.way)
  )

  io.metaWReq.valid := !resetFinish || task_s3.valid && (
    metaW_valid_s3_a || metaW_valid_s3_b || metaW_valid_s3_c || metaW_valid_s3_mshr
  )
  io.metaWReq.bits.set := Mux(resetFinish, req_s3.set, resetIdx)
  io.metaWReq.bits.wayOH := Mux(resetFinish, UIntToOH(metaW_way), Fill(cacheParams.ways, true.B))
  io.metaWReq.bits.wmeta := Mux(
    resetFinish,
    ParallelPriorityMux(
      Seq(metaW_valid_s3_a, metaW_valid_s3_b, metaW_valid_s3_c, metaW_valid_s3_mshr),
      Seq(metaW_s3_a, metaW_s3_b, metaW_s3_c, metaW_s3_mshr)
    ),
    MetaEntry()
  )

  io.tagWReq.valid := task_s3.valid && req_s3.tagWen && mshr_refill_s3 && !retry
  io.tagWReq.bits.set := req_s3.set
  io.tagWReq.bits.way := Mux(mshr_refill_s3 && req_s3.replTask, io.replResp.bits.way, req_s3.way)
  io.tagWReq.bits.wtag := req_s3.tag

  /* ======== Interact with Channels (SourceD/TXREQ/TXRSP/TXDAT) ======== */
  val chnl_fire_s3 = d_s3.fire || txreq_s3.fire || txrsp_s3.fire || txdat_s3.fire
  val req_drop_s3 = !need_write_releaseBuf && (
    !mshr_req_s3 && need_mshr_s3 || chnl_fire_s3
  ) || mshr_refill_s3 && retry

  val data_unready_s3 = hasData_s3 && !mshr_req_s3
  val data_unready_s3_tl = hasData_s3_tl && !mshr_req_s3
  /**
    * The combinational logic path of
    *     Directory metaAll
    * ->  Directory response
    * ->  MainPipe judging whether to respond data
    * is too long. Therefore the sinkB response may be latched to s4 for better timing.
    */
  val d_s3_latch = true
  val txdat_s3_latch = true
  val isD_s3 = Mux(
    mshr_req_s3,
    mshr_refill_s3 && !retry,
    req_s3.fromC || req_s3.fromA && !need_mshr_s3_a && !data_unready_s3_tl && req_s3.opcode =/= Hint
  )
  val isD_s3_ready = Mux(
    mshr_req_s3,
    mshr_refill_s3 && !retry,
    req_s3.fromC || req_s3.fromA && !need_mshr_s3_a && !data_unready_s3_tl && req_s3.opcode =/= Hint && !d_s3_latch.B
  )
  val isTXRSP_s3 = Mux(
    mshr_req_s3,
    mshr_snpRespX_s3,
    req_s3.fromB && !need_mshr_s3 && !hasData_s3
  )
  val isTXDAT_s3 = Mux(
    mshr_req_s3,
    mshr_snpRespDataX_s3 || mshr_cbWrData_s3 || mshr_dct_s3,
    req_s3.fromB && !need_mshr_s3 && (doRespDataHitRelease || doRespData && !data_unready_s3)
  )
  val isTXDAT_s3_ready = Mux(
    mshr_req_s3,
    mshr_snpRespDataX_s3 || mshr_cbWrData_s3 || mshr_dct_s3,
    req_s3.fromB && !need_mshr_s3 && (doRespDataHitRelease || doRespData && !data_unready_s3) && !txdat_s3_latch.B
  )
  val isTXREQ_s3 = mshr_req_s3 && (mshr_writeBackFull_s3 || mshr_evict_s3)

  txreq_s3.valid := task_s3.valid && isTXREQ_s3
  txrsp_s3.valid := task_s3.valid && isTXRSP_s3
  txdat_s3.valid := task_s3.valid && isTXDAT_s3_ready
  d_s3.valid := task_s3.valid && isD_s3_ready
  txreq_s3.bits := source_req_s3.toCHIREQBundle()
  txrsp_s3.bits := source_req_s3
  txdat_s3.bits.task := source_req_s3
  txdat_s3.bits.data.data := data_s3
  d_s3.bits.task := source_req_s3
  d_s3.bits.data.data := data_s3

  when (task_s3.valid) {
    OneHot.checkOneHot(Seq(isTXREQ_s3, isTXRSP_s3, isTXDAT_s3, isD_s3))
  }

  /* ======== nested writeback ======== */
  io.nestedwb.set := req_s3.set
  io.nestedwb.tag := req_s3.tag
  // This serves as VALID signal
  // c_set_dirty is true iff Release has Data
  io.nestedwb.c_set_dirty := task_s3.valid && task_s3.bits.fromC && task_s3.bits.opcode === ReleaseData && task_s3.bits.param === TtoN
  /**
    * Snoop nesting happens when:
    * 1. snoop nests a copy-back request
    * 2. snoop nests a Read/MakeUnique request
    */
  io.nestedwb.b_inv_dirty := task_s3.valid && task_s3.bits.fromB && source_req_s3.snpHitRelease
  io.nestedwb.b_toB.foreach(_ :=
    task_s3.valid && task_s3.bits.fromB && source_req_s3.metaWen && source_req_s3.meta.state === BRANCH
  )
  io.nestedwb.b_toN.foreach(_ :=
    task_s3.valid && task_s3.bits.fromB && source_req_s3.metaWen && source_req_s3.meta.state === INVALID
  )

  io.nestedwbData := c_releaseData_s3.asTypeOf(new DSBlock)
  
  // TODO: add nested writeback from Snoop

  /* ======== prefetch ======== */
  io.prefetchTrain.foreach {
    train =>
      // train on request(with needHint flag) miss or hit on prefetched block
      // trigger train also in a_merge here
      train.valid := task_s3.valid && ((req_acquire_s3 || req_get_s3) && req_s3.needHint.getOrElse(false.B) &&
        (!dirResult_s3.hit || meta_s3.prefetch.get) || req_s3.mergeA)
      train.bits.tag := req_s3.tag
      train.bits.set := req_s3.set
      train.bits.needT := Mux(
        req_s3.mergeA,
        needT(req_s3.aMergeTask.opcode, req_s3.aMergeTask.param),
        req_needT_s3
      )
      train.bits.source := Mux(req_s3.mergeA, req_s3.aMergeTask.sourceId, req_s3.sourceId)
      train.bits.vaddr.foreach(_ := Mux(req_s3.mergeA, req_s3.aMergeTask.vaddr.getOrElse(0.U), req_s3.vaddr.getOrElse(0.U)))
      train.bits.hit := Mux(req_s3.mergeA, true.B, dirResult_s3.hit)
      train.bits.prefetched := Mux(req_s3.mergeA, true.B, meta_s3.prefetch.getOrElse(false.B))
      train.bits.pfsource := meta_s3.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) // TODO
      train.bits.reqsource := req_s3.reqSource
  }

  /* ======== Stage 4 ======== */
  val task_s4 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  val data_unready_s4 = RegInit(false.B)
  val data_s4 = Reg(UInt((blockBytes * 8).W))
  val ren_s4 = RegInit(false.B)
  val need_write_releaseBuf_s4 = RegInit(false.B)
  val isD_s4, isTXREQ_s4, isTXRSP_s4, isTXDAT_s4 = RegInit(false.B)
  val pendingTXDAT_s4 = task_s4.bits.fromB && !task_s4.bits.mshrTask && task_s4.bits.toTXDAT
  val pendingD_s4 = task_s4.bits.fromA && !task_s4.bits.mshrTask && (
    task_s4.bits.opcode === GrantData || task_s4.bits.opcode === AccessAckData
  )

  task_s4.valid := task_s3.valid && !req_drop_s3

  when (task_s3.valid && !req_drop_s3) {
    task_s4.bits := source_req_s3

    when (!task_s3.bits.mshrTask && need_mshr_s3) {
      task_s4.bits.mshrId := io.fromMSHRCtl.mshr_alloc_ptr
    }

    data_unready_s4 := data_unready_s3
    data_s4 := data_s3
    ren_s4 := ren
    need_write_releaseBuf_s4 := need_write_releaseBuf
    isD_s4 := isD_s3
    isTXREQ_s4 := isTXREQ_s3
    isTXRSP_s4 := isTXRSP_s3
    isTXDAT_s4 := isTXDAT_s3
  }

  // for reqs that CANNOT give response in MainPipe, but needs to write releaseBuf/refillBuf
  // we cannot drop them at s3, we must let them go to s4/s5
  val chnl_fire_s4 = d_s4.fire || txreq_s4.fire || txrsp_s4.fire || txdat_s4.fire
  val req_drop_s4 = !need_write_releaseBuf_s4 && chnl_fire_s4

  val chnl_valid_s4 = task_s4.valid && !RegNext(chnl_fire_s3, false.B)
  d_s4.valid := chnl_valid_s4 && isD_s4
  txreq_s4.valid := chnl_valid_s4 && isTXREQ_s4
  txrsp_s4.valid := chnl_valid_s4 && isTXRSP_s4
  txdat_s4.valid := chnl_valid_s4 && isTXDAT_s4
  d_s4.bits.task := task_s4.bits
  d_s4.bits.data.data := data_s4
  txreq_s4.bits := task_s4.bits.toCHIREQBundle()
  txrsp_s4.bits := task_s4.bits
  txdat_s4.bits.task := task_s4.bits
  txdat_s4.bits.data.data := data_s4

  /* ======== Stage 5 ======== */
  val task_s5 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  val ren_s5 = RegInit(false.B)
  val data_s5 = Reg(UInt((blockBytes * 8).W))
  val need_write_releaseBuf_s5 = RegInit(false.B)
  val isD_s5, isTXREQ_s5, isTXRSP_s5, isTXDAT_s5 = RegInit(false.B)


  task_s5.valid := task_s4.valid && !req_drop_s4

  when (task_s4.valid && !req_drop_s4) {
    task_s5.bits := task_s4.bits
    ren_s5 := ren_s4
    data_s5 := data_s4
    need_write_releaseBuf_s5 := need_write_releaseBuf_s4
    isD_s5 := isD_s4 || pendingD_s4
    isTXREQ_s5 := isTXREQ_s4
    isTXRSP_s5 := isTXRSP_s4
    isTXDAT_s5 := isTXDAT_s4 || pendingTXDAT_s4
  }
  val rdata_s5 = io.toDS.rdata_s5.data
  val out_data_s5 = Mux(task_s5.bits.mshrTask || task_s5.bits.snpHitReleaseWithData, data_s5, rdata_s5)
  val chnl_fire_s5 = d_s5.fire || txreq_s5.fire || txrsp_s5.fire || txdat_s5.fire

  // TODO: check this
  val customL1Hint = Module(new CustomL1Hint)

  customL1Hint.io.s1 := io.taskInfo_s1
  // customL1Hint.io.s2 := task_s2

  customL1Hint.io.s3.task      := task_s3
  // overwrite opcode: if sinkReq can respond, use sink_resp_s3.bits.opcode = Grant/GrantData
  customL1Hint.io.s3.task.bits.opcode := Mux(sink_resp_s3.valid, sink_resp_s3.bits.opcode, task_s3.bits.opcode)
  // customL1Hint.io.s3.d         := d_s3.valid
  customL1Hint.io.s3.need_mshr := need_mshr_s3

  // customL1Hint.io.s4.task                  := task_s4
  // customL1Hint.io.s4.d                     := d_s4.valid
  // customL1Hint.io.s4.need_write_releaseBuf := need_write_releaseBuf_s4

  // customL1Hint.io.s5.task      := task_s5
  // customL1Hint.io.s5.d         := d_s5.valid

  // customL1Hint.io.globalCounter   := io.globalCounter
  // customL1Hint.io.grantBufferHint <> io.grantBufferHint

  customL1Hint.io.l1Hint <> io.l1Hint

  io.releaseBufWrite.valid := task_s5.valid && need_write_releaseBuf_s5
  io.releaseBufWrite.bits.id := task_s5.bits.mshrId
  io.releaseBufWrite.bits.data.data := rdata_s5
  io.releaseBufWrite.bits.beatMask := Fill(beatSize, true.B)

  val chnl_valid_s5 = task_s5.valid && !RegNext(chnl_fire_s4, false.B) && !RegNextN(chnl_fire_s3, 2, Some(false.B))
  d_s5.valid := chnl_valid_s5 && isD_s5
  txreq_s5.valid := chnl_valid_s5 && isTXREQ_s5
  txrsp_s5.valid := chnl_valid_s5 && isTXRSP_s5
  txdat_s5.valid := chnl_valid_s5 && isTXDAT_s5
  d_s5.bits.task := task_s5.bits
  d_s5.bits.data.data := out_data_s5
  txreq_s5.bits := task_s5.bits.toCHIREQBundle()
  txrsp_s5.bits := task_s5.bits
  txdat_s5.bits.task := task_s5.bits
  txdat_s5.bits.data.data := out_data_s5

  /* ======== BlockInfo ======== */
  // if s2/s3 might write Dir, we must block s1 sink entrance
  // TODO:[Check] it seems that s3 Dir write will naturally block all s1 by dirRead.ready
  //        (an even stronger blocking than set blocking)
  //         so we might not need s3 blocking here
  def s23Block(chn: Char, s: TaskBundle): Bool = {
    val s1 = io.fromReqArb.status_s1
    val s1_set = chn match {
      case 'a' => s1.a_set
      case 'b' => s1.b_set
      case 'c' => s1.c_set
      case 'g' => s1.g_set
    }
    s.set === s1_set && !(s.mshrTask && !s.metaWen) // if guaranteed not to write meta, no blocking needed
  }
  def bBlock(s: TaskBundle, tag: Boolean = false): Bool = {
    val s1 = io.fromReqArb.status_s1
    // tag true: compare tag + set
    s.set === s1.b_set && (if(tag) s.tag === s1.b_tag else true.B)
  }

  io.toReqBuf(0) := task_s2.valid && s23Block('a', task_s2.bits)
  io.toReqBuf(1) := task_s3.valid && s23Block('a', task_s3.bits)

  io.toReqArb.blockC_s1 := task_s2.valid && s23Block('c', task_s2.bits)

  io.toReqArb.blockB_s1 :=
    task_s2.valid && bBlock(task_s2.bits) ||
    task_s3.valid && bBlock(task_s3.bits) ||
    task_s4.valid && bBlock(task_s4.bits, tag = true) ||
    task_s5.valid && bBlock(task_s5.bits, tag = true)
  
  io.toReqArb.blockA_s1 := io.toReqBuf(0) || io.toReqBuf(1)

  io.toReqArb.blockG_s1 := task_s2.valid && s23Block('g', task_s2.bits)

  /* ======== Pipeline Status ======== */
  require(io.status_vec_toD.size == 3)
  io.status_vec_toD(0).valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_refill_s3 && !retry,
    true.B
    // TODO:
    // To consider grantBuffer capacity conflict, only " req_s3.fromC || req_s3.fromA && !need_mshr_s3 " is needed
    // But to consider mshrFull, all channel_reqs are needed
    // so maybe it is excessive for grantBuf capacity conflict
  )

  io.status_vec_toD(0).bits.channel := task_s3.bits.channel
  io.status_vec_toD(1).valid        := task_s4.valid && (isD_s4 || pendingD_s4)
  io.status_vec_toD(1).bits.channel := task_s4.bits.channel
  io.status_vec_toD(2).valid        := d_s5.valid
  io.status_vec_toD(2).bits.channel := task_s5.bits.channel

  // capacity control of TX channels
  val tx_task_s3 = Wire(Valid(new TaskBundle))
  tx_task_s3.valid := task_s3.valid // TODO: review this
  tx_task_s3.bits := source_req_s3
  val tasks = Seq(tx_task_s3, task_s4, task_s5)
  io.status_vec_toTX.zip(tasks).foreach { case (status, task) =>
    status.valid := task.valid
    status.bits.channel := task.bits.channel
    // To optimize timing, we restrict the blocking condition of TXRSP and TXDAT.
    // This may be inaccurate, but it works.
    status.bits.txChannel := Cat(
      // TXDAT
      !neverRespData,
      // TXRSP
      !doRespDataHitRelease,
      // TXREQ
      task.bits.toTXREQ
    )
    status.bits.mshrTask := task.bits.mshrTask
  }

  /* ======== Other Signals Assignment ======== */
  // Initial state assignment
  // ! Caution: s_ and w_ are false-as-valid
  when (req_s3.fromA) {
    alloc_state.s_refill := false.B
    alloc_state.w_replResp := dirResult_s3.hit
    // need Acquire downwards
    when (need_acquire_s3_a) {
      alloc_state.s_acquire := false.B
      alloc_state.s_compack.get := false.B
      alloc_state.w_grantfirst := false.B
      alloc_state.w_grantlast := false.B
      alloc_state.w_grant := false.B
    }
    // need Probe for alias
    // need Probe when Get hits on a TRUNK block
    when (cache_alias || need_probe_s3_a) {
      alloc_state.s_rprobe := false.B
      alloc_state.w_rprobeackfirst := false.B
      alloc_state.w_rprobeacklast := false.B
    }
  }

  when (req_s3.fromB) {
    alloc_state.s_probeack := false.B
    // need pprobe
    when (need_pprobe_s3_b) {
      alloc_state.s_pprobe := false.B
      alloc_state.w_pprobeackfirst := false.B
      alloc_state.w_pprobeacklast := false.B
      alloc_state.w_pprobeack := false.B
    }
    // need forwarding response
    when (need_dct_s3_b) {
      alloc_state.s_dct.get := false.B
    }
  }

  val d = Seq(d_s5, d_s4, d_s3)
  val txreq = Seq(txreq_s5, txreq_s4, txreq_s3)
  val txrsp = Seq(txrsp_s5, txrsp_s4, txrsp_s3)
  val txdat = Seq(txdat_s5, txdat_s4, txdat_s3)
  // DO NOT use TLArbiter because TLArbiter will send continuous beats for the same source
  arb(d, io.toSourceD, Some("toSourceD"))
  arb(txreq, io.toTXREQ, Some("toTXREQ"))
  arb(txrsp, io.toTXRSP, Some("toTXRSP"))
  arb(txdat, io.toTXDAT, Some("toTXDAT"))


  /* ===== Performance counters ===== */
  // num of mshr req
  XSPerfAccumulate(cacheParams, "mshr_grant_req", task_s3.valid && mshr_grant_s3 && !retry)
  XSPerfAccumulate(cacheParams, "mshr_grantdata_req", task_s3.valid && mshr_grantdata_s3 && !retry)
  XSPerfAccumulate(cacheParams, "mshr_accessackdata_req", task_s3.valid && mshr_accessackdata_s3 && !retry)
  XSPerfAccumulate(cacheParams, "mshr_hintack_req", task_s3.valid && mshr_hintack_s3 && !retry)
  // XSPerfAccumulate(cacheParams, "mshr_probeack_req", task_s3.valid && mshr_probeack_s3)
  // XSPerfAccumulate(cacheParams, "mshr_probeackdata_req", task_s3.valid && mshr_probeackdata_s3)
  // XSPerfAccumulate(cacheParams, "mshr_release_req", task_s3.valid && mshr_release_s3)
  XSPerfAccumulate(cacheParams, "mshr_snpResp_req", task_s3.valid && mshr_snpResp_s3)
  XSPerfAccumulate(cacheParams, "mshr_snpRespFwded_req", task_s3.valid && mshr_snpRespFwded_s3)
  XSPerfAccumulate(cacheParams, "mshr_snpRespData_req", task_s3.valid && mshr_snpRespData_s3)
  XSPerfAccumulate(cacheParams, "mshr_snpRespDataPtl_req", task_s3.valid && mshr_snpRespDataPtl_s3)
  XSPerfAccumulate(cacheParams, "mshr_snpRespDataFwded_req", task_s3.valid && mshr_snpRespDataFwded_s3)
  XSPerfAccumulate(cacheParams, "mshr_writeBackFull", task_s3.valid && mshr_writeBackFull_s3)
  XSPerfAccumulate(cacheParams, "mshr_evict_s3", task_s3.valid && mshr_evict_s3)
  

  // directory access result
  val hit_s3 = task_s3.valid && !mshr_req_s3 && dirResult_s3.hit
  val miss_s3 = task_s3.valid && !mshr_req_s3 && !dirResult_s3.hit
  XSPerfAccumulate(cacheParams, "a_req_hit", hit_s3 && req_s3.fromA)
  XSPerfAccumulate(cacheParams, "acquire_hit", hit_s3 && req_s3.fromA &&
    (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm))
  XSPerfAccumulate(cacheParams, "get_hit", hit_s3 && req_s3.fromA && req_s3.opcode === Get)
  XSPerfAccumulate(cacheParams, "retry", mshr_refill_s3 && retry)

  XSPerfAccumulate(cacheParams, "a_req_miss", miss_s3 && req_s3.fromA)
  XSPerfAccumulate(cacheParams, "acquire_miss", miss_s3 && req_s3.fromA &&
    (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm))
  XSPerfAccumulate(cacheParams, "get_miss", miss_s3 && req_s3.fromA && req_s3.opcode === Get)

  XSPerfAccumulate(cacheParams, "b_req_hit", hit_s3 && req_s3.fromB)
  XSPerfAccumulate(cacheParams, "b_req_miss", miss_s3 && req_s3.fromB)

  XSPerfHistogram(cacheParams, "a_req_access_way", perfCnt = dirResult_s3.way,
    enable = task_s3.valid && !mshr_req_s3 && req_s3.fromA, start = 0, stop = cacheParams.ways, step = 1)
  XSPerfHistogram(cacheParams, "a_req_hit_way", perfCnt = dirResult_s3.way,
    enable = hit_s3 && req_s3.fromA, start = 0, stop = cacheParams.ways, step = 1)
  XSPerfHistogram(cacheParams, "a_req_miss_way_choice", perfCnt = dirResult_s3.way,
    enable = miss_s3 && req_s3.fromA, start = 0, stop = cacheParams.ways, step = 1)

  // pipeline stages for TX and sourceD reqs
  val pipe_len = Seq(5.U, 4.U, 3.U)
  val sourceD_pipe_len = ParallelMux(d.map(_.fire), pipe_len)
  val txreq_pipe_len = ParallelMux(txreq.map(_.fire), pipe_len)
  val txrsp_pipe_len = ParallelMux(txrsp.map(_.fire), pipe_len)
  val txdat_pipe_len = ParallelMux(txdat.map(_.fire), pipe_len)
  XSPerfHistogram(cacheParams, "sourceD_pipeline_stages", sourceD_pipe_len,
    enable = io.toSourceD.fire, start = 3, stop = 5+1, step = 1)
  XSPerfHistogram(cacheParams, "txreq_pipeline_stages", txreq_pipe_len,
    enable = io.toTXREQ.fire, start = 3, stop = 5+1, step = 1)
  XSPerfHistogram(cacheParams, "txrsp_pipeline_stages", txrsp_pipe_len,
    enable = io.toTXRSP.fire, start = 3, stop = 5+1, step = 1)
  XSPerfHistogram(cacheParams, "txdat_pipeline_stages", txdat_pipe_len,
    enable = io.toTXDAT.fire, start = 3, stop = 5+1, step = 1)

  // XSPerfAccumulate(cacheParams, "a_req_tigger_prefetch", io.prefetchTrain.)
  prefetchOpt.foreach {
    _ =>
      XSPerfAccumulate(cacheParams, "a_req_trigger_prefetch", io.prefetchTrain.get.fire)
      XSPerfAccumulate(cacheParams, "a_req_trigger_prefetch_not_ready", io.prefetchTrain.get.valid && !io.prefetchTrain.get.ready)
      XSPerfAccumulate(cacheParams, "acquire_trigger_prefetch_on_miss", io.prefetchTrain.get.fire && req_acquire_s3 && !dirResult_s3.hit)
      XSPerfAccumulate(cacheParams, "acquire_trigger_prefetch_on_hit_pft", io.prefetchTrain.get.fire && req_acquire_s3 && dirResult_s3.hit && meta_s3.prefetch.get)
      // TODO
      // XSPerfAccumulate(cacheParams, "release_all", mshr_release_s3)
      // XSPerfAccumulate(cacheParams, "release_prefetch_accessed", mshr_release_s3 && meta_s3.prefetch.get && meta_s3.accessed)
      // XSPerfAccumulate(cacheParams, "release_prefetch_not_accessed", mshr_release_s3 && meta_s3.prefetch.get && !meta_s3.accessed)
      XSPerfAccumulate(cacheParams, "get_trigger_prefetch_on_miss", io.prefetchTrain.get.fire && req_get_s3 && !dirResult_s3.hit)
      XSPerfAccumulate(cacheParams, "get_trigger_prefetch_on_hit_pft", io.prefetchTrain.get.fire && req_get_s3 && dirResult_s3.hit && meta_s3.prefetch.get)
  }

  XSPerfAccumulate(cacheParams, "early_prefetch", meta_s3.prefetch.getOrElse(false.B) && !meta_s3.accessed && !dirResult_s3.hit && task_s3.valid)

}