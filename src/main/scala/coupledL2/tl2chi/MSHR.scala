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
import coupledL2.MetaData._
import utility.{MemReqSource, ParallelLookUp, ParallelPriorityMux}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters
import coupledL2.prefetch.{PfSource, PrefetchTrain}
import coupledL2.utils.XSPerfAccumulate
import coupledL2.tl2chi.CHIOpcode._
import coupledL2.tl2chi.CHIOpcode.DATOpcodes._
import coupledL2.tl2chi.CHIOpcode.REQOpcodes._
import coupledL2.tl2chi.CHIOpcode.RSPOpcodes._
import coupledL2.tl2chi.CHIOpcode.SNPOpcodes._
import coupledL2.tl2chi.CHICohStates._
import coupledL2.tl2chi.CHIChannel
import coupledL2.MetaData._
import coupledL2._


class MSHRTasks(implicit p: Parameters) extends TL2CHIL2Bundle {
  // outer
  val txreq = DecoupledIO(new CHIREQ) //TODO: no need to use decoupled Shandshake
  val txrsp = DecoupledIO(new CHIRSP) //TODO: no need to use decoupled handshake
  val source_b = DecoupledIO(new SourceBReq)
  val mainpipe = DecoupledIO(new TaskBundle) // To Mainpipe (SourceC or SourceD)
  // val prefetchTrain = prefetchOpt.map(_ => DecoupledIO(new PrefetchTrain)) // To prefetcher
}

class MSHRResps(implicit p: Parameters) extends TL2CHIL2Bundle {
  val sinkC = Flipped(ValidIO(new RespInfoBundle))  
  val rxrsp = Flipped(ValidIO(new RespInfoBundle))  
  val rxdat = Flipped(ValidIO(new RespInfoBundle))  
//  val rxrsp = new RespBundle()  
//  val rxdat = new RespBundle()  
}

class MSHR(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val id = Input(UInt(mshrBits.W))
    val status = ValidIO(new MSHRStatus)
    val msInfo = ValidIO(new MSHRInfo)
    val alloc = Flipped(ValidIO(new MSHRRequest))
    val tasks = new MSHRTasks()
    val resps = new MSHRResps()
    val nestedwb = Input(new NestedWriteback)
    val nestedwbData = Output(Bool())
    val aMergeTask = Flipped(ValidIO(new TaskBundle))
    val replResp = Flipped(ValidIO(new ReplacerResult))
    val pCamPri = Input(Bool())
    val waitPCrdInfo = Output(new PCrdInfo)
  })

  require (chiOpt.isDefined)

  val gotT = RegInit(false.B) // L3 might return T even though L2 wants B
  val gotDirty = RegInit(false.B)
  val gotGrantData = RegInit(false.B)
  val probeDirty = RegInit(false.B)
  val probeGotN = RegInit(false.B)
  val timer = RegInit(0.U(64.W)) // for performance analysis

  val req_valid = RegInit(false.B)
  val req       = RegInit(0.U.asTypeOf(new TaskBundle()))
  val dirResult = RegInit(0.U.asTypeOf(new DirResult()))
  val meta      = dirResult.meta
  val initState = Wire(new FSMState())
  initState.elements.foreach(_._2 := true.B)
  val state     = RegInit(new FSMState(), initState)

  //for CHI
  val srcid = RegInit(0.U(NODEID_WIDTH.W))
  val homenid = RegInit(0.U(NODEID_WIDTH.W))
  val dbid = RegInit(0.U(DBID_WIDTH.W))
  val pcrdtype = RegInit(0.U(PCRDTYPE_WIDTH.W))
  val gotRetryAck = RegInit(false.B)
  val gotPCrdGrant = RegInit(false.B)
  val gotReissued = RegInit(false.B)
  val metaChi = ParallelLookUp(
    Cat(meta.dirty, meta.state),
    Seq(
      Cat(false.B, INVALID)-> I,
      Cat(false.B, BRANCH) -> SC,
      Cat(false.B, TRUNK) -> UC,
      Cat(false.B, TIP)   -> UC,
      Cat( true.B, TRUNK) -> UD,
      Cat( true.B, TIP)   -> UD
    ))
  //for PCrdGrant info. search
  io.waitPCrdInfo.valid := gotRetryAck && !gotReissued
  io.waitPCrdInfo.srcID.get := srcid
  io.waitPCrdInfo.pCrdType.get := pcrdtype

  /* Allocation */
  when(io.alloc.valid) {
    req_valid := true.B
    state     := io.alloc.bits.state
    dirResult := io.alloc.bits.dirResult
    req       := io.alloc.bits.task
    gotT        := false.B
    gotDirty    := false.B
    gotGrantData := false.B
    probeDirty  := false.B
    probeGotN   := false.B
    timer       := 1.U

    gotRetryAck := false.B
    gotPCrdGrant := false.B
    gotReissued := false.B
    srcid := 0.U
    dbid := 0.U
    pcrdtype := 0.U
  }

  /* ======== Enchantment ======== */
  val meta_pft = meta.prefetch.getOrElse(false.B)
  val meta_no_client = !meta.clients.orR

  val req_needT = needT(req.opcode, req.param)
  val req_needB = needB(req.opcode, req.param)
  val req_acquire = req.opcode === AcquireBlock && req.fromA || req.opcode === AcquirePerm // AcquireBlock and Probe share the same opcode
  val req_acquirePerm = req.opcode === AcquirePerm
  val req_get = req.opcode === Get
  val req_prefetch = req.opcode === Hint

  val req_chiOpcode = req.chiOpcode.get

  val snpToN = isSnpToN(req_chiOpcode)
  val snpToB = isSnpToB(req_chiOpcode)

  /**
    * About which snoop should echo SnpRespData[Fwded] instead of SnpResp[Fwded]:
    * 1. When the snooped block is dirty, always echo SnpRespData[Fwded], except for SnpMakeInvalid*, SnpStash*,
    *    SnpOnceFwd, and SnpUniqueFwd.
    * 2. When the snoop opcode is SnpCleanFwd, SnpNotSharedDirtyFwd or SnpSharedFwd, always echo SnpRespDataFwded
    *    if RetToSrc = 1 as long as the snooped block is valid.
    */
  val doRespData = isT(meta.state) && meta.dirty && (
    req_chiOpcode === SnpOnce ||
    snpToB ||
    req_chiOpcode === SnpUnique ||
    req_chiOpcode === SnpUniqueStash ||
    req_chiOpcode === SnpCleanShared ||
    req_chiOpcode === SnpCleanInvalid
  ) || dirResult.hit && req.retToSrc.get && isSnpToBFwd(req_chiOpcode)
  /**
    * About which snoop should echo SnpResp[Data]Fwded instead of SnpResp[Data]:
    * 1. When the snoop opcode is Snp*Fwd and the snooped block is valid.
    */
  val doFwd = isSnpXFwd(req_chiOpcode) && dirResult.hit

  val gotUD = meta.dirty & isT(meta.state) //TC/TTC -> UD
  val promoteT_normal =  dirResult.hit && meta_no_client && meta.state === TIP
  val promoteT_L3     = !dirResult.hit && gotT
  val promoteT_alias  =  dirResult.hit && req.aliasTask.getOrElse(false.B) && (meta.state === TRUNK || meta.state === TIP)
  // under above circumstances, we grant T to L1 even if it wants B
  val req_promoteT = (req_acquire || req_get || req_prefetch) && (promoteT_normal || promoteT_L3 || promoteT_alias)

  assert(!(req_valid && req_prefetch && dirResult.hit), "MSHR can not receive prefetch hit req")

  /* ======== Task allocation ======== */
  // Theoretically, data to be released is saved in ReleaseBuffer, so Acquire can be sent as soon as req enters mshr
//  io.tasks.txreq.valid := !state.s_acquire || !state.s_reissue
  io.tasks.txreq.valid := !state.s_acquire || !state.s_reissue.getOrElse(false.B) && gotRetryAck && gotPCrdGrant
  io.tasks.txrsp.valid := !state.s_compack.get && state.w_grantlast
  io.tasks.source_b.valid := !state.s_pprobe || !state.s_rprobe
  val mp_release_valid = !state.s_release && state.w_rprobeacklast && state.w_grantlast &&
        state.w_replResp // release after Grant to L1 sent and replRead returns
  val mp_cbwrdata_valid = !state.s_cbwrdata.getOrElse(true.B) && state.w_releaseack
  val mp_probeack_valid = !state.s_probeack && state.w_pprobeacklast
  val mp_grant_valid = !state.s_refill && state.w_grantlast && state.w_rprobeacklast // [Alias] grant after rprobe done
  val mp_dct_valid = !state.s_dct.getOrElse(true.B) && state.s_probeack
  io.tasks.mainpipe.valid :=
    mp_release_valid  ||
    mp_probeack_valid ||
    mp_grant_valid    ||
    mp_cbwrdata_valid ||
    mp_dct_valid
  // io.tasks.prefetchTrain.foreach(t => t.valid := !state.s_triggerprefetch.getOrElse(true.B))

  // resp and fwdState
  val respCacheState = ParallelPriorityMux(Seq(
    snpToN -> I,
    snpToB -> SC,
    (isSnpOnceX(req_chiOpcode) || isSnpStashX(req_chiOpcode)) ->
      Mux(probeDirty || meta.dirty, UD, metaChi),
    isSnpCleanShared(req_chiOpcode) -> Mux(isT(meta.state), UC, metaChi)
  ))
  val respPassDirty = (meta.dirty || probeDirty) && (
    snpToB ||
    req_chiOpcode === SnpUnique ||
    req_chiOpcode === SnpUniqueStash ||
    req_chiOpcode === SnpCleanShared ||
    req_chiOpcode === SnpCleanInvalid
  )
  val fwdCacheState = Mux(
    isSnpToBFwd(req_chiOpcode),
    SC,
    Mux(
      req_chiOpcode === SnpUniqueFwd,
      Mux(meta.dirty || probeDirty, UD, UC),
      I
    )
  )
  val fwdPassDirty = req_chiOpcode === SnpUniqueFwd && (meta.dirty || probeDirty)

  /*TXRSP for CompAck */
    val txrsp_task = {
      val orsp = io.tasks.txrsp.bits
      orsp := 0.U.asTypeOf(io.tasks.txrsp.bits.cloneType)
      orsp.tgtID := Mux(req.opcode === AcquirePerm && req.param === NtoT, srcid, homenid)
      orsp.srcID := 0.U
      orsp.txnID := dbid
      orsp.dbID := 0.U
      orsp.opcode := CompAck
//      orsp.resperr := 0.U
      orsp.resp  := 0.U
      orsp.fwdState := 0.U
//      orsp.stashhit := 0.U
//      orsp.datapull :=0.U
//      orsp.pcrdtype := 0.U
//      orsp.tracetag := 0.U
    }

  /*TXREQ for Transaction Request*/
  val a_task = {
    val oa = io.tasks.txreq.bits
    oa := 0.U.asTypeOf(io.tasks.txreq.bits.cloneType)
//    oa.qos := Mux(!state.s_reissue, 3.U, 0.U) //TODO increase qos when retry
    oa.tgtID := Mux(!state.s_reissue.getOrElse(false.B), srcid, 0.U)
    oa.srcID := 0.U
    oa.txnID := io.id
    oa.returnNID := 0.U
    oa.stashNID := 0.U
    oa.stashNIDValid := false.B
    /**
      *           TL                  CHI
      *  --------------------------------------------
      *  Get                  |  ReadNotSharedDirty
      *  AcquireBlock NtoB    |  ReadNotSharedDirty
      *  AcquireBlock NtoT    |  ReadUnique
      *  AcquirePerm NtoT     |  MakeUnique
      *  AcquirePerm BtoT     |  ReadUnique
      *  PrefetchRead         |  ReadNotSharedDirty
      *  PrefetchWrite        |  ReadUnique
      */
    oa.opcode := ParallelPriorityMux(Seq(
      (req.opcode === AcquirePerm && req.param === NtoT) -> MakeUnique,
      req_needT                                          -> ReadUnique,
      req_needB /* Default */                            -> ReadNotSharedDirty
    ))
    oa.size := log2Ceil(blockBytes).U
    oa.addr := Cat(req.tag, req.set, 0.U(offsetBits.W)) //TODO 36bit -> 48bit
    oa.ns := false.B
    oa.likelyshared := false.B
    oa.allowRetry := state.s_reissue.getOrElse(false.B)
    oa.order := OrderEncodings.None // TODO: To be confirmed
    oa.pCrdType := Mux(!state.s_reissue.getOrElse(false.B), pcrdtype, 0.U)
    oa.expCompAck := true.B
    oa.memAttr := MemAttr(cacheable = true.B, allocate = true.B, device = false.B, ewa = true.B)
    oa.snpAttr := true.B
    oa.lpID := 0.U
    oa.excl := false.B
    oa.snoopMe := false.B
    oa.traceTag := false.B
    oa
  }

  val b_task = {
    val ob = io.tasks.source_b.bits
    ob.tag := dirResult.tag
    ob.set := dirResult.set
    ob.off := 0.U
    ob.opcode := Probe
    ob.param := Mux(
      !state.s_pprobe,
      Mux(
        snpToB,
        toB,
        Mux(snpToN, toN, toT)
      ),
      Mux(
        req_get && dirResult.hit && meta.state === TRUNK,
        toB,
        toN
      )
    )
    ob.alias.foreach(_ := meta.alias.getOrElse(0.U))
    ob
  }

  val mp_release, mp_probeack, mp_grant, mp_cbwrdata, mp_dct = WireInit(0.U.asTypeOf(new TaskBundle))
  val mp_release_task = {
    mp_release.channel := req.channel
    mp_release.txChannel := CHIChannel.TXREQ
    mp_release.tag := dirResult.tag
    mp_release.set := req.set
    mp_release.off := 0.U
    mp_release.alias.foreach(_ := 0.U)
    mp_release.vaddr.foreach(_ := 0.U)
    mp_release.isKeyword.foreach(_ := false.B)
    // if dirty, we must ReleaseData
    // if accessed, we ReleaseData to keep the data in L3, for future access to be faster
    // [Access] TODO: consider use a counter
    mp_release.opcode := 0.U // use chiOpcode
    mp_release.param := Mux(isT(meta.state), TtoN, BtoN)
    mp_release.size := log2Ceil(blockBytes).U
    mp_release.sourceId := 0.U(sourceIdBits.W)
    mp_release.bufIdx := 0.U(bufIdxBits.W)
    mp_release.needProbeAckData := false.B
    mp_release.mshrTask := true.B
    mp_release.mshrId := io.id
    mp_release.aliasTask.foreach(_ := false.B)
    // mp_release definitely read releaseBuf and refillBuf at ReqArb
    // and it needs to write refillData to DS, so useProbeData is set false according to DS.wdata logic
    mp_release.useProbeData := false.B
    mp_release.mshrRetry := false.B
    mp_release.way := dirResult.way
    mp_release.fromL2pft.foreach(_ := false.B)
    mp_release.needHint.foreach(_ := false.B)
    mp_release.dirty := false.B//meta.dirty && meta.state =/= INVALID || probeDirty
    mp_release.metaWen := false.B
    mp_release.meta := MetaEntry()
    mp_release.tagWen := false.B
    mp_release.dsWen := true.B // write refillData to DS
    mp_release.replTask := true.B
    mp_release.wayMask := 0.U(cacheParams.ways.W)
    mp_release.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_release.mergeA := false.B
    mp_release.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_release.tgtID.get := 0.U
    mp_release.srcID.get := 0.U
    mp_release.txnID.get := io.id
    mp_release.homeNID.get := 0.U
    mp_release.dbID.get := 0.U 
    mp_release.chiOpcode.get := Mux(isT(meta.state) && meta.dirty, WriteBackFull, Evict)
    mp_release.resp.get := 0.U // DontCare
    mp_release.fwdState.get := 0.U // DontCare
    mp_release.pCrdType.get := 0.U // DontCare // TODO: consider retry of WriteBackFull/Evict
    mp_release.retToSrc.get := req.retToSrc.get
    mp_release.expCompAck.get := false.B
    mp_release
  }
  
  val mp_cbwrdata_task = {
    mp_cbwrdata.channel := req.channel
    mp_cbwrdata.txChannel := CHIChannel.TXDAT
    mp_cbwrdata.tag := dirResult.tag
    mp_cbwrdata.set := req.set
    mp_cbwrdata.off := 0.U
    mp_cbwrdata.alias.foreach(_ := 0.U)
    mp_cbwrdata.vaddr.foreach(_ := 0.U)
    mp_cbwrdata.isKeyword.foreach(_ := false.B)
    mp_cbwrdata.opcode := 0.U
    mp_cbwrdata.param := 0.U
    mp_cbwrdata.size := log2Ceil(blockBytes).U
    mp_cbwrdata.sourceId := 0.U(sourceIdBits.W)
    mp_cbwrdata.bufIdx := 0.U(bufIdxBits.W)
    mp_cbwrdata.needProbeAckData := false.B
    mp_cbwrdata.mshrTask := true.B
    mp_cbwrdata.mshrId := io.id
    mp_cbwrdata.aliasTask.foreach(_ := false.B)
    mp_cbwrdata.useProbeData := false.B // DontCare
    mp_cbwrdata.mshrRetry := false.B
    mp_cbwrdata.way := dirResult.way
    mp_cbwrdata.fromL2pft.foreach(_ := false.B)
    mp_cbwrdata.needHint.foreach(_ := false.B)
    mp_cbwrdata.dirty := false.B // DontCare
    mp_cbwrdata.metaWen := false.B
    mp_cbwrdata.meta := MetaEntry()
    mp_cbwrdata.tagWen := false.B
    mp_cbwrdata.dsWen := false.B
    mp_cbwrdata.replTask := false.B
    mp_cbwrdata.wayMask := 0.U
    mp_cbwrdata.reqSource := 0.U
    mp_cbwrdata.mergeA := false.B
    mp_cbwrdata.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_cbwrdata.tgtID.get := srcid
    mp_cbwrdata.srcID.get := 0.U
    mp_cbwrdata.txnID.get := dbid
    mp_cbwrdata.homeNID.get := 0.U
    mp_cbwrdata.dbID.get := 0.U
    mp_cbwrdata.chiOpcode.get := CopyBackWrData
    mp_cbwrdata.resp.get := UD_PD // Only a dirty block 
    mp_cbwrdata.fwdState.get := 0.U
    mp_cbwrdata.pCrdType.get := 0.U // TODO
    mp_cbwrdata.retToSrc.get := req.retToSrc.get // DontCare
    mp_cbwrdata.expCompAck.get := false.B
    mp_cbwrdata
  }

  val mp_probeack_task = {
    mp_probeack.channel := req.channel
    mp_probeack.txChannel := Mux(doRespData, CHIChannel.TXDAT, CHIChannel.TXRSP)
    mp_probeack.tag := req.tag
    mp_probeack.set := req.set
    mp_probeack.off := req.off
    mp_probeack.alias.foreach(_ := 0.U)
    mp_probeack.vaddr.foreach(_ := 0.U)
    mp_probeack.isKeyword.foreach(_ := false.B)
    mp_probeack.opcode := 0.U /* Mux(
      meta.dirty && isT(meta.state) || probeDirty || req.needProbeAckData,
      ProbeAckData,
      ProbeAck
    ) */ // DontCare
    mp_probeack.param := DontCare
    mp_probeack.size := log2Ceil(blockBytes).U
    mp_probeack.sourceId := 0.U(sourceIdBits.W)
    mp_probeack.bufIdx := 0.U(bufIdxBits.W)
    mp_probeack.needProbeAckData := false.B
    mp_probeack.mshrTask := true.B
    mp_probeack.mshrId := io.id
    mp_probeack.aliasTask.foreach(_ := false.B)
    mp_probeack.useProbeData := true.B // write [probeAckData] to DS, if not probed toN // ???
    mp_probeack.mshrRetry := false.B
    mp_probeack.way := dirResult.way
    mp_probeack.fromL2pft.foreach(_ := false.B)
    mp_probeack.needHint.foreach(_ := false.B)
    mp_probeack.dirty := meta.dirty && meta.state =/= INVALID || probeDirty
    mp_probeack.meta := MetaEntry(
      /**
        * Under what circumstances should the dirty bit be cleared:
        * 1. If the snoop belongs to SnpToN
        * 2. If the snoop belongs to SnpToB
        * 3. If the snoop is SnpCleanShared
        * Otherwise, the dirty bit should stay the same as before.
        */
      dirty = !snpToN && !snpToB && req_chiOpcode =/= SnpCleanShared && meta.dirty,
      state = Mux(
        snpToN,
        INVALID,
        Mux(snpToB, BRANCH, meta.state)
      ),
      clients = Fill(clientBits, !probeGotN && !snpToN),
      alias = meta.alias, //[Alias] Keep alias bits unchanged
      prefetch = !snpToN && meta_pft,
      accessed = !snpToN && meta.accessed
    )
    mp_probeack.metaWen := true.B
    mp_probeack.tagWen := false.B
    mp_probeack.dsWen := !snpToN && probeDirty
    mp_probeack.wayMask := 0.U(cacheParams.ways.W)
    mp_probeack.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_probeack.replTask := false.B
    mp_probeack.mergeA := false.B
    mp_probeack.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_probeack.tgtID.get := req.srcID.get
    mp_probeack.srcID.get := 0.U
    mp_probeack.txnID.get := req.txnID.get
    mp_probeack.homeNID.get := 0.U
    // For SnpRespData or SnpRespData, DBID is set to the same value as the TxnID of the snoop.
    // For SnpRespDataFwded or SnpRespDataFwded, DBID is not defined and can be any value.
    mp_probeack.dbID.get := req.txnID.getOrElse(0.U)
    mp_probeack.chiOpcode.get := MuxLookup(Cat(doFwd, doRespData), SnpResp)(Seq(
      Cat(false.B, false.B) -> SnpResp,
      Cat(true.B, false.B)  -> SnpRespFwded,
      Cat(false.B, true.B)  -> SnpRespData, // ignore SnpRespDataPtl for now
      Cat(true.B, true.B)   -> SnpRespDataFwded
    ))
    mp_probeack.resp.get := setPD(respCacheState, respPassDirty)
    mp_probeack.fwdState.get := setPD(fwdCacheState, fwdPassDirty)
    mp_probeack.pCrdType.get := 0.U
    mp_probeack.retToSrc.get := req.retToSrc.get // DontCare
    mp_probeack.expCompAck.get := false.B

    mp_probeack
  }


  val mergeA = RegInit(false.B)
  when(io.aMergeTask.valid) {
    mergeA := true.B
  }.elsewhen(io.alloc.valid) {
    mergeA := false.B
  }
  val mp_grant_task    = {
    mp_grant.channel := req.channel
    mp_grant.tag := req.tag
    mp_grant.set := req.set
    mp_grant.off := req.off
    mp_grant.sourceId := req.sourceId
    mp_grant.alias.foreach(_ := 0.U)
    mp_grant.vaddr.foreach(_ := 0.U)
    mp_grant.isKeyword.foreach(_ := req.isKeyword.getOrElse(false.B))
    mp_grant.opcode := odOpGen(req.opcode)
    mp_grant.param := Mux(
      req_get || req_prefetch,
      0.U, // Get -> AccessAckData
      MuxLookup( // Acquire -> Grant
        req.param,
        req.param)(
        Seq(
          NtoB -> Mux(req_promoteT, toT, toB),
          BtoT -> toT,
          NtoT -> toT
        )
      )
    )
    mp_grant.size := 0.U(msgSizeBits.W)
    mp_grant.bufIdx := 0.U(bufIdxBits.W)
    mp_grant.needProbeAckData := false.B
    mp_grant.mshrTask := true.B
    mp_grant.mshrId := io.id
    mp_grant.way := dirResult.way
    // if it is a Get or Prefetch, then we must keep alias bits unchanged
    // in case future probes gets the wrong alias bits
    val aliasFinal = Mux(req_get || req_prefetch, meta.alias.getOrElse(0.U), req.alias.getOrElse(0.U))
    mp_grant.alias.foreach(_ := aliasFinal)
    mp_grant.aliasTask.foreach(_ := req.aliasTask.getOrElse(false.B))
    // [Alias] write probeData into DS for alias-caused Probe, but not replacement-caused Probe
    // Exception case when multi-core: if aliasTask is AcquireBlock NtoT and self_state is Branch, 
    // and there is a nested Probe toN from L3 (means the data Granted from L3 in the future may be a new data),
    // useProbeData will be set false to use data in RefillBuffer
    mp_grant.useProbeData := (dirResult.hit && req_get) || 
      (req.aliasTask.getOrElse(false.B) && 
        !(dirResult.meta.state === BRANCH && req_needT) 
      )
    mp_grant.dirty := false.B

    mp_grant.meta := MetaEntry(
      dirty = gotDirty || dirResult.hit && (meta.dirty || probeDirty),
      state = Mux(
        req_get,
        Mux( // Get
          dirResult.hit,
          Mux(isT(meta.state), TIP, BRANCH),
          Mux(req_promoteT, TIP, BRANCH)
        ),
        Mux( // Acquire
          req_promoteT || req_needT,
          Mux(req_prefetch, TIP, TRUNK),
          BRANCH
        )
      ),
      clients = Mux(
        req_prefetch,
        Mux(dirResult.hit, meta.clients, Fill(clientBits, false.B)),
        Fill(clientBits, !(req_get && (!dirResult.hit || meta_no_client || probeGotN)))
      ),
      alias = Some(aliasFinal),
      prefetch = req_prefetch || dirResult.hit && meta_pft,
      pfsrc = PfSource.fromMemReqSource(req.reqSource),
      accessed = req_acquire || req_get
    )
    mp_grant.metaWen := true.B
    mp_grant.tagWen := !dirResult.hit
    mp_grant.dsWen := (!dirResult.hit || gotDirty) && gotGrantData || probeDirty && (req_get || req.aliasTask.getOrElse(false.B))
    mp_grant.fromL2pft.foreach(_ := req.fromL2pft.get)
    mp_grant.needHint.foreach(_ := false.B)
    mp_grant.replTask := !dirResult.hit // Get and Alias are hit that does not need replacement
    mp_grant.wayMask := 0.U(cacheParams.ways.W)
    mp_grant.mshrRetry := !state.s_retry
    mp_grant.reqSource := 0.U(MemReqSource.reqSourceBits.W)

    // Add merge grant task for Acquire and late Prefetch
    mp_grant.mergeA := mergeA || io.aMergeTask.valid
    val merge_task_r = RegEnable(io.aMergeTask.bits, 0.U.asTypeOf(new TaskBundle), io.aMergeTask.valid)
    val merge_task = Mux(io.aMergeTask.valid, io.aMergeTask.bits, merge_task_r)
    val merge_task_isKeyword = Mux(io.aMergeTask.valid, io.aMergeTask.bits.isKeyword.getOrElse(false.B), merge_task_r.isKeyword.getOrElse(false.B) )

    mp_grant.aMergeTask.off := merge_task.off
    mp_grant.aMergeTask.alias.foreach(_ := merge_task.alias.getOrElse(0.U))
    mp_grant.aMergeTask.vaddr.foreach(_ := merge_task.vaddr.getOrElse(0.U))
    mp_grant.aMergeTask.isKeyword.foreach(_ := merge_task_isKeyword)
    mp_grant.aMergeTask.opcode := odOpGen(merge_task.opcode)
    mp_grant.aMergeTask.param := MuxLookup( // Acquire -> Grant
      merge_task.param,
      merge_task.param)(
      Seq(
        NtoB -> Mux(req_promoteT, toT, toB),
        BtoT -> toT,
        NtoT -> toT
      )
    )
    mp_grant.aMergeTask.sourceId := merge_task.sourceId
    mp_grant.aMergeTask.meta := MetaEntry(
      dirty = gotDirty || dirResult.hit && (meta.dirty || probeDirty),
      state = Mux( // Acquire
        req_promoteT || needT(merge_task.opcode, merge_task.param),
        TRUNK,
        BRANCH
      ),
      clients = Fill(clientBits, true.B),
      alias = Some(merge_task.alias.getOrElse(0.U)),
      prefetch = false.B,
      accessed = true.B
    )

    mp_grant
  }

  val mp_dct_task = {
    mp_dct.channel := req.channel
    mp_dct.txChannel := CHIChannel.TXDAT
    mp_dct.tag := req.tag
    mp_dct.set := req.set
    mp_dct.off := req.off
    mp_dct.alias.foreach(_ := 0.U)
    mp_dct.vaddr.foreach(_ := 0.U)
    mp_dct.isKeyword.foreach(_ := 0.U)
    mp_dct.opcode := 0.U // DontCare
    mp_dct.param := 0.U // DontCare
    mp_dct.size := log2Ceil(blockBytes).U
    mp_dct.sourceId := 0.U(sourceIdBits.W)
    mp_dct.bufIdx := 0.U(sourceIdBits.W)
    mp_dct.needProbeAckData := false.B
    mp_dct.mshrTask := true.B
    mp_dct.mshrId := io.id
    mp_dct.aliasTask.foreach(_ := false.B)
    mp_dct.useProbeData := true.B
    mp_dct.mshrRetry := false.B
    mp_dct.way := dirResult.way
    mp_dct.fromL2pft.foreach(_ := false.B)
    mp_dct.needHint.foreach(_ := false.B)
    mp_dct.dirty := meta.dirty && meta.state =/= INVALID || probeDirty
    mp_dct.meta := MetaEntry()
    mp_dct.metaWen := false.B // meta is written by SnpResp[Data]Fwded, not CompData
    mp_dct.tagWen := false.B
    mp_dct.dsWen := false.B
    mp_dct.wayMask := 0.U(cacheParams.ways.W)
    mp_dct.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_dct.replTask := false.B
    mp_dct.mergeA := false.B
    mp_dct.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_dct.tgtID.get := req.fwdNID.get
    mp_dct.srcID.get := 0.U
    mp_dct.txnID.get := req.fwdTxnID.get
    mp_dct.homeNID.get := req.srcID.get
    mp_dct.chiOpcode.get := CompData
    mp_dct.resp.get := setPD(fwdCacheState, fwdPassDirty)
    mp_dct.fwdState.get := 0.U
    mp_dct.pCrdType.get := 0.U // DontCare
    mp_dct.retToSrc.get := false.B // DontCare
    mp_dct.expCompAck.get := false.B // DontCare

    mp_dct
  }
  io.tasks.mainpipe.bits := ParallelPriorityMux(
    Seq(
      mp_grant_valid         -> mp_grant,
      mp_release_valid       -> mp_release,
      mp_cbwrdata_valid      -> mp_cbwrdata,
      mp_probeack_valid      -> mp_probeack,
      mp_dct_valid           -> mp_dct
    )
  )
  io.tasks.mainpipe.bits.reqSource := req.reqSource
  io.tasks.mainpipe.bits.isKeyword.foreach(_:= req.isKeyword.getOrElse(false.B))
  // io.tasks.prefetchTrain.foreach {
  //   train =>
  //     train.bits.tag := req.tag
  //     train.bits.set := req.set
  //     train.bits.needT := req_needT
  //     train.bits.source := req.source
  // }

  /* ======== Task update ======== */
  when (io.tasks.txreq.fire) {
    state.s_acquire := true.B 
    state.s_reissue.get := true.B 
  }
  when (io.tasks.txrsp.fire) {
    state.s_compack.get := true.B
  }
  when (io.tasks.source_b.fire) {
    state.s_pprobe := true.B
    state.s_rprobe := true.B
  }
  when (io.tasks.mainpipe.ready) {
    when (mp_grant_valid) {
      state.s_refill := true.B
      state.s_retry := true.B
    }.elsewhen (mp_release_valid) {
      state.s_release := true.B
      state.s_cbwrdata.get := !(isT(meta.state) && meta.dirty)
      // meta.state := INVALID
    }.elsewhen (mp_cbwrdata_valid) {
      state.s_cbwrdata.get := true.B
      meta.state := INVALID
    }.elsewhen (mp_probeack_valid) {
      state.s_probeack := true.B
    }.elsewhen (mp_dct_valid) {
      state.s_dct.get := true.B
    }
  }

  /*                      Handling response

      TL                    CHI             CHI Resp              CHI channel 
  -----------------------------------------------------------------------------
  AcquireBlock       |  ReadNotShareDirty |  CompData           |    rxdat 
  AcquirePerm(miss)  |  ReadUnique        |  CompData           |    rxdat 
  AcquirePerm(hit B) |  MakeUnique        |  Comp               |    rxrsp <- TODO
  Get                |  ReadClean         |  CompData           |    rxdat 
  Hint               |  ReadNotShareDirty |  CompData           |    rxdat 
  Release            |  WriteBackFull     |  CompDBID           |    rxrsp 
                     |  *                 |  RetryAck+PCrdGrant |    rxrsp <-
  */
  val c_resp = io.resps.sinkC
  val rxrsp = io.resps.rxrsp
  val rxdat = io.resps.rxdat
  // Probe core response
  when (c_resp.valid) {
    when (c_resp.bits.opcode === ProbeAck || c_resp.bits.opcode === ProbeAckData) {
      state.w_rprobeackfirst := true.B
      state.w_rprobeacklast := state.w_rprobeacklast || c_resp.bits.last
      state.w_pprobeackfirst := true.B
      state.w_pprobeacklast := state.w_pprobeacklast || c_resp.bits.last
      state.w_pprobeack := state.w_pprobeack || req.off === 0.U || c_resp.bits.last
    }
    when (c_resp.bits.opcode === ProbeAckData) {
      probeDirty := true.B
    }
    when (isToN(c_resp.bits.param)) {
      probeGotN := true.B
    }
  }

  val rxdatIsU = rxdat.bits.resp.get === UC
  val rxdatIsU_PD = rxdat.bits.resp.get === UC_PD

  val rxrspIsU = rxrsp.bits.resp.get === UC

  // RXDAT
  when (rxdat.valid) {
    when (rxdat.bits.chiOpcode.get === CompData) {
      state.w_grantfirst := true.B
      state.w_grantlast := rxdat.bits.last
      state.w_grant := req.off === 0.U || rxdat.bits.last  // TODO? why offset?
      gotT := rxdatIsU || rxdatIsU_PD
      gotDirty := gotDirty || rxdatIsU_PD
      gotGrantData := true.B
      dbid := rxdat.bits.dbID.getOrElse(0.U)
      homenid := rxdat.bits.homeNID.getOrElse(0.U)
    }
  }

  // RXRSP for dataless
  when (rxrsp.valid) {
    when (rxrsp.bits.chiOpcode.get === Comp) {
      // There is a pending Read transaction waiting for the Comp resp
      when (!state.w_grant) {
        state.w_grantfirst := true.B
        state.w_grantlast := rxrsp.bits.last
        state.w_grant := req.off === 0.U || rxrsp.bits.last  // TODO? why offset?
        gotT := rxrspIsU
        gotDirty := false.B
      }

      // There is a pending Evict transaction waiting for the Comp resp
      when (!state.w_releaseack) {
        state.w_releaseack := true.B
      }

      // Comp for Dataless transaction that include CompAck
      // Use DBID as a identifier for CompAck
      dbid := rxrsp.bits.dbID.getOrElse(0.U)
      srcid := rxrsp.bits.srcID.getOrElse(0.U)
    }
    when(rxrsp.bits.chiOpcode.get === CompDBIDResp) {
      state.w_releaseack := true.B
      srcid := rxrsp.bits.srcID.getOrElse(0.U)
      dbid := rxrsp.bits.dbID.getOrElse(0.U)
    }
    when(rxrsp.bits.chiOpcode.get === RetryAck) {
      srcid := rxrsp.bits.srcID.getOrElse(0.U)
      pcrdtype := rxrsp.bits.pCrdType.getOrElse(0.U)
      gotRetryAck := true.B
    }
    when((rxrsp.bits.chiOpcode.get === PCrdGrant) && !gotReissued) {
      state.s_reissue.get := false.B
      gotPCrdGrant := true.B
      gotReissued := true.B
    }
  }
 // when there is this type of pCredit in pCam -> reissue
  when (io.pCamPri) {
    state.s_reissue.get := false.B
    gotPCrdGrant := true.B
    gotReissued := true.B
  }

  // replay
  val replResp = io.replResp.bits
  when (io.replResp.valid && replResp.retry) {
    state.s_refill := false.B
    state.s_retry := false.B
    dirResult.way := replResp.way
  }
  when (io.replResp.valid && !replResp.retry) {
    state.w_replResp := true.B

    // update meta (no need to update hit/set/error/replacerInfo of dirResult)
    dirResult.tag := replResp.tag
    dirResult.way := replResp.way
    dirResult.meta := replResp.meta

    // replacer choosing:
    // 1. an invalid way, release no longer needed
    // 2. the same way, just release as normal (only now we set s_release)
    // 3. differet way, we need to update meta and release that way
    // if meta has client, rprobe client
    when (replResp.meta.state =/= INVALID) {
      // set release flags
      state.s_release := false.B
      state.w_releaseack := false.B
      // rprobe clients if any
      when (replResp.meta.clients.orR) {
        state.s_rprobe := false.B
        state.w_rprobeackfirst := false.B
        state.w_rprobeacklast := false.B
      }
    }
  }

  when (req_valid) {
    timer := timer + 1.U
  }

  val no_schedule = state.s_refill && state.s_probeack && state.s_release &&
    state.s_compack.getOrElse(true.B) &&
    state.s_cbwrdata.getOrElse(true.B) &&
    state.s_reissue.getOrElse(true.B) &&
    state.s_dct.getOrElse(true.B)
  val no_wait = state.w_rprobeacklast && state.w_pprobeacklast && state.w_grantlast && state.w_releaseack && state.w_replResp
  val will_free = no_schedule && no_wait
  when (will_free && req_valid) {
    req_valid := false.B
    timer := 0.U
  }

  // when grant not received, B can nest A
  val nestB = !state.w_grantfirst

  // alias: should protect meta from being accessed or occupied
  val releaseNotSent = !state.s_release
  io.status.valid := req_valid
  io.status.bits.channel := req.channel
  io.status.bits.txChannel := req.txChannel // TODO
  io.status.bits.set := req.set
  io.status.bits.reqTag := req.tag
  io.status.bits.metaTag := dirResult.tag
  io.status.bits.needsRepl := releaseNotSent
  // wait for resps, high as valid
  io.status.bits.w_c_resp := !state.w_rprobeacklast || !state.w_pprobeacklast || !state.w_pprobeack
  io.status.bits.w_d_resp := !state.w_grantlast || !state.w_grant || !state.w_releaseack
  io.status.bits.will_free := will_free
  io.status.bits.is_miss := !dirResult.hit
  io.status.bits.is_prefetch := req_prefetch
  io.status.bits.reqSource := req.reqSource

  io.msInfo.valid := req_valid
  io.msInfo.bits.set := req.set
  io.msInfo.bits.way := dirResult.way
  io.msInfo.bits.reqTag := req.tag
  io.msInfo.bits.needRelease := !state.w_releaseack
  // if releaseTask is already in mainpipe_s1/s2, while a refillTask in mainpipe_s3, the refill should also be blocked and retry
  io.msInfo.bits.blockRefill := releaseNotSent || RegNext(releaseNotSent,false.B) || RegNext(RegNext(releaseNotSent,false.B),false.B)
  io.msInfo.bits.dirHit := dirResult.hit
  io.msInfo.bits.metaTag := dirResult.tag
  io.msInfo.bits.willFree := will_free
  io.msInfo.bits.nestB := nestB
  io.msInfo.bits.isAcqOrPrefetch := req_acquire || req_prefetch
  io.msInfo.bits.isPrefetch := req_prefetch
  io.msInfo.bits.s_refill := state.s_refill
  io.msInfo.bits.param := req.param
  io.msInfo.bits.mergeA := mergeA
  io.msInfo.bits.w_releaseack := state.w_releaseack

  assert(!(c_resp.valid && !io.status.bits.w_c_resp))
  assert(!(rxrsp.valid && !io.status.bits.w_d_resp))

  /* ======== Handling Nested C ======== */
  // for A miss, only when replResp do we finally choose a way, allowing nested C
  // for A-alias, always allowing nested C (state.w_replResp === true.B)
  val nestedwb_match = req_valid && meta.state =/= INVALID &&
    dirResult.set === io.nestedwb.set &&
    dirResult.tag === io.nestedwb.tag &&
    state.w_replResp

  when (nestedwb_match) {
    when (io.nestedwb.c_set_dirty) {
      meta.dirty := true.B
    }
  }
  // let nested C write ReleaseData to the MSHRBuffer entry of this MSHR id
  // This is the VALID signal for releaseBuf.io.w(2)
  io.nestedwbData := nestedwb_match && io.nestedwb.c_set_dirty

  dontTouch(state)


  // 
  // deadlock check
  // 
  val validCnt = RegInit(0.U(64.W))
  when(io.alloc.valid) {
    validCnt := 0.U
  }

  when(req_valid) {
    validCnt := validCnt + 1.U
  }

  val mshrAddr = Cat(req.tag, req.set, 0.U(6.W)) // TODO: consider multibank
  val VALID_CNT_MAX = 30000.U
  assert(validCnt <= VALID_CNT_MAX, "validCnt > 5000, may be there is a deadlock! addr => 0x%x req_opcode => %d channel => 0b%b", mshrAddr, req.opcode, req.channel)


  /* ======== Performance counters ======== */
  // time stamp
  // if (cacheParams.enablePerf) {
    val acquire_ts = RegEnable(timer, false.B, io.tasks.txreq.fire)
    val probe_ts = RegEnable(timer, false.B, io.tasks.source_b.fire)
    val release_ts = RegEnable(timer, false.B, !mp_grant_valid && mp_release_valid && io.tasks.mainpipe.ready)
    val acquire_period = IO(Output(UInt(64.W)))
    val probe_period = IO(Output(UInt(64.W)))
    val release_period = IO(Output(UInt(64.W)))
    acquire_period := timer - acquire_ts
    probe_period := timer - probe_ts
    release_period := timer - release_ts
  // }
}
