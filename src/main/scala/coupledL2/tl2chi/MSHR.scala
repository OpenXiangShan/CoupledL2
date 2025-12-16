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
import utility.{MemReqSource, ParallelLookUp, ParallelMux, ParallelPriorityMux}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters
import coupledL2.prefetch.{PfSource, PrefetchTrain}
import coupledL2.tl2chi.CHICohStates._
import coupledL2.tl2chi.CHIChannel
import coupledL2.tl2chi.RespErrEncodings._
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
}

class MSHR(implicit p: Parameters) extends TL2CHIL2Module with HasCHIOpcodes {
  val io = IO(new Bundle() {
    val id = Input(UInt(mshrBits.W))
    val status = ValidIO(new MSHRStatus)
    val statAlloc = ValidIO(new MSHRAllocStatus)
    val msInfo = ValidIO(new MSHRInfo)
    val alloc = Flipped(ValidIO(new MSHRRequest))
    val tasks = new MSHRTasks()
    val resps = new MSHRResps()
    val nestedwb = Input(new NestedWriteback)
    val nestedwbData = Output(Bool())
    val aMergeTask = Flipped(ValidIO(new TaskBundle))
    val replResp = Flipped(ValidIO(new ReplacerResult))
    val pCrd = new PCrdQueryBundle
  })

  require (chiOpt.isDefined)

  val gotT = RegInit(false.B) // L3 might return T even though L2 wants B
  val gotDirty = RegInit(false.B)
  val gotGrantData = RegInit(false.B)
  val probeDirty = RegInit(false.B)
  val releaseDirty = RegInit(false.B)
  val timer = RegInit(0.U(64.W)) // for performance analysis
  val beatCnt = RegInit(0.U(log2Ceil(beatSize).W))

  val req_valid = RegInit(false.B)
  val req       = RegInit(0.U.asTypeOf(new TaskBundle()))
  val dirResult = RegInit(0.U.asTypeOf(new DirResult()))
  val meta      = dirResult.meta
  val initState = Wire(new FSMState())
  initState.elements.foreach(_._2 := true.B)
  val state     = RegInit(new FSMState(), initState)

  val req_released_chiOpcode = RegInit(0.U.asTypeOf(UInt(OPCODE_WIDTH.W)))

  assert(!(req_valid && dirResult.hit && !isT(meta.state) && meta.dirty),
    "directory valid read with dirty under non-T state")

  /**
    * When all the ways are occupied with some mshr, other mshrs with the same set may retry to find a way to replace
    * over and over again, which may block the entrance of main pipe and lead to potential deadlock. To resolve the
    * problem, we allow mshr to retry immediately for 3 times (backoffThreshold). If it still fails to find a way, the
    * mshr must back off for a period of time (backoffCycles) to yield the opportunity to access main pipe.
    */
  val backoffThreshold = 3
  val backoffCycles = 20
  val retryTimes = RegInit(0.U(log2Up(backoffThreshold).W))
  val backoffTimer = RegInit(0.U(log2Up(backoffCycles).W))

  val tgtid_rcompack = Reg(UInt(NODEID_WIDTH.W)) // TgtID in CompAck of read / dataless transactions
  val txnid_rcompack = Reg(UInt(TXNID_WIDTH.W)) // TxnID in CompAck of read / dataless transactions
  val tgtid_wcompack = Reg(UInt(NODEID_WIDTH.W)) // TgtID in WriteData / CompAck of write transactions
  val txnid_wcompack = Reg(UInt(TXNID_WIDTH.W)) // TxnID in WriteData / CompAck of write transactions
  val srcid_retryack = Reg(UInt(NODEID_WIDTH.W)) // SrcID in RetryAck, only used for protocol retry
  
  val pcrdtype = RegInit(0.U(PCRDTYPE_WIDTH.W))
  val gotRetryAck = RegInit(false.B)
  val gotPCrdGrant = RegInit(false.B)

  val tagErr = RegInit(false.B) // L2 Tag Error
  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)
  val cbWrDataTraceTag = RegInit(false.B)
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

  io.pCrd.query.valid := gotRetryAck && !gotPCrdGrant
  io.pCrd.query.bits.pCrdType := pcrdtype
  io.pCrd.query.bits.srcID := srcid_retryack

  /* Allocation */
  when (io.alloc.valid) {
    req_valid := true.B
    state     := io.alloc.bits.state
    dirResult := io.alloc.bits.dirResult
    req       := io.alloc.bits.task
    gotT        := false.B
    gotDirty    := false.B
    gotGrantData := false.B
    probeDirty  := false.B
    releaseDirty := false.B
    timer       := 1.U
    beatCnt     := 0.U

    gotRetryAck := false.B
    gotPCrdGrant := false.B

    pcrdtype := 0.U
    tagErr := io.alloc.bits.dirResult.hit && (io.alloc.bits.dirResult.meta.tagErr || io.alloc.bits.dirResult.error)
    denied := false.B
    corrupt := false.B
    cbWrDataTraceTag := false.B

    retryTimes := 0.U
    backoffTimer := 0.U
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

  val req_mayRepl = req_acquire || req_get || req_prefetch

  val req_chiOpcode = req.chiOpcode.get

  val snpToN = isSnpToN(req_chiOpcode)
  val snpToB = isSnpToB(req_chiOpcode)

  val req_cboClean = req.fromA && req.opcode === CBOClean
  val req_cboFlush = req.fromA && req.opcode === CBOFlush
  val req_cboInval = req.fromA && req.opcode === CBOInval

  val cmo_cbo = req_cboClean || req_cboFlush || req_cboInval

  // *NOTICE: WriteBack/WriteClean(s) with nested snoops that passed dirty were not considered as
  //          a nested hit here, which would no longer pass latest data to lower tier memories.
  val hitDirty = dirResult.hit && meta.dirty
  val hitWriteBack = req.snpHitRelease && req.snpHitReleaseWithData && req.snpHitReleaseMeta.dirty && req.snpHitReleaseToInval
  val hitWriteClean = req.snpHitRelease && req.snpHitReleaseWithData && req.snpHitReleaseMeta.dirty && req.snpHitReleaseToClean
  val hitWriteEvict = req.snpHitRelease && req.snpHitReleaseWithData && !req.snpHitReleaseMeta.dirty

  val hitWriteX = hitWriteBack || hitWriteClean || hitWriteEvict
  val hitWriteDirty = hitWriteBack || hitWriteClean
  val hitDirtyOrWriteDirty = hitDirty || hitWriteDirty

  val releaseToClean = req_cboClean

  /**
    * About which snoop should echo SnpRespData[Fwded] instead of SnpResp[Fwded]:
    * 1. When the snooped block is dirty, always echo SnpRespData[Fwded], except for SnpMakeInvalid*, SnpStash*,
    *    SnpOnceFwd, and SnpUniqueFwd.
    * 2. When the snoop opcode is SnpCleanFwd, SnpNotSharedDirtyFwd or SnpSharedFwd, always echo SnpRespDataFwded
    *    if RetToSrc = 1 as long as the snooped block is valid.
    *    if L2 tagErr, not forward data
    * 3. When the snoop opcode is non-forwarding non-stashing snoop, echo SnpRespData if RetToSrc = 1 as long as the
    *    cache line is Shared Clean and the snoopee retains a copy of the cache line.
    */
  val doRespData_dirty = hitDirtyOrWriteDirty && (
    req_chiOpcode === SnpOnce ||
    snpToB ||
    req_chiOpcode === SnpUnique ||
    req_chiOpcode === SnpUniqueStash ||
    req_chiOpcode === SnpCleanShared ||
    req_chiOpcode === SnpCleanInvalid ||
    req_chiOpcode === SnpPreferUnique
  )
  // *NOTICE: Careful on future implementation of adding 'isSnpToNFwd' into condition
  //          'doRespData_retToSrc_fwd'. For now, 'isSnpToNFwd' only covers SnpUniqueFwd,
  //          which should never return data to Home Node except No Fwd to Requester.
  //          No Fwds on DCT are not implemented because Fwded responses are always perferred.
  val doRespData_retToSrc_fwd = req.retToSrc.get &&
    (isSnpToBFwd(req_chiOpcode) /*|| isSnpToNFwd(req_chiOpcode)*/)
  val doRespData_retToSrc_nonFwd = req.retToSrc.get && (
    dirResult.hit && meta.state === BRANCH &&
      (isSnpToBNonFwd(req_chiOpcode) || isSnpToNNonFwd(req_chiOpcode) || isSnpOnce(req_chiOpcode)))
  // doRespData_once includes 
  //  1. SnpOnceFwd : UD -> I     (nesting WriteBack)
  //  2. SnpOnceFwd : UD -> SC    (nesting WriteClean)
  //  3. SnpOnce    : UC -> UC    (non-nesting)
  //  4. SnpOnce    : UC -> I     (nesting WriteEvict)
  val doRespData_once = (hitWriteBack || hitWriteClean) &&
      isSnpOnceFwd(req_chiOpcode) ||
    (dirResult.hit && !meta.dirty && meta.state =/= BRANCH || hitWriteEvict) &&
      isSnpOnce(req_chiOpcode)
  val doRespData = (doRespData_dirty || doRespData_retToSrc_fwd || doRespData_retToSrc_nonFwd || doRespData_once) && !tagErr

  dontTouch(doRespData_dirty)
  dontTouch(doRespData_retToSrc_fwd)
  dontTouch(doRespData_retToSrc_nonFwd)

  // *NOTICE: SnpUniqueStash was included in condition 'doRespData_retToSrc_nonFwd', while
  //          the 'retToSrc' of SnpUniqueStash must be bound to 0, and whether responding
  //          SnpRespData or SnpResp was not determined by 'retToSrc'.
  //          the 'retToSrc' of SnpQuery must be bound to 0
  assert(!(req_valid && req_chiOpcode === SnpUniqueStash && req.retToSrc.get),
    "specification failure: received SnpUniqueStash with RetToSrc = 1")
  assert(!(req_valid && isSnpQuery(req_chiOpcode) && req.retToSrc.get),
    "specification failure: received SnpQuery with RetToSrc = 1")

  /**
    * About which snoop should echo SnpResp[Data]Fwded instead of SnpResp[Data]:
    * 1. When the snoop opcode is Snp*Fwd and the snooped block is valid.
    */
  val doFwd = isSnpXFwd(req_chiOpcode) && dirResult.hit
  val doFwdHitRelease = isSnpXFwd(req_chiOpcode) && hitWriteX

  val gotUD = meta.dirty //TC/TTC -> UD
  val promoteT_normal =  dirResult.hit && meta_no_client && meta.state === TIP
  val promoteT_L3     = !dirResult.hit && gotT
  val promoteT_alias  =  dirResult.hit && req.aliasTask.getOrElse(false.B) && (meta.state === TRUNK || meta.state === TIP)
  // under above circumstances, we grant T to L1 even if it wants B
  val req_promoteT = (req_acquire || req_get || req_prefetch) && (promoteT_normal || promoteT_L3 || promoteT_alias)

  assert(!(req_valid && req_prefetch && dirResult.hit), "MSHR can not receive prefetch hit req")

  /* ======== Task allocation ======== */
  // The first Release with AllowRetry = 1 is sent to main pipe, because the task needs to write DS.
  // The second Release with AllowRetry = 0 is sent to TXREQ directly, because DS is already written.
  val release_valid1_cmo = !state.s_release && state.w_rprobeacklast && cmo_cbo
  val release_valid1 = !state.s_release && state.w_rprobeacklast && state.w_grantlast && state.w_grant && state.w_replResp || release_valid1_cmo
  val release_valid2 = !state.s_reissue.getOrElse(false.B) && !state.w_releaseack && gotRetryAck && gotPCrdGrant
  // Theoretically, data to be released is saved in ReleaseBuffer, so Acquire can be sent as soon as req enters mshr
  // For cmo_clean/flush, dirty data should be released downward first, then Clean req can be sent
  io.tasks.txreq.valid := !state.s_acquire && !(cmo_cbo && (!state.w_rprobeacklast || !state.w_releaseack || !state.s_cmometaw || !state.s_cbwrdata.get)) || 
                          !state.s_reissue.getOrElse(false.B) && !state.w_grant && gotRetryAck && gotPCrdGrant ||
                          release_valid2
  val rcompack_valid = !state.s_rcompack.get && state.w_grant &&
    // For issue B, CompAck must not be sent until all transfers of read data have been received.
    // For issue C and afterwards, CompAck is allowed to be sent after at least one CompData packet is received.
    afterIssueCOrElse(state.w_grantfirst, state.w_grantlast)
  val wcompack_valid = !state.s_wcompack.get && state.s_rcompack.get // wcompack can only be sent after rcompack
  io.tasks.txrsp.valid := rcompack_valid || wcompack_valid
  io.tasks.source_b.valid := !state.s_pprobe || !state.s_rprobe
  val mp_release_valid = release_valid1
  val mp_cbwrdata_valid = !state.s_cbwrdata.getOrElse(true.B) && state.w_releaseack
  val mp_probeack_valid = !state.s_probeack && state.w_pprobeacklast
  val pending_grant_valid = (!state.s_refill || (!state.s_cmoresp && state.w_releaseack && state.s_cbwrdata.get)) &&
    state.w_grantlast && state.w_grant && state.w_rprobeacklast
  val mp_grant_valid = pending_grant_valid && (retryTimes < backoffThreshold.U || backoffTimer === backoffCycles.U)
  val mp_dct_valid = !state.s_dct.getOrElse(true.B) && state.s_probeack
  val mp_cmometaw_valid = !state.s_cmometaw
  io.tasks.mainpipe.valid :=
    mp_release_valid  ||
    mp_probeack_valid ||
    mp_grant_valid    ||
    mp_cbwrdata_valid ||
    mp_dct_valid      ||
    mp_cmometaw_valid
  // io.tasks.prefetchTrain.foreach(t => t.valid := !state.s_triggerprefetch.getOrElse(true.B))

  assert(state.s_refill || state.s_cmoresp, "refill not allowed on CMO operation")

  when (
    pending_grant_valid &&
    backoffTimer < backoffCycles.U &&
    retryTimes === backoffThreshold.U
  ) {
    backoffTimer := backoffTimer + 1.U
  }

  // resp and fwdState
  // *NOTICE: Snp*Fwd would enter MSHR on directory missing
  val respCacheState = ParallelPriorityMux(Seq(
    (snpToN || tagErr) -> I,
    snpToB -> Mux(req.snpHitReleaseToInval, I, SC),
    isSnpOnceX(req_chiOpcode) ->
      Mux(req.snpHitReleaseToInval, I, Mux(
        req.snpHitReleaseToClean,
        Mux(req.snpHitReleaseMeta.dirty, SC, metaChi),
        Mux(meta.dirty, UD, metaChi)
      )),
    (isSnpStashX(req_chiOpcode) || isSnpQuery(req_chiOpcode)) ->
      Mux(meta.dirty, UD, metaChi),
    isSnpCleanShared(req_chiOpcode) -> 
      Mux(isT(meta.state), UC, metaChi)
  ))
  val respPassDirty = hitDirtyOrWriteDirty && !tagErr && (
    snpToB ||
    req_chiOpcode === SnpUnique ||
    req_chiOpcode === SnpUniqueStash ||
    req_chiOpcode === SnpCleanShared ||
    req_chiOpcode === SnpCleanInvalid ||
    req_chiOpcode === SnpPreferUnique
  ) || hitWriteDirty && isSnpOnceFwd(req_chiOpcode)
  val fwdCacheState = Mux(tagErr, I, Mux(
    isSnpToBFwd(req_chiOpcode),
    SC,
    Mux(isSnpToNFwd(req_chiOpcode), UC /*UC_UD*/, I)
  ))
  val fwdPassDirty = isSnpToNFwd(req_chiOpcode) && hitDirtyOrWriteDirty && !tagErr

  /*TXRSP for CompAck */
  val orsp = io.tasks.txrsp.bits
  orsp := 0.U.asTypeOf(io.tasks.txrsp.bits.cloneType)
  orsp.tgtID := Mux(wcompack_valid, tgtid_wcompack, tgtid_rcompack)
  orsp.srcID := 0.U
  orsp.txnID := Mux(wcompack_valid, txnid_wcompack, txnid_rcompack)
  orsp.dbID := 0.U
  orsp.opcode := CompAck
  orsp.resp  := 0.U
  orsp.fwdState := 0.U
  orsp.traceTag := req.traceTag.get

  /*TXREQ for Transaction Request*/
  // *NOTICE: By the time of issuing Write Back (WriteBackFull or Evict), the directory
  //          was already updated by replacing, so we should never check directory hit
  //          on replacer-issued WriteBackFull condition.
  val isWriteCleanFull = req_cboClean
  val isWriteBackFull = !req_cboClean && !req_cboInval && isT(meta.state) && meta.dirty
  val isWriteEvictFull = false.B
  val isWriteEvictOrEvict = afterIssueEbOrElse(
    !req_cboFlush && !req_cboInval && !isWriteCleanFull && !isWriteBackFull && !isWriteEvictFull,
    false.B
  )
  val isEvict = !isWriteCleanFull && !isWriteBackFull && !isWriteEvictFull && !isWriteEvictOrEvict
  val a_task = {
    val oa = io.tasks.txreq.bits
    oa := 0.U.asTypeOf(io.tasks.txreq.bits.cloneType)
    oa.qos := Fill(QOS_WIDTH, 1.U(1.W)) - 1.U // TODO
    oa.tgtID := Mux(!state.s_reissue.getOrElse(false.B), srcid_retryack, 0.U)
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
      *  AcquirePerm BtoT     |  MakeUnique
      *  PrefetchRead         |  ReadNotSharedDirty
      *  PrefetchWrite        |  ReadUnique
      */
    oa.opcode := ParallelPriorityMux(Seq(
      release_valid2                                     -> req_released_chiOpcode,
      req_cboClean                                       -> CleanShared,
      req_cboFlush                                       -> CleanInvalid,
      req_cboInval                                       -> MakeInvalid,
      req_acquirePerm                                    -> MakeUnique,
      req_needT                                          -> ReadUnique,
      req_needB /* Default */                            -> ReadNotSharedDirty
    ))
    oa.size := log2Ceil(blockBytes).U
    oa.addr := Cat(Mux(release_valid2, dirResult.tag, req.tag), req.set, 0.U(offsetBits.W))
    oa.ns := enableNS.B
    // set 'LikelyShared' to 1 here when:
    //  - WriteEvictOrEvict (on retry) with SC state
    oa.likelyshared := Mux(
      release_valid2,
      afterIssueEbOrElse(req_released_chiOpcode === WriteEvictOrEvict && meta.state === BRANCH, false.B),
      false.B
    )
    oa.allowRetry := state.s_reissue.getOrElse(false.B)
    oa.order := OrderEncodings.None
    oa.pCrdType := Mux(!state.s_reissue.getOrElse(false.B), pcrdtype, 0.U)
    // set 'ExpCompAck' to 1 here when:
    //  - MakeUnique
    //  - ReadUnique, ReadNotSharedDirty
    //  - WriteEvictOrEvict (on retry)
    oa.expCompAck := Mux(
      release_valid2,
      afterIssueEbOrElse(req_released_chiOpcode === WriteEvictOrEvict, false.B),
      !cmo_cbo
    )
    oa.memAttr := MemAttr(
      cacheable = true.B,
      allocate = !release_valid2 || !isEvict && !cmo_cbo,
      device = false.B,
      ewa = true.B
    )
    oa.snpAttr := true.B
    oa.lpIDWithPadding := 0.U
    oa.excl := false.B
    oa.snoopMe := false.B
    oa.traceTag := false.B
    oa.mpam.foreach(_ := MPAM(oa.ns))
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
        // *NOTICE: CBOClean derives upper Probe toB for now.
        (req_get || req_cboClean) && dirResult.hit && meta.state === TRUNK,
        toB,
        toN
      )
    )
    ob.alias.foreach(_ := meta.alias.getOrElse(0.U))
    ob
  }

  val mp_release, mp_probeack, mp_grant, mp_cbwrdata, mp_dct, mp_cmometaw = WireInit(0.U.asTypeOf(new TaskBundle))
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
    // * but on CMO requests, data were not fetched by the refill procedure, but written to releaseBuf
    //   by mainpipe, so useProbeData is set to true to write data from releaseBuf into DS
    mp_release.useProbeData := false.B
    mp_release.readProbeDataDown := false.B
    mp_release.mshrRetry := false.B
    mp_release.way := dirResult.way
    mp_release.fromL2pft.foreach(_ := false.B)
    mp_release.needHint.foreach(_ := false.B)
    mp_release.dirty := false.B//meta.dirty && meta.state =/= INVALID || probeDirty
    mp_release.metaWen := false.B
    mp_release.meta := MetaEntry()
    mp_release.tagWen := false.B
    // write refillData to DS on refill, write releaseData to DS on CMO
    // When refillBuf has no valid data, it should be avoided to write data of RefillBuf to DS which is MCP2
    mp_release.dsWen := !req_acquirePerm
    mp_release.replTask := true.B
    mp_release.cmoTask := cmo_cbo
    mp_release.wayMask := 0.U(cacheParams.ways.W)
    mp_release.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_release.mergeA := false.B
    mp_release.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    mp_release.denied := denied
    mp_release.corrupt := corrupt

    // CHI
    // *NOTICE: See 'isWriteBackFull' above.
    mp_release.tgtID.get := 0.U
    mp_release.srcID.get := 0.U
    mp_release.txnID.get := io.id
    mp_release.homeNID.get := 0.U
    mp_release.dbID.get := 0.U 
    mp_release.chiOpcode.get := ParallelPriorityMux(Seq(
      isWriteBackFull       -> WriteBackFull,
      isWriteEvictFull      -> WriteEvictFull,
      isWriteEvictOrEvict   -> afterIssueEbOrElse(WriteEvictOrEvict, DontCare),
      isEvict /* Default */ -> Evict
    ))
    mp_release.resp.get := 0.U // DontCare
    mp_release.fwdState.get := 0.U // DontCare
    mp_release.pCrdType.get := 0.U // DontCare // TODO: consider retry of WriteBackFull/Evict
    mp_release.retToSrc.get := req.retToSrc.get
    mp_release.likelyshared.get := Mux(isWriteEvictOrEvict, meta.state === BRANCH, false.B)
    mp_release.expCompAck.get := isWriteEvictOrEvict
    mp_release.allowRetry.get := state.s_reissue.getOrElse(false.B)
    mp_release.memAttr.get := MemAttr(allocate = !isEvict, cacheable = true.B, device = false.B, ewa = true.B)

    // CMO
    when (cmo_cbo) {
      mp_release.useProbeData := true.B
      mp_release.readProbeDataDown := ParallelPriorityMux(Seq(
        req_cboClean  -> true.B,
        req_cboFlush  -> isWriteBackFull,
        req_cboInval  -> false.B
      ))
      mp_release.param := ParallelPriorityMux(Seq(
        req_cboClean  -> TtoB,
        req_cboFlush  -> Mux(isT(meta.state), TtoN, BtoN),
        req_cboInval  -> Mux(isT(meta.state), TtoN, BtoN)
      ))
      mp_release.meta := Mux(req_cboClean, meta, MetaEntry())
      mp_release.meta.dirty := false.B
      mp_release.meta.state := Mux(req_cboClean,
        // *NOTICE: CBOClean derives upper Probe toB for now,
        //          so TRUNK should be turned into TIP.
        //
        //          ** IMPORTANT **
        //          For operations that require subsequent Release, derived upper Probes
        //          must be set to 'toB' to simplify and correct Release nesting mechanism.
        Mux(meta.state === TRUNK, TIP, meta.state),
        INVALID)
      mp_release.metaWen := true.B
      mp_release.dsWen := probeDirty
      mp_release.replTask := false.B
      mp_release.chiOpcode.get := ParallelPriorityMux(Seq(
        req_cboClean  -> WriteCleanFull,
        req_cboFlush  -> Mux(isWriteBackFull, WriteBackFull, Evict),
        req_cboInval  -> Evict
      ))
      mp_release.likelyshared.get := false.B
      mp_release.memAttr.get := MemAttr(allocate = false.B, cacheable = true.B, device = false.B, ewa = true.B)
    }

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
    mp_cbwrdata.readProbeDataDown := true.B
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
    mp_cbwrdata.cmoTask := cmo_cbo
    mp_cbwrdata.wayMask := 0.U
    mp_cbwrdata.reqSource := 0.U
    mp_cbwrdata.mergeA := false.B
    mp_cbwrdata.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    mp_cbwrdata.denied := denied
    mp_cbwrdata.corrupt := corrupt

    // CHI
    mp_cbwrdata.tgtID.get := tgtid_wcompack
    mp_cbwrdata.srcID.get := 0.U
    mp_cbwrdata.txnID.get := txnid_wcompack
    mp_cbwrdata.homeNID.get := 0.U
    mp_cbwrdata.dbID.get := 0.U
    mp_cbwrdata.chiOpcode.get := CopyBackWrData
    mp_cbwrdata.resp.get := setPD(metaChi, meta.dirty)
    mp_cbwrdata.fwdState.get := 0.U
    mp_cbwrdata.pCrdType.get := 0.U // TODO
    mp_cbwrdata.retToSrc.get := req.retToSrc.get // DontCare
    mp_cbwrdata.likelyshared.get := false.B
    mp_cbwrdata.expCompAck.get := false.B
    mp_cbwrdata.traceTag.get := cbWrDataTraceTag
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
    mp_probeack.opcode := 0.U
    mp_probeack.param := DontCare
    mp_probeack.size := log2Ceil(blockBytes).U
    mp_probeack.sourceId := 0.U(sourceIdBits.W)
    mp_probeack.bufIdx := 0.U(bufIdxBits.W)
    mp_probeack.needProbeAckData := false.B
    mp_probeack.mshrTask := true.B
    mp_probeack.mshrId := io.id
    mp_probeack.aliasTask.foreach(_ := false.B)
    mp_probeack.useProbeData := true.B // write [probeAckData] to DS, if not probed toN // ???
    mp_probeack.readProbeDataDown := doRespData || mp_probeack.dsWen
    mp_probeack.mshrRetry := false.B
    mp_probeack.way := dirResult.way
    mp_probeack.fromL2pft.foreach(_ := false.B)
    mp_probeack.needHint.foreach(_ := false.B)
    mp_probeack.dirty := hitDirty
    mp_probeack.meta := MetaEntry(
      /**
        * Under what circumstances should the dirty bit be cleared:
        * 1. If the snoop belongs to SnpToN
        * 2. If the snoop belongs to SnpToB
        * 3. If the snoop is SnpCleanShared
        * 4. If the snoop is SnpOnce/SnpOnceFwd and nesting WriteCleanFull
        * 5. If the snoop encounters tagErr
        * Otherwise, the dirty bit should stay the same as before.
        */
      dirty = !tagErr && (!(
        !dirResult.hit || !meta.dirty ||
        snpToN ||
        snpToB ||
        isSnpCleanShared(req_chiOpcode) ||
        isSnpOnceX(req_chiOpcode) && req.snpHitReleaseToClean
      ) || isSnpOnceX(req_chiOpcode) && probeDirty),
      state = Mux(
        snpToN || tagErr,
        INVALID,
        Mux(
          // On SnpOnceFwd nesting WriteCleanFull with UD, we went UD -> SC (T -> B here)
          snpToB || isSnpOnceFwd(req_chiOpcode) && hitWriteClean,
          BRANCH,
          meta.state)
      ),
      clients = meta.clients & Fill(clientBits, !snpToN),
      alias = meta.alias, //[Alias] Keep alias bits unchanged
      prefetch = !snpToN && meta_pft,
      accessed = !snpToN && meta.accessed
    )
    mp_probeack.metaWen := !req.snpHitReleaseToInval
    mp_probeack.tagWen := false.B
    mp_probeack.dsWen := !(snpToN || tagErr) && probeDirty && !releaseDirty
    mp_probeack.wayMask := 0.U(cacheParams.ways.W)
    mp_probeack.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_probeack.replTask := false.B
    mp_probeack.cmoTask := cmo_cbo
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
    mp_probeack.chiOpcode.get := MuxLookup(
      Cat(doFwd || doFwdHitRelease, doRespData),
      SnpResp
    )(Seq(
      Cat(false.B, false.B) -> SnpResp,
      Cat(true.B, false.B)  -> SnpRespFwded,
      Cat(false.B, true.B)  -> SnpRespData, // ignore SnpRespDataPtl for now
      Cat(true.B, true.B)   -> SnpRespDataFwded
    ))
    mp_probeack.resp.get := setPD(respCacheState, respPassDirty)
    mp_probeack.fwdState.get := setPD(fwdCacheState, fwdPassDirty)
    mp_probeack.pCrdType.get := 0.U
    mp_probeack.retToSrc.get := req.retToSrc.get // DontCare
    mp_probeack.likelyshared.get := false.B
    mp_probeack.expCompAck.get := false.B
    mp_probeack.traceTag.get := req.traceTag.get
    mp_probeack.snpHitRelease := req.snpHitRelease
    mp_probeack.snpHitReleaseToInval := req.snpHitReleaseToInval
    mp_probeack.snpHitReleaseToClean := req.snpHitReleaseToClean
    mp_probeack.snpHitReleaseWithData := req.snpHitReleaseWithData
    mp_probeack.snpHitReleaseIdx := req.snpHitReleaseIdx

    mp_probeack
  }


  val mergeA = RegInit(false.B)
  when (io.aMergeTask.valid) {
    mergeA := true.B
  }.elsewhen (io.alloc.valid) {
    mergeA := false.B
  }
  val mp_grant_task = {
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
    mp_grant.denied := denied
    mp_grant.corrupt := corrupt
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
    mp_grant.readProbeDataDown := false.B
    mp_grant.dirty := false.B

    mp_grant.meta := MetaEntry(
      dirty = gotDirty || dirResult.hit && meta.dirty,
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
        Fill(clientBits, !(req_get && (!dirResult.hit || meta_no_client)))
      ),
      alias = Some(aliasFinal),
      prefetch = req_prefetch || dirResult.hit && meta_pft,
      pfsrc = PfSource.fromMemReqSource(req.reqSource),
      accessed = req_acquire || req_get
    )
    mp_grant.metaWen := !cmo_cbo && !denied
    mp_grant.tagWen := !cmo_cbo && !dirResult.hit && !denied
    mp_grant.dsWen := (gotGrantData || probeDirty && (req_get || req.aliasTask.getOrElse(false.B))) && !denied
    mp_grant.fromL2pft.foreach(_ := req.fromL2pft.get)
    mp_grant.needHint.foreach(_ := false.B)
    mp_grant.replTask := !dirResult.hit && !state.w_replResp && !denied
    mp_grant.cmoTask := cmo_cbo
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
      dirty = gotDirty || dirResult.hit && meta.dirty,
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
    mp_dct.readProbeDataDown := true.B
    mp_dct.mshrRetry := false.B
    mp_dct.way := dirResult.way
    mp_dct.fromL2pft.foreach(_ := false.B)
    mp_dct.needHint.foreach(_ := false.B)
    mp_dct.dirty := hitDirty
    mp_dct.meta := MetaEntry()
    mp_dct.metaWen := false.B // meta is written by SnpResp[Data]Fwded, not CompData
    mp_dct.tagWen := false.B
    mp_dct.dsWen := false.B
    mp_dct.wayMask := 0.U(cacheParams.ways.W)
    mp_dct.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_dct.replTask := false.B
    mp_dct.cmoTask := cmo_cbo
    mp_dct.mergeA := false.B
    mp_dct.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_dct.tgtID.get := req.fwdNID.get
    mp_dct.srcID.get := 0.U
    mp_dct.txnID.get := req.fwdTxnID.get
    mp_dct.homeNID.get := req.srcID.get
    mp_dct.dbID.get := req.txnID.get
    mp_dct.chiOpcode.get := CompData
    mp_dct.resp.get := setPD(fwdCacheState, fwdPassDirty)
    mp_dct.fwdState.get := 0.U
    mp_dct.pCrdType.get := 0.U // DontCare
    mp_dct.retToSrc.get := false.B // DontCare
    mp_dct.likelyshared.get := false.B
    mp_dct.expCompAck.get := false.B // DontCare
    mp_dct.traceTag.get := req.traceTag.get
    mp_dct.snpHitRelease := req.snpHitRelease
    mp_dct.snpHitReleaseToInval := req.snpHitReleaseToInval
    mp_dct.snpHitReleaseToClean := req.snpHitReleaseToClean
    mp_dct.snpHitReleaseWithData := req.snpHitReleaseWithData
    mp_dct.snpHitReleaseIdx := req.snpHitReleaseIdx
    mp_dct.snpHitReleaseMeta := req.snpHitReleaseMeta

    mp_dct
  }

  val mp_cmometaw_task = {
    mp_cmometaw.channel := 0.U
    mp_cmometaw.txChannel := 0.U
    mp_cmometaw.tag := req.tag
    mp_cmometaw.set := req.set
    mp_cmometaw.off := req.off
    mp_cmometaw.alias.foreach(_ := 0.U)
    mp_cmometaw.vaddr.foreach(_ := 0.U)
    mp_cmometaw.isKeyword.foreach(_ := 0.U)
    mp_cmometaw.opcode := 0.U // DontCare
    mp_cmometaw.param := 0.U // DontCare
    mp_cmometaw.size := log2Ceil(blockBytes).U
    mp_cmometaw.sourceId := 0.U(sourceIdBits.W)
    mp_cmometaw.bufIdx := 0.U(sourceIdBits.W)
    mp_cmometaw.needProbeAckData := false.B
    mp_cmometaw.mshrTask := true.B
    mp_cmometaw.mshrId := io.id
    mp_cmometaw.aliasTask.foreach(_ := false.B)
    mp_cmometaw.useProbeData := false.B
    mp_cmometaw.readProbeDataDown := false.B
    mp_cmometaw.mshrRetry := false.B
    mp_cmometaw.way := dirResult.way
    mp_cmometaw.fromL2pft.foreach(_ := false.B)
    mp_cmometaw.needHint.foreach(_ := false.B)
    mp_cmometaw.dirty := hitDirty
    
    // write meta for compensation of ProbeAck TtoB/TtoN by cbo.clean
    // *NOTICE: There is no possible nest for 'cmometaw' task, snoops should be blocked by RXSNP.
    mp_cmometaw.meta := meta
    mp_cmometaw.meta.clients := meta.clients
    mp_cmometaw.meta.dirty := false.B
    mp_cmometaw.meta.state := TIP // write TIP for compensation of ProbeAck TtoB/TtoN by cbo.clean
    mp_cmometaw.metaWen := true.B

    mp_cmometaw.tagWen := false.B
    mp_cmometaw.dsWen := false.B
    mp_cmometaw.wayMask := 0.U(cacheParams.ways.W)
    mp_cmometaw.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_cmometaw.replTask := false.B
    mp_cmometaw.cmoTask := cmo_cbo
    mp_cmometaw.mergeA := false.B
    mp_cmometaw.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_cmometaw.tgtID.get := 0.U
    mp_cmometaw.srcID.get := 0.U
    mp_cmometaw.txnID.get := 0.U
    mp_cmometaw.homeNID.get := 0.U
    mp_cmometaw.dbID.get := 0.U
    mp_cmometaw.chiOpcode.get := 0.U
    mp_cmometaw.resp.get := 0.U
    mp_cmometaw.fwdState.get := 0.U
    mp_cmometaw.pCrdType.get := 0.U // DontCare
    mp_cmometaw.retToSrc.get := false.B // DontCare
    mp_cmometaw.likelyshared.get := false.B
    mp_cmometaw.expCompAck.get := false.B // DontCare
    mp_cmometaw.traceTag.get := 0.U
    mp_cmometaw.snpHitRelease := req.snpHitRelease
    mp_cmometaw.snpHitReleaseToInval := req.snpHitReleaseToInval
    mp_cmometaw.snpHitReleaseToClean := req.snpHitReleaseToClean
    mp_cmometaw.snpHitReleaseWithData := req.snpHitReleaseWithData
    mp_cmometaw.snpHitReleaseIdx := req.snpHitReleaseIdx
    mp_cmometaw.snpHitReleaseMeta := req.snpHitReleaseMeta

    mp_cmometaw
  }

  io.tasks.mainpipe.bits := ParallelPriorityMux(
    Seq(
      mp_grant_valid         -> mp_grant,
      mp_release_valid       -> mp_release,
      mp_cbwrdata_valid      -> mp_cbwrdata,
      mp_probeack_valid      -> mp_probeack,
      mp_dct_valid           -> mp_dct,
      mp_cmometaw_valid      -> mp_cmometaw
    )
  )
  io.tasks.mainpipe.bits.reqSource := req.reqSource
  io.tasks.mainpipe.bits.isKeyword.foreach(_:= req.isKeyword.getOrElse(false.B))

  val mp_valid = io.tasks.mainpipe.valid
  val mp = io.tasks.mainpipe.bits

  /* ======== Assertions for common transaction ======== */
  Seq(
    ("CopyBackWrData", CHICohStateTransSet.ofCopyBackWrData(CopyBackWrData)),
    ("CompData", CHICohStateTransSet.ofCompData(CompData)),
    ("SnpResp", CHICohStateTransSet.ofSnpResp(SnpResp)),
    ("SnpRespData", CHICohStateTransSet.ofSnpRespData(SnpRespData)),
    ("SnpRespDataPtl", CHICohStateTransSet.ofSnpRespDataPtl(SnpRespDataPtl)),
    ("NonCopyBackWrData", CHICohStateTransSet.ofNonCopyBackWrData(NonCopyBackWrData)),
    ("WriteDataCancel", CHICohStateTransSet.ofWriteDataCancel(WriteDataCancel))
  ).foreach { case (name, set) => {
    assert(!mp_valid || CHICohStateTransSet.isValid(set, 
        mp.txChannel, mp.chiOpcode.get, mp.resp.get),
      s"invalid Resp for ${name}")
  }}

  ifAfterIssueC {
    Seq(
      ("DataSepResp", CHICohStateTransSet.ofDataSepResp(DataSepResp)),
      ("RespSepData", CHICohStateTransSet.ofRespSepData(RespSepData))
    ).foreach { case (name, set) => {
      assert(!mp_valid || CHICohStateTransSet.isValid(set, 
          mp.txChannel, mp.chiOpcode.get, mp.resp.get),
        s"invalid Resp for ${name}")
    }}
  }

  /* ======== Assertions for DCT forwarded snoop ======== */
  Seq(
    ("SnpRespFwded", CHICohStateFwdedTransSet.ofSnpRespFwded(SnpRespFwded)),
    ("SnpRespDataFwded", CHICohStateFwdedTransSet.ofSnpRespDataFwded(SnpRespDataFwded))
  ).foreach { case (name, set) => {
    assert(!mp_valid || CHICohStateFwdedTransSet.isValid(set, 
        mp.txChannel, mp.chiOpcode.get, mp.resp.get, mp.fwdState.get),
      s"invalid combination of Resp and FwdState for ${name}")
  }}

  /* ======== Task update ======== */
  when (io.tasks.txreq.fire) {
    // *NOTICE: Two parts of CMO procedure (release, acquire) were all possible to be retried.
    state.s_acquire := Mux(cmo_cbo, state.s_acquire || state.w_releaseack, true.B)
    when (!state.s_reissue.get) {
      state.s_reissue.get := true.B
      gotRetryAck := false.B
      gotPCrdGrant := false.B
    }
  }
  when (io.tasks.txrsp.fire) {
    when (rcompack_valid) {
      state.s_rcompack.get := true.B
    }
    when (wcompack_valid) {
      state.s_wcompack.get := true.B
    }
    assert(!(rcompack_valid && wcompack_valid))
  }
  when (io.tasks.source_b.fire) {
    state.s_pprobe := true.B
    state.s_rprobe := true.B
  }
  when (io.tasks.mainpipe.ready) {
    when (mp_grant_valid) {
      state.s_refill := true.B
      state.s_retry := true.B
      when (mp_grant.opcode === CBOAck) {
        state.s_cmoresp := true.B
      }
    }.elsewhen (mp_release_valid) {
      req_released_chiOpcode := mp_release.chiOpcode.get
      state.s_release := true.B
      state.s_cbwrdata.get := isEvict
      when (isEvict) {
        meta.state := INVALID
        meta.dirty := false.B
      }
    }.elsewhen (mp_cbwrdata_valid) {
      state.s_cbwrdata.get := true.B
      meta.state := INVALID
      meta.dirty := false.B
    }.elsewhen (mp_probeack_valid) {
      state.s_probeack := true.B
    }.elsewhen (mp_dct_valid) {
      state.s_dct.get := true.B
    }.elsewhen (mp_cmometaw_valid) {
      state.s_cmometaw := true.B
    }
  }

  /*                      Handling response

      TL                    CHI             CHI Resp              CHI channel 
  -----------------------------------------------------------------------------
  AcquireBlock       |  ReadNotShareDirty |  CompData           |    rxdat 
  AcquirePerm(hit B) |  MakeUnique        |  Comp               |    rxrsp <-
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
    }
    when (c_resp.bits.opcode === ProbeAckData) {
      probeDirty := true.B
      meta.dirty := true.B
    }
    when (isToN(c_resp.bits.param)) {
      meta.state := Mux(isT(meta.state), TIP, meta.state)
      meta.clients := Fill(clientBits, false.B)
    }
    when (isToB(c_resp.bits.param)) {
      meta.state := Mux(isT(meta.state), TIP, meta.state)
    }
    when (isParamFromT(c_resp.bits.param)) {
      meta.tagErr := c_resp.bits.denied
      meta.dataErr := c_resp.bits.corrupt
      denied := denied || c_resp.bits.denied
      corrupt := corrupt || c_resp.bits.corrupt
    }

    // CMO update release on ProbeAck/ProbeAckData
    when (req_cboClean) {
      when (c_resp.bits.opcode === ProbeAckData && state.w_rprobeackfirst) {
        state.s_release := false.B
        state.w_releaseack := false.B
      }
      when (c_resp.bits.opcode === ProbeAck) {
        when (meta.dirty) {
          state.s_release := false.B
          state.w_releaseack := false.B
        }.otherwise {
          // meta write compensation on ProbeAck TtoB
          state.s_cmometaw := false.B
        }
      }
    }
  }

  val rxdatIsU = rxdat.bits.resp.get === UC /*UC_UD*/
  val rxdatIsU_PD = rxdat.bits.resp.get === UC_PD

  val rxrspIsU = rxrsp.bits.resp.get === UC /*UC_UD*/

  // RXDAT
  when (rxdat.valid) {
    val nderr = rxdat.bits.respErr.getOrElse(OK) === NDERR
    val derr = rxdat.bits.respErr.getOrElse(OK) === DERR
    val rxdatCorrupt = rxdat.bits.corrupt
    ifAfterIssueC {
      when (rxdat.bits.chiOpcode.get === DataSepResp) {
        require(beatSize == 2) // TODO: This is ugly
        beatCnt := beatCnt + 1.U
        state.w_grantfirst := true.B
        state.w_grantlast := state.w_grantfirst && beatCnt === (beatSize - 1).U
        state.w_replResp := state.w_replResp || nderr
        gotT := rxdatIsU || rxdatIsU_PD
        gotDirty := gotDirty || rxdatIsU_PD
        gotGrantData := true.B
        denied := denied || nderr
        corrupt := corrupt || derr || nderr || rxdatCorrupt
      }
    }

    when (rxdat.bits.chiOpcode.get === CompData) {
      require(beatSize == 2) // TODO: This is ugly
      state.w_grantfirst := true.B
      state.w_grantlast := state.w_grantfirst
      state.w_grant := true.B
      state.w_replResp := state.w_replResp || nderr
      gotT := rxdatIsU || rxdatIsU_PD
      gotDirty := gotDirty || rxdatIsU_PD
      gotGrantData := true.B
      // The TxnID of CompAck is set to the same value as the DBID of the read data.
      txnid_rcompack := rxdat.bits.dbID.getOrElse(0.U)
      // The TgtID of CompAck is set to the same value as the HomeNID of the read data.
      tgtid_rcompack := rxdat.bits.homeNID.getOrElse(0.U)
      denied := denied || nderr
      corrupt := corrupt || derr || nderr || rxdatCorrupt
      req.traceTag.get := req.traceTag.get || rxdat.bits.traceTag.getOrElse(false.B)
    }
  }

  // RXRSP
  when (rxrsp.valid) {
    val nderr = rxrsp.bits.respErr.getOrElse(OK) === NDERR
    ifAfterIssueC {
      when (rxrsp.bits.chiOpcode.get === RespSepData) {
        state.w_grant := true.B
        state.w_replResp := state.w_replResp || nderr
        // The TgtID of CompAck is set to the same value as the SrcID of the read response.
        tgtid_rcompack := rxrsp.bits.srcID.getOrElse(0.U)
        // The TxnID of CompAck is set to the unique DBID value generated by the Home.
        txnid_rcompack := rxrsp.bits.dbID.getOrElse(0.U)
        denied := denied || nderr
        req.traceTag.get := rxrsp.bits.traceTag.get
      }
    }

    when (rxrsp.bits.chiOpcode.get === Comp) {
      // There is a pending Read transaction waiting for the Comp resp
      // *NOTICE: In CMO transactions, releases (if there was one) always happen before acquire
      when (!state.w_grant && (state.s_cmoresp || state.w_releaseack)) {
        state.w_grantfirst := true.B
        state.w_grantlast := true.B
        state.w_grant := true.B
        gotT := rxrspIsU
        gotDirty := false.B
        denied := denied || nderr
        req.traceTag.get := rxrsp.bits.traceTag.get
        tgtid_rcompack := rxrsp.bits.srcID.getOrElse(0.U)
        txnid_rcompack := rxrsp.bits.dbID.getOrElse(0.U)
      }

      // There is a pending Evict/WriteEvictOrEvict transaction waiting for the Comp resp
      when (!state.w_releaseack) {
        state.w_releaseack := true.B
        // There is no CompAck for Comp in response of Evict. Thus there is no need to record TraceTag.
        // Except on WriteEvictOrEvict:
        when (isWriteEvictOrEvict) {
          req.traceTag.get := rxrsp.bits.traceTag.get
          // For WriteEvictOrEvict, drop CopyBackWrData on Comp
          state.s_cbwrdata.get := true.B
          // Schedule CompAck on Comp
          state.s_wcompack.get := false.B
          tgtid_wcompack := rxrsp.bits.srcID.getOrElse(0.U)
          txnid_wcompack := rxrsp.bits.dbID.getOrElse(0.U)
        }
      }
    }
    when (rxrsp.bits.chiOpcode.get === CompDBIDResp) {
      state.w_releaseack := true.B
      tgtid_wcompack := rxrsp.bits.srcID.getOrElse(0.U)
      txnid_wcompack := rxrsp.bits.dbID.getOrElse(0.U)
      cbWrDataTraceTag := rxrsp.bits.traceTag.get
    }
    when (rxrsp.bits.chiOpcode.get === RetryAck) {
      srcid_retryack := rxrsp.bits.srcID.getOrElse(0.U)
      pcrdtype := rxrsp.bits.pCrdType.getOrElse(0.U)
      gotRetryAck := true.B
    }
  }

  // when rxrsp is PCrdGrant
  when (io.pCrd.grant) {
    state.s_reissue.get := false.B
    gotPCrdGrant := true.B
  }

  // replay
  val replResp = io.replResp.bits
  when (io.replResp.valid && replResp.retry) {
    state.s_refill := false.B
    state.s_retry := false.B
    dirResult.way := replResp.way
    when (retryTimes < backoffThreshold.U) {
      retryTimes := retryTimes + 1.U
    }
    backoffTimer := 0.U
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
    state.s_rcompack.getOrElse(true.B) &&
    state.s_wcompack.getOrElse(true.B) &&
    state.s_cbwrdata.getOrElse(true.B) &&
    state.s_reissue.getOrElse(true.B) &&
    state.s_dct.getOrElse(true.B) &&
    state.s_cmoresp &&
    state.s_cmometaw
  val no_wait = state.w_rprobeacklast && state.w_pprobeacklast && state.w_grantlast && state.w_grant &&
    state.w_releaseack && state.w_replResp
  val will_free = no_schedule && no_wait
  when (will_free && req_valid) {
    req_valid := false.B
    timer := 0.U
  }

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
  io.status.bits.w_c_resp := !state.w_rprobeacklast || !state.w_pprobeacklast
  io.status.bits.w_d_resp := !state.w_grantlast || !state.w_grant || !state.w_releaseack
  io.status.bits.will_free := will_free
  io.status.bits.is_miss := !dirResult.hit
  io.status.bits.is_prefetch := req_prefetch
  io.status.bits.reqSource := req.reqSource

  io.statAlloc.valid := io.alloc.valid
  io.statAlloc.bits.is_miss := !io.alloc.bits.dirResult.hit
  io.statAlloc.bits.is_prefetch := io.alloc.bits.task.opcode === Hint
  io.statAlloc.bits.channel := io.alloc.bits.task.channel

  io.msInfo.valid := req_valid
  io.msInfo.bits.set := req.set
  io.msInfo.bits.way := dirResult.way
  io.msInfo.bits.reqTag := req.tag
  io.msInfo.bits.aliasTask.foreach(_ := req.aliasTask.getOrElse(false.B))
  io.msInfo.bits.needRelease := !state.w_releaseack
  // if releaseTask is already in mainpipe_s1/s2, while a refillTask in mainpipe_s3, the refill should also be blocked and retry
  // also block refill when the CMO-derived ProbeAckData was not written to DS
  io.msInfo.bits.blockRefill := releaseNotSent || RegNext(releaseNotSent, false.B) || RegNext(RegNext(releaseNotSent, false.B), false.B)
  io.msInfo.bits.dirHit := dirResult.hit
  io.msInfo.bits.metaTag := dirResult.tag
  io.msInfo.bits.meta := meta
  io.msInfo.bits.meta.dirty := meta.dirty
  io.msInfo.bits.willFree := will_free
  io.msInfo.bits.isAcqOrPrefetch := req_acquire || req_prefetch
  io.msInfo.bits.isPrefetch := req_prefetch
  io.msInfo.bits.param := req.param
  io.msInfo.bits.mergeA := mergeA
  io.msInfo.bits.w_grantfirst := state.w_grantfirst
  io.msInfo.bits.s_release := state.s_release
  io.msInfo.bits.s_refill := state.s_refill
  io.msInfo.bits.s_cmoresp := state.s_cmoresp
  io.msInfo.bits.s_cmometaw := state.s_cmometaw
  io.msInfo.bits.w_releaseack := state.w_releaseack
  io.msInfo.bits.w_replResp := state.w_replResp
  io.msInfo.bits.w_rprobeacklast := state.w_rprobeacklast
  io.msInfo.bits.replaceData := isT(meta.state) && meta.dirty || // including WriteCleanFull
                                isWriteEvictFull || isWriteEvictOrEvict
  io.msInfo.bits.releaseToClean := releaseToClean
  io.msInfo.bits.channel := req.channel

  assert(!(c_resp.valid && !io.status.bits.w_c_resp))
  assert(!(rxrsp.valid && rxrsp.bits.chiOpcode.get =/= PCrdGrant && !io.status.bits.w_d_resp))

  /* ======== Handling Nested C ======== */
  // for A miss, only when replResp do we finally choose a way, allowing nested C
  // for A-alias, always allowing nested C (state.w_replResp === true.B)
  // for CMO, always allowing nested C on directory hit (state.s_cmoresp === false.B)
  val nestedwb_match = req_valid && meta.state =/= INVALID &&
    dirResult.set === io.nestedwb.set &&
    dirResult.tag === io.nestedwb.tag &&
    state.w_replResp && 
    (state.s_cmoresp || dirResult.hit) &&   // exclude CMO on directory miss
    (req_mayRepl || dirResult.hit)          // exclude non-repl tasks (e.g. Forward Snoop) on directory miss
  val nestedwb_hit_match = req_valid && dirResult.hit &&
    dirResult.set === io.nestedwb.set &&
    dirResult.tag === io.nestedwb.tag

  when (nestedwb_match) {
    when (io.nestedwb.c_set_dirty) {
      meta.dirty := true.B
      meta.state := TIP
      meta.clients := Fill(clientBits, false.B)
      releaseDirty := true.B
    }
    when (io.nestedwb.c_set_tip) {
      meta.state := TIP
      meta.clients := Fill(clientBits, false.B)
    }
    when (io.nestedwb.b_inv_dirty && req.fromA) {
      meta.dirty := false.B
      meta.state := INVALID
      probeDirty := false.B
    }
  }
  when (nestedwb_hit_match) {
    when (io.nestedwb.b_toClean.get && req.fromA) {
      meta.dirty := false.B
      probeDirty := false.B
    }
    when (io.nestedwb.b_toB.get && req.fromA) {
      meta.state := Mux(meta.state >= BRANCH, BRANCH, INVALID)
    }
    when (io.nestedwb.b_toN.get && req.fromA) {
      meta.state := INVALID
      dirResult.hit := false.B
      meta.clients := Fill(clientBits, false.B)
      state.w_replResp := cmo_cbo // never query replacer on CMO
      req.aliasTask.foreach(_ := false.B)
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
  when (io.alloc.valid) {
    validCnt := 0.U
  }

  when (req_valid) {
    validCnt := validCnt + 1.U
  }

  val mshrAddr = Cat(req.tag, req.set, 0.U(6.W)) // TODO: consider multibank
  val VALID_CNT_MAX = 400000.U
  assert(validCnt <= VALID_CNT_MAX, "validCnt full!, maybe there is a deadlock! addr => 0x%x req_opcode => %d channel => 0b%b", mshrAddr, req.opcode, req.channel)

  val evictFire = io.tasks.txreq.fire && io.tasks.txreq.bits.opcode === Evict ||
    io.tasks.mainpipe.fire && io.tasks.mainpipe.bits.opcode === Evict && io.tasks.mainpipe.bits.toTXREQ
  val wbFire = io.tasks.txreq.fire && io.tasks.txreq.bits.opcode === WriteBackFull ||
    io.tasks.mainpipe.fire && io.tasks.mainpipe.bits.opcode === WriteBackFull && io.tasks.mainpipe.bits.toTXREQ
  val wcFire = io.tasks.txreq.fire && io.tasks.txreq.bits.opcode === WriteCleanFull ||
    io.tasks.mainpipe.fire && io.tasks.mainpipe.bits.opcode === WriteCleanFull && io.tasks.mainpipe.bits.toTXREQ
  val weFire = io.tasks.txreq.fire && io.tasks.txreq.bits.opcode === WriteEvictFull ||
    io.tasks.mainpipe.fire && io.tasks.mainpipe.bits.opcode === WriteEvictFull && io.tasks.mainpipe.bits.toTXREQ
  assert(!RegNext(evictFire) || state.s_cbwrdata.get, "There should be no CopyBackWrData after Evict")
  assert(!RegNext(wbFire) || !state.s_cbwrdata.get, "There must be a CopyBackWrData after WriteBack")
  assert(!RegNext(wcFire) || !state.s_cbwrdata.get, "There must be a CopyBackWrData after WriteClean")
  assert(!RegNext(weFire) || !state.s_cbwrdata.get, "There must be a CopyBackWrData after WriteEvictFull")

  /* ======== Performance counters ======== */
  // time stamp
  val acquire_period = Option.when(cacheParams.enablePerf)(IO(ValidIO(UInt(64.W))))
  val release_period = Option.when(cacheParams.enablePerf)(IO(ValidIO(UInt(64.W))))
  if (cacheParams.enablePerf) {
    val acquire_start = io.tasks.txreq.fire && !state.s_acquire
    val release_start = io.tasks.mainpipe.fire && !state.s_release
    val acquire_ts = RegEnable(timer, acquire_start)
    val release_ts = RegEnable(timer, release_start)
    val acquire_finish = state.w_grant && state.w_grantlast
    val release_finish = state.w_releaseack

    acquire_period.get.valid := acquire_finish && !RegNext(acquire_finish)
    acquire_period.get.bits := timer - acquire_ts
    release_period.get.valid := release_finish && !RegNext(release_finish)
    release_period.get.bits := timer - release_ts
  }
}
