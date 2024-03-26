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
import utility.{MemReqSource, ParallelLookUp, ParallelPriorityMux}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters
import coupledL2.prefetch.{PfSource, PrefetchTrain}
import coupledL2.utils.XSPerfAccumulate

class MSHRTasks(implicit p: Parameters) extends L2Bundle {
  // outer
  val txreq = DecoupledIO(new CHIREQ) //TODO: no need to use decoupled handshake
  val txrsp = DecoupledIO(new CHIRSP) //TODO: no need to use decoupled handshake
  val source_b = DecoupledIO(new SourceBReq)
  val mainpipe = DecoupledIO(new TaskBundle) // To Mainpipe (SourceC or SourceD)
  // val prefetchTrain = prefetchOpt.map(_ => DecoupledIO(new PrefetchTrain)) // To prefetcher
}

class MSHRResps(implicit p: Parameters) extends L2Bundle {
  val sinkC = new RespBundle()  
  val rxrsp = new RespBundle()  
  val rxdat = new RespBundle()  
}

class MSHR(implicit p: Parameters) extends L2Module {
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
  })

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
  }

  /* ======== Enchantment ======== */
  val meta_pft = meta.prefetch.getOrElse(false.B)
  val meta_no_client = !meta.clients.orR

  val req_needT = needT(req.opcode, req.param)
  val req_acquire = req.opcode === AcquireBlock && req.fromA || req.opcode === AcquirePerm // AcquireBlock and Probe share the same opcode
  val req_acquirePerm = req.opcode === AcquirePerm
  val req_get = req.opcode === Get
  val req_prefetch = req.opcode === Hint
  val snpNoData = isSnpMakeInvalidX(req.chiOpcode) || isSnpStashX(req.chiOpcode)
  val gotUD = meta.dirty & (meta === TRUNK || meta.state === TIP) //TC/TTC -> UD 
  val promoteT_normal =  dirResult.hit && meta_no_client && meta.state === TIP
  val promoteT_L3     = !dirResult.hit && gotT
  val promoteT_alias  =  dirResult.hit && req.aliasTask.getOrElse(false.B) && (meta.state === TRUNK || meta.state === TIP)
  // under above circumstances, we grant T to L1 even if it wants B
  val req_promoteT = (req_acquire || req_get || req_prefetch) && (promoteT_normal || promoteT_L3 || promoteT_alias)

  assert(!(req_valid && req_prefetch && dirResult.hit), "MSHR can not receive prefetch hit req")


  /* ======== Task allocation ======== */
  // Theoretically, data to be released is saved in ReleaseBuffer, so Acquire can be sent as soon as req enters mshr
  io.tasks.txreq.valid := !state.s_acquire || !state.s_reIssue
  io.tasks.txrsp.valid := RegNext(io.rxdat.valid)
  io.tasks.source_b.valid := !state.s_pprobe || !state.s_rprobe
  val mp_wbfull_valid = !state.s_release && state.w_rprobeacklast && state.w_grantlast &&
        state.w_replResp // release after Grant to L1 sent and replRead returns

  val mp_cbwrdata_valid = !state.s_cbwrdata
  val mp_snpresp_valid = !state.s_probeack && state.w_pprobeacklast
  val mp_grant_valid = !state.s_refill && state.w_grantlast && state.w_rprobeacklast // [Alias] grant after rprobe done
  io.tasks.mainpipe.valid := mp_wbfull_valid || mp_snpresp_valid || mp_grant_valid || mp_cbwrdata_valid
  // io.tasks.prefetchTrain.foreach(t => t.valid := !state.s_triggerprefetch.getOrElse(true.B))

  /*TXRSP for CompAck/DBID */
    val txrsp_task = {
      val orsp = io.tasks.txrsp.bits
      orsp.qos := 0.U
      orsp.tgtID := 0.U
      orsp.srcID := 0.U
      orsp.txnID := io.id
      orsp.opcode := CompAck
      orsp.resperr := 0.U
      orsp.resp  := gotResp
      orsp.fwdState := 0.U
      orsp.stashhit := 0.U
      orsp.datapull :=0.U
      orsp.dbid := 0.U
      orsp.pcrdtype := 0.U
      orsp.tracetag := 0.U
    }

  /*TXREQ for Transaction Request*/
  val a_task = {
    val oa = io.tasks.txreq.bits
    oa.qos := 0.U
    oa.tgtID := 0.U
    oa.srcID := 0.U
    oa.txnID := io.id
    oa.returnNid := 0.U
    oa.stashNid := 0.U
    oa.stashNidValid := 0.U
    oa.stashInfo := 0.U
    oa.opcode := ParallelLookUp(
      Cat(req.opcode, dirResult.hit, isT(meta.state))
        Seq(
          Cat(AcquireBlock, false.B, false.B) -> ReadNotShareDirty, //load miss/store miss
          Cat(AcquirePerm,  false.B, false.B) -> ReadUnique,        //store upgrade miss
          Cat(AcquirePerm,   true.B, false.B) -> ReadUnique,        //TODO may use MakeUnique
//          Cat(AcquirePerm,   true.B, false.B) -> MakedUnique,     //store upgrade hit + noT
          Cat(Get,          false.B, false.B) -> ReadClean,
          Cat(Hint,         false.B, false.B) -> ReadNotShareDirty
        ))
    oa.size := "b110"  //64Byte
    oa.addr := Cat(task.tag, task.set, 0.U(offsetBits.W)) //TODO 36bit -> 48bit
    oa.ns := "b0"
    oa.likelyshared := "b0"
    oa.allowRetry := "b1"
    oa.order := "b00"
    oa.PCrdType := "b0000"
    oa.expCompAck := true.B
    oa.memAttr := "b1101" // {allocate, cacheable, Device, EWA}
    oa.snpAttr := "b1"
    oa.snoopable := "b1"
    oa.lpid := "b00000"
    oa.excel := "b0"
    oa.snoopme := "b0"
    oa.expcompack := "b0"
    oa.tracetag := "b0"
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
      req.param,
      Mux(
        req_get && dirResult.hit && meta.state === TRUNK,
        toB,
        toN
      )
    )
    ob.alias.foreach(_ := meta.alias.getOrElse(0.U))
    ob
  }

  val mp_wbfull, mp_snpresp, mp_grant, mp_cbwrdata = Wire(new TaskBundle)
  val mp_wbfull_task = {
    mp_wbfull.channel := req.channel
    mp_wbfull.tag := dirResult.tag
    mp_wbfull.set := req.set
    mp_wbfull.off := 0.U
    mp_wbfull.alias.foreach(_ := 0.U)
    mp_wbfull.vaddr.foreach(_ := 0.U)
    mp_wbfull.isKeyword.foreach(_ := false.B)
    // if dirty, we must ReleaseData
    // if accessed, we ReleaseData to keep the data in L3, for future access to be faster
    // [Access] TODO: consider use a counter
    mp_wbfull.opcode := {
      cacheParams.releaseData match {
        case 0 => Mux(meta.dirty && meta.state =/= INVALID || probeDirty, ReleaseData, Release)
        case 1 => Mux(meta.dirty && meta.state =/= INVALID || probeDirty || meta.accessed, ReleaseData, Release)
        case 2 => Mux(meta.prefetch.getOrElse(false.B) && !meta.accessed, Release, ReleaseData) //TODO: has problem with this
        case 3 => ReleaseData // best performance with HuanCun-L3
      }
    }
    mp_wbfull.param := Mux(isT(meta.state), TtoN, BtoN)
    mp_wbfull.size := 0.U(msgSizeBits.W)
    mp_wbfull.sourceId := 0.U(sourceIdBits.W)
    mp_wbfull.bufIdx := 0.U(bufIdxBits.W)
    mp_wbfull.needProbeAckData := false.B
    mp_wbfull.mshrTask := true.B
    mp_wbfull.mshrId := io.id
    mp_wbfull.aliasTask.foreach(_ := false.B)
    // mp_wbfull definitely read releaseBuf and refillBuf at ReqArb
    // and it needs to write refillData to DS, so useProbeData is set false according to DS.wdata logic
    mp_wbfull.useProbeData := false.B
    mp_wbfull.mshrRetry := false.B
    mp_wbfull.way := dirResult.way
    mp_wbfull.fromL2pft.foreach(_ := false.B)
    mp_wbfull.needHint.foreach(_ := false.B)
    mp_wbfull.dirty := meta.dirty && meta.state =/= INVALID || probeDirty
    mp_wbfull.metaWen := false.B
    mp_wbfull.meta := MetaEntry()
    mp_wbfull.tagWen := false.B
    mp_wbfull.dsWen := true.B // write refillData to DS
    mp_wbfull.replTask := true.B
    mp_wbfull.wayMask := 0.U(cacheParams.ways.W)
    mp_wbfull.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_wbfull.mergeA := false.B
    mp_wbfull.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    mp_wbfull.isCBWrDataTask := !state.s_cbWrData

    // CHI
    mp_wbfull.tgtID := 0.U
    mp_wbfull.srcID := 0.U
    mp_wbfull.txnID := 0.U
    mp_wbfull.dbID := 0.U 
    mp_wbfull.chiOpcode := Mux(mp_cbwrdata_valid, CopyBackWrData, Mux(got_Dirty, WriteBackFull, Evict))
    mp_wbfull.resp := I
    mp_wbfull.fwdState := "b000"
    mp_wbfull.pCrdType := "b0000" 
    mp_wbfull.retToSrc := req.retToSrc
    mp_wbfull.expCompAck := "b0"
    mp_wbfull
  }

  val mp_snpresp_task = {
    mp_snpresp.channel := req.channel
    mp_snpresp.tag := req.tag
    mp_snpresp.set := req.set
    mp_snpresp.off := req.off
    mp_snpresp.alias.foreach(_ := 0.U)
    mp_snpresp.vaddr.foreach(_ := 0.U)
    mp_snpresp.isKeyword.foreach(_ := false.B)
    mp_snpresp.opcode := Mux(
      meta.dirty && isT(meta.state) || probeDirty || req.needProbeAckData,
      ProbeAckData,
      ProbeAck
    )
    mp_snpresp.param := ParallelLookUp(
      Cat(isT(meta.state), req.param(bdWidth - 1, 0)),
      Seq(
        Cat(false.B, toN) -> BtoN,
        Cat(true.B, toN) -> TtoN,
        Cat(true.B, toB) -> TtoB
      )
    )
    mp_snpresp.size := 0.U(msgSizeBits.W)
    mp_snpresp.sourceId := 0.U(sourceIdBits.W)
    mp_snpresp.bufIdx := 0.U(bufIdxBits.W)
    mp_snpresp.needProbeAckData := false.B
    mp_snpresp.mshrTask := true.B
    mp_snpresp.mshrId := io.id
    mp_snpresp.aliasTask.foreach(_ := false.B)
    mp_snpresp.useProbeData := true.B // write [probeAckData] to DS, if not probed toN
    mp_snpresp.mshrRetry := false.B
    mp_snpresp.way := dirResult.way
    mp_snpresp.fromL2pft.foreach(_ := false.B)
    mp_snpresp.needHint.foreach(_ := false.B)
    mp_snpresp.dirty := meta.dirty && meta.state =/= INVALID || probeDirty
    mp_snpresp.meta := MetaEntry(
      dirty = false.B,
      state = Mux(
        req.param === toN,
        INVALID,
        Mux(
          req.param === toB,
          BRANCH,
          meta.state
        )
      ),
      clients = Fill(clientBits, !probeGotN),
      alias = meta.alias, //[Alias] Keep alias bits unchanged
      prefetch = req.param =/= toN && meta_pft,
      accessed = req.param =/= toN && meta.accessed
    )
    mp_snpresp.metaWen := true.B
    mp_snpresp.tagWen := false.B
    mp_snpresp.dsWen := req.param =/= toN && probeDirty
    mp_snpresp.wayMask := 0.U(cacheParams.ways.W)
    mp_snpresp.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_snpresp.replTask := false.B
    mp_snpresp.mergeA := false.B
    mp_snpresp.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_snpresp.tgtID = 0.U
    mp_snpresp.srcID = 0.U
    mp_snpresp.txnID = 0.U
    mp_snpresp.dbID = 0.U
    mp_snpresp.chiOpcode = Mux(snpNoData, SnpResp, SnpRespData)
    mp_snpresp.resp = ParallelLookUp(
      Cat(isSnptoN, isSnptoB, isSnptoPeek, isSnptoPD),
      Seq(
        "b1000" -> I,
        "b0100" -> SC,
        "b0010" -> dirResultChi,
        "b0001" -> Mux(gotUD, UC, dirResultChi)
      ))
    mp_snpresp.fwdState = "b000"
    mp_snpresp.pCrdType = "b0000" 
    mp_snpresp.retToSrc = req.retToSrc
    mp_snpresp.expCompAck = "b0"
    mp_snpresp

    mp_snpresp
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
    mp_grant.dsWen := (!dirResult.hit || gotDirty && gotGrantData || probeDirty && (req_get || req.aliasTask.getOrElse(false.B))
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
  io.tasks.mainpipe.bits := ParallelPriorityMux(
    Seq(
      mp_grant_valid        -> mp_grant,
      mp_wbfull_valid       -> mp_wbfull,
      mp_cbwrdata_valid     -> mp_wbfull,
      mp_snpresp_valid      -> mp_snpresp
      mp_snprespdata_valid  -> mp_snpresp
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
    state.s_reIssue := true.B 
  }
  when (io.tasks.source_b.fire) {
    state.s_pprobe := true.B
    state.s_rprobe := true.B
  }
  when (io.tasks.mainpipe.ready) {
    when (mp_grant_valid) {
      state.s_refill := true.B
      state.s_retry := true.B
    }.elsewhen (mp_wbfull_valid) {
      state.s_release := true.B
      meta.state := INVALID
    }.elsewhen (mp_cbwrdata_valid) {
      state.s_cbWrData := true.B
      state.w_releaseack := true.B
      meta.state := INVALID
    }.elsewhen (mp_snpresp_valid) {
      state.s_probeack := true.B
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
      //Probe core response
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
    val req_needT = needT(req.opcode, req.param)
    val req_acquire = req.opcode === AcquireBlock && req.fromA || req.opcode === AcquirePerm // AcquireBlock and Probe share the same opcode
    val req_acquirePerm = req.opcode === AcquirePerm
    val req_get = req.opcode === Get
    val req_prefetch = req.opcode === Hint

    val rxrspIsUC = (rxdat.bits.resp === 3'b010)
    val rxrspIsUD = (rxdat.bits.resp === 3'b110)

    //RXDAT
    when (rxdat.valid) {
      when(rxdat.bits.opcode === CompData) {
        state.w_grantfirst := true.B
        state.w_grantlast := rxdat.bits.last
        state.w_grant := req.off === 0.U || rxdat.bits.last  // TODO? why offset?
        gotT := rxrspIsUC || rxrspIsUD 
        gotDirty := gotDirty || rxrspIsUD
        gotGrantData := true.B

      }
    }

    //RXRSP for dataless
    when (rxrsp.valid) {
      when(rxdat.bits.opcode === Comp) {
        state.w_grantfirst := true.B
        state.w_grantlast := rxrsp.bits.last
        state.w_grant := req.off === 0.U || rxrsp.bits.last  // TODO? why offset?
        gotT := isUC
        gotDirty := false.B
      }
      when(rxrsp.bits.opcode === CompDBID) {
//        state.w_releaseack := true.B
        state.s_cbWrData := false.B
      }
      when(rxrsp.bits.opcode === RetryAck) {
        state.w_credit := false.B  
      }
      when(rxrsp.bits.opcode === PCrdGrant) {
        state.w_credit := true.B
        state.s_reIssue := true.B 
      }
    }

    //replay
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
      when(replResp.meta.clients.orR) {
        state.s_rprobe := false.B
        state.w_rprobeackfirst := false.B
        state.w_rprobeacklast := false.B
      }
    }
  }

  when (req_valid) {
    timer := timer + 1.U
  }

  val no_schedule = state.s_refill && state.s_probeack && state.s_release // && state.s_triggerprefetch.getOrElse(true.B)
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

  assert(!(c_resp.valid && !io.status.bits.w_c_resp))
  assert(!(d_resp.valid && !io.status.bits.w_d_resp))

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

  /* ======== Performance counters ======== */
  // time stamp
  // if (cacheParams.enablePerf) {
    val acquire_ts = RegEnable(timer, false.B, io.tasks.txreq.fire)
    val probe_ts = RegEnable(timer, false.B, io.tasks.source_b.fire)
    val release_ts = RegEnable(timer, false.B, !mp_grant_valid && mp_wbfull_valid && io.tasks.mainpipe.ready)
    val acquire_period = IO(Output(UInt(64.W)))
    val probe_period = IO(Output(UInt(64.W)))
    val release_period = IO(Output(UInt(64.W)))
    acquire_period := timer - acquire_ts
    probe_period := timer - probe_ts
    release_period := timer - release_ts
  // }
}
