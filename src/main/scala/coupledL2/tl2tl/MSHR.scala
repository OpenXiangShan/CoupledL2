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

package coupledL2.tl2tl

import chisel3._
import chisel3.util._
import coupledL2.MetaData._
import utility.{MemReqSource, ParallelLookUp, ParallelPriorityMux}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters
import coupledL2._
import coupledL2.prefetch.{PfSource, PrefetchTrain}

class MSHRTasks(implicit p: Parameters) extends L2Bundle {
  // outer
  val source_a = DecoupledIO(new SourceAReq) // To AcquireUnit  // TODO: no need to use decoupled handshake
  val source_b = DecoupledIO(new SourceBReq)
  val mainpipe = DecoupledIO(new TaskBundle) // To Mainpipe (SourceC or SourceD)
  // val prefetchTrain = prefetchOpt.map(_ => DecoupledIO(new PrefetchTrain)) // To prefetcher
}

class MSHRResps(implicit p: Parameters) extends L2Bundle {
  val sink_c = Flipped(ValidIO(new RespInfoBundle))
  val sink_d = Flipped(ValidIO(new RespInfoBundle))
  // make sure that Acquire is sent after Release,
  // so resp from SourceC is needed to initiate Acquire
  val source_c = Flipped(ValidIO(new RespInfoBundle))
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

  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)

  val timer = RegInit(0.U(64.W)) // for performance analysis

  /* MSHR Allocation */
  val req_valid = RegInit(false.B)
  val req = RegInit(0.U.asTypeOf(new TaskBundle()))
  val dirResult = RegInit(0.U.asTypeOf(new DirResult()))
  val meta = dirResult.meta
  val initState = Wire(new FSMState())
  initState.elements.foreach(_._2 := true.B)
  val state = RegInit(new FSMState(), initState)

  when(io.alloc.valid) {
    req_valid := true.B
    state := io.alloc.bits.state
    dirResult := io.alloc.bits.dirResult
    req := io.alloc.bits.task
    gotT := false.B
    gotDirty := false.B
    gotGrantData := false.B
    probeDirty := false.B
    probeGotN := false.B
    denied := false.B
    corrupt := false.B
    timer := 1.U
  }

  /* ======== Enchantment ======== */
  val meta_pft = meta.prefetch.getOrElse(false.B)
  val meta_no_client = !meta.clients.orR

  val req_needT = needT(req.opcode, req.param)
  val req_acquire =
    req.opcode === AcquireBlock && req.fromA || req.opcode === AcquirePerm // AcquireBlock and Probe share the same opcode
  val req_acquirePerm = req.opcode === AcquirePerm
  val req_get = req.opcode === Get
  val req_prefetch = req.opcode === Hint

  val promoteT_normal = dirResult.hit && meta_no_client && meta.state === TIP
  val promoteT_L3 = !dirResult.hit && gotT
  val promoteT_alias = dirResult.hit && req.aliasTask.getOrElse(false.B) && (meta.state === TRUNK || meta.state === TIP)
  // under above circumstances, we grant T to L1 even if it wants B
  val req_promoteT = (req_acquire || req_get || req_prefetch) && (promoteT_normal || promoteT_L3 || promoteT_alias)

  assert(!(req_valid && req_prefetch && dirResult.hit), "MSHR can not receive prefetch hit req")

  /* ======== Task allocation ======== */
  // Theoretically, data to be released is saved in ReleaseBuffer, so Acquire can be sent as soon as req enters mshr
  io.tasks.source_a.valid := !state.s_acquire
  io.tasks.source_b.valid := !state.s_pprobe || !state.s_rprobe
  val mp_release_valid = !state.s_release && state.w_rprobeacklast && state.w_grantlast &&
    state.w_replResp // release after Grant to L1 sent and replRead returns

  val mp_probeack_valid = !state.s_probeack && state.w_pprobeacklast
  val mp_grant_valid = !state.s_refill && state.w_grantlast && state.w_rprobeacklast // [Alias] grant after rprobe done
  io.tasks.mainpipe.valid := mp_release_valid || mp_probeack_valid || mp_grant_valid
  // io.tasks.prefetchTrain.foreach(t => t.valid := !state.s_triggerprefetch.getOrElse(true.B))

  val a_task = {
    val oa = io.tasks.source_a.bits
    oa.tag := req.tag
    oa.set := req.set
    oa.off := req.off
    oa.source := io.id
    oa.opcode := Mux(
      req_acquirePerm && dirResult.hit,
      req.opcode,
      // Get or AcquireBlock
      AcquireBlock
    )
    oa.param := Mux(
      req_needT,
      Mux(dirResult.hit, BtoT, NtoT),
      NtoB
    )
    oa.size := req.size
    oa.reqSource := req.reqSource
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
  val mp_release, mp_probeack, mp_grant = Wire(new TaskBundle)
  val mp_release_task = {
    mp_release := 0.U.asTypeOf(new TaskBundle)
    mp_release.channel := req.channel
    mp_release.tag := dirResult.tag
    mp_release.set := req.set
    mp_release.off := 0.U
    mp_release.alias.foreach(_ := 0.U)
    mp_release.vaddr.foreach(_ := req.vaddr.getOrElse(0.U))
    mp_release.isKeyword.foreach(_ := false.B)
    // if dirty, we must ReleaseData
    // if accessed, we ReleaseData to keep the data in L3, for future access to be faster
    // [Access] TODO: consider use a counter
    mp_release.opcode := {
      cacheParams.releaseData match {
        case 0 => Mux(meta.dirty && meta.state =/= INVALID || probeDirty, ReleaseData, Release)
        case 1 => Mux(meta.dirty && meta.state =/= INVALID || probeDirty || meta.accessed, ReleaseData, Release)
        case 2 =>
          Mux(meta.prefetch.getOrElse(false.B) && !meta.accessed, Release, ReleaseData) // TODO: has problem with this
        case 3 => ReleaseData // best performance with HuanCun-L3
      }
    }
    mp_release.param := Mux(isT(meta.state), TtoN, BtoN)
    mp_release.size := 0.U(msgSizeBits.W)
    mp_release.sourceId := 0.U(sourceIdBits.W)
    mp_release.corrupt := false.B
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
    mp_release.dirty := meta.dirty && meta.state =/= INVALID || probeDirty
    mp_release.metaWen := false.B
    mp_release.meta := MetaEntry()
    mp_release.tagWen := false.B
    mp_release.dsWen := true.B // write refillData to DS
    mp_release.replTask := true.B
    mp_release.wayMask := 0.U(cacheParams.ways.W)
    mp_release.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_release.mergeA := false.B
    mp_release.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    mp_release.txChannel := 0.U
    mp_release
  }

  val mp_probeack_task = {
    mp_probeack := 0.U.asTypeOf(new TaskBundle)
    mp_probeack.channel := req.channel
    mp_probeack.tag := req.tag
    mp_probeack.set := req.set
    mp_probeack.off := req.off
    mp_probeack.alias.foreach(_ := 0.U)
    mp_probeack.vaddr.foreach(_ := req.vaddr.getOrElse(0.U))
    mp_probeack.isKeyword.foreach(_ := false.B)
    mp_probeack.opcode := Mux(
      meta.dirty && isT(meta.state) || probeDirty || req.needProbeAckData,
      ProbeAckData,
      ProbeAck
    )
    mp_probeack.param := ParallelLookUp(
      Cat(isT(meta.state), req.param(bdWidth - 1, 0)),
      Seq(
        Cat(false.B, toN) -> BtoN,
        Cat(true.B, toN) -> TtoN,
        Cat(true.B, toB) -> TtoB
      )
    )
    mp_probeack.size := 0.U(msgSizeBits.W)
    mp_probeack.sourceId := 0.U(sourceIdBits.W)
    mp_probeack.corrupt := false.B
    mp_probeack.bufIdx := 0.U(bufIdxBits.W)
    mp_probeack.needProbeAckData := false.B
    mp_probeack.mshrTask := true.B
    mp_probeack.mshrId := io.id
    mp_probeack.aliasTask.foreach(_ := false.B)
    mp_probeack.useProbeData := true.B // write [probeAckData] to DS, if not probed toN
    mp_probeack.mshrRetry := false.B
    mp_probeack.way := dirResult.way
    mp_probeack.fromL2pft.foreach(_ := false.B)
    mp_probeack.needHint.foreach(_ := false.B)
    mp_probeack.dirty := meta.dirty && meta.state =/= INVALID || probeDirty
    mp_probeack.meta := MetaEntry(
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
      alias = meta.alias, // [Alias] Keep alias bits unchanged
      prefetch = req.param =/= toN && meta_pft,
      accessed = req.param =/= toN && meta.accessed
    )
    mp_probeack.metaWen := true.B
    mp_probeack.tagWen := false.B
    mp_probeack.dsWen := req.param =/= toN && probeDirty
    mp_probeack.wayMask := 0.U(cacheParams.ways.W)
    mp_probeack.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_probeack.replTask := false.B
    mp_probeack.mergeA := false.B
    mp_probeack.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    mp_probeack.txChannel := 0.U
    mp_probeack
  }

  val mergeA = RegInit(false.B)
  when(io.aMergeTask.valid) {
    mergeA := true.B
  }.elsewhen(io.alloc.valid) {
    mergeA := false.B
  }
  val mp_grant_task = {
    mp_grant := 0.U.asTypeOf(new TaskBundle)
    mp_grant.channel := req.channel
    mp_grant.tag := req.tag
    mp_grant.set := req.set
    mp_grant.off := req.off
    mp_grant.sourceId := req.sourceId
    mp_grant.alias.foreach(_ := 0.U)
    mp_grant.vaddr.foreach(_ := req.vaddr.getOrElse(0.U))
    mp_grant.isKeyword.foreach(_ := req.isKeyword.getOrElse(false.B))
    mp_grant.opcode := odOpGen(req.opcode)
    mp_grant.param := Mux(
      req_get || req_prefetch,
      0.U, // Get -> AccessAckData
      MuxLookup( // Acquire -> Grant
        req.param,
        req.param
      )(
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
        !(dirResult.meta.state === BRANCH && req_needT))
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
    mp_grant.dsWen := gotGrantData || probeDirty && (req_get || req.aliasTask.getOrElse(false.B))
    mp_grant.fromL2pft.foreach(_ := req.fromL2pft.get)
    mp_grant.needHint.foreach(_ := false.B)
    mp_grant.replTask := !dirResult.hit // Get and Alias are hit that does not need replacement
    mp_grant.wayMask := 0.U(cacheParams.ways.W)
    mp_grant.mshrRetry := !state.s_retry
    mp_grant.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_grant.denied := denied
    mp_grant.corrupt := corrupt

    // Add merge grant task for Acquire and late Prefetch
    mp_grant.mergeA := mergeA || io.aMergeTask.valid
    val merge_task_r = RegEnable(io.aMergeTask.bits, 0.U.asTypeOf(new TaskBundle), io.aMergeTask.valid)
    val merge_task = Mux(io.aMergeTask.valid, io.aMergeTask.bits, merge_task_r)
    val merge_task_isKeyword = Mux(
      io.aMergeTask.valid,
      io.aMergeTask.bits.isKeyword.getOrElse(false.B),
      merge_task_r.isKeyword.getOrElse(false.B)
    )

    mp_grant.aMergeTask.off := merge_task.off
    mp_grant.aMergeTask.alias.foreach(_ := merge_task.alias.getOrElse(0.U))
    mp_grant.aMergeTask.vaddr.foreach(_ := merge_task.vaddr.getOrElse(0.U))
    mp_grant.aMergeTask.isKeyword.foreach(_ := merge_task_isKeyword)
    mp_grant.aMergeTask.opcode := odOpGen(merge_task.opcode)
    mp_grant.aMergeTask.param := MuxLookup( // Acquire -> Grant
      merge_task.param,
      merge_task.param
    )(
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
    mp_grant.txChannel := 0.U

    mp_grant
  }
  io.tasks.mainpipe.bits := ParallelPriorityMux(
    Seq(
      mp_grant_valid -> mp_grant,
      mp_release_valid -> mp_release,
      mp_probeack_valid -> mp_probeack
    )
  )
  io.tasks.mainpipe.bits.reqSource := req.reqSource
  io.tasks.mainpipe.bits.isKeyword.foreach(_ := req.isKeyword.getOrElse(false.B))
  // io.tasks.prefetchTrain.foreach {
  //   train =>
  //     train.bits.tag := req.tag
  //     train.bits.set := req.set
  //     train.bits.needT := req_needT
  //     train.bits.source := req.source
  // }

  /* ======== Task update ======== */
  when(io.tasks.source_a.fire) {
    state.s_acquire := true.B
  }
  when(io.tasks.source_b.fire) {
    state.s_pprobe := true.B
    state.s_rprobe := true.B
  }
  when(io.tasks.mainpipe.ready) {
    when(mp_grant_valid) {
      state.s_refill := true.B
      state.s_retry := true.B
    }.elsewhen(mp_release_valid) {
      state.s_release := true.B
      meta.state := INVALID
    }.elsewhen(mp_probeack_valid) {
      state.s_probeack := true.B
    }
  }
  // prefetchOpt.foreach {
  //   _ =>
  //     when (io.tasks.prefetchTrain.get.fire) {
  //       state.s_triggerprefetch.get := true.B
  //     }
  // }

  /* ======== Handling response ======== */
  val c_resp = io.resps.sink_c
  val d_resp = io.resps.sink_d
  when(c_resp.valid) {
    when(c_resp.bits.opcode === ProbeAck || c_resp.bits.opcode === ProbeAckData) {
      state.w_rprobeackfirst := true.B
      state.w_rprobeacklast := state.w_rprobeacklast || c_resp.bits.last
      state.w_pprobeackfirst := true.B
      state.w_pprobeacklast := state.w_pprobeacklast || c_resp.bits.last
      state.w_pprobeack := state.w_pprobeack || req.off === 0.U || c_resp.bits.last
    }
    when(c_resp.bits.opcode === ProbeAckData) {
      probeDirty := true.B
    }
    when(isToN(c_resp.bits.param)) {
      probeGotN := true.B
    }
  }

  when(d_resp.valid) {
    when(d_resp.bits.opcode === Grant || d_resp.bits.opcode === GrantData || d_resp.bits.opcode === AccessAck) {
      state.w_grantfirst := true.B
      state.w_grantlast := d_resp.bits.last
      state.w_grant := req.off === 0.U || d_resp.bits.last // TODO? why offset?
    }
    when(d_resp.bits.opcode === Grant || d_resp.bits.opcode === GrantData) {
      gotT := d_resp.bits.param === toT
      gotDirty := gotDirty || d_resp.bits.dirty
    }
    when(d_resp.bits.opcode === GrantData) {
      gotGrantData := true.B
      corrupt := d_resp.bits.corrupt
    }
    when(d_resp.bits.opcode === ReleaseAck) {
      state.w_releaseack := true.B
    }
  }

  val replResp = io.replResp.bits
  when(io.replResp.valid && replResp.retry) {
    state.s_refill := false.B
    state.s_retry := false.B
    dirResult.way := replResp.way
  }
  when(io.replResp.valid && !replResp.retry) {
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
    when(replResp.meta.state =/= INVALID) {
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

  when(req_valid) {
    timer := timer + 1.U
  }

  val no_schedule =
    state.s_refill && state.s_probeack && state.s_release // && state.s_triggerprefetch.getOrElse(true.B)
  val no_wait =
    state.w_rprobeacklast && state.w_pprobeacklast && state.w_grantlast && state.w_releaseack && state.w_replResp
  val will_free = no_schedule && no_wait
  when(will_free && req_valid) {
    req_valid := false.B
    timer := 0.U
  }

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
  io.msInfo.bits.aliasTask.foreach(_ := req.aliasTask.getOrElse(false.B))
  io.msInfo.bits.needRelease := !state.w_releaseack
  // if releaseTask is already in mainpipe_s1/s2, while a refillTask in mainpipe_s3, the refill should also be blocked and retry
  io.msInfo.bits.blockRefill := releaseNotSent || RegNext(releaseNotSent, false.B) || RegNext(
    RegNext(releaseNotSent, false.B),
    false.B
  )
  io.msInfo.bits.dirHit := dirResult.hit
  io.msInfo.bits.metaTag := dirResult.tag
  io.msInfo.bits.willFree := will_free
  io.msInfo.bits.isAcqOrPrefetch := req_acquire || req_prefetch
  io.msInfo.bits.isPrefetch := req_prefetch
  io.msInfo.bits.param := req.param
  io.msInfo.bits.mergeA := mergeA
  io.msInfo.bits.w_grantfirst := state.w_grantfirst
  io.msInfo.bits.s_refill := state.s_refill
  io.msInfo.bits.s_release := state.s_release
  io.msInfo.bits.s_cmoresp := false.B
  io.msInfo.bits.w_releaseack := state.w_releaseack
  io.msInfo.bits.w_replResp := state.w_replResp
  io.msInfo.bits.w_rprobeacklast := state.w_rprobeacklast
  io.msInfo.bits.replaceData := mp_release.opcode === ReleaseData
  io.msInfo.bits.releaseToB := false.B
  io.msInfo.bits.metaState := meta.state
  io.msInfo.bits.channel := req.channel

  assert(!(c_resp.valid && !io.status.bits.w_c_resp))
  assert(!(d_resp.valid && !io.status.bits.w_d_resp))

  /* ======== Handling Nested C ======== */
  // for A miss, only when replResp do we finally choose a way, allowing nested C
  // for A-alias, always allowing nested C (state.w_replResp === true.B)
  val nestedwb_match = req_valid && meta.state =/= INVALID &&
    dirResult.set === io.nestedwb.set &&
    dirResult.tag === io.nestedwb.tag &&
    state.w_replResp

  when(nestedwb_match) {
    when(io.nestedwb.c_set_dirty) {
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
  val acquire_ts = RegEnable(timer, false.B, io.tasks.source_a.fire)
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
