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

package openLLC

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2chi._
import coupledL2.tl2chi.CHICohStates._
import utility._

class MainPipe(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    /* receive incoming task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new Task()))

    /* get meta at stage 3 */
    val dirResp_s3 = Input(new DirResult())

    /* update self/client directory at stage 3 */
    val dirWReq_s3 = new DirWriteIO()

    /* get refill data at stage 4 */
    val refillBufResp_s4 = Input(new DSBlock())

    /* send allocation request to RefillUnit at stage 4 */
    val refillReq_s4 = ValidIO(new RefillRequest())

    /* send Snoop request via upstream TXSNP channel */
    val snoopTask_s4 = ValidIO(new Task())

    /* send ReadNoSnp/WriteNoSnp task to MemUnit */
    val toMemUnit = new Bundle() {
      val alloc_s4 = ValidIO(new MemRequest(withData = false))
      val alloc_s6 = ValidIO(new MemRequest(withData = true))
    }

    /* send CompDBIDResp/CompData/Comp allocation request to ResponseUnit */
    val toResponseUnit = new Bundle() {
      val alloc_s4 = ValidIO(new ResponseRequest(withData = false))
      val alloc_s6 = ValidIO(new ResponseRequest(withData = true))
    }

    /* interact with datastorage */
    val toDS_s4 = new Bundle() {
      val read  = ValidIO(new DSRequest())
      val write = ValidIO(new DSRequest())
      val wdata = Output(new DSBlock())
    }
    val rdataFromDS_s6 = Input(new DSBlock())

    /* status of each pipeline stage */
    val pipeInfo = Output(new PipeStatus())
  })

  val snp_s4    = io.snoopTask_s4
  val refill_s4 = io.refillReq_s4
  val mem_s4    = io.toMemUnit.alloc_s4
  val comp_s4   = io.toResponseUnit.alloc_s4
  val mem_s6    = io.toMemUnit.alloc_s6
  val comp_s6   = io.toResponseUnit.alloc_s6

  val refillData_s4 = io.refillBufResp_s4
  val rdata_s6      = io.rdataFromDS_s6
  val pipeInfo      = io.pipeInfo

  /* Stage 2 */
  val task_s2 = io.taskFromArb_s2

  pipeInfo.s2_valid := task_s2.valid
  pipeInfo.s2_tag := task_s2.bits.tag
  pipeInfo.s2_set := task_s2.bits.set
  pipeInfo.s2_reqID := task_s2.bits.reqID

  /* Stage 3 */
  val task_s3 = RegInit(0.U.asTypeOf(Valid(new Task())))
  task_s3.valid := task_s2.valid
  when(task_s2.valid) {
    task_s3.bits := task_s2.bits
  }

  val selfDirResp_s3    = io.dirResp_s3.self
  val clientsDirResp_s3 = io.dirResp_s3.clients
  val self_meta_s3      = selfDirResp_s3.meta
  val clients_meta_s3   = clientsDirResp_s3.meta

  val req_s3         = task_s3.bits
  val opcode_s3      = req_s3.chiOpcode
  val refill_task_s3 = req_s3.refillTask
  val srcID_s3       = req_s3.srcID
  val passDirty_s3   = req_s3.resp(2) // Resp[2: 0] = {PassDirty, CacheState[1: 0]}

  val self_hit_s3       = selfDirResp_s3.hit
  val selfDirty_s3      = self_meta_s3.dirty
  val clients_hit_s3    = clientsDirResp_s3.hit
  val originalRN_hit_s3 = clients_hit_s3 && clients_meta_s3(srcID_s3).valid
  val peerRNs_hit_s3    = Cat(clients_meta_s3.zipWithIndex.map { case (meta, i) =>
    Mux(i.U =/= srcID_s3, clients_meta_s3(i).valid, false.B) 
  }).orR && clients_hit_s3

  if (inclusion == "Exclusive") {
    assert(!(self_hit_s3 && clients_hit_s3), "Non-exclusive?")
  }

  val readNotSharedDirty_s3 = !refill_task_s3 && opcode_s3 === ReadNotSharedDirty
  val readUnique_s3         = !refill_task_s3 && opcode_s3 === ReadUnique
  val makeUnique_s3         = !refill_task_s3 && opcode_s3 === MakeUnique
  val writeBackFull_s3      = !refill_task_s3 && opcode_s3 === WriteBackFull
  val evict_s3              = !refill_task_s3 && opcode_s3 === Evict
  val makeInvalid_s3        = !refill_task_s3 && opcode_s3 === MakeInvalid
  val cleanInvalid_s3       = !refill_task_s3 && opcode_s3 === CleanInvalid
  val cleanShared_s3        = !refill_task_s3 && opcode_s3 === CleanShared

  /**
    * Requests have different coherence states after processing
    * 1. Exclusive: all cache lines except owned by the requester will be invalidated
    * 2. Shared: the required block is held by multiple RNs
    * 3. Release: the cache line in the requester is no longer valid
    * 4. Invalid: the cache lines in the RNs and HN are no longer valid
    * 5. Clean: the cache lines in the RNs and HN are no longer dirty
    */
  val exclusiveReq_s3 = readUnique_s3 || readNotSharedDirty_s3 && !peerRNs_hit_s3 || makeUnique_s3
  val sharedReq_s3    = readNotSharedDirty_s3 && peerRNs_hit_s3
  val releaseReq_s3   = writeBackFull_s3 || evict_s3
  val invalidReq_s3   = makeInvalid_s3 || cleanInvalid_s3
  val cleanReq_s3     = cleanInvalid_s3 || cleanShared_s3

  /** Directory update logic **/
  val clientsTagW_s3  = io.dirWReq_s3.clientTagWReq
  val clientsMetaW_s3 = io.dirWReq_s3.clientMetaWReq
  val selfTagW_s3     = io.dirWReq_s3.selfTagWReq
  val selfMetaW_s3    = io.dirWReq_s3.selfMetaWReq

  val reqLineAddr_s3 = Cat(req_s3.tag, req_s3.set)

  // Final meta to be written
  val new_self_meta_s3 = WireInit(self_meta_s3)
  when(refill_task_s3) {
    new_self_meta_s3.valid := true.B
    new_self_meta_s3.dirty := passDirty_s3 || self_hit_s3 && selfDirty_s3
  }
  when(exclusiveReq_s3 || invalidReq_s3) {
    new_self_meta_s3.valid := false.B
  }
  when(cleanReq_s3) {
    new_self_meta_s3.dirty := false.B
  }

  val new_clients_meta_s3 = WireInit(clients_meta_s3)
  new_clients_meta_s3.zipWithIndex.foreach { case (meta, i) =>
    when(srcID_s3 === i.U) {
      meta.valid := !(releaseReq_s3 || invalidReq_s3)
    }.otherwise {
      when(exclusiveReq_s3 || invalidReq_s3) {
        meta.valid := false.B
      }
    }
  }

  /*** Client directory update ***/
  // tagArray is updated when directory access of an exclusive request does not hit
  clientsTagW_s3.valid := task_s3.valid && exclusiveReq_s3 && !clients_hit_s3
  clientsTagW_s3.bits.apply(
    lineAddr = reqLineAddr_s3,
    way = clientsDirResp_s3.way
  )
  /**
    * metaArray is updated when:
    * 1. shared request where requester does not have the required block
    * 2. exclusive request where requestor does not have the required block 
    *    or peer-RNs have required block
    * 3. release request where requester has the required block
    * 4. invalid request where peer-RNs have required block
    */
  clientsMetaW_s3.valid := task_s3.valid && (
    sharedReq_s3 && !originalRN_hit_s3 ||
    exclusiveReq_s3 && (peerRNs_hit_s3 || !originalRN_hit_s3) ||
    releaseReq_s3 && originalRN_hit_s3 ||
    invalidReq_s3 && peerRNs_hit_s3
  )
  clientsMetaW_s3.bits.apply(
    lineAddr = reqLineAddr_s3,
    wayOH = UIntToOH(clientsDirResp_s3.way),
    data = new_clients_meta_s3
  )

  /*** Local Directory update ***/
  // tagArray is updated when directory access of a refill task does not hit
  selfTagW_s3.valid := task_s3.valid && refill_task_s3 && !selfDirResp_s3.hit
  selfTagW_s3.bits.apply(
    lineAddr = reqLineAddr_s3,
    way = selfDirResp_s3.way
  )
  /**
    * metaArray is updated when:
    * 1. exclusive/invalid request where the directory access hits
    * 2. clean request where the required block is dirty
    * 3. refill task
    */
  selfMetaW_s3.valid := task_s3.valid && (
    (exclusiveReq_s3 || invalidReq_s3) && self_hit_s3 ||
    cleanReq_s3 && self_hit_s3 && selfDirty_s3 ||
    refill_task_s3
  )
  selfMetaW_s3.bits.apply(
    lineAddr = reqLineAddr_s3,
    wayOH = UIntToOH(selfDirResp_s3.way),
    data = new_self_meta_s3
  )

  // assert(
  //   !releaseReq_s3 || releaseReq_s3 && clients_meta_s3(srcID_s3).valid,
  //   "Non-exist block release?(addr: 0x%x)",
  //   Cat(reqLineAddr_s3, req_s3.bank, req_s3.off)
  // )

  pipeInfo.s3_valid := task_s3.valid
  pipeInfo.s3_tag := task_s3.bits.tag
  pipeInfo.s3_set := task_s3.bits.set
  pipeInfo.s3_reqID := task_s3.bits.reqID

  /* Stage 4 */
  val task_s4 = RegInit(0.U.asTypeOf(Valid(new Task())))
  task_s4.valid := task_s3.valid
  when(task_s3.valid) {
    task_s4.bits := task_s3.bits
  }

  val selfDirResp_s4    = RegEnable(selfDirResp_s3, 0.U.asTypeOf(selfDirResp_s3), task_s3.valid)
  val clientsDirResp_s4 = RegEnable(clientsDirResp_s3, 0.U.asTypeOf(clientsDirResp_s3), task_s3.valid)

  val readNotSharedDirty_s4 = RegNext(readNotSharedDirty_s3, false.B)
  val readUnique_s4         = RegNext(readUnique_s3, false.B)
  val makeUnique_s4         = RegNext(makeUnique_s3, false.B)
  val writeBackFull_s4      = RegNext(writeBackFull_s3, false.B)
  val evict_s4              = RegNext(evict_s3, false.B)
  val makeInvalid_s4        = RegNext(makeInvalid_s3, false.B)
  val cleanInvalid_s4       = RegNext(cleanInvalid_s3, false.B)
  val cleanShared_s4        = RegNext(cleanShared_s3, false.B)
  val sharedReq_s4          = RegNext(sharedReq_s3, false.B)
  val exclusiveReq_s4       = RegNext(exclusiveReq_s3, false.B)
  val releaseReq_s4         = RegNext(releaseReq_s3, false.B)
  val invalidReq_s4         = RegNext(invalidReq_s3, false.B)
  val cleanReq_s4           = RegNext(cleanReq_s3, false.B)
  val peerRNs_hit_s4        = RegNext(peerRNs_hit_s3, false.B)

  val req_s4          = task_s4.bits
  val refill_task_s4  = req_s4.refillTask
  val srcID_s4        = req_s4.srcID
  val opcode_s4       = req_s4.chiOpcode
  val self_hit_s4     = selfDirResp_s4.hit
  val self_meta_s4    = selfDirResp_s4.meta
  val clients_hit_s4  = clientsDirResp_s4.hit
  val clients_meta_s4 = clientsDirResp_s4.meta
  val selfDirty_s4    = self_meta_s4.dirty

  /** Send Snoop task **/
  val clients_valids_vec_s4 = VecInit(clients_meta_s4.map(_.valid))
  val peerRNs_valids_vec_s4 = VecInit(clients_valids_vec_s4.zipWithIndex.map { case (valid, i) =>
    Mux(i.U === srcID_s4, false.B, valid)
  })

  val clients_meta_conflict_s4 = !clients_hit_s4 && clients_valids_vec_s4.asUInt.orR &&
    (readNotSharedDirty_s4 || readUnique_s4 || makeUnique_s4)
  /**
    * snoop occurs when:
    * 1. conflict miss of client directory
    * 2. exclusive/invalid request where peer-RNs have required block
    * 3. shared read where local cache miss
    * 4. clean request where the target cache block exists only in one peerRN
    */
  val unique_peerRN_s4 = !self_hit_s4 && peerRNs_hit_s4 && PopCount(clients_valids_vec_s4) === 1.U
  val replace_snoop_s4 = clients_meta_conflict_s4
  val request_snoop_s4 = (exclusiveReq_s4 || invalidReq_s4) && peerRNs_hit_s4 || sharedReq_s4 && !self_hit_s4 ||
    cleanReq_s4 && unique_peerRN_s4
  val need_snoop_s4 = replace_snoop_s4 || request_snoop_s4
  val snp_address_s4 = Mux(
    replace_snoop_s4,
    Cat(clientsDirResp_s4.tag, clientsDirResp_s4.set, req_s4.bank, req_s4.off),
    Cat(req_s4.tag, req_s4.set, req_s4.bank, req_s4.off)
  )

  val snp_task_s4 = WireInit(0.U.asTypeOf(req_s4))
  snp_task_s4.tag := parseAddress(snp_address_s4)._1
  snp_task_s4.set := parseAddress(snp_address_s4)._2
  snp_task_s4.bank := parseAddress(snp_address_s4)._3
  snp_task_s4.replSnp := replace_snoop_s4
  snp_task_s4.txnID := req_s4.reqID
  snp_task_s4.doNotGoToSD := true.B
  snp_task_s4.snpVec := VecInit(
    Mux(
      replace_snoop_s4,
      clients_valids_vec_s4.asUInt,
      peerRNs_valids_vec_s4.asUInt
    ).asBools
  )
  snp_task_s4.chiOpcode := Mux(
    replace_snoop_s4,
    SnpUnique,
    MuxLookup(
      Cat(readUnique_s4, readNotSharedDirty_s4, makeUnique_s4 || makeInvalid_s4, cleanInvalid_s4, cleanShared_s4),
      SnpUnique
    )(
      Seq(
        Cat(false.B, false.B, false.B, false.B, true.B) -> SnpCleanShared,
        Cat(false.B, false.B, false.B, true.B, false.B) -> SnpCleanInvalid,
        Cat(false.B, false.B, true.B, false.B, false.B) -> SnpMakeInvalid,
        Cat(false.B, true.B, false.B, false.B, false.B) -> SnpNotSharedDirty,
        Cat(true.B, false.B, false.B, false.B, false.B) -> SnpUnique
      )
    )
  )
  snp_task_s4.retToSrc := Mux(
    !replace_snoop_s4,
    Mux(makeUnique_s4 || makeInvalid_s4 || cleanInvalid_s4 || cleanShared_s4, false.B, !self_hit_s4),
    true.B
  )

  snp_s4.valid := task_s4.valid && need_snoop_s4
  snp_s4.bits := snp_task_s4

  /** Send refill task **/
  // Cache allocation policy: EXCLUSIVE for single-core, and INCLUSIVE for multi-core
  // Data blocks are written to local cache only when an upper-level cache writes them back,
  // or when they are shared among multiple cores
  val snpVec_refill_s4 = VecInit(
    Mux(
      replace_snoop_s4,
      clients_valids_vec_s4.asUInt,
      Mux(
        sharedReq_s4,
        peerRNs_valids_vec_s4.asUInt,
        Cat(Seq.fill(numRNs)(false.B))
      )
    ).asBools
  )
  refill_s4.valid := task_s4.valid && (
    (sharedReq_s4 || writeBackFull_s4) && !self_hit_s4 ||
    replace_snoop_s4
  )
  refill_s4.bits.state.s_refill := false.B
  refill_s4.bits.state.w_datRsp := false.B
  refill_s4.bits.state.w_snpRsp := !Cat(snpVec_refill_s4).orR
  refill_s4.bits.task := req_s4
  refill_s4.bits.task.tag := parseAddress(snp_address_s4)._1
  refill_s4.bits.task.set := parseAddress(snp_address_s4)._2
  refill_s4.bits.task.refillTask := true.B
  refill_s4.bits.task.snpVec := snpVec_refill_s4
  refill_s4.bits.task.replSnp := replace_snoop_s4
  refill_s4.bits.dirResult.self := selfDirResp_s4
  refill_s4.bits.dirResult.clients := clientsDirResp_s4

  /** Comp task to ResponseUnit **/
  val respSC_s4 = sharedReq_s4
  val respUC_s4 = makeUnique_s4 || !makeUnique_s4 && exclusiveReq_s4 && (!selfDirty_s4 || !self_hit_s4)
  val respUD_s4 = !makeUnique_s4 && exclusiveReq_s4 && self_hit_s4 && selfDirty_s4
  val respI_s4  = releaseReq_s4 || invalidReq_s4 || cleanReq_s4
  val snpVec_comp_s4 = VecInit(
    Mux(
      request_snoop_s4,
      peerRNs_valids_vec_s4.asUInt,
      Cat(Seq.fill(numRNs)(false.B))
    ).asBools
  )
  val comp_task_s4 = WireInit(req_s4)
  comp_task_s4.snpVec := snpVec_comp_s4
  comp_task_s4.replSnp := replace_snoop_s4
  comp_task_s4.tgtID := srcID_s4
  comp_task_s4.homeNID := req_s4.tgtID
  comp_task_s4.dbID := req_s4.reqID
  comp_task_s4.resp := ParallelPriorityMux(
    Seq(respSC_s4, respUC_s4, respUD_s4, respI_s4),
    Seq(SC, UC, UD_PD, I)
  )

  comp_s4.valid := task_s4.valid && (
    releaseReq_s4 || invalidReq_s4 || cleanReq_s4 || makeUnique_s4 ||
    (readNotSharedDirty_s4 || readUnique_s4) && !self_hit_s4
  )
  comp_s4.bits.state.s_comp := false.B
  comp_s4.bits.state.s_urgentRead := true.B
  comp_s4.bits.state.w_datRsp := !(readNotSharedDirty_s4 || readUnique_s4)
  comp_s4.bits.state.w_snpRsp := !Cat(snpVec_comp_s4).orR
  comp_s4.bits.state.w_compack := !(readUnique_s4 || readNotSharedDirty_s4 || makeUnique_s4)
  comp_s4.bits.state.w_comp := !(cleanInvalid_s4 && self_hit_s4 && selfDirty_s4)
  comp_s4.bits.task := comp_task_s4

  /**  Read/Write request to MemUnit **/
  val mem_task_s4 = WireInit(req_s4)
  mem_task_s4.tag := Mux(refill_task_s4, selfDirResp_s4.tag, req_s4.tag)
  mem_task_s4.set := Mux(refill_task_s4, selfDirResp_s4.set, req_s4.set)
  mem_task_s4.txnID := req_s4.reqID
  mem_task_s4.homeNID := req_s4.tgtID
  mem_task_s4.chiOpcode := Mux(refill_task_s4 || cleanReq_s4, WriteNoSnpFull, ReadNoSnp)
  mem_task_s4.size := log2Ceil(64).U
  mem_task_s4.allowRetry := true.B
  mem_task_s4.order := OrderEncodings.None // TODO: order requirement?
  mem_task_s4.memAttr := MemAttr()
  mem_task_s4.snpAttr := false.B
  mem_task_s4.expCompAck := false.B

  // need ReadNoSnp/WriteNoSnp downwards
  val memRead_s4 = (readNotSharedDirty_s4 || readUnique_s4) && !self_hit_s4 && !peerRNs_hit_s4
  val memWrite_s4 = cleanReq_s4 && unique_peerRN_s4
  mem_s4.valid := task_s4.valid && (memRead_s4 || memWrite_s4)
  mem_s4.bits.state.s_issueReq := false.B
  mem_s4.bits.state.s_issueDat := !memWrite_s4
  mem_s4.bits.state.w_datRsp := !memWrite_s4
  mem_s4.bits.state.w_dbid := !memWrite_s4
  mem_s4.bits.state.w_comp := !memWrite_s4
  mem_s4.bits.task := mem_task_s4

  /** DS read/write **/
  val dataUnready_s4 = (readNotSharedDirty_s4 || readUnique_s4) && self_hit_s4
  val cleanSelfDirty_s4 = refill_task_s4 && !self_hit_s4 && self_meta_s4.valid && selfDirty_s4 ||
    cleanReq_s4 && self_hit_s4 && selfDirty_s4

  io.toDS_s4.read.valid := task_s4.valid && (dataUnready_s4 || cleanSelfDirty_s4)
  io.toDS_s4.read.bits.way := selfDirResp_s4.way
  io.toDS_s4.read.bits.set := selfDirResp_s4.set
  io.toDS_s4.write.valid := task_s4.valid && refill_task_s4
  io.toDS_s4.write.bits.way := selfDirResp_s4.way
  io.toDS_s4.write.bits.set := selfDirResp_s4.set
  io.toDS_s4.wdata := refillData_s4

  val req_drop_s4 = !dataUnready_s4 && !cleanSelfDirty_s4

  pipeInfo.s4_valid := task_s4.valid
  pipeInfo.s4_tag := task_s4.bits.tag
  pipeInfo.s4_set := task_s4.bits.set
  pipeInfo.s4_reqID := task_s4.bits.reqID

  /* Stage 5 */
  val task_s5 = RegInit(0.U.asTypeOf(Valid(new Task())))
  task_s5.valid := task_s4.valid && !req_drop_s4
  when(task_s4.valid && !req_drop_s4) {
    task_s5.bits := Mux(dataUnready_s4, comp_task_s4, mem_task_s4)
  }

  pipeInfo.s5_valid := task_s5.valid
  pipeInfo.s5_tag := task_s5.bits.tag
  pipeInfo.s5_set := task_s5.bits.set
  pipeInfo.s5_reqID := task_s5.bits.reqID

  /* Stage 6 */
  val task_s6 = RegInit(0.U.asTypeOf(Valid(new Task())))
  val cleanSelfDirty_s6 = RegNextN(cleanSelfDirty_s4, 2, Some(false.B))
  task_s6.valid := task_s5.valid
  when(task_s5.valid) {
    task_s6.bits := task_s5.bits
  }

  val req_s6 = task_s6.bits

  // Return CompData when local cache access hits
  comp_s6.valid := task_s6.valid && !cleanSelfDirty_s6
  comp_s6.bits.state.s_comp := false.B
  comp_s6.bits.state.s_urgentRead := true.B
  comp_s6.bits.state.w_datRsp := true.B
  comp_s6.bits.state.w_snpRsp := !Cat(req_s6.snpVec).orR
  comp_s6.bits.state.w_compack := false.B
  comp_s6.bits.state.w_comp := true.B
  comp_s6.bits.task := req_s6
  comp_s6.bits.data.get := rdata_s6

  // Update memory when a dirty block is cleaned
  mem_s6.valid := task_s6.valid && cleanSelfDirty_s6
  mem_s6.bits.state.s_issueReq := false.B
  mem_s6.bits.state.s_issueDat := false.B
  mem_s6.bits.state.w_datRsp := true.B
  mem_s6.bits.state.w_dbid := false.B
  mem_s6.bits.state.w_comp := false.B
  mem_s6.bits.task := req_s6
  mem_s6.bits.data.get := rdata_s6

  pipeInfo.s6_valid := task_s6.valid
  pipeInfo.s6_tag := task_s6.bits.tag
  pipeInfo.s6_set := task_s6.bits.set
  pipeInfo.s6_reqID := task_s6.bits.reqID
}
