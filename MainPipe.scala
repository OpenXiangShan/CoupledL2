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
import coupledL2.tl2chi.CHIOpcode.REQOpcodes._
import coupledL2.tl2chi.CHIOpcode.SNPOpcodes._
import coupledL2.tl2chi.CHIOpcode.RSPOpcodes._
import coupledL2.tl2chi.CHICohStates._
import utility._

class MainPipe(implicit p: Parameters) extends LLCModule {
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
    val refillTask_s4 = ValidIO(new RefillTask())

    /* send Snoop request via upstream TXSNP channel */
    val snoopTask_s4 = ValidIO(new Task())

    /* send ReadNoSnp/WriteNoSnp task to MemUnit */
    val toMemUnit = new Bundle() {
      val task_s4 = ValidIO(new Task())
      val task_s6 = ValidIO(new TaskWithData())
    }

    /* send CompDBIDResp/CompData task to ResponseUnit */
    val toResponseUnit = new Bundle() {
      val task_s4 = ValidIO(new Task())
      val task_s6 = ValidIO(new TaskWithData())
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

  val snpTask_s4    = io.snoopTask_s4
  val refillTask_s4 = io.refillTask_s4
  val readTask_s4   = io.toMemUnit.task_s4
  val compTask_s4   = io.toResponseUnit.task_s4
  val writeTask_s6  = io.toMemUnit.task_s6
  val compTask_s6   = io.toResponseUnit.task_s6

  val refillData_s4 = io.refillBufResp_s4
  val rdata_s6      = io.rdataFromDS_s6
  val pipeInfo      = io.pipeInfo

  /* Stage 2 */
  val task_s2 = io.taskFromArb_s2

  pipeInfo.s2_valid := task_s2.valid
  pipeInfo.s2_tag := task_s2.bits.tag
  pipeInfo.s2_set := task_s2.bits.set

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
  val clients_hit_s3    = clientsDirResp_s3.hit
  val originalRN_hit_s3 = clients_hit_s3 && clients_meta_s3(srcID_s3).valid
  val peerRNs_hit_s3    = Cat(clients_meta_s3.zipWithIndex.map { case (meta, i) =>
    Mux(i.U =/= srcID_s3, clients_meta_s3(i).valid, false.B) 
  }).orR && clients_hit_s3

  val readNotSharedDirty_s3 = !refill_task_s3 && opcode_s3 === ReadNotSharedDirty
  val readUnique_s3         = !refill_task_s3 && opcode_s3 === ReadUnique
  val makeUnique_s3         = !refill_task_s3 && opcode_s3 === MakeUnique
  val writeBackFull_s3      = !refill_task_s3 && opcode_s3 === WriteBackFull
  val evict_s3              = !refill_task_s3 && opcode_s3 === Evict

  /**
    * Requests have 3 different consistency states after processing
    * 1. Exclusive: all cache lines except owned by the requester will be invalidated
    * 2. Shared: the required block is held by multiple RNs
    * 3. Release: the cache line in the requester is no longer valid
    */
  val exclusiveReq_s3 = readUnique_s3 || readNotSharedDirty_s3 && !peerRNs_hit_s3 || makeUnique_s3
  val sharedReq_s3    = readNotSharedDirty_s3 && peerRNs_hit_s3
  val releaseReq_s3   = writeBackFull_s3 || evict_s3

  /** Directory update logic **/
  val clientsTagW_s3  = io.dirWReq_s3.clientTagWReq
  val clientsMetaW_s3 = io.dirWReq_s3.clientMetaWReq
  val selfTagW_s3     = io.dirWReq_s3.selfTagWReq
  val selfMetaW_s3    = io.dirWReq_s3.selfMetaWReq

  val reqLineAddr_s3 = Cat(req_s3.tag, req_s3.set)

  // Final meta to be written
  val new_self_meta_s3 = SelfMetaEntry(
    valid = refill_task_s3,
    dirty = Mux(refill_task_s3, passDirty_s3, false.B)
  )
  val new_clients_meta_s3 = WireInit(clients_meta_s3)
  new_clients_meta_s3.zipWithIndex.foreach { case (meta, i) =>
    when(srcID_s3 === i.U) {
      meta.valid := !releaseReq_s3
    }.otherwise {
      when(exclusiveReq_s3) {
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
    * 3. release request
    */
  clientsMetaW_s3.valid := task_s3.valid && (
    sharedReq_s3 && !originalRN_hit_s3 ||
    exclusiveReq_s3 && (peerRNs_hit_s3 || !originalRN_hit_s3) ||
    releaseReq_s3
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
    * 1. exclusive request where the directory access hits
    * 2. refill task
    */
  selfMetaW_s3.valid := task_s3.valid && (
    exclusiveReq_s3 && self_hit_s3 ||
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
  val sharedReq_s4          = RegNext(sharedReq_s3, false.B)
  val exclusiveReq_s4       = RegNext(exclusiveReq_s3, false.B)
  val releaseReq_s4         = RegNext(releaseReq_s3, false.B)
  val peersRNs_hit_s4       = RegNext(peerRNs_hit_s3, false.B)

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
    * 2. exclusive read where peer-RNs have required block
    * 3. shared read where local cache miss
    */
  val replace_snoop_s4 = clients_meta_conflict_s4
  val read_snoop_s4    = exclusiveReq_s4 && peersRNs_hit_s4 || sharedReq_s4 && !self_hit_s4
  val need_snoop_s4    = replace_snoop_s4 || read_snoop_s4
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
      Cat(readUnique_s4, readNotSharedDirty_s4, makeUnique_s4),
      SnpUnique
    )(
      Seq(
        Cat(false.B, false.B, true.B) -> SnpMakeInvalid,
        Cat(false.B, true.B, false.B) -> SnpNotSharedDirty,
        Cat(true.B, false.B, false.B) -> SnpUnique
      )
    )
  )
  snp_task_s4.retToSrc := Mux(
    !replace_snoop_s4,
    Mux(makeUnique_s4, false.B, !self_hit_s4),
    true.B
  )

  snpTask_s4.valid := task_s4.valid && need_snoop_s4
  snpTask_s4.bits := snp_task_s4

  /** Send refill task **/
  // Cache allocation policy: EXCLUSIVE for single-core, and INCLUSIVE for multi-core
  // Data blocks are written to local cache only when an upper-level cache writes them back,
  // or when they are shared among multiple cores
  refillTask_s4.valid := task_s4.valid && (
    (sharedReq_s4 || writeBackFull_s4) && !self_hit_s4 ||
    replace_snoop_s4
  )
  refillTask_s4.bits.dirResult.self := selfDirResp_s4
  refillTask_s4.bits.dirResult.clients := clientsDirResp_s4
  refillTask_s4.bits.task := req_s4
  refillTask_s4.bits.task.tag := parseAddress(snp_address_s4)._1
  refillTask_s4.bits.task.set := parseAddress(snp_address_s4)._2
  refillTask_s4.bits.task.refillTask := true.B
  refillTask_s4.bits.task.snpVec := VecInit(
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
  refillTask_s4.bits.task.replSnp := replace_snoop_s4

  /** Comp task to ResponseUnit **/
  val respSC_s4 = sharedReq_s4
  val respUC_s4 = makeUnique_s4 || !makeUnique_s4 && exclusiveReq_s4 && !selfDirty_s4
  val respUD_s4 = !makeUnique_s4 && exclusiveReq_s4 && self_hit_s4 && selfDirty_s4
  val respI_s4  = releaseReq_s4
  val comp_task_s4 = WireInit(req_s4)
  comp_task_s4.snpVec := VecInit(
    Mux(
      read_snoop_s4,
      peerRNs_valids_vec_s4.asUInt,
      Cat(Seq.fill(numRNs)(false.B))
    ).asBools
  )
  comp_task_s4.tgtID := srcID_s4
  comp_task_s4.homeNID := req_s4.tgtID
  comp_task_s4.dbID := req_s4.reqID
  comp_task_s4.resp := ParallelPriorityMux(
    Seq(respSC_s4, respUC_s4, respUD_s4, respI_s4),
    Seq(SC, UC, UD_PD, I)
  )

  compTask_s4.valid := task_s4.valid &&
    (releaseReq_s4 || (readNotSharedDirty_s4 || readUnique_s4) && !self_hit_s4 || makeUnique_s4)
  compTask_s4.bits := comp_task_s4

  /**  Read task to MemUnit **/
  val mem_task_s4 = WireInit(req_s4)
  mem_task_s4.tag := Mux(refill_task_s4, selfDirResp_s4.tag, req_s4.tag)
  mem_task_s4.set := Mux(refill_task_s4, selfDirResp_s4.set, req_s4.set)
  mem_task_s4.txnID := req_s4.reqID
  mem_task_s4.homeNID := req_s4.tgtID
  mem_task_s4.chiOpcode := Mux(refill_task_s4, WriteNoSnpFull, ReadNoSnp)
  mem_task_s4.size := log2Ceil(64).U
  mem_task_s4.allowRetry := false.B
  mem_task_s4.order := OrderEncodings.None // TODO: order requirement?
  mem_task_s4.memAttr := MemAttr()
  mem_task_s4.snpAttr := false.B
  mem_task_s4.expCompAck := false.B

  // need ReadNoSnp downwards
  readTask_s4.valid := task_s4.valid && (readNotSharedDirty_s4 || readUnique_s4) &&
    !self_hit_s4 && !clients_hit_s4
  readTask_s4.bits := mem_task_s4

  /** DS read/write **/
  val dataUnready_s4      = (readNotSharedDirty_s4 || readUnique_s4) && self_hit_s4
  val repl_dirty_block_s4 = refill_task_s4 && !self_hit_s4 && self_meta_s4.valid && selfDirty_s4

  io.toDS_s4.read.valid := task_s4.valid && (dataUnready_s4 || repl_dirty_block_s4)
  io.toDS_s4.read.bits.way := selfDirResp_s4.way
  io.toDS_s4.read.bits.set := selfDirResp_s4.set
  io.toDS_s4.write.valid := task_s4.valid && refill_task_s4
  io.toDS_s4.write.bits.way := selfDirResp_s4.way
  io.toDS_s4.write.bits.set := selfDirResp_s4.set
  io.toDS_s4.wdata := refillData_s4

  val req_drop_s4 = !dataUnready_s4 && !repl_dirty_block_s4

  pipeInfo.s4_valid := task_s4.valid
  pipeInfo.s4_tag := task_s4.bits.tag
  pipeInfo.s4_set := task_s4.bits.set

  /* Stage 5 */
  val task_s5 = RegInit(0.U.asTypeOf(Valid(new Task())))
  task_s5.valid := task_s4.valid && !req_drop_s4
  when(task_s4.valid && !req_drop_s4) {
    task_s5.bits := Mux(dataUnready_s4, comp_task_s4, mem_task_s4)
  }

  pipeInfo.s5_valid := task_s5.valid
  pipeInfo.s5_tag := task_s5.bits.tag
  pipeInfo.s5_set := task_s5.bits.set

  /* Stage 6 */
  val task_s6 = RegInit(0.U.asTypeOf(Valid(new Task())))
  val repl_dirty_block_s6 = RegNextN(repl_dirty_block_s4, 2, Some(false.B))
  task_s6.valid := task_s5.valid
  when(task_s5.valid) {
    task_s6.bits := task_s5.bits
  }

  val req_s6 = task_s6.bits

  // Return CompData when local cache access hits
  compTask_s6.valid := task_s6.valid && !repl_dirty_block_s6
  compTask_s6.bits.task := req_s6
  compTask_s6.bits.data := rdata_s6

  // Update memory when a dirty block is replaced
  writeTask_s6.valid := task_s6.valid && repl_dirty_block_s6
  writeTask_s6.bits.task := req_s6
  writeTask_s6.bits.data := rdata_s6

  pipeInfo.s6_valid := task_s6.valid
  pipeInfo.s6_tag := task_s6.bits.tag
  pipeInfo.s6_set := task_s6.bits.set

}
