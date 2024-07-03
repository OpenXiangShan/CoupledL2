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

class MainPipe(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /* receive incoming task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new Task()))

    /* get meta at stage 3 */
    val dirResp_s3 = Input(new DirResult())

    /* update self/client directory at stage 3 */
    val dirWReq_s3 = new DirWriteIO()

    /* get RefillBuffer read result at stage 4 */
    val refillBufResp_s4 = Flipped(new MSHRBufResp())

    /* send allocation request to MSHRCtl at stage 4 */
    val toMSHRCtl = new Bundle() {
      val mshr_alloc_s4 = ValidIO(new MSHRRequest())
    }

    /* send Snoop request via upstream TXSNP channel */
    val toTXSNP = new Bundle() {
      val task_s4 = DecoupledIO(new Task())
    }

    /* send ReadNoSnp/WriteNoSnp task to RequestUnit */
    val toRequestUnit = new Bundle() {
      val task_s4 = DecoupledIO(new Task())
    }

    /* send CompDBIDResp/CompData task to ResponseUnit */
    val toResponseUnit = new Bundle() {
      val compDBIDResp_s4 = DecoupledIO(new Task())
      val compData_s6 = DecoupledIO(new TaskWithData())
    }

    /* interact with datastorage */
    val toDS = new Bundle() {
      val read_s4 = ValidIO(new DSRequest())
      val write_s4 = ValidIO(new DSRequest())
      val wdata_s4 = Output(new DSBlock())
    }
    val rdataFromDS_s6 = Input(new DSBlock())
  })

  /* Stage 2 */
  val task_s2 = io.taskFromArb_s2

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
  val refill_task_s3 = req_s3.mshrTask
  val srcID_s3       = req_s3.srcID
  val passDirty_s3   = req_s3.resp(2) // Resp[2: 0] = {PassDirty, CacheState[1: 0]}

  val self_hit_s3       = selfDirResp_s3.hit
  val originalRN_hit_s3 = clientsDirResp_s3.hit && clients_meta_s3(srcID_s3).valid
  val peerRNs_hit_s3    = Cat(clients_meta_s3.zipWithIndex.map { case (meta, i) =>
    Mux(i.U =/= srcID_s3, clients_meta_s3(i).valid, false.B) 
  }).orR && clientsDirResp_s3.hit

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
  clientsTagW_s3.valid := task_s3.valid && exclusiveReq_s3 && !clientsDirResp_s3.hit
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

  assert(
    !releaseReq_s3 || releaseReq_s3 && clients_meta_s3(srcID_s3).valid,
    "Non-exist block release?(addr: 0x%d)",
    reqLineAddr_s3
  )

  io.toDS := DontCare
  io.toMSHRCtl := DontCare
  io.toTXSNP := DontCare
  io.toRequestUnit := DontCare
  io.toResponseUnit := DontCare
}
