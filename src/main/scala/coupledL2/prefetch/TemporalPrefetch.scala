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

package coupledL2.prefetch

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import freechips.rocketchip.util.Random
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, MemReqSource, ParallelPriorityMux, SRAMTemplate}
import coupledL2.{HasCoupledL2Parameters, L2Bundle, TPHitFeedback, TPmetaL2Req, TPmetaL2ReqBundle, TPmetaL2Resp, TPmetaReq, TPmetaResp}
import coupledL2.utils.{ReplacementPolicy, XSPerfAccumulate}
//import huancun.{TPmetaReq, TPmetaResp}

case class TPParameters(
    tpTableEntries: Int = 16384 / 2,
    tpTableAssoc: Int = 16 / 4,
    vaddrBits: Int = 39,
    blockOffBits: Int = 6,
    dataReadQueueDepth: Int = 8,
    dataWriteQueueDepth: Int = 4,
    tpDataQueueDepth: Int = 8,
    triggerQueueDepth: Int = 4,
    throttleCycles: Int = 4,  // unused yet
    replacementPolicy: String = "tpbrrip",
    debug: Boolean = false
) extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val hasPrefetchSrc:  Boolean = true
  override val hasTP: Boolean = true
  override val inflightEntries: Int = 16
}

trait HasTPParams extends HasCoupledL2Parameters {
  val tpParams = prefetchOpt.get.asInstanceOf[TPParameters]
  val tpTableAssoc = tpParams.tpTableAssoc
  val tpTableNrSet = tpParams.tpTableEntries / tpTableAssoc
  val tpTableSetBits = log2Ceil(tpTableNrSet)
  val tpEntryMaxLen = tpParams.inflightEntries
  val tpTableReplacementPolicy = tpParams.replacementPolicy
  val debug = tpParams.debug
  val vaddrBits = tpParams.vaddrBits
  val blockOffBits = tpParams.blockOffBits
  val dataReadQueueDepth = tpParams.dataReadQueueDepth
  val dataWriteQueueDepth = tpParams.dataWriteQueueDepth
  val tpDataQueueDepth = tpParams.tpDataQueueDepth
  val triggerQueueDepth = tpParams.triggerQueueDepth
  val metaDataLength = fullAddressBits - offsetBits
  val tpPfEntries = log2Ceil(tpParams.tpTableEntries * tpParams.inflightEntries) // actually less
//  val tpThrottleCycles = tpParams.throttleCycles
//  require(tpThrottleCycles > 0, "tpThrottleCycles must be greater than 0")
}

abstract class TPBundle(implicit val p: Parameters) extends Bundle with HasTPParams
abstract class TPModule(implicit val p: Parameters) extends Module with HasTPParams

class tpMetaEntry(implicit p:Parameters) extends TPBundle {
  val valid = Bool()
  val triggerTag = UInt((vaddrBits-blockOffBits-tpTableSetBits).W)
  val l2ReqBundle = new TPmetaL2ReqBundle()
  // val tab = UInt(2.W)
}

class tpDataEntry(implicit p:Parameters) extends TPBundle {
  val rawData = Vec(tpEntryMaxLen, UInt(fullAddressBits.W))
  // val rawData_debug = Vec(tpEntryMaxLen, UInt(vaddrBits.W))
  // TODO: val compressedData = UInt(512.W)
}

class triggerBundle(implicit p: Parameters) extends TPBundle {
  val vaddr = UInt(vaddrBits.W)
  val paddr = UInt(fullAddressBits.W)
  val way = UInt(log2Ceil(tpTableAssoc).W)
  val replTag = UInt(tagBits.W)
}

class trainBundle(implicit p: Parameters) extends TPBundle {
  val vaddr = UInt(vaddrBits.W)
  val paddr = UInt(fullAddressBits.W)
  val hit = Bool()
  val prefetched = Bool()
  val pfsource = UInt(PfSource.pfSourceBits.W)
  val metahit = Bool()
}

class sendBundle(implicit p: Parameters) extends TPBundle {
  val paddr = UInt(fullAddressBits.W)
  val vaddr = UInt(vaddrBits.W)
}

class tpMetaRespBundle(implicit p: Parameters) extends TPBundle {
  val set = UInt(tpTableSetBits.W)
  val tag = UInt((vaddrBits - tpTableSetBits).W)
}
/*
class tpmetaPortIO(implicit p: Parameters) extends Bundle {
  val req = DecoupledIO(new TPmetaReq)
  val resp = Flipped(ValidIO(new TPmetaResp))
}
*/

class tpmetaL2PortIO(implicit p: Parameters) extends Bundle {
  val req = DecoupledIO(new TPmetaL2Req())
  val resp = Flipped(DecoupledIO(new TPmetaL2Resp))
}


class tpmetaRepl(implicit p: Parameters) extends Bundle {
  val hit = Bool()
  val replValid = Bool()
  val replEmpty = Bool()
}

/*
class tpmetaL3PortIO(implicit p: Parameters) extends Bundle {
  val req = DecoupledIO(new TPmetaL3Req())
  val resp = Flipped(ValidIO(new TPmetaL3Resp))
}
 */


/* VIVT, Physical Data */
class TemporalPrefetch(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val tpmeta_port = new tpmetaL2PortIO()
    val hartid = Input(UInt(hartIdLen.W))
    val tpHitFeedback = Flipped(DecoupledIO(new TPHitFeedback()))
  })

  def parseVaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, tpTableSetBits), x(tpTableSetBits-1, 0))
  }

  def parsePaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, tpTableSetBits+blockOffBits), x(tpTableSetBits+blockOffBits-1, blockOffBits))
  }

  def recoverVaddr(x: UInt): UInt = {
    (x << offsetBits.U).asUInt
  }

  val tpMetaTable = Module(
    new SRAMTemplate(new tpMetaEntry(), set = tpTableNrSet, way = tpTableAssoc, shouldReset = false, singlePort = true)
  )
  val dataReadQueue = Module(new Queue(new TPmetaReq(), dataReadQueueDepth, pipe = false, flow = false))
  val dataWriteQueue = Module(new Queue(new TPmetaReq(), dataWriteQueueDepth, pipe = false, flow = false))
  val tpDataQueue = Module(new Queue(new tpDataEntry(), tpDataQueueDepth + 1, pipe = false, flow = false))
  val triggerQueue = Module(new Queue(new triggerBundle(), triggerQueueDepth, pipe = false, flow = false))
  val tpMetaReadQueue = Module(new Queue(new tpMetaRespBundle(), dataReadQueueDepth * 2, pipe = false, flow = false))
  val tpMetaRespQueue = Module(new Queue(new tpMetaRespBundle(), dataReadQueueDepth, pipe = false, flow = false))
  val triggerQEmpty = !triggerQueue.io.deq.valid
  val triggerQFull = !triggerQueue.io.enq.ready
  val repl = ReplacementPolicy.fromString(tpTableReplacementPolicy, tpTableAssoc)
  val random_repl = tpTableReplacementPolicy == "random"
  val replacer_sram_opt = if (random_repl) None else
    Some(Module(new SRAMTemplate(UInt(repl.nBits.W), tpTableNrSet, 1, singlePort = true)))
  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((tpTableNrSet - 1).U)

  /* Constantin Parameters */

//  require(cacheParams.hartIds.size == 1)
  val hartid = if (cacheParams.hartIds.size == 0)  {0} else {cacheParams.hartIds.head}
  // 0 / 1: whether to enable temporal prefetcher
//  private val enableTP = WireInit(Constantin.createRecord("enableTP"+hartid.toString, initValue = 1.U))
  private val enableTP = WireInit(1.U)
  // 0 ~ N: throttle cycles for each prefetch request
//  private val tpThrottleCycles = WireInit(Constantin.createRecord("tp_throttleCycles"+hartid.toString, initValue = 4.U(3.W)))
  private val tpThrottleCycles = WireInit(4.U(3.W))
  // 0 / 1: whether request to set as trigger on meta hit
//  private val hitAsTrigger = WireInit(Constantin.createRecord("tp_hitAsTrigger"+hartid.toString, initValue = 1.U))
  private val hitAsTrigger = WireInit(1.U)
  // 1 ~ triggerQueueDepth: enqueue threshold for triggerQueue
//  private val triggerThres = WireInit(Constantin.createRecord("tp_triggerThres"+hartid.toString, initValue = 1.U(3.W)))
  private val triggerThres = WireInit(1.U(3.W))
  // 1 ~ tpEntryMaxLen: record threshold for recorder and sender (storage size will not be affected)
//  private val recordThres = WireInit(Constantin.createRecord("tp_recordThres"+hartid.toString, initValue = tpEntryMaxLen.U))
  private val recordThres = WireInit(tpEntryMaxLen.U)
  // 0 / 1: whether to train on vaddr
//  private val trainOnVaddr = WireInit(Constantin.createRecord("tp_trainOnVaddr"+hartid.toString, initValue = 0.U))
  private val trainOnVaddr = WireInit(0.U)
  // 0 / 1: whether to eliminate L1 prefetch request training
//  private val trainOnL1PF = WireInit(Constantin.createRecord("tp_trainOnL1PF"+hartid.toString, initValue = 0.U))
  private val trainOnL1PF = WireInit(0.U)

  if (vaddrBitsOpt.isEmpty) {
    assert(!trainOnVaddr)
  }

  /* hit feedback regulation  */
  val tpFbCtrl = RegInit((1 << tpPfEntries/2).asUInt(tpPfEntries.W))
  val tpFbResetCount = RegInit(0.U((tpPfEntries).W))
  val tpFbReset = tpFbResetCount(tpPfEntries - 2)
  val tpFbHit = Mux(tpFbCtrl.andR, false.B, io.tpHitFeedback.bits.latepf | io.tpHitFeedback.bits.hit)
  val tpFbMiss = Mux(tpFbCtrl.orR, io.tpHitFeedback.bits.replMiss, false.B)
  val tpDisable = !(tpFbCtrl(tpPfEntries - 1) | tpFbCtrl(tpPfEntries - 2))
  when (io.tpHitFeedback.valid) {
    tpFbCtrl := Mux(tpFbHit, tpFbCtrl + 1.U, Mux(tpFbMiss, tpFbCtrl - 1.U, tpFbCtrl))
  }.elsewhen (tpFbReset) {
    tpFbCtrl := (1 << tpPfEntries/2).asUInt
  }
  when (tpDisable && io.train.fire) {
    tpFbResetCount := tpFbResetCount + 1.U
  }.elsewhen(tpFbReset) {
    tpFbResetCount := 0.U
  }

  io.tpHitFeedback.ready := true.B

  /* Stage 0: query tpMetaTable */

  val s0_valid = io.train.fire && !tpDisable &&
    Mux(trainOnVaddr.orR, io.train.bits.vaddr.getOrElse(0.U) =/= 0.U, true.B) &&
    Mux(trainOnL1PF.orR, true.B, io.train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U) || tpMetaRespQueue.io.deq.fire
  val trainVaddr = io.train.bits.vaddr.getOrElse(0.U)
  val trainPaddr = io.train.bits.addr
  val (vtag_s0, vset_s0) = if (vaddrBitsOpt.nonEmpty) parseVaddr(trainVaddr) else (0.U, 0.U)
  val (ptag_s0, pset_s0) = parsePaddr(trainPaddr)
  val train_set_s0 = Mux(trainOnVaddr.orR, vset_s0, pset_s0)

  val tpMetaRespValid_s0 = tpMetaRespQueue.io.deq.fire
  val tpMetaResp_s0 = tpMetaRespQueue.io.deq.bits
  val metas = tpMetaTable.io.r(s0_valid, Mux(tpMetaRespValid_s0, tpMetaResp_s0.set, train_set_s0)).resp.data


  /* Stage 1: parse tpMeta to judge hit or miss, choose the victim */

  val s1_valid = RegNext(s0_valid, false.B)
  val train_s1 = RegEnable(io.train.bits, s0_valid)
  val tpMetaRespValid_s1 = RegEnable(tpMetaRespValid_s0, s0_valid)
  val tpMetaResp_s1 = RegEnable(tpMetaResp_s0, s0_valid)
  val trainVaddr_s1 = train_s1.vaddr.getOrElse(0.U)
  val trainPaddr_s1 = train_s1.addr
  val (vtag_s1, vset_s1) = if (vaddrBitsOpt.nonEmpty) parseVaddr(trainVaddr_s1) else (0.U, 0.U)
  val (ptag_s1, pset_s1) = parsePaddr(trainPaddr_s1)
  val train_tag_s1 = Mux(trainOnVaddr.orR, vtag_s1, ptag_s1)

  val tagMatchVec = metas.map(_.triggerTag === Mux(tpMetaRespValid_s1, tpMetaResp_s1.tag, train_tag_s1))
  val metaValidVec = metas.map(_.valid === true.B)
  val metaInvalidVec = metas.map(_.valid === false.B)

  /*
  val debug_metaValidVec = VecInit(metaValidVec.map(_ && s1_valid))
  val debug_tagVec = VecInit(metas.map(x => Mux(s1_valid, x.triggerTag, 0.U)))
  dontTouch(debug_metaValidVec)
  dontTouch(debug_tagVec)
  */

  /* Replacement logic */
  val repl_state_s1 = if(random_repl) {
    0.U
  } else {
    val repl_sram_r = replacer_sram_opt.get.io.r(s0_valid, Mux(trainOnVaddr.orR, vset_s1, pset_s1)).resp.data(0)
    val repl_state = RegEnable(repl_sram_r, 0.U(repl.nBits.W), s0_valid)
    repl_state
  }

  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)
  val victimWay = repl.get_replace_way(repl_state_s1 /*never mind for random*/)
  val invalidWay = ParallelPriorityMux(metaInvalidVec.zipWithIndex.map(x => x._1 -> x._2.U(log2Ceil(tpTableAssoc).W)))

  val hit_s1 = Cat(hitVec).orR
  val hasInvalidWay_s1 = Cat(metaInvalidVec).orR
  val way_s1 = Mux(hit_s1, hitWay, Mux(hasInvalidWay_s1, invalidWay, victimWay))
  val l2ReqBundle_s1 = metas(way_s1).l2ReqBundle

  val replTag_s1 = metas(way_s1 /*dont care hit*/).triggerTag
  val replTagValid_s1 = metas(way_s1 /*dont care hit*/).valid

    // /* test
  val repl_s1 = Wire(new tpmetaRepl())
  repl_s1.hit := hit_s1
  repl_s1.replValid := ~hit_s1 && metas(way_s1).valid
  repl_s1.replEmpty := ~hit_s1 && ~metas(way_s1).valid
  // */

  /* Stage 2: access tpData on meta hit, record it on meta miss */

  val s2_valid = RegNext(s1_valid, false.B)
  val tpMetaRespValid_s2 = RegEnable(tpMetaRespValid_s1, s1_valid)
  val tpMetaResp_s2 = RegEnable(tpMetaResp_s0, s1_valid)
  val hit_s2 = RegEnable(hit_s1, false.B, s1_valid)
  val hasInvalidWay_s2 = RegEnable(hasInvalidWay_s1, false.B, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val repl_state_s2 = RegEnable(repl_state_s1, s1_valid)
  val vset_s2 = RegEnable(vset_s1, s1_valid)
  val pset_s2 = RegEnable(pset_s1, s1_valid)
  val vtag_s2 = RegEnable(vtag_s1, s1_valid)
  val ptag_s2 = RegEnable(ptag_s1, s1_valid)
  val train_s2 = RegEnable(train_s1, s1_valid)
  val l2ReqBundle_s2 = RegEnable(l2ReqBundle_s1, s1_valid)
  val replTag_s2 = RegEnable(replTag_s1, s1_valid)
  val replTagValid_s2 = RegEnable(replTagValid_s1, s1_valid)
  val repl_s2 = RegEnable(repl_s1, s1_valid)

  val triggerEnq_s2 = s2_valid && !tpMetaRespValid_s2 && !triggerQFull && (Mux(hitAsTrigger.orR, hit_s2, false.B) || (triggerQueue.io.count < triggerThres))
  val dorecord_s2 = s2_valid && !tpMetaRespValid_s2 && !triggerEnq_s2 && !triggerQEmpty // && !hit_s2

  /* Update replacer*/
  val replacerWen = triggerEnq_s2

  val req_type = WireInit(0.U(5.W)) //same as Directory, wait for change
  // req_type[2]: hit
  // req_type[1]: replInvalid
  // req_type[0]: replValid
//  val lfsr = LFSR(16, true.B)
//  val random = Random(16, lfsr)
//  req_type := Cat(0.U(1.W), 0.U(1.W), hit_s2, hasInvalidWay_s2 && random.andR, triggerEnq_s2)
  req_type := Cat(0.U(1.W), 0.U(1.W), hit_s2, hasInvalidWay_s2, triggerEnq_s2)
  val set_s2 = Mux(trainOnVaddr.orR, vset_s2, pset_s2)

  if (tpTableReplacementPolicy == "srrip") {
    val next_state_s2 = repl.get_next_state(repl_state_s2, way_s2, hit_s2, hasInvalidWay_s2, req_type)
    val repl_init = Wire(Vec(tpTableAssoc, UInt(rrpvBits.W)))
    repl_init.foreach(_ := Fill(rrpvBits, 1.U(1.W)))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s2, repl_init.asUInt),
      Mux(resetFinish, set_s2, resetIdx),
      1.U
    )
  } else if (tpTableReplacementPolicy == "drrip") {
    val PSEL = RegInit(512.U(10.W))
    val setBits = log2Ceil(tpTableNrSet)
    val half_setBits = setBits >> 1
    val match_a = set_s2(setBits-1,setBits-half_setBits-1)===set_s2(setBits-half_setBits-1,0)
    val match_b = set_s2(setBits-1,setBits-half_setBits-1)===(~set_s2(setBits-half_setBits-1,0))
    when(triggerEnq_s2 && match_a && !hit_s2 && (PSEL =/= 1023.U)) {
      PSEL := PSEL + 1.U
    }.elsewhen(triggerEnq_s2 && match_b && !hit_s2 && (PSEL =/= 0.U)) {
      PSEL := PSEL - 1.U
    }
    val repl_type = WireInit(false.B)
    repl_type := Mux(match_a, false.B,
                  Mux(match_b, true.B,
                    Mux(PSEL(9) === 0.U, false.B, true.B))) // false.B - srrip, true.B - brrip

    val next_state_s2 = repl.get_next_state(repl_state_s2, way_s2, hit_s2, hasInvalidWay_s2, repl_type, req_type)

    val repl_init = Wire(Vec(tpTableAssoc, UInt(rrpvBits.W)))
    repl_init.foreach(_ := Fill(rrpvBits, 1.U(1.W)))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s2, repl_init.asUInt),
      Mux(resetFinish, set_s2, resetIdx),
      1.U
    )
  } else if (tpTableReplacementPolicy == "tpbrrip") {
    val next_state_s2 = repl.get_next_state(repl_state_s2, way_s2, hit_s2, hasInvalidWay_s2, req_type)
    val repl_init = Wire(Vec(tpTableAssoc, UInt(rrpvBits.W)))
    repl_init.foreach(_ := Fill(rrpvBits, 1.U(1.W)))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s2, repl_init.asUInt),
      Mux(resetFinish, set_s2, resetIdx),
      1.U
    )
  } else {
      val next_state_s2 = repl.get_next_state(repl_state_s2, way_s2)
      replacer_sram_opt.get.io.w(
        !resetFinish || replacerWen,
        Mux(resetFinish, next_state_s2, 0.U),
        Mux(resetFinish, set_s2, resetIdx),
        1.U
      )
  }

  triggerQueue.io.enq.valid := triggerEnq_s2
  triggerQueue.io.enq.bits.vaddr := train_s2.vaddr.getOrElse(0.U)
  triggerQueue.io.enq.bits.paddr := train_s2.addr
  triggerQueue.io.enq.bits.way := way_s2
  triggerQueue.io.enq.bits.replTag := Mux(replTagValid_s2, replTag_s2, Mux(trainOnVaddr.orR, vtag_s2, ptag_s2))
  triggerQueue.io.deq.ready := false.B // will be override

  // dataReadQueue enqueue
  dataReadQueue.io.enq.valid := s2_valid && !tpMetaRespValid_s2 && hit_s2
  dataReadQueue.io.enq.bits.l2ReqBundle := l2ReqBundle_s2
  dataReadQueue.io.enq.bits.wmode := false.B
  dataReadQueue.io.enq.bits.rawData := DontCare
  dataReadQueue.io.enq.bits.hartid := io.hartid
  dataReadQueue.io.enq.bits.replTag := DontCare

  tpMetaReadQueue.io.enq.valid := dataReadQueue.io.deq.fire
  tpMetaReadQueue.io.enq.bits.set := Mux(trainOnVaddr.orR, vset_s2, pset_s2)
  tpMetaReadQueue.io.enq.bits.tag := Mux(trainOnVaddr.orR, vtag_s2, ptag_s2)

  /* Async Stage: try to fetch or write tpData */

  // dataReadQueue/dataWriteQueue dequeue
  val pendingRead = RegInit(0.U)  // TODO: monitor pending read request and throttle it
  dataReadQueue.io.deq.ready := io.tpmeta_port.req.ready && !dataWriteQueue.io.deq.valid
  dataWriteQueue.io.deq.ready := io.tpmeta_port.req.ready

  // tpmeta_port req assignment, priority: write > read
  val readReqValid = dataReadQueue.io.deq.valid
  val writeReqValid = dataWriteQueue.io.deq.valid
  val readReqRawData = Cat(dataReadQueue.io.deq.bits.hartid, Cat(dataReadQueue.io.deq.bits.rawData.reverse).pad(508))
  val writeReqRawData = Cat(dataWriteQueue.io.deq.bits.hartid, Cat(dataWriteQueue.io.deq.bits.rawData.reverse).pad(508))
  io.tpmeta_port.req.valid := readReqValid || writeReqValid
  io.tpmeta_port.req.bits.l2ReqBundle := Mux(writeReqValid, dataWriteQueue.io.deq.bits.l2ReqBundle, dataReadQueue.io.deq.bits.l2ReqBundle)
  io.tpmeta_port.req.bits.wmode := Mux(writeReqValid, dataWriteQueue.io.deq.bits.wmode, dataReadQueue.io.deq.bits.wmode)
  io.tpmeta_port.req.bits.rawData := Mux(writeReqValid, writeReqRawData, readReqRawData)
  io.tpmeta_port.req.bits.replTag := Mux(writeReqValid, dataWriteQueue.io.deq.bits.replTag, dataReadQueue.io.deq.bits.replTag)


  /* Async Stage: get tpMeta and insert it into tpDataQueue */

  io.tpmeta_port.resp.ready := true.B

  val tpmetaRespHartid = io.tpmeta_port.resp.bits.rawData(511, 508)
  val tpmetaRespRawData = VecInit((0 until 16).map(i => io.tpmeta_port.resp.bits.rawData(metaDataLength * (i + 1) - 1, metaDataLength * i)))
  tpDataQueue.io.enq.valid := io.tpmeta_port.resp.valid && tpmetaRespHartid === hartid.U && io.tpmeta_port.resp.bits.exist
  tpDataQueue.io.enq.bits.rawData := tpmetaRespRawData
  assert(tpDataQueue.io.enq.ready === true.B) // tpDataQueue is never full

  tpMetaReadQueue.io.deq.ready := io.tpmeta_port.resp.valid && tpmetaRespHartid === hartid.U
  tpMetaRespQueue.io.enq.valid := io.tpmeta_port.resp.valid && tpmetaRespHartid === hartid.U && !io.tpmeta_port.resp.bits.exist
  tpMetaRespQueue.io.enq.bits := tpMetaReadQueue.io.deq.bits

  /* Recorder logic TODO: compress data based on max delta */

  val recorder_idx = RegInit(0.U(offsetBits.W))
  val recorder_data = Reg(Vec(tpEntryMaxLen, UInt(fullAddressBits.W)))
  // val recorder_data_debug = Reg(Vec(tpEntryMaxLen, UInt(vaddrBits.W)))
  val record_data_in = train_s2.addr
  require(record_data_in.getWidth == fullAddressBits)
  val write_record = RegInit(false.B)
  val write_record_trigger = RegInit(0.U.asTypeOf(new triggerBundle()))

  when(dorecord_s2) {
    recorder_idx := recorder_idx + 1.U
    recorder_data(recorder_idx) := record_data_in >> offsetBits.U // eliminate cacheline offset
    assert((record_data_in >> offsetBits.U)(fullAddressBits - 1, metaDataLength) === 0.U)
    when(recorder_idx === (recordThres-1.U)) {
      write_record := true.B
      recorder_idx := 0.U
      write_record_trigger := triggerQueue.io.deq.bits
      triggerQueue.io.deq.ready := true.B
      assert(triggerQueue.io.deq.valid)
    }
  }
  when(write_record) {
    write_record := false.B
  }

  val tpmetaInvalidate = RegEnable(tpMetaRespValid_s2 && hit_s2, s2_valid) && RegNext(s2_valid, false.B)
  val tpTable_w_valid = write_record || tpmetaInvalidate
  assert(RegNext(s2_valid, false.B) || !tpTable_w_valid, "tpTable_w_valid can only be true in s3")

  val (write_record_vtag, write_record_vset) = parseVaddr(write_record_trigger.vaddr)
  val (write_record_ptag, write_record_pset) = parsePaddr(write_record_trigger.paddr)
  val (write_record_l2_vtag, write_record_l2_vset, write_record_l2_vbank, write_record_l2_voff) = parseFullAddressWithBank(write_record_trigger.vaddr)
  val (write_record_l2_ptag, write_record_l2_pset, write_record_l2_pbank, write_record_l2_poff) = parseFullAddressWithBank(write_record_trigger.paddr)

  val tpMeta_w_bits = Wire(new tpMetaEntry())
  tpMeta_w_bits.valid := Mux(tpMetaRespValid_s2, false.B, true.B)
  tpMeta_w_bits.triggerTag := Mux(trainOnVaddr.orR, write_record_vtag, write_record_ptag)
  tpMeta_w_bits.l2ReqBundle.tag := Mux(trainOnVaddr.orR, write_record_l2_vtag, write_record_l2_ptag)
  tpMeta_w_bits.l2ReqBundle.set := Mux(trainOnVaddr.orR, write_record_l2_vset, write_record_l2_pset)
  tpMeta_w_bits.l2ReqBundle.bank := Mux(trainOnVaddr.orR, write_record_l2_vbank, write_record_l2_pbank)
  tpMeta_w_bits.l2ReqBundle.off := Mux(trainOnVaddr.orR, write_record_l2_voff, write_record_l2_poff)
  when(!resetFinish) {
    tpMeta_w_bits.valid := false.B
    tpMeta_w_bits.triggerTag := 0.U
    tpMeta_w_bits.l2ReqBundle.tag := 0.U
    tpMeta_w_bits.l2ReqBundle.set := 0.U
    tpMeta_w_bits.l2ReqBundle.bank := 0.U
    tpMeta_w_bits.l2ReqBundle.off := 0.U
  }

  val tpTable_w_set = Mux(resetFinish, Mux(tpmetaInvalidate, tpMetaResp_s2.set,
                        Mux(trainOnVaddr.orR, write_record_vset, write_record_pset)), resetIdx)
  val tpTable_w_way = Mux(tpmetaInvalidate, way_s2, write_record_trigger.way)
  val tpTable_w_wayOH = Mux(resetFinish, UIntToOH(tpTable_w_way), Fill(tpTableAssoc, true.B))

  tpMetaTable.io.w.apply(tpTable_w_valid || !resetFinish, tpMeta_w_bits, tpTable_w_set, tpTable_w_wayOH)

  dataWriteQueue.io.enq.valid := tpTable_w_valid && !tpmetaInvalidate
  dataWriteQueue.io.enq.bits.wmode := true.B
  dataWriteQueue.io.enq.bits.rawData.zip(recorder_data).foreach(x => x._1 := x._2(fullAddressBits - offsetBits - 1, 0))
  dataWriteQueue.io.enq.bits.l2ReqBundle.tag := Mux(trainOnVaddr.orR, write_record_l2_vtag, write_record_l2_ptag)
  dataWriteQueue.io.enq.bits.l2ReqBundle.set := Mux(trainOnVaddr.orR, write_record_l2_vset, write_record_l2_pset)
  dataWriteQueue.io.enq.bits.l2ReqBundle.bank := Mux(trainOnVaddr.orR, write_record_l2_vbank, write_record_l2_pbank)
  dataWriteQueue.io.enq.bits.l2ReqBundle.off := Mux(trainOnVaddr.orR, write_record_l2_voff, write_record_l2_poff)
  dataWriteQueue.io.enq.bits.hartid := io.hartid
  dataWriteQueue.io.enq.bits.replTag := write_record_trigger.replTag
  assert(dataWriteQueue.io.enq.ready === true.B) // TODO: support back-pressure

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  // TODO: resolve r/w conflict


  /* Send prefetch request */

  val do_sending = RegInit(false.B)
  val sending_idx = RegInit(0.U(offsetBits.W))
  val sending_data = Reg(Vec(tpEntryMaxLen, UInt(fullAddressBits.W)))
  // val sending_data_debug = Reg(Vec(tpEntryMaxLen, UInt(vaddrBits.W)))
  val sending_throttle = RegInit(0.U(4.W))
  val tpDataQFull = tpDataQueue.io.count === tpDataQueueDepth.U

  val sending_valid = do_sending && !tpDataQFull && sending_throttle === tpThrottleCycles
  val current_sending_data = Cat(sending_data(sending_idx), 0.U(offsetBits.W))
  val (sendingTag, sendingSet, _) = parseFullAddress(current_sending_data)

  tpDataQueue.io.deq.ready := tpDataQFull || !do_sending
  when(tpDataQueue.io.deq.fire) {
    sending_data := tpDataQueue.io.deq.bits.rawData
    // sending_data_debug := tpDataQueue.io.deq.bits.rawData_debug
    sending_idx := 1.U  // TODO: dismiss the first addr because it is the trigger
    do_sending := true.B
  }
  when(((do_sending && !tpDataQFull) || sending_throttle =/= 0.U) && (sending_throttle =/= tpThrottleCycles)) {
    sending_throttle := sending_throttle + 1.U
  }
  when(io.req.fire) {
    sending_idx := sending_idx + 1.U
    sending_throttle := 0.U
    if (debug) {
      printf("[TP] sending data: %x\n", current_sending_data)
    }
    when(sending_idx === (recordThres-1.U)) {
      do_sending := false.B
    }
  }

  io.req.valid := Mux(enableTP.orR, sending_valid, false.B)
  io.req.bits.tag := sendingTag
  io.req.bits.set := sendingSet
  io.req.bits.needT := true.B
  io.req.bits.source := 0.U // TODO: ensure source 0 is dcache
  io.req.bits.pfSource := MemReqSource.Prefetch2L2TP.id.U

  io.resp.ready := true.B
  io.train.ready := resetFinish && !tpMetaRespQueue.io.deq.valid
  tpMetaRespQueue.io.deq.ready := true.B


  /* Performance collection */
  val triggerDB = ChiselDB.createTable("tptrigger", new triggerBundle(), basicDB = true)
  val triggerPt = Wire(new triggerBundle())
  triggerPt.paddr := write_record_trigger.paddr
  triggerPt.vaddr := recoverVaddr(write_record_trigger.vaddr)
  triggerPt.way := write_record_trigger.way
  triggerPt.replTag := write_record_trigger.replTag

  val trainDB = ChiselDB.createTable("tptrain", new trainBundle(), basicDB = true)
  val trainPt = Wire(new trainBundle())
  trainPt.vaddr := recoverVaddr(train_s2.vaddr.getOrElse(0.U))
  trainPt.paddr := train_s2.addr
  trainPt.hit := train_s2.hit
  trainPt.prefetched := train_s2.prefetched
  trainPt.pfsource := train_s2.pfsource
  trainPt.metahit := hit_s2

  val sendDB = ChiselDB.createTable("tpsend", new sendBundle(), basicDB = true)
  val sendPt = Wire(new sendBundle())
  sendPt.paddr := current_sending_data
  sendPt.vaddr := 0.U

  val replDB = ChiselDB.createTable("tprepl", new tpmetaRepl(), basicDB = true)
  val replPt = Wire(new tpmetaRepl())
  replPt := repl_s2

  triggerDB.log(triggerPt, tpTable_w_valid, "", clock, reset)
  trainDB.log(trainPt, s2_valid, "", clock, reset)
  sendDB.log(sendPt, io.req.fire, "", clock, reset)
  replDB.log(replPt, triggerEnq_s2, "", clock, reset)

  XSPerfAccumulate(cacheParams, "tp_send", io.req.fire)
  XSPerfAccumulate(cacheParams, "tp_meta_read", io.tpmeta_port.req.fire && !io.tpmeta_port.req.bits.wmode)
  XSPerfAccumulate(cacheParams, "tp_meta_read_miss", io.tpmeta_port.resp.valid && !io.tpmeta_port.resp.bits.exist)
  XSPerfAccumulate(cacheParams, "tp_meta_invalid", tpMetaRespValid_s2)
  XSPerfAccumulate(cacheParams, "tp_meta_write", io.tpmeta_port.req.fire && io.tpmeta_port.req.bits.wmode)
  XSPerfAccumulate(cacheParams, "tp_close", tpDisable)
  XSPerfAccumulate(cacheParams, "tp_reset", tpFbReset)
  XSPerfAccumulate(cacheParams, "tp_hit", io.tpHitFeedback.valid && io.tpHitFeedback.bits.hit)
  XSPerfAccumulate(cacheParams, "tp_latepf", io.tpHitFeedback.valid && io.tpHitFeedback.bits.latepf)
  XSPerfAccumulate(cacheParams, "tp_miss", io.tpHitFeedback.valid && io.tpHitFeedback.bits.replMiss)
}
