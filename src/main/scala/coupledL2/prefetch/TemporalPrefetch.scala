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
  *
  *
  * Acknowledgement
  *
  * This implementation is inspired by several key papers:
  * [1] Hao Wu, Nathella Krishnendra, Pusdesris Joseph, Sunwoo Dam, Jain Akanksha, and Lin Calvin. "[Temporal
  * prefetching without the off-chip metadata.](https://doi.org/10.1145/3352460.3358300)" Proceedings of the 52nd Annual
  * IEEE/ACM International Symposium on Microarchitecture. 2019.
  * [2] Ainsworth Sam, and Lev Mukhanov. "[Triangel: A High-Performance, Accurate, Timely On-Chip Temporal Prefetcher.]
  * (https://doi.org/10.1109/ISCA59077.2024.00090)" 2024 ACM/IEEE 51st Annual International Symposium on Computer
  * Architecture. 2024.
  * *************************************************************************************
  */

package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, MemReqSource, SRAMTemplate}
import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.ReplacementPolicy
import huancun.{TPmetaReq, TPmetaResp}

case class TPParameters(
    tpTableEntries: Int = 16384,
    tpTableAssoc: Int = 16,
    vaddrBits: Int = 39,
    blockOffBits: Int = 6,
    dataReadQueueDepth: Int = 8,
    dataWriteQueueDepth: Int = 4,
    tpDataQueueDepth: Int = 8,
    triggerQueueDepth: Int = 4,
    throttleCycles: Int = 4,  // unused yet
    replacementPolicy: String = "random",
    debug: Boolean = false
) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true
  override val inflightEntries: Int = 16 // changed in sv48
}

trait HasTPParams extends HasCoupledL2Parameters {
  def tpParams = prefetchers.find {
    case p: TPParameters => true
    case _ => false
  }.get.asInstanceOf[TPParameters]
  def tpTableAssoc = tpParams.tpTableAssoc
  def tpTableNrSet = tpParams.tpTableEntries / tpTableAssoc
  def tpTableSetBits = log2Ceil(tpTableNrSet)
  def tpEntryMaxLen = log2Floor(512 / (fullAddressBits - offsetBits))
  def tpTableReplacementPolicy = tpParams.replacementPolicy
  def debug = tpParams.debug
  def vaddrBits = tpParams.vaddrBits
  def blockOffBits = tpParams.blockOffBits
  def dataReadQueueDepth = tpParams.dataReadQueueDepth
  def dataWriteQueueDepth = tpParams.dataWriteQueueDepth
  def tpDataQueueDepth = tpParams.tpDataQueueDepth
  def triggerQueueDepth = tpParams.triggerQueueDepth
  def metaDataLength = fullAddressBits - offsetBits
//  val tpThrottleCycles = tpParams.throttleCycles
//  require(tpThrottleCycles > 0, "tpThrottleCycles must be greater than 0")
}

abstract class TPBundle(implicit val p: Parameters) extends Bundle with HasTPParams
abstract class TPModule(implicit val p: Parameters) extends Module with HasTPParams

class tpMetaEntry(implicit p:Parameters) extends TPBundle {
  val valid = Bool()
  val triggerTag = UInt((vaddrBits-blockOffBits-tpTableSetBits).W)
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

class tpmetaPortIO(hartIdLen: Int, fullAddressBits: Int, offsetBits: Int)(implicit p: Parameters) extends Bundle {
  val req = DecoupledIO(new TPmetaReq(hartIdLen, fullAddressBits, offsetBits))
  val resp = Flipped(ValidIO(new TPmetaResp(hartIdLen, fullAddressBits, offsetBits)))
}

/* VIVT, Physical Data */
class TemporalPrefetch(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val tpmeta_port = new tpmetaPortIO(hartIdLen, fullAddressBits, offsetBits)
    val hartid = Input(UInt(hartIdLen.W))
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
  val dataReadQueue = Module(new Queue(new TPmetaReq(hartIdLen, fullAddressBits, offsetBits), dataReadQueueDepth, pipe = false, flow = false))
  val dataWriteQueue = Module(new Queue(new TPmetaReq(hartIdLen, fullAddressBits, offsetBits), dataWriteQueueDepth, pipe = false, flow = false))
  val tpDataQueue = Module(new Queue(new tpDataEntry(), tpDataQueueDepth + 1, pipe = false, flow = false))
  val triggerQueue = Module(new Queue(new triggerBundle(), triggerQueueDepth, pipe = false, flow = false))
  val triggerQEmpty = !triggerQueue.io.deq.valid
  val triggerQFull = !triggerQueue.io.enq.ready
  val repl = ReplacementPolicy.fromString(tpTableReplacementPolicy, tpTableAssoc)
  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((tpTableNrSet - 1).U)

  /* Constantin Parameters */

  val hartid = cacheParams.hartId
  // 0 / 1: whether to enable temporal prefetcher
  private val enableTP = Constantin.createRecord("enableTP"+hartid.toString, initValue = 1)
  // 0 ~ N: throttle cycles for each prefetch request
  private val tpThrottleCycles = Constantin.createRecord("tp_throttleCycles"+hartid.toString, initValue = 4)
  // 0 / 1: whether request to set as trigger on meta hit
  private val hitAsTrigger = Constantin.createRecord("tp_hitAsTrigger"+hartid.toString, initValue = 1)
  // 1 ~ triggerQueueDepth: enqueue threshold for triggerQueue
  private val triggerThres = Constantin.createRecord("tp_triggerThres"+hartid.toString, initValue = 1)
  // 1 ~ tpEntryMaxLen: record threshold for recorder and sender (storage size will not be affected)
  private val recordThres = Constantin.createRecord("tp_recordThres"+hartid.toString, initValue = tpEntryMaxLen)
  // 0 / 1: whether to train on vaddr
  private val trainOnVaddr = Constantin.createRecord("tp_trainOnVaddr"+hartid.toString, initValue = 0)
  // 0 / 1: whether to eliminate L1 prefetch request training
  private val trainOnL1PF = Constantin.createRecord("tp_trainOnL1PF"+hartid.toString, initValue = 0)

  if (vaddrBitsOpt.isEmpty) {
    assert(!trainOnVaddr)
  }

  /* Stage 0: query tpMetaTable */

  val s0_valid = io.train.fire && Mux(trainOnVaddr.orR, io.train.bits.vaddr.getOrElse(0.U) =/= 0.U, true.B) &&
    Mux(trainOnL1PF.orR, true.B, io.train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U)
  val trainVaddr = io.train.bits.vaddr.getOrElse(0.U)
  val trainPaddr = io.train.bits.addr
  val (vtag_s0, vset_s0) = if (vaddrBitsOpt.nonEmpty) parseVaddr(trainVaddr) else (0.U, 0.U)
  val (ptag_s0, pset_s0) = parsePaddr(trainPaddr)
  val metas = tpMetaTable.io.r(s0_valid, Mux(trainOnVaddr.orR, vset_s0, pset_s0)).resp.data


  /* Stage 1: parse tpMeta to judge hit or miss, choose the victim */

  val s1_valid = RegNext(s0_valid, false.B)
  val train_s1 = RegEnable(io.train.bits, s0_valid)
  val trainVaddr_s1 = train_s1.vaddr.getOrElse(0.U)
  val trainPaddr_s1 = train_s1.addr
  val (vtag_s1, vset_s1) = if (vaddrBitsOpt.nonEmpty) parseVaddr(trainVaddr_s1) else (0.U, 0.U)
  val (ptag_s1, pset_s1) = parsePaddr(trainPaddr_s1)

  val tagMatchVec = metas.map(_.triggerTag === Mux(trainOnVaddr.orR, vtag_s1, ptag_s1))
  val metaValidVec = metas.map(_.valid === true.B)

  /*
  val debug_metaValidVec = VecInit(metaValidVec.map(_ && s1_valid))
  val debug_tagVec = VecInit(metas.map(x => Mux(s1_valid, x.triggerTag, 0.U)))
  dontTouch(debug_metaValidVec)
  dontTouch(debug_tagVec)
  */

  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)
  val victimWay = repl.get_replace_way(0.U /*never mind for random*/)

  val hit_s1 = Cat(hitVec).orR
  val way_s1 = Mux(hit_s1, hitWay, victimWay)


  /* Stage 2: access tpData on meta hit, record it on meta miss */

  val s2_valid = RegNext(s1_valid, false.B)
  val hit_s2 = RegEnable(hit_s1, false.B, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val vset_s2 = RegEnable(vset_s1, s1_valid)
  val pset_s2 = RegEnable(pset_s1, s1_valid)
  val train_s2 = RegEnable(train_s1, s1_valid)

  val triggerEnq_s2 = s2_valid && !triggerQFull && (Mux(hitAsTrigger.orR, hit_s2, false.B) || (triggerQueue.io.count < triggerThres))
  val dorecord_s2 = s2_valid && !triggerEnq_s2 && !triggerQEmpty // && !hit_s2

  triggerQueue.io.enq.valid := triggerEnq_s2
  triggerQueue.io.enq.bits.vaddr := train_s2.vaddr.getOrElse(0.U)
  triggerQueue.io.enq.bits.paddr := train_s2.addr
  triggerQueue.io.enq.bits.way := way_s2
  triggerQueue.io.deq.ready := false.B // will be override

  // dataReadQueue enqueue
  dataReadQueue.io.enq.valid := s2_valid && hit_s2
  dataReadQueue.io.enq.bits.set := Mux(trainOnVaddr.orR, vset_s2, pset_s2)
  dataReadQueue.io.enq.bits.way := way_s2
  dataReadQueue.io.enq.bits.wmode := false.B
  dataReadQueue.io.enq.bits.rawData := DontCare
  dataReadQueue.io.enq.bits.hartid := io.hartid


  /* Async Stage: try to fetch or write tpData */

  // dataReadQueue/dataWriteQueue dequeue
  val pendingRead = RegInit(0.U)  // TODO: monitor pending read request and throttle it
  dataReadQueue.io.deq.ready := io.tpmeta_port.req.ready && !dataWriteQueue.io.deq.valid
  dataWriteQueue.io.deq.ready := io.tpmeta_port.req.ready

  // tpmeta_port req assignment, priority: write > read
  val readReqValid = dataReadQueue.io.deq.valid
  val writeReqValid = dataWriteQueue.io.deq.valid
  io.tpmeta_port.req.valid := readReqValid || writeReqValid
  io.tpmeta_port.req.bits := Mux(writeReqValid, dataWriteQueue.io.deq.bits, dataReadQueue.io.deq.bits)


  /* Async Stage: get tpMeta and insert it into tpDataQueue */

  tpDataQueue.io.enq.valid := io.tpmeta_port.resp.valid && io.tpmeta_port.resp.bits.hartid === io.hartid
  tpDataQueue.io.enq.bits.rawData := io.tpmeta_port.resp.bits.rawData
  assert(tpDataQueue.io.enq.ready === true.B) // tpDataQueue is never full


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

  val tpTable_w_valid = write_record
  assert(RegNext(s2_valid, false.B) || !tpTable_w_valid, "tpTable_w_valid can only be true in s3")

  val (write_record_vtag, write_record_vset) = parseVaddr(write_record_trigger.vaddr)
  val (write_record_ptag, write_record_pset) = parsePaddr(write_record_trigger.paddr)

  val tpMeta_w_bits = Wire(new tpMetaEntry())
  tpMeta_w_bits.valid := true.B
  tpMeta_w_bits.triggerTag := Mux(trainOnVaddr.orR, write_record_vtag, write_record_ptag)
  when(!resetFinish) {
    tpMeta_w_bits.valid := false.B
    tpMeta_w_bits.triggerTag := 0.U
  }

  val tpTable_w_set = Mux(resetFinish, Mux(trainOnVaddr.orR, write_record_vset, write_record_pset), resetIdx)
  val tpTable_w_way = write_record_trigger.way
  val tpTable_w_wayOH = Mux(resetFinish, UIntToOH(write_record_trigger.way), Fill(tpTableAssoc, true.B))

  tpMetaTable.io.w.apply(tpTable_w_valid || !resetFinish, tpMeta_w_bits, tpTable_w_set, tpTable_w_wayOH)

  dataWriteQueue.io.enq.valid := tpTable_w_valid
  dataWriteQueue.io.enq.bits.wmode := true.B
  dataWriteQueue.io.enq.bits.rawData.zip(recorder_data).foreach(x => x._1 := x._2(fullAddressBits - offsetBits - 1, 0))
  dataWriteQueue.io.enq.bits.set := tpTable_w_set
  dataWriteQueue.io.enq.bits.way := tpTable_w_way
  dataWriteQueue.io.enq.bits.hartid := io.hartid
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
  io.req.bits.vaddr.foreach(_ := 0.U)
  io.req.bits.needT := true.B
  io.req.bits.source := 0.U // TODO: ensure source 0 is dcache
  io.req.bits.pfSource := MemReqSource.Prefetch2L2TP.id.U

  io.resp.ready := true.B
  io.train.ready := resetFinish


  /* Performance collection */
  val triggerDB = ChiselDB.createTable("tptrigger", new triggerBundle(), basicDB = debug)
  val triggerPt = Wire(new triggerBundle())
  triggerPt.paddr := write_record_trigger.paddr
  triggerPt.vaddr := recoverVaddr(write_record_trigger.vaddr)
  triggerPt.way := write_record_trigger.way

  val trainDB = ChiselDB.createTable("tptrain", new trainBundle(), basicDB = debug)
  val trainPt = Wire(new trainBundle())
  trainPt.vaddr := recoverVaddr(train_s2.vaddr.getOrElse(0.U))
  trainPt.paddr := train_s2.addr
  trainPt.hit := train_s2.hit
  trainPt.prefetched := train_s2.prefetched
  trainPt.pfsource := train_s2.pfsource
  trainPt.metahit := hit_s2

  val sendDB = ChiselDB.createTable("tpsend", new sendBundle(), basicDB = debug)
  val sendPt = Wire(new sendBundle())
  sendPt.paddr := current_sending_data
  sendPt.vaddr := 0.U

  triggerDB.log(triggerPt, tpTable_w_valid, "", clock, reset)
  trainDB.log(trainPt, s2_valid, "", clock, reset)
  sendDB.log(sendPt, io.req.fire, "", clock, reset)
}
