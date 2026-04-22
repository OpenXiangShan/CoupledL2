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
 * [1] Hao Wu, Krishnendra Nathella, Joseph Pusdesris, Dam Sunwoo, Akanksha Jain, and Calvin Lin. "[Temporal
 * Prefetching Without the Off-Chip Metadata.](https://doi.org/10.1145/3352460.3358300)" 52nd Annual IEEE/ACM
 * International Symposium on Microarchitecture (MICRO). 2019.
 * [2] Sam Ainsworth, and Lev Mukhanov. "[Triangel: A High-Performance, Accurate, Timely On-Chip Temporal Prefetcher.]
 * (https://doi.org/10.1109/ISCA59077.2024.00090)" ACM/IEEE 51st Annual International Symposium on Computer
 * Architecture (ISCA). 2024.
 * *************************************************************************************
 */

package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, MemReqSource, SRAMTemplate, XSPerfAccumulate, XSPerfHistogram}
import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.ReplacementPolicy
import coupledL2.{TPmetaReq, TPmetaResp}

case class TPParameters(
  tpTableEntries: Int = 16384,
  tpTableAssoc: Int = 16,
  // vaddrBits: Int = 50, sv48x4, no longer used(use fullVaddrBits)
  blockOffBits: Int = 6,
  tpTrainQueueDepth: Int = 8,
  dataReadQueueDepth: Int = 8,
  dataWriteQueueDepth: Int = 4,
  tpDataQueueDepth: Int = 8,
  tpMetaWQueueDepth: Int = 8,
  tpMetaResetQueueDepth: Int = 8, // extreme condition: use 9
  throttleCycles: Int = 4,  // unused yet
  replacementPolicy: String = "plru",

  // sampler filter parameters
  samplerFileterEntries: Int = 1024 * 4,
  samplerFilterAssoc: Int = 4 * 2,
  samplerFilterReplacememntPolicy: String = "plru",
  pcHashHeadReservedWidth: Int = 2,
  pcHashTailReservedWidth: Int = 6,
  pcHashMidWidth: Int = 14, // pc width = 50
  trainQueueDepth: Int = 100,
  // sampler table parameters
  samplerTableEntries: Int = 16384,
  samplerTableAssoc: Int = 16,
  samplerTableMatchCntWidth: Int = 3,
  samplerTableReplacementPolicy: String = "plru",
  // recorder table parameters
  recorderTableEntries: Int = 512,
  recorderTableAssoc: Int = 2,
  recorderTableReplacementPolicy: String = "plru",
  // confidence table parameters
  confTableEntries: Int = 512,
  confTableAssoc: Int = 8,
  confTableReplacementPolicy: String = "plru",
  confReqQueueDepth: Int = 8,

  globalHitCountConfidenceWidth: Int = 5,
  globalHitCountConfidenceInitVal: Int = 21,
  globalHitCountConfidenceThrottle: Int = 10,

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
  def tpEntryMaxLen = 512 / (fullAddressBits - offsetBits)
  def tpTableReplacementPolicy = tpParams.replacementPolicy
  def debug = tpParams.debug
  def vaddrBits = fullVAddrBits
  def blockOffBits = tpParams.blockOffBits
  def tpTrainQueueDepth = tpParams.tpTrainQueueDepth
  def dataReadQueueDepth = tpParams.dataReadQueueDepth
  def dataWriteQueueDepth = tpParams.dataWriteQueueDepth
  def tpDataQueueDepth = tpParams.tpDataQueueDepth
  def tpMetaWQueueDepth = tpParams.tpMetaWQueueDepth
  def tpMetaResetQueueDepth = tpParams.tpMetaResetQueueDepth
  def metaDataLength = fullAddressBits - offsetBits

  def pcHashHeadReservedWidth = tpParams.pcHashHeadReservedWidth
  def pcHashTailReservedWidth = tpParams.pcHashTailReservedWidth
  def pcHashMidWidth = tpParams.pcHashMidWidth
  def pcHashWidth = pcHashHeadReservedWidth + pcHashMidWidth + pcHashTailReservedWidth
  def pcAddrHashWidth = Math.max(metaDataLength, pcHashWidth)

  // sampler filter parameters
  def samplerFilterAssoc = tpParams.samplerFilterAssoc
  def samplerFilterNrSet = tpParams.samplerFileterEntries / samplerFilterAssoc
  def samplerFilterSetBits = log2Ceil(samplerFilterNrSet)
  def samplerFilterReplacementPolicy = tpParams.samplerFilterReplacememntPolicy
  def trainQueueDepth = tpParams.trainQueueDepth
  def filteredCntWidth = 5
  def filteredCntThrottle = 20
  // sampler table parameters
  def samplerTableAssoc = tpParams.samplerTableAssoc
  def samplerTableNrSet = tpParams.samplerTableEntries / samplerTableAssoc
  def samplerTableSetBits = log2Ceil(samplerTableNrSet)
  def samplerTableMatchCntWidth = tpParams.samplerTableMatchCntWidth
  def samplerTableReplacementPolicy = tpParams.samplerTableReplacementPolicy
  // recorder table parameters
  def recorderTableAssoc = tpParams.samplerTableAssoc
  def recorderTableNrSet = tpParams.samplerTableEntries / samplerTableAssoc
  def recorderTableSetBits = log2Ceil(samplerTableNrSet)
  def recorderTableReplacementPolicy = tpParams.samplerTableReplacementPolicy
  // confidence table parameters
  def confTableAssoc = tpParams.confTableAssoc
  def confTableNrSet = tpParams.confTableEntries / confTableAssoc
  def confTableSetBits = log2Ceil(confTableNrSet)
  def confTableReplacementPolicy = tpParams.confTableReplacementPolicy
  def confReqQueueDepth = tpParams.confReqQueueDepth
  def accConfWidth = 6
  // TODO: other conf

  //  val tpThrottleCycles = tpParams.throttleCycles
  //  require(tpThrottleCycles > 0, "tpThrottleCycles must be greater than 0")

  def globalHitCountConfidenceWidth = tpParams.globalHitCountConfidenceWidth
  def globalHitCountConfidenceInitVal = tpParams.globalHitCountConfidenceInitVal
  def globalHitCountConfidenceThrottle = tpParams.globalHitCountConfidenceThrottle
}

abstract class TPmetaBundle(implicit val p: Parameters) extends Bundle with HasTPParams
abstract class TPmetaModule(implicit val p: Parameters) extends Module with HasTPParams

abstract class TPBundle(implicit val p: Parameters) extends Bundle with HasTPParams
abstract class TPModule(implicit val p: Parameters) extends Module with HasTPParams

class TPmetaIO(implicit p: Parameters) extends TPmetaBundle {
  val req = Flipped(DecoupledIO(new TPmetaReq()))
  val resp = ValidIO(new TPmetaResp())
}

class metaEntry(implicit p:Parameters) extends TPmetaBundle {
  val rawData = Vec(512 / (fullAddressBits - offsetBits), UInt((fullAddressBits - offsetBits).W))
  val length = UInt(log2Ceil(tpEntryMaxLen).W)
  // val hartid = UInt(hartIdLen.W)
}

class TPmeta(implicit p: Parameters) extends TPmetaModule
{
  val io = IO(new TPmetaIO())
  val tpDataTable = Module(
    new SRAMTemplate(
      new metaEntry(),
      set = tpTableNrSet,
      way = tpTableAssoc,
      shouldReset = false,
      singlePort = true)
  )

  val readReqValid = io.req.valid && !io.req.bits.wmode
  val writeReqValid = io.req.valid && io.req.bits.wmode

  tpDataTable.io.r.req.valid := readReqValid
  tpDataTable.io.r.req.bits.setIdx := io.req.bits.set

  val wdata = Wire(new metaEntry())
  wdata.rawData := io.req.bits.rawData
  // wdata.hartid := io.req.bits.hartid
  wdata.length := io.req.bits.length
  tpDataTable.io.w.apply(
    valid = writeReqValid,
    data = wdata,
    setIdx = io.req.bits.set,
    waymask = UIntToOH(io.req.bits.way)
  )

  val readReqValidReg = RegNext(readReqValid, false.B)
  val readReqReg = RegEnable(io.req.bits, readReqValid)

  val rdata = Reg(new metaEntry())
  when(readReqValidReg) {
    rdata := tpDataTable.io.r.resp.data(readReqReg.way)
  }

  io.resp.valid := RegNext(readReqValidReg) // && (rdata.hartid === RegNext(readReqReg).hartid)
  io.resp.bits.rawData := rdata.rawData
  io.resp.bits.hartid := RegNext(readReqReg).hartid
  io.resp.bits.length := rdata.length
  io.resp.bits.hitCount := RegNext(readReqReg).hitCount
  io.req.ready := true.B
}

class filteredEntry(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val lastAddr = UInt(metaDataLength.W)
  val currAddr = UInt(metaDataLength.W)
  val cnt = UInt(filteredCntWidth.W)
}

class filterTableEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val pcTag = UInt((pcHashWidth - samplerFilterSetBits).W)
  val lastAddr = UInt(metaDataLength.W)
  val cnt = UInt(filteredCntWidth.W)

  def apply(valid: Bool, tag: UInt, addr: UInt, cnt: UInt) = {
    val entry = Wire(new filterTableEntry)
    entry.valid := valid
    entry.pcTag := tag
    entry.lastAddr := addr
    entry.cnt := cnt
    entry
  }
}

class trainEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val pc = UInt(pcHashWidth.W)
  val addr = UInt(metaDataLength.W)
}

class SamplerFilter(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(ValidIO(new PrefetchTrain()))
    val trained = ValidIO(new filteredEntry())
  })

  def hashPC(pc: UInt) = {
    val reservedHead = pc(pc.getWidth - 1, pc.getWidth - pcHashHeadReservedWidth)
    val reservedTail = pc(pcHashTailReservedWidth - 1, 0)
    val mid = pc(pc.getWidth - pcHashHeadReservedWidth - 1, pcHashTailReservedWidth)
    val hashMid = mid(pcHashMidWidth - 1, 0) ^ mid(pcHashMidWidth * 2 - 1, pcHashMidWidth) ^
      mid(pcHashMidWidth * 3 - 1, pcHashMidWidth * 2)
    Cat(reservedTail, hashMid, reservedHead)
  }

  def parsePaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth - 1, samplerFilterSetBits), x(samplerFilterSetBits - 1, 0))
  }

  val trainQueue = Module(new Queue(new trainEntry(), trainQueueDepth + 1, pipe = false, flow = false))

  val filterTable = Module(
    new SRAMTemplate(
      new filterTableEntry(),
      set = samplerFilterNrSet,
      way = samplerFilterAssoc,
      shouldReset = false,
      singlePort = true
    )
  )
  val repl = ReplacementPolicy.fromString(samplerFilterReplacementPolicy, samplerFilterAssoc)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((samplerFilterNrSet - 1).U)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }


  /* ------- stage pre ------- */
  // handle SRAM RW conflict
  val filterTableWValid = Wire(Bool())
  trainQueue.io.enq.valid := io.train.valid
  trainQueue.io.enq.bits.valid := io.train.valid
  trainQueue.io.enq.bits.pc := hashPC(io.train.bits.pc)
  trainQueue.io.enq.bits.addr := io.train.bits.addr >> offsetBits
  trainQueue.io.deq.ready := !filterTableWValid && !RegNext(trainQueue.io.deq.fire) //TODO: acatually should be same-set block

  /* ------- stage 0 ------- */
  // use hash pc query filter table
  val s0_valid = trainQueue.io.deq.fire
  val pc_s0 = trainQueue.io.deq.bits.pc
  val trainAddr_s0 = trainQueue.io.deq.bits.addr
  val currAddr_s0 = WireInit(0.U(metaDataLength.W))
  currAddr_s0 := trainAddr_s0

  assert(io.train.bits.pc.getWidth == pcHashHeadReservedWidth + pcHashTailReservedWidth + 3 * pcHashMidWidth)

  val filterTableRValid_s0 = s0_valid
  val (pcTag_s0, pcSet_s0) = parsePaddr(pc_s0)
  val filterRecord_s1 = filterTable.io.r(filterTableRValid_s0, pcSet_s0).resp.data

  /* ------- stage 1 ------- */
  // compare pc
  val s1_valid = RegNext(s0_valid, false.B)
  val pc_s1 = RegEnable(pc_s0, s0_valid)
  val currAddr_s1 = RegEnable(currAddr_s0, s0_valid)
  val (pcTag_s1, pcSet_s1) = parsePaddr(pc_s1)

  val tagMatchVec_s1 = filterRecord_s1.map(_.pcTag === pcTag_s1)
  val validVec_s1 = filterRecord_s1.map(_.valid)
  val hitVec_s1 = tagMatchVec_s1.zip(validVec_s1).map(x => x._1 && x._2)
  val hit_s1 = Cat(hitVec_s1).orR

  val hitWay_s1 = OHToUInt(hitVec_s1)
  val victimWay_s1 = repl.way
  val way_s1 = Mux(hit_s1, hitWay_s1, victimWay_s1)
  val lastAddr_s1 = filterRecord_s1(way_s1).lastAddr
  val cnt_s1 = filterRecord_s1(way_s1).cnt

  when(hit_s1) {
    repl.access(hitWay_s1)
  }.otherwise {
    repl.miss
  }

  /* ------- stage 2 ------- */
  // 1. miss: generate new entry; 2. hit: update entry & generate pair for sampler table
  // update sampler filter table
  // TODO: add "cnt" to preserve frequent pc
  val s2_valid = RegNext(s1_valid, false.B)
  val pc_s2 = RegEnable(pc_s1, s1_valid)
  val currAddr_s2 = RegEnable(currAddr_s1, s1_valid)
  val lastAddr_s2 = RegEnable(lastAddr_s1, s1_valid)
  val cnt_s2 = RegEnable(cnt_s1, s1_valid)
  val hit_s2 = RegEnable(hit_s1, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val (pcTag_s2, pcSet_s2) = parsePaddr(pc_s2)
  val updateCnt = Mux(hit_s2 && !cnt_s2.andR, cnt_s2 + 1.U, cnt_s2)

  val updateEntry = WireInit(new filterTableEntry().apply(false.B, pcTag_s2, currAddr_s2, updateCnt))
  val replEntry = WireInit(new filterTableEntry().apply(true.B, pcTag_s2, currAddr_s2, 0.U))
  val resetEntry = WireInit(new filterTableEntry().apply(false.B, 0.U, 0.U, 0.U))

  val filterTableWValid_s2 =  s2_valid || !resetFinish
  val filterTableWSet_s2 = Mux(resetFinish, pcSet_s2, resetIdx)
  val filterTableWWayOH_s2 = Mux(resetFinish, UIntToOH(way_s2), Fill(samplerFilterAssoc, true.B))
  val filterTableWEntry_s2 = Mux(resetFinish, Mux(hit_s2, updateEntry, replEntry), resetEntry)
  filterTableWValid := filterTableWValid_s2

  filterTable.io.w.apply(
    valid = filterTableWValid_s2,
    data = filterTableWEntry_s2,
    setIdx = filterTableWSet_s2,
    waymask = filterTableWWayOH_s2
  )

  io.trained.valid := s2_valid && hit_s2
  io.trained.bits.pc := pc_s2
  io.trained.bits.lastAddr := lastAddr_s2
  io.trained.bits.currAddr := currAddr_s2
  io.trained.bits.cnt := cnt_s2

  // assert(io.trained.valid === io.tpTableWay_s2.valid) TODO
  assert(!(filterTable.io.r.req.valid && filterTable.io.w.req.valid))
  assert(trainQueue.io.enq.ready === true.B)

  XSPerfAccumulate("tp_filter_trained_valid", io.train.valid)
  XSPerfAccumulate("tp_filter_hit", hit_s2 & s2_valid)
  XSPerfAccumulate("tp_filter_miss", !hit_s2 & s2_valid)

  val filterDB = ChiselDB.createTable("tpfilter", new filteredEntry(), basicDB = true)
  val filterPt = Wire(new filteredEntry())
  filterPt := io.trained.bits
  filterDB.log(filterPt, io.trained.valid, "", clock, reset)
}

class trainedPair(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val addr1 = UInt(metaDataLength.W)
  val addr2 = UInt(metaDataLength.W)
}

class samplerTableEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val baseTag = UInt((metaDataLength - samplerTableSetBits).W)
  val targetAddr = UInt(metaDataLength.W)
  val pc = UInt(pcHashWidth.W)
  val matchCnt = UInt(samplerTableMatchCntWidth.W)

  def apply(valid: Bool, base: UInt, target: UInt, pc: UInt, cnt: UInt) = {
    val entry = Wire(new samplerTableEntry)
    entry.valid := valid
    entry.baseTag := base
    entry.targetAddr := target
    entry.pc := pc
    entry.matchCnt := cnt
    entry
  }
}

class SamplerTable(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(ValidIO(new filteredEntry()))
    val trained = ValidIO(new trainedPair())
  })

  def parsePaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, samplerTableSetBits), x(samplerTableSetBits - 1, 0))
  }

  val samplerTable = Module(
    new SRAMTemplate(
      new samplerTableEntry(),
      set = samplerTableNrSet,
      way = samplerTableAssoc,
      shouldReset = false,
      singlePort = true
    )
  )
  val repl = ReplacementPolicy.fromString(samplerTableReplacementPolicy, samplerTableAssoc)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((samplerTableNrSet - 1).U)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  /* ------- stage pre ------- */
  // handle SRAM RW conflict
  val samplerTableRPendingValid = Wire(Bool())
  val samplerTableWValid = Wire(Bool())
  val trainPending = RegEnable(io.train.bits, samplerTableRPendingValid)
  val pendingValid_s0 = RegNext(samplerTableRPendingValid, false.B)
  samplerTableRPendingValid := io.train.valid && (samplerTableWValid || pendingValid_s0)

  /* ------- stage 0 ------- */
  // query samplerTable
  val s0_valid = io.train.valid && !samplerTableWValid || pendingValid_s0
  val baseAddr_s0 = Mux(pendingValid_s0, trainPending.lastAddr, io.train.bits.lastAddr)
  val targetAddr_s0 = Mux(pendingValid_s0, trainPending.currAddr, io.train.bits.currAddr)
  val pc_s0 = Mux(pendingValid_s0, trainPending.pc, io.train.bits.pc)
  val cnt_s0 = Mux(pendingValid_s0, trainPending.cnt, io.train.bits.cnt)

  val samplerTableRValid = s0_valid
  val (baseTag_s0, baseSet_s0) = parsePaddr(baseAddr_s0)
  val pairs_s1 = samplerTable.io.r(samplerTableRValid, baseSet_s0).resp.data

  /* ------- stage 1 ------- */
  // parse baseAddr to judge whether hit; choose victim way
  val s1_valid = RegNext(s0_valid, false.B)
  val baseAddr_s1 = RegEnable(baseAddr_s0, s0_valid)
  val targetAddr_s1 = RegEnable(targetAddr_s0, s0_valid)
  val pc_s1 = RegEnable(pc_s0, s0_valid)
  val cnt_s1 = RegEnable(cnt_s0, s0_valid)

  val (baseTag_s1, baseSet_s1) = parsePaddr(baseAddr_s1)
  val tagMatchVec_s1 = pairs_s1.map(_.baseTag === baseTag_s1)
  val validVec_s1 = pairs_s1.map(_.valid)
  val hitVec_s1 = tagMatchVec_s1.zip(validVec_s1).map(x => x._1 && x._2)
  val hit_s1 = Cat(hitVec_s1).orR

  val hitWay_s1 = OHToUInt(hitVec_s1)
  val victimWay_s1 = repl.way
  val way_s1 = Mux(hit_s1, hitWay_s1, victimWay_s1)
  val lastPair_s1 = pairs_s1(way_s1)

  when(hit_s1) {
    repl.access(hitWay_s1)
  }.otherwise {
    repl.miss
  }

  /* ------- stage 2 ------- */
  // (1) hit: compare targetAddr with hit entry's targetAddr; generate new entry
  // (2) miss: generate new entry
  // hit & match: update hit entry state; send pair to recorder
  // hit & missmatch: update hit entry
  // miss: replace victim entry
  val s2_valid = RegNext(s1_valid, false.B)
  val baseAddr_s2 = RegEnable(baseAddr_s1, s1_valid)
  val targetAddr_s2 = RegEnable(targetAddr_s1, s1_valid)
  val pc_s2 = RegEnable(pc_s1, s1_valid)
  val cnt_s2 = RegEnable(cnt_s1, s1_valid)
  val hit_s2 = RegEnable(hit_s1, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val lastPair_s2 = RegEnable(lastPair_s1, s1_valid)

  val cntValid_s2 = cnt_s2 >= filteredCntThrottle.U && s2_valid

  val match_s2 = lastPair_s2.targetAddr === targetAddr_s2
  val pcMatch_s2 = pc_s2 === lastPair_s2.pc
  val matchValid_s2 = match_s2 && pcMatch_s2
  val recordValid_s2 = (matchValid_s2 || lastPair_s2.matchCnt.orR) && hit_s2 && s2_valid

  val (baseTag_s2, baseSet_s2) = parsePaddr(baseAddr_s2)
  val maxMatch = lastPair_s2.matchCnt.andR
  val minMatch = !lastPair_s2.matchCnt.orR
  val updateMatchCnt = Mux(match_s2 && pcMatch_s2 && !maxMatch, lastPair_s2.matchCnt + 1.U,
    Mux(match_s2 && !pcMatch_s2 && !maxMatch, lastPair_s2.matchCnt,
      Mux(!match_s2 && !minMatch, lastPair_s2.matchCnt - 1.U, lastPair_s2.matchCnt)))
  val updateEntry = WireInit(new samplerTableEntry().apply(true.B, baseTag_s2, targetAddr_s2, pc_s2, updateMatchCnt))
  val replEntry = WireInit(new samplerTableEntry().apply(true.B, baseTag_s2, targetAddr_s2, pc_s2, 0.U))
  val resetEntry = WireInit(new samplerTableEntry().apply(false.B, 0.U, 0.U, 0.U, 0.U))

  val samplerTableWValid_s2 = recordValid_s2 || !hit_s2 && s2_valid || !resetFinish
  val samplerTableWSet_s2 = Mux(resetFinish, baseSet_s2, resetIdx)
  val samplerTableWWayOH_s2 = Mux(resetFinish, UIntToOH(way_s2), Fill(samplerTableAssoc, true.B))
  val samplerTableWEntry_s2 = Mux(resetFinish, Mux(hit_s2, updateEntry, replEntry), resetEntry)

  samplerTableWValid := samplerTableWValid_s2

  samplerTable.io.w.apply(
    valid = samplerTableWValid_s2,
    data = samplerTableWEntry_s2,
    setIdx = samplerTableWSet_s2,
    waymask = samplerTableWWayOH_s2
  )

  io.trained.valid := recordValid_s2 || cntValid_s2
  io.trained.bits.pc := pc_s2
  io.trained.bits.addr1 := baseAddr_s2
  io.trained.bits.addr2 := targetAddr_s2


  XSPerfAccumulate("tp_sampler_table_trained_valid", io.trained.valid)
  XSPerfAccumulate("tp_sampler_table_hit", s2_valid && hit_s2)
  XSPerfAccumulate("tp_sampler_table_hit_pc_match", s2_valid && hit_s2 && pcMatch_s2)
  XSPerfAccumulate("tp_sampler_table_hit_addr_match", s2_valid && hit_s2 && match_s2)
  XSPerfHistogram("tp_sampler_table_matchCnt", perfCnt = lastPair_s2.matchCnt, enable = s2_valid && hit_s2, start = 0, stop = log2Ceil(samplerTableMatchCntWidth) - 1, step = 1)
  XSPerfAccumulate("tp_sampler_table_record_valid", recordValid_s2)
  XSPerfAccumulate("tp_sampler_table_cnt_valid", cntValid_s2)
  XSPerfAccumulate("tp_sampler_table_W", samplerTableWValid)

  val samplerDB = ChiselDB.createTable("tpsampler", new trainedPair(), basicDB = true)
  val samplerPt = Wire(new trainedPair())
  samplerPt := io.trained.bits
  samplerDB.log(samplerPt, io.trained.valid, "", clock, reset)
}

class trainedRecord(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val data = Vec(tpEntryMaxLen, UInt(metaDataLength.W))
  val length = UInt(log2Ceil(tpEntryMaxLen).W)
  val trigger = UInt(metaDataLength.W)
}

class recorderTableEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val pcTag = UInt((pcHashWidth - recorderTableSetBits).W)
  val data = Vec(tpEntryMaxLen, UInt(metaDataLength.W))
  val index = UInt(log2Ceil(tpEntryMaxLen).W)
  val trigger = UInt(metaDataLength.W)
  val padding = UInt(log2Ceil(tpTableAssoc).W)

  def apply(valid: Bool, tag: UInt, data: Vec[UInt], index: UInt, trigger: UInt, pad: UInt) = {
    val entry = Wire(new recorderTableEntry)
    entry.valid := valid
    entry.pcTag := tag
    entry.data := data
    entry.index := index
    entry.trigger := trigger
    entry.padding := pad
    entry
  }
}

class RecorderTable(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val pair = Flipped(ValidIO(new trainedPair()))
    val record = ValidIO(new trainedRecord())
  })

  def parsePaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, recorderTableSetBits), x(recorderTableSetBits - 1, 0))
  }

  val recordThres = tpEntryMaxLen.U
  val recordFinish = RegInit(false.B)

  val recorderTable = Module( // change splitted or more sram
    new SRAMTemplate(
      new recorderTableEntry(),
      set = recorderTableNrSet,
      way = recorderTableAssoc,
      shouldReset = false,
      singlePort = true
    )
  )
  val repl = ReplacementPolicy.fromString(recorderTableReplacementPolicy, recorderTableAssoc)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((recorderTableNrSet - 1).U)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  /* ------- stage pre ------- */
  // handle SRAM RW conflict
  val recorderTableRPendingValid = Wire(Bool())
  val recorderTableWValid = Wire(Bool())
  val pairPending = RegEnable(io.pair.bits, recorderTableRPendingValid)
  val pendingValid_s0 = RegNext(recorderTableRPendingValid, false.B)
  recorderTableRPendingValid := io.pair.valid && (recorderTableWValid || pendingValid_s0)

  /* ------- stage 0 ------- */
  // query recorderTable
  val s0_valid = io.pair.valid && !recorderTableWValid || pendingValid_s0
  val addr1_s0 = Mux(pendingValid_s0, pairPending.addr1, io.pair.bits.addr1)
  val addr2_s0 = Mux(pendingValid_s0, pairPending.addr2, io.pair.bits.addr2)
  val pc_s0 = Mux(pendingValid_s0, pairPending.pc, io.pair.bits.pc)

  val recorderTableRValid = s0_valid
  val (pcTag_s0, pcSet_s0) = parsePaddr(pc_s0)
  val recorders_s1 = recorderTable.io.r(recorderTableRValid, pcSet_s0).resp.data

  /* ------- stage 1 ------- */
  // parse pc to judge whether hit; choose victim way
  val s1_valid = RegNext(s0_valid, false.B)
  val addr1_s1 = RegEnable(addr1_s0, s0_valid)
  val addr2_s1 = RegEnable(addr2_s0, s0_valid)
  val pc_s1 = RegEnable(pc_s0, s0_valid)

  val (pcTag_s1, pcSet_s1) = parsePaddr(pc_s1)
  val tagMatchVec_s1 = recorders_s1.map(_.pcTag === pcTag_s1)
  val validVec_s1 = recorders_s1.map(_.valid)
  val hitVec_s1 = tagMatchVec_s1.zip(validVec_s1).map(x => x._1 && x._2)
  val hit_s1 = Cat(hitVec_s1).orR

  val hitWay_s1 = OHToUInt(hitVec_s1)
  val victimWay_s1 = repl.way
  val way_s1 = Mux(hit_s1, hitWay_s1, victimWay_s1)
  val recorder_s1 = recorders_s1(way_s1)

  when(hit_s1) {
    repl.access(hitWay_s1)
  }.otherwise {
    repl.miss
  }

  /* ------- stage 2 ------- */
  // hit: update record
  // miss: replace
  // if full or replaced, output record
  val s2_valid = RegNext(s1_valid, false.B)
  val addr1_s2 = RegEnable(addr1_s1, s1_valid)
  val addr2_s2 = RegEnable(addr2_s1, s1_valid)
  val pc_s2 = RegEnable(pc_s1, s1_valid)
  val hit_s2 = RegEnable(hit_s1, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val recorder_s2 = RegEnable(recorder_s1, s1_valid)
  val recorderIdx_s2 = RegEnable(recorder_s1.index, s1_valid)
  val recorderData_s2 = RegEnable(recorder_s1.data, s1_valid)
  val recorderValid_s2 = RegEnable(recorder_s1.valid, s1_valid)

  val recorderIdx_s3 = RegInit(0.U(log2Ceil(tpEntryMaxLen).W))
  val recorderData_s3 = RegInit(VecInit(Seq.fill(tpEntryMaxLen)(0.U(metaDataLength.W))))
  val recorderTrigger_s3 = RegInit(0.U(metaDataLength.W))

  val recordData_s2 = recorder_s2.data
  val recordAddr1HitVec = recordData_s2.map(_ === addr1_s2)
  val recordAddr2HitVec = recordData_s2.map(_ === addr2_s2)
  val addr1Unique_s2 = !Cat(recordAddr1HitVec).orR
  val addr2Unique_s2 = !Cat(recordAddr2HitVec).orR

  when(s2_valid && hit_s2) {
    recorderData_s3 := recorderData_s2
    recorderTrigger_s3 := recorder_s2.trigger

    when(addr1Unique_s2 ^ addr2Unique_s2) {
      recorderData_s3(recorderIdx_s2) := Mux(addr1Unique_s2, addr1_s2, addr2_s2)
      when(recorderIdx_s2 === (recordThres - 1.U)) {
        recorderIdx_s3 := recordThres
        recordFinish := true.B
      }.otherwise {
        recorderIdx_s3 := recorderIdx_s2 + 1.U
      }
    }.elsewhen(addr1Unique_s2 && addr2Unique_s2) {
      recorderData_s3(recorderIdx_s2) := addr1_s2
      when(recorderIdx_s2 === (recordThres - 1.U)) {
        recorderIdx_s3 := recordThres
        recordFinish := true.B
      }.elsewhen(recorderIdx_s2 === (recordThres - 2.U)) {
        recorderData_s3(recorderIdx_s2 + 1.U) := addr2_s2
        recorderIdx_s3 := recordThres
        recordFinish := true.B
      }.otherwise {
        recorderData_s3(recorderIdx_s2 + 1.U) := addr2_s2
        recorderIdx_s3 := recorderIdx_s2 + 2.U
      }
    }.otherwise {
      recorderIdx_s3 := recorderIdx_s2
    }
  }.elsewhen(s2_valid && !hit_s2) {
    recorderIdx_s3 := recorderIdx_s2
    recorderData_s3 := recorderData_s2
    recorderTrigger_s3 := recorder_s2.trigger
  }

  /* ------- stage 3 ------- */
  val s3_valid = RegNext(s2_valid, false.B)
  val addr1_s3 = RegEnable(addr1_s2, s2_valid)
  val addr2_s3 = RegEnable(addr2_s2, s2_valid)
  val pc_s3 = RegEnable(pc_s2, s2_valid)
  val recorderValid_s3 = RegEnable(recorderValid_s2, s2_valid)
  val hit_s3 = RegEnable(hit_s2, s2_valid)
  val way_s3 = RegEnable(way_s2, s2_valid)

  val (pcTag_s3, pcSet_s3) = parsePaddr(pc_s3)
  val full_s3 = recorderIdx_s3 === recordThres
  val recordValid_s3 = s3_valid && (full_s3 && hit_s3 || !hit_s3 && recorderValid_s3)
  val recordFull_s3 = s3_valid && full_s3 && hit_s3

  val replEntryData_s3 = WireInit(VecInit(Seq.fill(tpEntryMaxLen)(0.U(metaDataLength.W))))
  replEntryData_s3(0) := addr2_s3
  // replEntryData_s3(0) := addr1_s3
  // replEntryData_s3(1) := addr2_s3
  val resetEntryData_s3 = WireInit(VecInit(Seq.fill(tpEntryMaxLen)(0.U(metaDataLength.W))))

  val updateEntry = WireInit(new recorderTableEntry().apply(true.B, pcTag_s3, recorderData_s3, recorderIdx_s3, recorderTrigger_s3, 0.U))
  val replEntry = WireInit(new recorderTableEntry().apply(true.B, pcTag_s3, replEntryData_s3, 1.U, addr1_s3, 0.U))
  val resetEntry = WireInit(new recorderTableEntry().apply(false.B, 0.U, resetEntryData_s3, 0.U, 0.U, 0.U))

  val recorderTableWValid_s3 = s3_valid || !resetFinish
  val recorderTableWSet_s3 = Mux(resetFinish, pcSet_s3, resetIdx)
  val recorderTableWWayOH_s3 = Mux(resetFinish, UIntToOH(way_s2), Fill(recorderTableAssoc, true.B))
  val recorderTableWEntry_s3 = Mux(resetFinish && !recordFull_s3, Mux(hit_s3, updateEntry, replEntry), resetEntry)

  recorderTableWValid := recorderTableWValid_s3

  recorderTable.io.w.apply(
    valid = recorderTableWValid_s3,
    data = recorderTableWEntry_s3,
    setIdx = recorderTableWSet_s3,
    waymask = recorderTableWWayOH_s3
  )

  io.record.valid := recordValid_s3
  io.record.bits.pc := pc_s2
  io.record.bits.data := recorderData_s3
  io.record.bits.length := recorderIdx_s3
  io.record.bits.trigger := recorderTrigger_s3

  assert(!(io.record.valid && io.record.bits.length === 0.U))
  assert(!(recorderTableWValid_s3 && recorderTableWEntry_s3.valid && (recorderTableWEntry_s3.index >= recordThres)))
  assert(recorderIdx_s3 < (recordThres + 1.U))

  XSPerfAccumulate("tp_recorder_valid", io.record.valid)
  XSPerfAccumulate("tp_recorder_addr1_unique", s2_valid && addr1Unique_s2)
  XSPerfAccumulate("tp_recorder_addr2_unique", s2_valid && addr2Unique_s2)
  XSPerfAccumulate("tp_recorder_table_w", recorderTableWValid)
  XSPerfHistogram("tp_recorder_index", perfCnt = updateEntry.index, enable = hit_s3 && s3_valid, start = 0, stop = tpEntryMaxLen - 1, step = 1)

  val recorderDB = ChiselDB.createTable("tprecorder", new trainedRecord(), basicDB = true)
  val recorderPt = Wire(new trainedRecord())
  recorderPt := io.record.bits
  recorderDB.log(recorderPt, io.record.valid, "", clock, reset)
}

class Sampler(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(ValidIO(new PrefetchTrain()))
    val trained = ValidIO(new trainedRecord())
  })

  val filterTable = Module(new SamplerFilter())
  val samplerTable = Module(new SamplerTable())
  val recorderTable = Module(new RecorderTable())

  filterTable.io.train := io.train
  samplerTable.io.train := filterTable.io.trained
  // recorderTable.io.pair := samplerTable.io.trained
  io.trained := recorderTable.io.record

  recorderTable.io.pair.valid := filterTable.io.trained.valid
  recorderTable.io.pair.bits.pc := filterTable.io.trained.bits.pc
  recorderTable.io.pair.bits.addr1 := filterTable.io.trained.bits.lastAddr
  recorderTable.io.pair.bits.addr2 := filterTable.io.trained.bits.currAddr

}

class confTableEntry(implicit p: Parameters) extends TPBundle {
  val valid = Bool()
  val tag = UInt((pcHashWidth - confTableSetBits).W) // pc
  val accConf = UInt(accConfWidth.W)

  def apply(valid: Bool, tag: UInt, accConf: UInt) = {
    val entry = Wire(new confTableEntry)
    entry.valid := valid
    entry.tag := tag
    entry.accConf := accConf
    entry
  }
}

class confReq(implicit p: Parameters) extends TPBundle {
  val pc = UInt(pcHashWidth.W)
  val newMeta = Bool()
  val pfIssue = Bool()
  val pfHit = Bool()
  val pfLate = Bool()
  val pfMiss = Bool()
}

class confResp(implicit  p: Parameters) extends TPBundle {
  val issue = Bool()
  // TODO: issue length
}

class confTable(implicit p:Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val req = Flipped(ValidIO(new confReq()))
    val resp = ValidIO(new confResp())
  })

  def parsePC(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, confTableSetBits), x(confTableSetBits - 1, 0))
  }

  val confTable = Module(
    new SRAMTemplate(
      new confTableEntry(),
      set = confTableNrSet,
      way = confTableAssoc,
      shouldReset = false,
      singlePort = true
    )
  )
  val repl = ReplacementPolicy.fromString(confTableReplacementPolicy, confTableAssoc)

  val reqQueue = Module(new Queue(new confReq(), confReqQueueDepth, pipe = false, flow = false))

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((confTableNrSet - 1).U)
  val resetCnt = RegInit(100000.U)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }.elsewhen(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  when(resetCnt === 0.U) {
    resetCnt := 100000.U
    resetIdx := (confTableNrSet - 1).U
    resetFinish := false.B
  }.elsewhen(io.req.valid && io.req.bits.newMeta) {
    resetCnt := resetCnt - 1.U
  }

  reqQueue.io.enq.valid := io.req.valid
  reqQueue.io.enq.bits := io.req.bits
  reqQueue.io.deq.ready := !confTable.io.w.req.fire // W first

  /* ------- stage 0 ------- */
  // query confTable
  val s0_valid = reqQueue.io.deq.fire
  val req_s0 = reqQueue.io.deq.bits
  val (tag_s0, set_s0) = parsePC(req_s0.pc)
  val newMeta_s0 = req_s0.newMeta
  val pfHit_s0 = req_s0.pfHit
  val pfIssue_s0 = req_s0.pfIssue

  val confs = confTable.io.r(s0_valid && !newMeta_s0, set_s0).resp.data

  /* ------- stage 1 ------- */
  // parse pc to judge whether hit; choose victim way
  val s1_valid = RegNext(s0_valid, false.B)
  val tag_s1 = RegEnable(tag_s0, s0_valid)
  val set_s1 = RegEnable(set_s0, s0_valid)
  val newMeta_s1 = RegEnable(newMeta_s0, s0_valid)
  val pfHit_s1 = RegEnable(pfHit_s0, s0_valid)
  val pfIssue_s1 = RegEnable(pfIssue_s0, s0_valid)
  val needResp_s1 = RegEnable(req_s0.pfIssue, s0_valid)

  val tagMatchVec_s1 = confs.map(_.tag === tag_s1)
  val validVec_s1 = confs.map(_.valid)
  val hitVec_s1 = tagMatchVec_s1.zip(validVec_s1).map(x => x._1 && x._2)
  val hit_s1 = Cat(hitVec_s1).orR

  val hitWay_s1 = OHToUInt(hitVec_s1)
  val victimWay_s1 = repl.way
  val way_s1 = Mux(hit_s1, hitWay_s1, victimWay_s1)
  val conf_s1 = confs(way_s1)

  when(hit_s1) {
    repl.access(hitWay_s1)
  }.otherwise {
    repl.miss
  }

  /* ------- stage 2 ------- */
  // (1) hit: update conf && resp(if need
  // (2) miss: generate new entry
  val s2_valid = RegNext(s1_valid, false.B)
  val tag_s2 = RegEnable(tag_s1, s1_valid)
  val set_s2 = RegEnable(set_s1, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val conf_s2 = RegEnable(conf_s1, s1_valid)
  val hit_s2 = RegEnable(hit_s1, s1_valid)
  val newMeta_s2 = RegEnable(newMeta_s1, s1_valid)
  val pfHit_s2 = RegEnable(pfHit_s1, s1_valid)
  val pfIssue_s2 = RegEnable(pfIssue_s1, s1_valid)
  val needResp_s2 = RegEnable(needResp_s1, s1_valid)

  val accUpper = conf_s2.accConf === (1 << accConfWidth - 1).U && pfHit_s2 && !pfIssue_s2
  val accLower = conf_s2.accConf < 10.U && pfIssue_s2 && !pfHit_s2 || conf_s2.accConf < 9.U && pfIssue_s2 && pfHit_s2
  val accUpdate = Mux(accUpper, conf_s2.accConf, Mux(accLower, 0.U,
    Mux(pfHit_s2 && !pfIssue_s2, conf_s2.accConf + 1.U,
      Mux(pfHit_s2 && pfIssue_s2, conf_s2.accConf - 9.U,
        Mux(!pfHit_s2 && pfIssue_s0, conf_s2.accConf - 10.U, conf_s2.accConf)))))
  val updateEntry = WireInit(new confTableEntry().apply(true.B, tag_s2, conf_s2.accConf + accUpdate))
  val replEntry = WireInit(new confTableEntry().apply(true.B, tag_s2, (1 << accConfWidth - 1).U))
  val resetEntry = WireInit(new confTableEntry().apply(false.B, 0.U, 0.U))

  val confTableWValid_s2 = s2_valid || !resetFinish
  val confTableWSet_s2 = Mux(resetFinish, set_s2, resetIdx)
  val confTableWWayOH_s2 = Mux(resetFinish, UIntToOH(way_s2), Fill(confTableAssoc, true.B))
  val confTableWEntry_s2 = Mux(resetFinish, Mux(hit_s2, updateEntry, replEntry), resetEntry)

  confTable.io.w.apply(
    valid = confTableWValid_s2,
    data = confTableWEntry_s2,
    setIdx = confTableWSet_s2,
    waymask = confTableWWayOH_s2
  )

  io.resp.valid := s1_valid && needResp_s1
  io.resp.bits.issue := Mux(hit_s1, conf_s1.accConf > ((1 << accConfWidth - 1) >> 1).U, true.B)

  XSPerfAccumulate("tp_conf_table_pf_hit", io.req.valid && io.req.bits.pfHit)
  XSPerfAccumulate("tp_conf_table_pf_late", io.req.valid && io.req.bits.pfLate)
  XSPerfAccumulate("tp_conf_table_pf_miss", io.req.valid && io.req.bits.pfMiss)
  XSPerfAccumulate("tp_conf_table_pf_issue", io.req.valid && io.req.bits.pfIssue)
  XSPerfAccumulate("tp_conf_table_new_meta", io.req.valid && io.req.bits.newMeta)
  XSPerfAccumulate("tp_conf_table_resp", io.resp.valid)
  XSPerfAccumulate("tp_conf_table_resp_issue", io.resp.bits.issue && io.resp.valid)
}

class tpMetaEntry(implicit p:Parameters) extends TPBundle {
  val valid = Bool()
  // val triggerTag = UInt((fullAddressBits - blockOffBits - tpTableSetBits).W)
  // val pcTag = UInt((pcHashWidth - tpTableSetBits).W)
  val tag = UInt((pcAddrHashWidth - tpTableSetBits).W)
}

class tpDataEntry(implicit p:Parameters) extends TPBundle {
  val rawData = Vec(tpEntryMaxLen, UInt((fullAddressBits - offsetBits).W))
  val length = UInt(log2Ceil(tpEntryMaxLen).W)
  val hitCount = UInt(hitCountWidth.W)
  // val rawData_debug = Vec(tpEntryMaxLen, UInt(vaddrBits.W))
  // TODO: val compressedData = UInt(512.W)
}

class tpMetaResetEntry(implicit p:Parameters) extends TPBundle {
  val set = UInt(tpTableSetBits.W)
  val way = UInt(log2Ceil(tpTableAssoc).W)
  val tag = UInt((pcAddrHashWidth - tpTableSetBits).W)
  val hitCount = UInt(hitCountWidth.W)
}

class tpMetaHitCountEntry(implicit p:Parameters) extends TPBundle {
  val hitCount = UInt(hitCountWidth.W)
}

class trainBundle(implicit p: Parameters) extends TPBundle { //db
  val vaddr = UInt(vaddrBits.W)
  val paddr = UInt(fullAddressBits.W)
  val hit = Bool()
  val prefetched = Bool()
  val pfsource = UInt(PfSource.pfSourceBits.W)
  val metahit = Bool()
  val pc = UInt(pcHashWidth.W)
}

class sendBundle(implicit p: Parameters) extends TPBundle { //db
  val paddr = UInt(fullAddressBits.W)
  val vaddr = UInt(vaddrBits.W)
}

class TemporalPrefetch(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val enable = Input(Bool())
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val hartid = Input(UInt(hartIdLen.W))
    val feedBack = Flipped(DecoupledIO(new PrefetchFeedBack))
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

  def hashPC(pc: UInt) = {
    val reservedHead = pc(pc.getWidth - 1, pc.getWidth - pcHashHeadReservedWidth)
    val reservedTail = pc(pcHashTailReservedWidth - 1, 0)
    val mid = pc(pc.getWidth - pcHashHeadReservedWidth - 1, pcHashTailReservedWidth)
    val hashMid = mid(pcHashMidWidth - 1, 0) ^ mid(pcHashMidWidth * 2 - 1, pcHashMidWidth) ^
      mid(pcHashMidWidth * 3 - 1, pcHashMidWidth * 2)
    Cat(reservedTail, hashMid, reservedHead)
  }

  val tpmeta = Module(new TPmeta())
  val tpMetaTable = Module(
    new SRAMTemplate(
      new tpMetaEntry(),
      set = tpTableNrSet,
      way = tpTableAssoc,
      shouldReset = false,
      singlePort = true,
      hasMbist = cacheParams.hasMbist,
      hasSramCtl = cacheParams.hasSramCtl
    )
  )
  val hitCount = RegInit(VecInit(Seq.fill(tpTableNrSet)(VecInit(Seq.fill(tpTableAssoc)(0.U.asTypeOf(new tpMetaHitCountEntry))))))
  val globalConfidence = RegInit(VecInit(Seq.fill(1 << hitCountWidth)(globalHitCountConfidenceInitVal.U(globalHitCountConfidenceWidth.W))))
  val sampler = Module(new Sampler())
  val confTable = Module(new confTable())
  val trainQueue = Module(new Queue(new PrefetchTrain(), tpTrainQueueDepth, pipe = false, flow = false))
  val dataReadQueue = Module(new Queue(new TPmetaReq(), dataReadQueueDepth, pipe = false, flow = false))
  val dataWriteQueue = Module(new Queue(new TPmetaReq(), dataWriteQueueDepth, pipe = false, flow = false))
  val tpDataQueue = Module(new Queue(new tpDataEntry(), tpDataQueueDepth + 1, pipe = false, flow = false))
  val metaWQueue = Module(new Queue(new trainedRecord(), tpMetaWQueueDepth, pipe = false, flow = false))
  val tpMetaResetQueue = Module(new Queue(new tpMetaResetEntry(), tpMetaResetQueueDepth, pipe = false, flow = false))
  val confRespQueue = Module(new Queue(new confResp(), confReqQueueDepth + 1, pipe = false, flow = false))

  val repl = ReplacementPolicy.fromString(tpTableReplacementPolicy, tpTableAssoc)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((tpTableNrSet - 1).U)

  /* Constantin Parameters */

  val hartid = cacheParams.hartId
  // 0 / 1: whether to enable temporal prefetcher
  // private val cstEnable = Constantin.createRecord("tp_enable"+hartid.toString, initValue = 1)
  val cstEnable = true.B
  // 0 ~ N: throttle cycles for each prefetch request
  // private val tpThrottleCycles = Constantin.createRecord("tp_throttleCycles"+hartid.toString, initValue = 4)
  val tpThrottleCycles = 4.U
  // 0 / 1: whether request to set as trigger on meta hit
  // private val hitAsTrigger = Constantin.createRecord("tp_hitAsTrigger"+hartid.toString, initValue = 1)
  val recordThres = tpEntryMaxLen.U
  // 0 / 1: whether to train on vaddr
  // private val trainOnVaddr = Constantin.createRecord("tp_trainOnVaddr"+hartid.toString, initValue = 0)
  val trainOnVaddr = false.B
  // 0 / 1: whether to eliminate L1 prefetch request training
  // private val trainOnL1PF = Constantin.createRecord("tp_trainOnL1PF"+hartid.toString, initValue = 0)
  val trainOnL1PF = false.B
  // val enable = io.enable && cstEnable.orR
  // val enable = cstEnable.orR
  val enable = true.B

  if (vaddrBitsOpt.isEmpty) {
    assert(!trainOnVaddr)
  }

  trainQueue.io.enq.valid := io.train.fire
  trainQueue.io.enq.bits := io.train.bits
  trainQueue.io.deq.ready := !tpMetaTable.io.w.req.fire // meta table W first

  /* Stage 0: query tpMetaTable */

  val train_s0 = trainQueue.io.deq.bits
  val s0_valid = trainQueue.io.deq.fire && train_s0.pc.orR && // not trainOnL1PF
    Mux(trainOnVaddr.orR, train_s0.vaddr.getOrElse(0.U) =/= 0.U, true.B) &&
    Mux(trainOnL1PF.orR, true.B, train_s0.reqsource =/= MemReqSource.L1DataPrefetch.id.U)
  val trainVaddr = train_s0.vaddr.getOrElse(0.U)
  val trainPaddr = train_s0.addr
  val trainMeta = trainPaddr >> offsetBits
  val trainPC = hashPC(train_s0.pc)
  val trainIndex = trainMeta.asUInt.pad(pcAddrHashWidth) ^ trainPC.pad(pcAddrHashWidth)
  // val (vtag_s0, vset_s0) = if (vaddrBitsOpt.nonEmpty) parseVaddr(trainVaddr) else (0.U, 0.U)
  val (tag_s0, set_s0) = parsePaddr(trainIndex)
  // val metas = tpMetaTable.io.r(s0_valid, Mux(trainOnVaddr.orR, vset_s0, pset_s0)).resp.data
  val metas = tpMetaTable.io.r(s0_valid, set_s0).resp.data // get in s1

  // to sampler
  sampler.io.train.valid := io.train.fire && io.train.bits.pc.orR && // not trainOnL1PF
    Mux(trainOnVaddr.orR, io.train.bits.vaddr.getOrElse(0.U) =/= 0.U, true.B) &&
    Mux(trainOnL1PF.orR, true.B, io.train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U)
  sampler.io.train.bits := io.train.bits


  /* Stage 1: parse tpMeta to judge hit or miss, choose the victim */

  val s1_valid = RegNext(s0_valid, false.B)
  val train_s1 = RegEnable(train_s0, s0_valid)
  val trainVaddr_s1 = train_s1.vaddr.getOrElse(0.U)
  val trainPaddr_s1 = train_s1.addr
  val trainPC_s1 = hashPC(train_s1.pc)
  val trainIndex_s1 = RegEnable(trainIndex, s0_valid)
  // val (vtag_s1, vset_s1) = if (vaddrBitsOpt.nonEmpty) parseVaddr(trainVaddr_s1) else (0.U, 0.U)
  val (tag_s1, set_s1) = parsePaddr(trainIndex_s1)

  // val tagMatchVec = metas.map(_.triggerTag === Mux(trainOnVaddr.orR, vtag_s1, ptag_s1))
  val tagMatchVec = metas.map(_.tag === tag_s1)
  val metaValidVec = metas.map(_.valid === true.B)

  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)

  val hit_s1 = Cat(hitVec).orR
  val way_s1 = hitWay
  val hitCount_s1 = hitCount(set_s1)(way_s1).hitCount
  assert(PopCount(hitVec) <= 1.U)

  when(hit_s1) {
    repl.access(hitWay)
  }.elsewhen(metaWQueue.io.deq.fire) {
    repl.miss
  }

  // from sampler
  metaWQueue.io.enq.valid := sampler.io.trained.valid
  metaWQueue.io.enq.bits := sampler.io.trained.bits
  metaWQueue.io.deq.ready := !(hit_s1 && s1_valid) // when hit, update repl state first

  // meta reset queue
  // now use to upadte hitCount
  tpMetaResetQueue.io.enq.valid := s1_valid && hit_s1
  tpMetaResetQueue.io.enq.bits.set := set_s1
  tpMetaResetQueue.io.enq.bits.way := way_s1
  tpMetaResetQueue.io.enq.bits.tag := tag_s1
  tpMetaResetQueue.io.enq.bits.hitCount := Mux(hitCount_s1.andR, hitCount_s1, hitCount_s1 + 1.U)

  /* Stage 2: access tpData on meta hit, record it on meta miss */

  val s2_valid = RegNext(s1_valid, false.B)
  val hit_s2 = RegEnable(hit_s1, false.B, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  // val vset_s2 = RegEnable(vset_s1, s1_valid)
  val set_s2 = RegEnable(set_s1, s1_valid)
  val tag_s2 = RegEnable(tag_s1, s1_valid)
  val train_s2 = RegEnable(train_s1, s1_valid)
  val hitCount_s2 = RegEnable(hitCount_s1, s1_valid)

  // dataReadQueue enqueue
  dataReadQueue.io.enq.valid := s2_valid && hit_s2
  // dataReadQueue.io.enq.bits.set := Mux(trainOnVaddr.orR, vset_s2, pset_s2)
  dataReadQueue.io.enq.bits.set := set_s2
  dataReadQueue.io.enq.bits.way := way_s2
  dataReadQueue.io.enq.bits.wmode := false.B
  dataReadQueue.io.enq.bits.rawData := DontCare
  dataReadQueue.io.enq.bits.length := 0.U //DontCare
  dataReadQueue.io.enq.bits.hartid := io.hartid
  dataReadQueue.io.enq.bits.hitCount := hitCount_s2


  /* Async Stage: try to fetch or write tpData */

  // dataReadQueue/dataWriteQueue dequeue
  val pendingRead = RegInit(0.U)  // TODO: monitor pending read request and throttle it
  dataReadQueue.io.deq.ready := tpmeta.io.req.ready && !dataWriteQueue.io.deq.valid
  dataWriteQueue.io.deq.ready := tpmeta.io.req.ready

  // tpmeta_port req assignment, priority: write > read
  val readReqValid = dataReadQueue.io.deq.valid
  val writeReqValid = dataWriteQueue.io.deq.valid
  tpmeta.io.req.valid := readReqValid || writeReqValid
  tpmeta.io.req.bits := Mux(writeReqValid, dataWriteQueue.io.deq.bits, dataReadQueue.io.deq.bits)


  /* Async Stage: get tpMeta and insert it into tpDataQueue */
  // disable prefetched pc (wait for next round of training

  tpDataQueue.io.enq.valid := tpmeta.io.resp.valid && tpmeta.io.resp.bits.hartid === io.hartid
  tpDataQueue.io.enq.bits.rawData := tpmeta.io.resp.bits.rawData
  tpDataQueue.io.enq.bits.length := tpmeta.io.resp.bits.length
  tpDataQueue.io.enq.bits.hitCount := tpmeta.io.resp.bits.hitCount
  assert(tpDataQueue.io.enq.ready === true.B) // tpDataQueue is never full

  tpMetaResetQueue.io.deq.ready := !(sampler.io.trained.valid || !resetFinish)
  assert(tpMetaResetQueue.io.enq.ready === true.B)

  val tpTableWValid = metaWQueue.io.deq.fire || !resetFinish
  val metaWRecord = metaWQueue.io.deq.bits
  val metaWRecordAddr = metaWRecord.pc.pad(pcAddrHashWidth) ^ metaWRecord.trigger.pad(pcAddrHashWidth)
  val (metaWRecordTag, metaWRecordSet) = parsePaddr(metaWRecordAddr)
  val metaWEntry = Wire(new tpMetaEntry())
  metaWEntry.valid := true.B
  metaWEntry.tag := metaWRecordTag
  when(!resetFinish) {
    metaWEntry.valid := false.B
    metaWEntry.tag := 0.U
  }
  val tpTableWLength = metaWRecord.length
  val tpTableWSet = Mux(resetFinish, metaWRecordSet, resetIdx)
  val tpTableWWay = repl.way
  val tpTableWWayOH = Mux(resetFinish, UIntToOH(tpTableWWay), Fill(tpTableAssoc, true.B))

  tpMetaTable.io.w.apply(tpTableWValid || !resetFinish, metaWEntry, tpTableWSet, tpTableWWayOH)

  when(metaWQueue.io.deq.fire) {
    hitCount(metaWRecordSet)(repl.way).hitCount := 0.U
  }.elsewhen(tpMetaResetQueue.io.deq.valid) {
    hitCount(tpMetaResetQueue.io.deq.bits.set)(tpMetaResetQueue.io.deq.bits.way).hitCount := tpMetaResetQueue.io.deq.bits.hitCount
  }

  dataWriteQueue.io.enq.valid := tpTableWValid
  dataWriteQueue.io.enq.bits.wmode := true.B
  dataWriteQueue.io.enq.bits.rawData.zip(metaWRecord.data).foreach(x => x._1 := x._2(metaDataLength - 1, 0))
  dataWriteQueue.io.enq.bits.length := tpTableWLength
  dataWriteQueue.io.enq.bits.set := tpTableWSet
  dataWriteQueue.io.enq.bits.way := tpTableWWay
  dataWriteQueue.io.enq.bits.hartid := io.hartid
  dataWriteQueue.io.enq.bits.hitCount := 0.U // DontCare
  assert(dataWriteQueue.io.enq.ready === true.B) // TODO: support back-pressure

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }


  /* Send prefetch request */

  val do_sending = RegInit(false.B)
  val sending_idx = RegInit(0.U(offsetBits.W))
  val sending_data = Reg(Vec(tpEntryMaxLen, UInt((fullAddressBits - offsetBits).W)))
  val sending_length = RegInit(0.U(log2Ceil(tpEntryMaxLen).W))
  val sending_hitCount = RegInit(0.U(hitCountWidth.W))
  // val sending_data_debug = Reg(Vec(tpEntryMaxLen, UInt(vaddrBits.W)))
  val sending_throttle = RegInit(0.U(4.W))
  val tpDataQFull = tpDataQueue.io.count === tpDataQueueDepth.U

  val sending_valid = do_sending && !tpDataQFull && sending_throttle === tpThrottleCycles
  val current_sending_data = Cat(sending_data(sending_idx), 0.U(offsetBits.W))
  val (sendingTag, sendingSet, _) = parseFullAddress(current_sending_data)

  tpDataQueue.io.deq.ready := tpDataQFull || !do_sending
  when(tpDataQueue.io.deq.fire && (tpDataQueue.io.deq.bits.length =/= 0.U)) {
    sending_data := tpDataQueue.io.deq.bits.rawData
    sending_length := tpDataQueue.io.deq.bits.length
    sending_hitCount := tpDataQueue.io.deq.bits.hitCount
    // sending_data_debug := tpDataQueue.io.deq.bits.rawData_debug
    sending_idx := 0.U
    do_sending := globalConfidence(tpDataQueue.io.deq.bits.hitCount) >= globalHitCountConfidenceThrottle.asUInt
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
    when(sending_idx === (recordThres-1.U) || sending_idx === (sending_length - 1.U)) {
      do_sending := false.B
    }
  }

  io.req.valid := Mux(enable, sending_valid, false.B)
  // io.req.valid := false.B
  io.req.bits.tag := sendingTag
  io.req.bits.set := sendingSet
  io.req.bits.vaddr.foreach(_ := 0.U)
  io.req.bits.needT := true.B
  io.req.bits.source := 0.U // TODO: ensure source 0 is dcache
  io.req.bits.pfSource := MemReqSource.Prefetch2L2TP.id.U
  io.req.bits.hitCount := sending_hitCount

  io.resp.ready := true.B
  io.train.ready := resetFinish
  io.feedBack.ready := resetFinish

  // global confidence
  val globalConfidenceReset = RegInit(1.U(10.W))
  val globalConfidenceInc = io.train.fire && io.train.bits.hit
  val globalConfideceIncIndex = io.train.bits.hitCount
  val globalConfidenceIncFull = globalConfidence(globalConfideceIncIndex).andR
  val globalConfidenceDec = tpDataQueue.io.deq.fire && (tpDataQueue.io.deq.bits.length =/= 0.U)
  val globalConfidenceDecIndex = tpDataQueue.io.deq.bits.hitCount
  val globalConfidenceDecEmpty = !globalConfidence(globalConfidenceDecIndex).orR
  val globalConfidenceRemain = (!globalConfidenceInc && !globalConfidenceDec) ||
    (globalConfidenceInc && globalConfidenceDec && (globalConfideceIncIndex === globalConfidenceDecIndex)) ||
    globalConfidenceInc && globalConfidenceIncFull ||
    globalConfidenceDec && globalConfidenceDecEmpty
  when(globalConfidenceInc || globalConfidenceDec) {
    globalConfidenceReset := globalConfidenceReset + 1.U
  }.elsewhen(globalConfidenceReset.andR || !globalConfidenceReset.orR) {
    globalConfidenceReset := 1.U
  }
  when(globalConfidenceReset === 1.U) {
    globalConfidence.foreach(x => x := globalHitCountConfidenceInitVal.asUInt)
  }.elsewhen(globalConfidenceInc && !globalConfidenceRemain) {
    globalConfidence(globalConfideceIncIndex) := globalConfidence(globalConfideceIncIndex) + 1.U
  }.elsewhen(globalConfidenceDec && !globalConfidenceRemain) {
    globalConfidence(globalConfidenceDecIndex) := globalConfidence(globalConfidenceDecIndex) - 1.U
  }

  // confidence table
  val pfHit = io.feedBack.bits.hit && MemReqSource.isCPUReq(io.feedBack.bits.reqsource) &&
    io.feedBack.bits.pfsource === PfSource.TP.id.U
  val pfLate = io.feedBack.bits.hit && io.feedBack.bits.reqsource =/= MemReqSource.Prefetch2L2TP.id.U
  val pfMiss = false.B // TODO
  val pfIssue = s2_valid && hit_s2
  val newMeta = metaWQueue.io.deq.fire
  confTable.io.req.valid := io.feedBack.valid || pfIssue || newMeta
  confTable.io.req.bits.pc := Mux(pfIssue, hashPC(train_s2.pc), Mux(io.feedBack.valid, io.feedBack.bits.pc, metaWRecord.pc)) //TODO:add queue?
  confTable.io.req.bits.pfHit := pfHit
  confTable.io.req.bits.pfLate := pfLate
  confTable.io.req.bits.pfMiss := pfMiss
  confTable.io.req.bits.pfIssue := pfIssue
  confTable.io.req.bits.newMeta := newMeta

  confRespQueue.io.enq.valid := confTable.io.resp.valid
  confRespQueue.io.enq.bits.issue := confTable.io.resp.bits.issue
  confRespQueue.io.deq.ready := tpDataQueue.io.deq.fire
  assert(confRespQueue.io.enq.ready === true.B)

  /* Performance collection */
  val hitCountS1 = WireInit(0.U(hitCountWidth.W))
  hitCountS1 := hitCount_s1
  XSPerfHistogram("tp_meta_hit_count", perfCnt = hitCountS1, enable = hit_s1 && s1_valid, start = 0, stop = 1 << hitCountWidth, step = 1)
  XSPerfAccumulate("tpHit", tpMetaResetQueue.io.enq.valid)

  XSPerfHistogram("tp_pf_hit_count", perfCnt = io.train.bits.hitCount, enable = io.train.bits.hit, start = 0, stop = 1 << hitCountWidth, step = 1)
  XSPerfHistogram("tp_pf_count", perfCnt = io.req.bits.hitCount, enable = io.req.valid, start = 0, stop = 1 << hitCountWidth, step = 1)

  val trainDB = ChiselDB.createTable("tptrain", new trainBundle(), basicDB = true)
  val trainPt = Wire(new trainBundle())
  trainPt.vaddr := recoverVaddr(train_s2.vaddr.getOrElse(0.U))
  trainPt.paddr := train_s2.addr
  trainPt.hit := train_s2.hit
  trainPt.prefetched := train_s2.prefetched
  trainPt.pfsource := train_s2.pfsource
  trainPt.metahit := hit_s2
  trainPt.pc := hashPC(train_s2.pc)

  val sendDB = ChiselDB.createTable("tpsend", new sendBundle(), basicDB = true)
  val sendPt = Wire(new sendBundle())
  sendPt.paddr := current_sending_data
  sendPt.vaddr := 0.U

  //  triggerDB.log(triggerPt, tpTable_w_valid, "", clock, reset)
  trainDB.log(trainPt, s2_valid, "", clock, reset)
  sendDB.log(sendPt, io.req.fire, "", clock, reset)
}