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
 * [1] Pierre Michaud. "[A Best-Offset Prefetcher.](https://inria.hal.science/hal-01165600/)" 2nd Data Prefetching
 * Championship (DPC). 2015.
 * [2] Pierre Michaud. "[Best-Offset Hardware Prefetching.](https://doi.org/10.1109/HPCA.2016.7446087)" IEEE
 * International Symposium on High Performance Computer Architecture (HPCA). 2016.
 * *************************************************************************************
 */

package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.{Queue_Regs, ReplacementPolicy, SetAssociativeRegs, SetAssociativeRegsWriteReq, SetAssocReplacer}
import utility.{ChiselDB, MemReqSource, XSPerfAccumulate, XSPerfHistogram}

// Next-Line Prefetcher base parameters
case class NLParameters(
    L2SliceNum: Int = 4, //L2 cache slice number
    nlPrefetchQueueEntries: Int = 8,

    // timeSample
    timeSampleCounterBits: Int = 64, //Sampling counter bit width
    timeSampleRate: Int = 256, //Sampling rate
    timeSampleMinDistance :Int = 4, //Minimum sampling distance

    // Sample Table config 
    stWays: Int = 4, 
    stTouchedBits: Int = 1,
    stReplacementPolicy: String = "plru",
    stRPortNum: Int = 2,
    stWPortNum: Int = 2,
    
    // Pattern Table config 
    ptWays: Int = 64,
    ptSets: Int = 1, 
    ptSatBits: Int = 2,
    ptSatDefultValue:Int = 1,
    ptReplacementPolicy: String = "plru",
    ptRPortNum: Int = 2,
    ptWPortNum: Int = 1,

    // Table port id
    stUpdatePort: Int = 0,
    stInsertPort: Int = 1,
    ptTrainPort: Int = 0,
    ptPrefetchPort: Int = 1,
    ptWPort: Int = 0

) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true 
  override val hasPrefetchSrc: Boolean = true 
  override val inflightEntries: Int = 16 
}

// define Next-Line Prefetcher useful parameters
trait HasNLParams extends HasCoupledL2Parameters {
  val nlParams = prefetchers.find {
    case p: NLParameters => true 
    case _ => false
  }.get.asInstanceOf[NLParameters]
  def L2SliceNum: Int = {
    // Prefer an explicit configuration via L2NBanksKey if present; otherwise fall back to nlParams
    try {
      val nb = p(coupledL2.L2NBanksKey)
      if (nb > 0) nb else nlParams.L2SliceNum
    } catch {
      case _: Throwable => nlParams.L2SliceNum
    }
  }
  def blockAddrBits = fullVAddrBits - offsetBits
  def pcHashBits = blockAddrBits  // PC hash bits, same width as patternTable key

  // timeSampleCounter 
  def timeSampleCounterBits = nlParams.timeSampleCounterBits
  def timeSampleCounterMax = ((BigInt(1) << timeSampleCounterBits) - 1).U(timeSampleCounterBits.W)
  def timeSampleRateBits = log2Ceil(nlParams.timeSampleRate)
  def timeSampleMinDistance = nlParams.timeSampleMinDistance
  // Calculate how many blocks the whole L2 cache has, then /2(if set=512,way=8,slice=4,then 512*8*4/2)
  def timeSampleMaxDistance = blocks * L2SliceNum / 2 
  
  // sample
  def stBlocks = timeSampleMaxDistance / nlParams.timeSampleRate // eg:(512*8*4/2)/256=32
  def stSets = stBlocks / nlParams.stWays // eg:32/4=8
  def stSetBits = log2Ceil(stSets)
  def stWaysBits = log2Ceil(nlParams.stWays)
  def stTagBits = blockAddrBits - stSetBits - offsetBits // if:blockAddrBits=44,offsetBits=6bit，setBits=3bit,then tag=44-3-6=35
  def stPcHashBits = pcHashBits
  def stTimeSampleBits = nlParams.timeSampleCounterBits
  def stReplacementPolicy = nlParams.stReplacementPolicy
  def stUpdatePort = nlParams.stUpdatePort
  def stInsertPort = nlParams.stInsertPort

  // pattern 
  def ptSetBits = log2Ceil(nlParams.ptSets)
  def ptWaysBits = log2Ceil(nlParams.ptWays)
  def ptPcHashBits = pcHashBits
  def ptSatBits = nlParams.ptSatBits
  def ptMaxSat = (1.U << ptSatBits) - 1.U 
  def ptMinSat = 0.U
  def ptDefualtSat = nlParams.ptSatDefultValue
  def ptReplacementPolicy = nlParams.ptReplacementPolicy
  def ptTrainPort = nlParams.ptTrainPort
  def ptPrefetchPort = nlParams.ptPrefetchPort
  def ptWPort = nlParams.ptWPort  
}

// NL
abstract class NLBundle(implicit val p: Parameters) extends Bundle with HasNLParams

abstract class NLModule(implicit val p: Parameters) extends Module with HasNLParams {
  def getBlockAddr(addr: UInt): UInt = {
    addr >> offsetBits // This offsetBit is determined by the block size of the L2 cache.
  }
  def getfullVAddr(addr: UInt): UInt = {
    addr << offsetBits
  }
  def getSampleTableSet(blockAddr: UInt): UInt = {
    blockAddr(stSetBits - 1, 0)
  }
  def getSampleTableTag(blockAddr: UInt): UInt = {
    blockAddr(blockAddrBits - offsetBits - 1, stSetBits)
  }
  def getSampleTableTagAndSet(blockAddr: UInt): (UInt, UInt) = {
    val sampleTag = getSampleTableTag(blockAddr)
    val sampleSet = getSampleTableSet(blockAddr)
    (sampleTag,sampleSet)
  }
  def getPcHash(addr: UInt): UInt = {
    addr
  }
  
  // Helper: generic find hit info from a Vec of entries that have `valid` and `tag` fields
  def findHit[T <: Bundle](entries: Vec[T], key: UInt)(implicit ev: T <:< { val valid: Bool; val tag: UInt }) = {
    val hitVec = VecInit(entries.map(e => ev(e).valid && ev(e).tag === key))
    val hit = hitVec.asUInt.orR
    val hitIdx = PriorityEncoder(hitVec.asUInt)
    val hitEntry = entries(hitIdx)
    (hitVec, hit, hitIdx, hitEntry)
  }
}
 
// sample
class SampleTableEntryField(implicit p: Parameters) extends NLBundle{
    val tag = UInt(stTagBits.W)
    val sampleTime = UInt(stTimeSampleBits.W)
    val pcHash = UInt(stPcHashBits.W)
    val touched = Bool()
    val valid = Bool() 
}

class SampleTableWriteReq(implicit p: Parameters) extends NLBundle {
  val setIdx = UInt(stSetBits.W)
  val wayMask = UInt(nlParams.stWays.W)
  val entry = new SampleTableEntryField()
}

class SampleTrain(implicit p: Parameters) extends NLBundle {
    val addr = UInt(blockAddrBits.W)
    val pc = UInt(blockAddrBits.W)
    val timeSample = UInt(timeSampleCounterBits.W)
}

// Pattern
class PatternTableEntryField(implicit p: Parameters) extends NLBundle{
  val sat = UInt(ptSatBits.W)
  val valid = Bool()
  val tag = UInt(ptPcHashBits.W)
}

class PatternTrain(implicit p: Parameters) extends NLBundle {
    val pcHash = UInt(ptPcHashBits.W)
    val touched = Bool() 
}

class PatternReq(implicit p: Parameters) extends NLBundle {
    val pc = UInt(blockAddrBits.W)    
    val addr = UInt(blockAddrBits.W) 
}

class PatternResp(implicit p: Parameters) extends NLBundle {
    val nextAddr = UInt(blockAddrBits.W)  
}
// chiselDB interface
class SampleDb(implicit p: Parameters) extends NLBundle {
  val trainEn = Bool()
  val pc = UInt(blockAddrBits.W)
  val addr = UInt(blockAddrBits.W)
  val timeSample = UInt(timeSampleCounterBits.W)
  val hit = Bool()
 
  val updateEn = Bool()
  val updateIdx = UInt(stSetBits.W)
  val updateMask = UInt(nlParams.stWays.W)
  val updateData = new SampleTableEntryField()
  val timeSampleDelta = UInt(timeSampleCounterBits.W)

  val insertEn = Bool()
  val insertIdx = UInt(stSetBits.W)
  val insertMask = UInt(nlParams.stWays.W)
  val insertData = new SampleTableEntryField()
  val victimData = new SampleTableEntryField() // victim entry
}

class PatternDb(implicit p: Parameters) extends NLBundle {
 val hit = Bool()
 val hitData = new PatternTableEntryField()
 val sat = UInt(ptSatBits.W)

 val trainEn = Bool()
 val trainData = new SampleTableEntryField()

 val we = Bool()
 val writeIdx = UInt(ptSetBits.W)
 val writeData = new PatternTableEntryField()
}

/*
vaddr (50bit):
┌────────────────────┬─────────┬────────┐
│    Sample Tag      │   Set   │ Offset │
│    (41 bit)        │  (3 bit)│ (6 bit)│
└────────────────────┴─────────┴────────┘
     ↑                  ↑         ↑
     getSampleTableTag  │         getBlockAddr
                        getSampleTableSet
*/
class NextLineSample(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new SampleTrain)) 
    val resp = DecoupledIO(new PatternTrain) 
  })

  // ==================== Sample Table (st) ====================
  val st = Module(new SetAssociativeRegs(
      gen  = new SampleTableEntryField(),
      sets = stSets,
      ways = nlParams.stWays,
      numReadPorts = nlParams.stRPortNum,
      numWritePorts = nlParams.stWPortNum,
      shouldReset = true
  ))

  // Replacer: SetAssocReplacer maintains per-set PLRU state internally
  val replacer = new SetAssocReplacer(stSets, nlParams.stWays, stReplacementPolicy)

  // Local port aliases for readability
  private val updatePort = stUpdatePort
  private val insertPort = stInsertPort
  io.train.ready := true.B

  // ==================== Stage 0: Read Request ====================
  val s0_valid = io.train.fire
  val s0_timeSample = io.train.bits.timeSample
  val s0_blockAddr = io.train.bits.addr
  val s0_pc = io.train.bits.pc

  // Insert part: insert when timeSample reaches a multiple of timeSampleRate
  val s0_insertEn = s0_valid && !s0_timeSample(timeSampleRateBits - 1, 0).orR
  val s0_insertIdx = getSampleTableSet(s0_blockAddr)
  st.io.r(insertPort).req.setIdx := s0_insertIdx

  // Update part: check previous block address
  val s0_updateBlockAddr = s0_blockAddr - 1.U
  val s0_updateIdx = getSampleTableSet(s0_updateBlockAddr)
  st.io.r(updatePort).req.setIdx := s0_updateIdx

  // ==================== Stage 1: Process Read Data ====================
  val s1_valid = RegNext(s0_valid, false.B)
  val s1_timeSample = RegEnable(s0_timeSample, s0_valid)
  val s1_blockAddr = RegEnable(s0_blockAddr, s0_valid)
  val s1_pc = RegEnable(s0_pc, s0_valid)

  // -------------------- Update Route（uptRt） --------------------
  val s1_updateBlockAddr = s1_blockAddr - 1.U
  val s1_updateREntries = RegEnable(st.io.r(updatePort).resp.data, s0_valid)
  val (s1_updateTag, s1_updateIdx) = getSampleTableTagAndSet(s1_updateBlockAddr)

  // Hit check
  val (s1_updateHitVec, s1_updateHit, s1_updateHitWayIdx, s1_updateHitEntry) = findHit(s1_updateREntries, s1_updateTag)
  val s1_updateHitWayOH = UIntToOH(s1_updateHitWayIdx)

  // Check if should update
  val timeSampleDelta = s1_timeSample - s1_updateHitEntry.sampleTime
  val inRange = (timeSampleDelta < timeSampleMaxDistance.U) && (timeSampleMinDistance.U < timeSampleDelta)

  val s1_updateEn = Wire(Bool())
  val s1_updateEntry = Wire(new SampleTableEntryField)
  when(s1_valid && s1_updateHit && inRange) {
    s1_updateEn := true.B
    s1_updateEntry := s1_updateHitEntry
    s1_updateEntry.touched := true.B
  }.otherwise {
    s1_updateEn := false.B
    s1_updateEntry := 0.U.asTypeOf(new SampleTableEntryField)
  }

  // Create a Valid write request for update route at S1
  val s1_updateReq = Wire(Valid(new SetAssociativeRegsWriteReq(new SampleTableEntryField(), stSets, nlParams.stWays)))
  s1_updateReq.valid := s1_updateEn
  s1_updateReq.bits.setIdx := s1_updateIdx
  s1_updateReq.bits.wayMask := s1_updateHitWayOH
  s1_updateReq.bits.data := s1_updateEntry

  // Update PLRU state on hit
  when(s1_valid && s1_updateHit) {
    replacer.access(s1_updateIdx, s1_updateHitWayIdx)
  }

  // -------------------- Insert Route（insRt） --------------------
  val s1_insertEn = RegNext(s0_insertEn, false.B)
  val s1_insertREntries = RegEnable(st.io.r(insertPort).resp.data, s0_insertEn)

  val (s1_insertTag, s1_insertIdx) = getSampleTableTagAndSet(s1_blockAddr)
  val s1_pcHash = getPcHash(s1_pc)

  // Get victim way from SetAssocReplacer
  val s1_insertWayIdx = replacer.way(s1_insertIdx)
  val s1_insertWayOH = UIntToOH(s1_insertWayIdx)
  val s1_victimEntry = s1_insertREntries(s1_insertWayIdx)

  // Update PLRU state on insert
  when(s1_insertEn) {
    replacer.access(s1_insertIdx, s1_insertWayIdx)
  }

  // New entry to insert
  val s1_insertEntry = Wire(new SampleTableEntryField)
  s1_insertEntry.valid := true.B
  s1_insertEntry.tag := s1_insertTag
  s1_insertEntry.sampleTime := s1_timeSample
  s1_insertEntry.pcHash := s1_pcHash
  s1_insertEntry.touched := false.B

  // Create a Valid write request for insert route at S1
  val s1_insertReq = Wire(Valid(new SetAssociativeRegsWriteReq(new SampleTableEntryField(), stSets, nlParams.stWays)))
  s1_insertReq.valid := s1_insertEn
  s1_insertReq.bits.setIdx := s1_insertIdx
  s1_insertReq.bits.wayMask := s1_insertWayOH
  s1_insertReq.bits.data := s1_insertEntry

  // Output to pattern table
  io.resp.valid := s1_insertEn && s1_victimEntry.valid
  io.resp.bits.pcHash := s1_victimEntry.pcHash
  io.resp.bits.touched := s1_victimEntry.touched

  // ==================== Stage 2: Writeback ====================
  val s2_updateReq = RegNext(s1_updateReq)
  val s2_insertReq = RegNext(s1_insertReq )

  // Update writeback: assign the whole Valid bundle
  st.io.w(updatePort).req := s2_updateReq

  // Insert writeback: assign the whole Valid bundle
  st.io.w(insertPort).req := s2_insertReq

  // ==================== Debug & Performance ====================
  val dbTable = ChiselDB.createTable(s"NLsampledb", new SampleDb, basicDB = true)
  val dbData = Wire(new SampleDb())
  dbTable.log(dbData, dbData.trainEn && (dbData.hit || dbData.insertEn || (dbData.timeSample < (1.U << 14))), s"Nlsample", clock, reset)

  dbData.trainEn := s1_valid
  dbData.addr := s1_blockAddr
  dbData.pc := s1_pc
  dbData.timeSample := s1_timeSample
  dbData.hit := s1_updateHit
  dbData.updateEn := s1_updateEn
  dbData.updateIdx := s1_updateIdx
  dbData.updateMask := s1_updateHitWayOH
  dbData.updateData := s1_updateEntry
  dbData.timeSampleDelta := timeSampleDelta
  dbData.insertEn := s1_insertEn
  dbData.insertIdx := s1_insertIdx
  dbData.insertMask := s1_insertWayOH
  dbData.insertData := s1_insertEntry
  dbData.victimData := s1_victimEntry

  XSPerfAccumulate("nlSampleTrainTimes", s0_valid)
  XSPerfAccumulate("nlSampleInsertTimes", s0_insertEn)
  XSPerfAccumulate("nlSampleUpdateReqNotHitTimes", s1_valid && !s1_updateHit)
  XSPerfAccumulate("nlSampleUpdateReqHitOverBoardTimes", s1_valid && s1_updateHit && !inRange)
  XSPerfAccumulate("nlSampleUpdateTimes", s1_updateEn)
  XSPerfAccumulate("nlSampleStateUpdateTimes", s1_valid && s1_updateHit)
  XSPerfAccumulate("nlSampleUpdateReqMulHitTimes", s1_valid && PopCount(s1_updateHitVec) > 1.U)
  XSPerfAccumulate("nlSampleVictimTouchedTrueTimes", s1_insertEn && s1_victimEntry.touched)
  XSPerfAccumulate("nlSampleVictimTouchedFalseTimes", s1_insertEn && !s1_victimEntry.touched)
}

class NextLinePattern(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PatternTrain))
    val req   = Flipped(DecoupledIO(new PatternReq))
    val resp  = DecoupledIO(new PatternResp)
  })

    // ==================== Pattern Table (pt) ====================
    val pt = Module(new SetAssociativeRegs(
      gen = new PatternTableEntryField(),
      sets = nlParams.ptSets,
      ways = nlParams.ptWays,
      numReadPorts = nlParams.ptRPortNum,
      numWritePorts = nlParams.ptWPortNum,
      shouldReset = true
    ))

  // Replacer: SetAssocReplacer with single set (fully associative)
  val replacer = new SetAssocReplacer( nlParams.ptSets, nlParams.ptWays,ptReplacementPolicy)

  // Local port aliases
  private val trainPort = ptTrainPort
  private val prefetchPort = ptPrefetchPort
  private val writePort = ptWPort

  io.train.ready := true.B
  io.req.ready := true.B

  // ==================== Stage 0: Read Request ====================
  val s0_trainValid = io.train.fire
  val s0_trainPcHash = io.train.bits.pcHash
  val s0_trainTouched = io.train.bits.touched

  // For set-assoc, index by lower bits of pcHash
  val s0_trainSet = s0_trainPcHash(ptSetBits - 1, 0)
  pt.io.r(trainPort).req.setIdx := s0_trainSet

  //read data
  val s0_trainREntries = pt.io.r(trainPort).resp.data
  val (s0_trainHitVec, s0_trainHit, s0_trainHitWayIdx, s0_trainHitEntry) = findHit(s0_trainREntries, s0_trainPcHash)

  val s0_reqValid = io.req.fire
  val s0_reqPc = io.req.bits.pc
  val s0_reqAddr = io.req.bits.addr
  val s0_reqPcHash = getPcHash(s0_reqPc)
  val s0_reqSet = 0.U
  pt.io.r(prefetchPort).req.setIdx := s0_reqSet

  val s0_reqREntries = pt.io.r(prefetchPort).resp.data
  val (s0_reqHitVec, s0_reqHit, s0_reqHitWayIdx, s0_reqHitEntry) = findHit(s0_reqREntries, s0_reqPcHash)
  
  // ==================== Stage 1: Process Read Data ====================
  // -------------------- Train Part --------------------
  val s1_trainValid = RegNext(s0_trainValid, false.B)
  val s1_trainPcHash = RegEnable(s0_trainPcHash, s0_trainValid)
  val s1_trainTouched = RegEnable(s0_trainTouched, s0_trainValid)
  val s1_trainHit = RegEnable(s0_trainHit,s0_trainValid)
  val s1_trainHitWayIdx = RegEnable(s0_trainHitWayIdx, s0_trainValid)
  val s1_trainHitEntry = RegEnable(s0_trainHitEntry, s0_trainValid)
  
  val s1_we = WireInit(false.B)
  val s1_writeWay = WireInit(0.U(log2Ceil(nlParams.ptWays).W))
  val s1_writeEntry = WireInit(s1_trainHitEntry)

  when(s1_trainValid) {
    when(s1_trainHit) { // update
      s1_we := true.B
      s1_writeWay := s1_trainHitWayIdx
      s1_writeEntry := s1_trainHitEntry
      s1_writeEntry.valid := true.B
      val currentSat = s1_trainHitEntry.sat
      when(s1_trainTouched) {
        s1_writeEntry.sat := Mux(currentSat === ptMaxSat, ptMaxSat, currentSat + 1.U)
      }.otherwise {
        s1_writeEntry.sat := Mux(currentSat === ptMinSat, ptMinSat, currentSat - 1.U)
      }
    }.otherwise { // insert
      s1_we := s1_trainTouched
      s1_writeWay := replacer.way(0.U)
      s1_writeEntry.valid := s1_trainTouched
      s1_writeEntry.sat := ptDefualtSat.U
      s1_writeEntry.tag := s1_trainPcHash
    }
  }

  // Update PLRU state on access (hit or insert)
  when(s1_trainValid) {
    replacer.access(0.U, s1_writeWay)
  }

  // -------------------- Prefetch Part --------------------
  val s1_reqValid = RegNext(s0_reqValid, false.B)
  val s1_reqPcHash = RegEnable(s0_reqPcHash, s0_reqValid)
  val s1_reqAddr = RegEnable(s0_reqAddr, s0_reqValid)
  val s1_reqHit = RegEnable(s0_reqHit, s0_reqValid)
  val s1_reqHitWayIdx = RegEnable(s0_reqHitWayIdx, s0_reqValid)
  val s1_reqHitEntry = RegEnable(s0_reqHitEntry, s0_reqValid)

  val s1_prefetchAddr = s1_reqAddr + 1.U  
  val s1_crossPage = getPPN(getfullVAddr(s1_reqAddr)) =/= getPPN(getfullVAddr(s1_prefetchAddr))
  val s1_needPrefetch = s1_reqHit && (s1_reqHitEntry.sat === ptMaxSat) && !s1_crossPage

  // ==================== Stage 2: Writeback ====================
  val s2_we = RegNext(s1_we, false.B)
  val s2_writeWay = RegEnable(s1_writeWay, s1_we)
  val s2_writeEntry = RegEnable(s1_writeEntry, s1_we)

  //writeback: construct Valid write request for set-assoc table
  val s2_writeReq = Wire(Valid(new SetAssociativeRegsWriteReq(new PatternTableEntryField(), nlParams.ptSets, nlParams.ptWays)))
  s2_writeReq.valid := s2_we

  // single set (ptSets may be >1); use set 0 for replacer derived victim, otherwise derive from pcHash lower bits
  val s2_writeSet = 0.U 
  s2_writeReq.bits.setIdx := s2_writeSet
  s2_writeReq.bits.wayMask := UIntToOH(s2_writeWay)
  s2_writeReq.bits.data := s2_writeEntry
  pt.io.w(writePort).req := s2_writeReq

  // Output
  io.resp.valid := RegNext(s1_needPrefetch, false.B)
  io.resp.bits.nextAddr := RegEnable(s1_prefetchAddr, s1_needPrefetch)

  // ==================== Debug & Performance ====================
  val dbTable = ChiselDB.createTable(s"NLpatterndb", new PatternDb, basicDB = true)
  val dbData = Wire(new PatternDb())
  dbTable.log(dbData, dbData.trainEn && (dbData.hit || dbData.we), s"Nlpattern", clock, reset)

  dbData.sat := s1_trainHitEntry.sat
  dbData.hit := s1_trainHit
  dbData.hitData := s1_trainHitEntry
  dbData.trainEn := s1_trainValid
  dbData.trainData.pcHash := s1_trainPcHash
  dbData.trainData.valid := true.B
  dbData.trainData.tag := 0.U
  dbData.trainData.sampleTime := 0.U
  dbData.trainData.touched := s1_trainTouched
  dbData.we := s1_we
  dbData.writeIdx := s1_writeWay
  dbData.writeData := s1_writeEntry
 
  XSPerfAccumulate("nlPatternTrainTimes", s1_trainValid)
  XSPerfAccumulate("nlPatternTrainReplaceTimes", s1_trainValid && !s1_trainHit)
  XSPerfAccumulate("nlPatternUpdateTimes", s1_trainValid && s1_trainHit)
  XSPerfAccumulate("nlPatternUpdateTouchedTrueTimes", s1_trainValid && s1_trainHit && s1_trainTouched)
  XSPerfAccumulate("nlPatternUpdateTouchedFalseTimes", s1_trainValid && s1_trainHit && !s1_trainTouched)
  XSPerfAccumulate("nlPatternPcHitValidEntryTimes", s1_reqValid && s1_reqHit)
  XSPerfAccumulate("nlPatternCrossPageTimes", s1_reqValid && s1_reqHit && (s1_reqHitEntry.sat === ptMaxSat) && s1_crossPage)
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq3Times", s1_reqHit && (s1_reqHitEntry.sat === ptMaxSat))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq2Times", s1_reqHit && (s1_reqHitEntry.sat === 2.U))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq1Times", s1_reqHit && (s1_reqHitEntry.sat === 1.U))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq0Times", s1_reqHit && (s1_reqHitEntry.sat === ptMinSat))
}

class NextLinePrefetch(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val enable = Input(Bool()) //enable NL prefetcher
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val resp = Flipped(DecoupledIO(new PrefetchResp)) //
    val req = DecoupledIO(new PrefetchReq) //Next line prefetcher Request access L2
  })

  val validTrain  = io.enable && io.train.fire &&io.train.bits.reqsource === MemReqSource.CPULoadData.id.U  //Only use load requests for training and prediction
  val shouldTrain = validTrain && (!io.train.bits.hit || io.train.bits.prefetched)

  val timeSampleCounter = RegInit(0.U(timeSampleCounterBits.W))
  when(shouldTrain) {
    when(timeSampleCounter === timeSampleCounterMax) {
      timeSampleCounter := 0.U
    }.otherwise {
      timeSampleCounter := timeSampleCounter + 1.U
    }
  }

  val prefetcherSample = Module(new NextLineSample())
  val prefetcherPattern = Module(new NextLinePattern())
  val prefetchQueue = Module(new Queue_Regs( 
          gen = new PatternResp ,
          entries = nlParams.nlPrefetchQueueEntries,
          hasFlush = false, 
          hasOverWrite = true,
          hasFlow = true))
  
  io.train.ready := prefetcherSample.io.train.ready && prefetcherPattern.io.train.ready
  io.resp.ready := true.B  

  // io.train ---> sample.train        
  prefetcherSample.io.train.valid := shouldTrain
  prefetcherSample.io.train.bits.addr := getBlockAddr(io.train.bits.addr)
  prefetcherSample.io.train.bits.pc := getBlockAddr(io.train.bits.pc.getOrElse(0.U))
  prefetcherSample.io.train.bits.timeSample := timeSampleCounter

  // Sample ---> Pattern 
  prefetcherPattern.io.train <> prefetcherSample.io.resp

  //io.train ---> pattern.req 
  val shouldQuery = shouldTrain
  prefetcherPattern.io.req.valid := shouldQuery
  prefetcherPattern.io.req.bits.addr := getBlockAddr(io.train.bits.addr)
  prefetcherPattern.io.req.bits.pc := getBlockAddr(io.train.bits.pc.getOrElse(0.U))

  //pattern.resp ---> prefetchQueue.in
  prefetchQueue.io.enq <> prefetcherPattern.io.resp

  // ========== prefetchQueue.out --->io.req ==========
  val nextAddr = getfullVAddr(prefetchQueue.io.deq.bits.nextAddr)
  io.req.valid := io.enable && prefetchQueue.io.deq.valid
  prefetchQueue.io.deq.ready := io.req.ready
  
  //fullTagBits=[cacheTagBits,bankBits]
  //set=512，That is, a slice contains 512 rows, with offsetBits = 6 bits and bankBits = 2 bits.
  val (sendingTag, sendingSet, _) = parseFullAddress(nextAddr)
 
  //cache address is [cacheTag,cacheSet,cacheBank,cacheOffset]
  io.req.bits.tag := sendingTag //sendingTag=[cacheTag,cacheset[cacheBit-1:cacheBit-2]]
  io.req.bits.set := sendingSet //sendingSet=[cacheSet[cacheBit-3:0],cacheBank] 
  io.req.bits.vaddr.foreach(_ := 0.U)  
  io.req.bits.needT := true.B  
  io.req.bits.source := 0.U  
  io.req.bits.pfSource := MemReqSource.Prefetch2L2NL.id.U  

  // ========== performance counter==========
  XSPerfAccumulate("nlTotalTrainTimes", io.enable && io.train.fire)//nl accept req times
  XSPerfAccumulate("nlStoreTrainTimes", io.enable && io.train.fire && io.train.bits.reqsource === MemReqSource.CPUStoreData.id.U)
  XSPerfAccumulate("nlAtomicTrainTimes", io.enable && io.train.fire && io.train.bits.reqsource === MemReqSource.CPUAtomicData.id.U)
  XSPerfAccumulate("nlLoadMissTimes", validTrain & !io.train.bits.hit)
  XSPerfAccumulate("nlLoadHitPrefetchedTimes",validTrain & io.train.bits.prefetched)
  XSPerfAccumulate("nlLoadMissAndHitPrefetchedTimes",validTrain & !io.train.bits.hit & io.train.bits.prefetched)
  XSPerfAccumulate("nlPrefetchReqTimes",prefetcherPattern.io.resp.valid && io.enable)
  XSPerfAccumulate("nlTransmitPrefetchReqTimes",io.req.fire && io.enable)
  XSPerfAccumulate("nlTimeSampleCountResetTimes",(!timeSampleCounter.orR) & shouldTrain)
}

