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
import coupledL2.utils.{Queue_Regs, OverwriteQueue, ReplacementPolicy, SetAssociativeRegs, SetAssociativeRegsWriteReq, SetAssocReplacer}
import utility.{ChiselDB, MemReqSource, XSPerfAccumulate, XSPerfHistogram}
import freechips.rocketchip.rocket.CSR.X

// Next-Line Prefetcher base parameters
case class L2NLParameters(
    L2SliceNum: Int = 4, //L2 cache slice number
    nlPrefetchQueueEntries: Int = 64,

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
    ptRPortNum: Int = 1,
    ptWPortNum: Int = 1,

    // Table port id
    stUpdatePort: Int = 0,
    stInsertPort: Int = 1,

    ptRPort: Int = 0,
    ptWPort: Int = 0

) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true 
  override val hasPrefetchSrc: Boolean = true 
  override val inflightEntries: Int = 16 
}

// define Next-Line Prefetcher useful parameters
trait HasL2NLParams extends HasCoupledL2Parameters {
 val nlParams = prefetchers.find {
    case p: L2NLParameters => true 
    case _ => false
  }.get.asInstanceOf[L2NLParameters]
  
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
  def pcHashBits = fullVAddrBits  // PC hash bits, same width as patternTable key

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
  def stTagBits = blockAddrBits - stSetBits // if:blockAddrBits=44,setBits=3bit,then tag=44-3=41
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
  def ptRPort = nlParams.ptRPort
  def ptWPort = nlParams.ptWPort  
}

// NL
abstract class L2NLBundle(implicit val p: Parameters) extends Bundle with HasL2NLParams

abstract class L2NLModule(implicit val p: Parameters) extends Module with HasL2NLParams {
  def getBlockAddr(addr: UInt): UInt = {
    addr >> offsetBits // This offsetBit is determined by the block size of the L2 cache.
  }
  def getfullVAddr(addr: UInt): UInt = {
    addr << offsetBits
  }
  def getSampleTableSet(blockAddr: UInt): UInt = {
    if (stSetBits == 0) 0.U else blockAddr(stSetBits - 1, 0)
  }
  def getSampleTableTag(blockAddr: UInt): UInt = {
    blockAddr(blockAddrBits - 1, stSetBits)
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
class L2SampleTableEntryField(implicit p: Parameters) extends L2NLBundle{
    val tag = UInt(stTagBits.W)
    val sampleTime = UInt(stTimeSampleBits.W)
    val pcHash = UInt(stPcHashBits.W)
    val touched = Bool()
    val valid = Bool() 
}

class L2SampleTableWriteReq(implicit p: Parameters) extends L2NLBundle {
  val setIdx = UInt(stSetBits.W)
  val wayMask = UInt(nlParams.stWays.W)
  val entry = new L2SampleTableEntryField()
}

class L2SampleTrain(implicit p: Parameters) extends L2NLBundle {
    val addr = UInt(blockAddrBits.W)
    val pc = UInt(pcHashBits.W)
    val timeSample = UInt(timeSampleCounterBits.W)
}

// Pattern
class L2PatternTableEntryField(implicit p: Parameters) extends L2NLBundle{
  val sat = UInt(ptSatBits.W)
  val valid = Bool()
  val tag = UInt(ptPcHashBits.W)
}

class L2PatternTrain(implicit p: Parameters) extends L2NLBundle {
    val pcHash = UInt(ptPcHashBits.W)
    val touched = Bool() 
}

class L2PatternReq(implicit p: Parameters) extends L2NLBundle {
    val pc = UInt(pcHashBits.W)    
    val addr = UInt(blockAddrBits.W) 
}

class L2PatternResp(implicit p: Parameters) extends L2NLBundle {
    val nextAddr = UInt(blockAddrBits.W)  
}
// chiselDB interface
class L2SampleDb(implicit p: Parameters) extends L2NLBundle {
  val trainEn = Bool()
  val pc = UInt(pcHashBits.W)
  val addr = UInt(blockAddrBits.W)
  val timeSample = UInt(timeSampleCounterBits.W)
  val hit = Bool()
 
  val updateEn = Bool()
  val updateIdx = UInt(stSetBits.W)
  val updateMask = UInt(nlParams.stWays.W)
  val updateData = new L2SampleTableEntryField()
  val timeSampleDelta = UInt(timeSampleCounterBits.W)

  val insertEn = Bool()
  val insertIdx = UInt(stSetBits.W)
  val insertMask = UInt(nlParams.stWays.W)
  val insertData = new L2SampleTableEntryField()
  val victimData = new L2SampleTableEntryField() // victim entry
}

class L2PatternDb(implicit p: Parameters) extends L2NLBundle {
 val hit = Bool()
 val hitData = new L2PatternTableEntryField()
 val sat = UInt(ptSatBits.W)

 val trainEn = Bool()
 val trainData = new L2SampleTableEntryField()

 val we = Bool()
 val writeIdx = UInt(ptSetBits.W)
 val writeData = new L2PatternTableEntryField()
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
class L2NextLineSample(implicit p: Parameters) extends L2NLModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new L2SampleTrain)) 
    val resp = DecoupledIO(new L2PatternTrain) 
  })

  // ==================== Sample Table (st) ====================
  val st = Module(new SetAssociativeRegs(
      gen  = new L2SampleTableEntryField(),
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

  // Create a Valid write request for update route at S1
  val s1_updateReq = Wire(Valid(new SetAssociativeRegsWriteReq(new L2SampleTableEntryField(), stSets, nlParams.stWays)))
  val s1_updateEn = s1_valid && s1_updateHit && inRange
  s1_updateReq.valid := s1_updateEn
  s1_updateReq.bits.setIdx := s1_updateIdx
  s1_updateReq.bits.wayMask := s1_updateHitWayOH
  val s1_updateEntry = s1_updateReq.bits.data
  s1_updateEntry := s1_updateHitEntry
  s1_updateEntry.touched := true.B

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

  // Create a Valid write request for insert route at S1
  val s1_insertReq = Wire(Valid(new SetAssociativeRegsWriteReq(new L2SampleTableEntryField(), stSets, nlParams.stWays)))
  s1_insertReq.valid := s1_insertEn
  s1_insertReq.bits.setIdx := s1_insertIdx
  s1_insertReq.bits.wayMask := s1_insertWayOH
  val s1_insertEntry = s1_insertReq.bits.data
  s1_insertEntry.valid := true.B
  s1_insertEntry.tag := s1_insertTag
  s1_insertEntry.sampleTime := s1_timeSample
  s1_insertEntry.pcHash := s1_pcHash
  s1_insertEntry.touched := false.B

  // Output to pattern table
  io.resp.valid := s1_insertEn && s1_victimEntry.valid
  io.resp.bits.pcHash := s1_victimEntry.pcHash
  io.resp.bits.touched := s1_victimEntry.touched

  // ==================== Stage 2: Writeback ====================
  st.io.w(updatePort).req := RegNext(s1_updateReq)
  st.io.w(insertPort).req := RegNext(s1_insertReq )

  // ==================== Debug & Performance ====================
  val dbTable = ChiselDB.createTable(s"NLL2SampleDb", new L2SampleDb, basicDB = true)
  val dbData = Wire(new L2SampleDb())
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

  XSPerfAccumulate("nlL2SampleTrainTimes", s0_valid)
  XSPerfAccumulate("nlSampleInsertTimes", s0_insertEn)
  XSPerfAccumulate("nlSampleUpdateReqNotHitTimes", s1_valid && !s1_updateHit)
  XSPerfAccumulate("nlSampleUpdateReqHitOverBoardTimes", s1_valid && s1_updateHit && !inRange)
  XSPerfAccumulate("nlSampleUpdateTimes", s1_updateEn)
  XSPerfAccumulate("nlSampleStateUpdateTimes", s1_valid && s1_updateHit)
  XSPerfAccumulate("nlSampleUpdateReqMulHitTimes", s1_valid && PopCount(s1_updateHitVec) > 1.U)
  XSPerfAccumulate("nlSampleVictimTouchedTrueTimes", s1_insertEn && s1_victimEntry.touched)
  XSPerfAccumulate("nlSampleVictimTouchedFalseTimes", s1_insertEn && !s1_victimEntry.touched)
}

class L2NextLinePattern(implicit p: Parameters) extends L2NLModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new L2PatternTrain))
    val req   = Flipped(DecoupledIO(new L2PatternReq))
    val resp  = DecoupledIO(new L2PatternResp)
  })

    // ==================== Pattern Table (pt) ====================
    val pt = Module(new SetAssociativeRegs(
      gen = new L2PatternTableEntryField(),
      sets = nlParams.ptSets,
      ways = nlParams.ptWays,
      numReadPorts = nlParams.ptRPortNum,
      numWritePorts = nlParams.ptWPortNum,
      shouldReset = true
    ))

  // Replacer: SetAssocReplacer with single set (fully associative)
  val replacer = new SetAssocReplacer( nlParams.ptSets, nlParams.ptWays,ptReplacementPolicy)

  io.train.ready := true.B
  io.req.ready := true.B
  // ==================== Stage 0: Read Request ====================
  val s0_trainValid = io.train.fire
  val s0_trainPcHash = io.train.bits.pcHash
  val s0_trainTouched = io.train.bits.touched

  val s0_reqValid = io.req.fire
  val s0_reqAddr = io.req.bits.addr
  val s0_reqPcHash = getPcHash(io.req.bits.pc)

  // For set-assoc, index by lower bits of pcHash
  val s0_ptReadSet = if (ptSetBits == 0) 0.U else s0_reqPcHash(ptSetBits - 1, 0)
  pt.io.r(ptRPort).req.setIdx := s0_ptReadSet
  val s0_ptRespData = pt.io.r(ptRPort).resp.data
  
  val (s0_trainHitVec, s0_trainHit, s0_trainHitWayIdx, s0_trainHitEntry) = findHit(s0_ptRespData, s0_trainPcHash)
  val (s0_reqHitVec, s0_reqHit, s0_reqHitWayIdx, s0_reqHitEntry) = findHit(s0_ptRespData, s0_reqPcHash)
  
  // ==================== Stage 1: Process Read Data ====================
  // -------------------- Train Part --------------------
  val s1_trainValid = RegNext(s0_trainValid, false.B)
  val s1_trainPcHash = RegEnable(s0_trainPcHash, s0_trainValid)
  val s1_trainTouched = RegEnable(s0_trainTouched, s0_trainValid)

  val s1_trainHit = RegEnable(s0_trainHit,s0_trainValid)
  val s1_trainHitWayIdx = RegEnable(s0_trainHitWayIdx, s0_trainValid)
  val s1_trainHitEntry = RegEnable(s0_trainHitEntry, s0_trainValid)
  val s1_trainSet = RegEnable(s0_ptReadSet, s0_trainValid)
  val s1_writeWay = Mux(s1_trainHit,s1_trainHitWayIdx,replacer.way(s1_trainSet))

  val s1_writeReq = Wire(Valid(new SetAssociativeRegsWriteReq(new L2PatternTableEntryField(), nlParams.ptSets, nlParams.ptWays)))
  s1_writeReq.valid := s1_trainValid && (s1_trainHit || s1_trainTouched)
  s1_writeReq.bits.setIdx := s1_trainSet
  s1_writeReq.bits.wayMask := UIntToOH(s1_writeWay)
  val s1_writeEntry = s1_writeReq.bits.data
  s1_writeEntry.valid := true.B
  s1_writeEntry.tag := s1_trainPcHash

  when(s1_trainHit) { // update
      val currentSat = s1_trainHitEntry.sat
      when(s1_trainTouched) {
        s1_writeEntry.sat := Mux(currentSat === ptMaxSat, ptMaxSat, currentSat + 1.U)
      }.otherwise {
        s1_writeEntry.sat := Mux(currentSat === ptMinSat, ptMinSat, currentSat - 1.U)
      }
  }.otherwise { // insert
    s1_writeEntry.sat := ptDefualtSat.U
  }

  // Update PLRU state on access (hit or insert)
  when(s1_trainValid) {
    replacer.access(s1_trainSet, s1_writeWay)
  }

  // -------------------- Prefetch Part --------------------
  val s1_reqValid = RegNext(s0_reqValid, false.B)
  val s1_reqAddr = RegEnable(s0_reqAddr, s0_reqValid)
  val s1_reqHit = RegEnable(s0_reqHit, s0_reqValid)
  val s1_reqHitWayIdx = RegEnable(s0_reqHitWayIdx, s0_reqValid)
  val s1_reqHitEntry = RegEnable(s0_reqHitEntry, s0_reqValid)

  val s1_prefetchAddr = s1_reqAddr + 1.U  
  val s1_crossPage = getPPN(getfullVAddr(s1_reqAddr)) =/= getPPN(getfullVAddr(s1_prefetchAddr))
  val s1_needPrefetch = s1_reqValid && s1_reqHit && (s1_reqHitEntry.sat === ptMaxSat) && !s1_crossPage

  // Output
  io.resp.valid := s1_needPrefetch
  io.resp.bits.nextAddr := s1_prefetchAddr
  // ==================== Stage 2: Writeback ====================
  pt.io.w(ptWPort).req := RegNext(s1_writeReq)

  // ==================== Debug & Performance ====================
  val dbTable = ChiselDB.createTable(s"NLL2PatternDb", new L2PatternDb, basicDB = true)
  val dbData = Wire(new L2PatternDb())
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
  dbData.we := s1_writeReq.valid
  dbData.writeIdx := s1_writeWay
  dbData.writeData := s1_writeEntry
 
  XSPerfAccumulate("nlL2PatternTrainTimes", s1_trainValid)
  XSPerfAccumulate("nlL2PatternTrainReplaceTimes", s1_trainValid && !s1_trainHit)
  XSPerfAccumulate("nlPatternUpdateTimes", s1_trainValid && s1_trainHit)
  XSPerfAccumulate("nlPatternUpdateTouchedTrueTimes", s1_trainValid && s1_trainHit && s1_trainTouched)
  XSPerfAccumulate("nlPatternUpdateTouchedFalseTimes", s1_trainValid && s1_trainHit && !s1_trainTouched)
  XSPerfAccumulate("nlPatternPcHitValidEntryTimes", s1_reqValid && s1_reqHit)
  XSPerfAccumulate("nlPatternCrossPageTimes", s1_reqValid && s1_reqHit && (s1_reqHitEntry.sat === ptMaxSat) && s1_crossPage)
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq3Times", s1_reqValid && s1_reqHit && (s1_reqHitEntry.sat === ptMaxSat))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq2Times", s1_reqValid && s1_reqHit && (s1_reqHitEntry.sat === 2.U))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq1Times", s1_reqValid && s1_reqHit && (s1_reqHitEntry.sat === 1.U))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq0Times", s1_reqValid && s1_reqHit && (s1_reqHitEntry.sat === ptMinSat))
}

class NextLinePrefetch(implicit p: Parameters) extends L2NLModule {
  val io = IO(new Bundle() {
    val enable = Input(Bool()) //enable NL prefetcher
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val resp = Flipped(DecoupledIO(new PrefetchResp)) //
    val req = DecoupledIO(new PrefetchReq) //Next line prefetcher Request access L2
  })

  val validTrain  = io.enable && io.train.fire &&io.train.bits.reqsource === MemReqSource.CPULoadData.id.U  //Only use load requests for training and prediction
  val shouldTrain = validTrain && (!io.train.bits.hit || io.train.bits.prefetched)
  val timeSampleCounter = RegInit(0.U(timeSampleCounterBits.W))
  timeSampleCounter := Mux(shouldTrain && timeSampleCounter === timeSampleCounterMax, 0.U,
                         Mux(shouldTrain, timeSampleCounter + 1.U, timeSampleCounter))
  val prefetcherSample = Module(new L2NextLineSample())
  val prefetcherPattern = Module(new L2NextLinePattern())

  val prefetchQueue     = Module(new OverwriteQueue( 
          gen = new PatternResp ,
          entries = nlParams.nlPrefetchQueueEntries,
          foreverFlow = false,
          flow = true))
  // val prefetchQueue = Module(new Queue_Regs( 
  //         gen = new L2PatternResp ,
  //         entries = nlParams.nlPrefetchQueueEntries,
  //         hasFlush = false, 
  //         hasOverWrite = true,
  //         hasFlow = true))
  
  io.train.ready := prefetcherSample.io.train.ready && prefetcherPattern.io.train.ready
  io.resp.ready := true.B  

  // io.train ---> sample.train        
  prefetcherSample.io.train.valid := shouldTrain
  prefetcherSample.io.train.bits.addr := getBlockAddr(io.train.bits.addr)
  prefetcherSample.io.train.bits.pc := io.train.bits.pc.getOrElse(0.U)
  prefetcherSample.io.train.bits.timeSample := timeSampleCounter

  // Sample ---> Pattern 
  prefetcherPattern.io.train <> prefetcherSample.io.resp

  //io.train ---> pattern.req 
  val shouldQuery = shouldTrain
  prefetcherPattern.io.req.valid := shouldQuery
  prefetcherPattern.io.req.bits.addr := getBlockAddr(io.train.bits.addr)
  prefetcherPattern.io.req.bits.pc := io.train.bits.pc.getOrElse(0.U)

  // pattern.resp ---> prefetchQueue.in
  prefetchQueue.io.enq <> prefetcherPattern.io.resp

  // ========== prefetchQueue.out --->io.req ==========
  val nextAddr = getfullVAddr(prefetchQueue.io.deq.bits.nextAddr)
  io.req.valid := io.enable && prefetchQueue.io.deq.valid
  prefetchQueue.io.deq.ready := io.req.ready

  // prefetcherPattern.io.resp.ready := io.req.ready
  // val nextAddr = getfullVAddr(prefetcherPattern.io.resp.bits.nextAddr)
  // io.req.valid := io.enable && prefetcherPattern.io.resp.valid
  
  
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

   /********test**********/
  val newPrefetcherSample  = prefetcherSample
  val newPrefetcherPattern = prefetcherPattern

  val oldPrefetcherSample  = Module(new NextLineSample())
  val oldPrefetcherPattern = Module(new NextLinePattern())
  // io.train ---> sample.train        
  oldPrefetcherSample.io.train.valid := shouldTrain
  oldPrefetcherSample.io.train.bits.addr := io.train.bits.addr
  oldPrefetcherSample.io.train.bits.pc := io.train.bits.pc.getOrElse(0.U)
  oldPrefetcherSample.io.train.bits.timeSample := timeSampleCounter

  // Sample ---> Pattern 
  oldPrefetcherSample.io.resp.ready := true.B
  oldPrefetcherPattern.io.train.valid := newPrefetcherPattern.io.resp.valid
  oldPrefetcherPattern.io.train.bits.pcTag := newPrefetcherSample.io.resp.bits.pcHash
  oldPrefetcherPattern.io.train.bits.touched := newPrefetcherSample.io.resp.bits.touched
  
  //io.train ---> pattern.req 
  oldPrefetcherPattern.io.req.valid := shouldQuery
  oldPrefetcherPattern.io.req.bits.addr := io.train.bits.addr
  oldPrefetcherPattern.io.req.bits.pc := io.train.bits.pc.getOrElse(0.U)
  oldPrefetcherPattern.io.resp.ready := true.B

  val SampleError = Wire(Bool())
  when(newPrefetcherSample.io.resp.valid) {
    when(oldPrefetcherSample.io.resp.valid ) {
      when(newPrefetcherSample.io.resp.bits.pcHash =/= oldPrefetcherSample.io.resp.bits.pcTag) {
        SampleError := true.B
      }.elsewhen(newPrefetcherSample.io.resp.bits.touched =/= oldPrefetcherSample.io.resp.bits.touched) {
        SampleError := true.B
      }.otherwise {
        SampleError := false.B
      }
    }.otherwise{
      SampleError := true.B
    }
  }.otherwise {
    when(oldPrefetcherSample.io.resp.valid) {
      SampleError := true.B
    }.otherwise {
      SampleError := false.B
    }
  }

  val PatternError = Wire(Bool())
  when(newPrefetcherPattern.io.resp.valid) {
    when(oldPrefetcherPattern.io.resp.valid) {
      when(newPrefetcherPattern.io.resp.bits.nextAddr =/= getBlockAddr(oldPrefetcherPattern.io.resp.bits.nextAddr)) {
        PatternError := true.B
      }.otherwise{
        PatternError := false.B
      }
    }.otherwise {
      PatternError := true.B
    }
  }.otherwise(
    when(oldPrefetcherPattern.io.resp.valid) {
      PatternError := true.B
    }.otherwise { 
      PatternError := false.B
    }
  )
  XSPerfAccumulate("nlSampleTrainErrorTimes", SampleError)
  XSPerfAccumulate("nlPatternTrainErrorTimes", PatternError)

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

