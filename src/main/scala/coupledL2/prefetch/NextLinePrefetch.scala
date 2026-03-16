package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.{FullyAssociativeMemory, OverwriteQueue, ReplacementPolicy, SetAssociativeMemory, SetAssocReplacer}
import utility.{ChiselDB, MemReqSource, XSPerfAccumulate, XSPerfHistogram}

// Next-Line Prefetcher base parameters
case class NLParameters(
    L2SliceNum: Int = 4, //L2 cache slice number
    nlPrefetchQueueEntries: Int = 8,

    //timeSample
    timeSampleCounterBits: Int = 64, //Sampling counter bit width
    timeSampleRate: Int = 256, //Sampling rate
    timeSampleMinDistance :Int = 4, //Minimum sampling distance

    //Sample Table config 
    sampleTableWays: Int = 4, 
    sampleTableTouchedBits: Int = 1,
    sampleTableReplacementPolicy: String = "plru",
    sampleTableRPortNum: Int = 2,
    sampleTableWPortNum: Int = 2,
    
    //Pattern Table config 
    patternTableWays: Int = 1,
    patternTableSets: Int = 64, 
    patternTableSatBits: Int = 2,
    patternTableReplacementPolicy: String = "plru",
    patternTableRPortNum: Int = 2,
    patternTableWPortNum: Int = 1,

    //Table  port id
    sampleTableUpdatePort: Int = 0 ,
    sampleTableInsertPort: Int = 1,
    patternTableTrainPort: Int = 0,
    patternTablePrefetchPort: Int = 1,
    patternTableInsertPort: Int = 0,
    patternTableUpdatePort: Int = 1,

) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true 
  override val hasPrefetchSrc: Boolean = true 
  override val inflightEntries: Int = 16 
}

//define Next-Line Prefetcher useful parameters
trait HasNLParams extends HasCoupledL2Parameters {
  val nlParams = prefetchers.find {
    case p: NLParameters => true 
    case _ => false
  }.get.asInstanceOf[NLParameters]

  def blockAddrBits = fullVAddrBits - offsetBits
  def pcHashBits = blockAddrBits  // PC hash bits, same width as patternTable key

  //timeSampleCounter 
  def timeSampleCounterBits = nlParams.timeSampleCounterBits
  def timeSampleCounterMax = ((BigInt(1) << timeSampleCounterBits) - 1).U(timeSampleCounterBits.W)
  def timeSampleRateBits = log2Ceil(nlParams.timeSampleRate)
  def timeSampleMinDistance = nlParams.timeSampleMinDistance
  def timeSampleMaxDistance = blocks * nlParams.L2SliceNum / 2 //it Calculate how many bytes the L2 cache is. then div 2, 512*8*4/2
  
  //sample
  def sampleTableBlocks = timeSampleMaxDistance / nlParams.timeSampleRate //512*8*4/2/256=32
  def sampleTableSets = sampleTableBlocks / nlParams.sampleTableWays //32/4=8
  def sampleTableSetBits = log2Ceil(sampleTableSets)
  def sampleTableWaysBits = log2Ceil(nlParams.sampleTableWays)
  def sampleTableTagBits = blockAddrBits - sampleTableSetBits - offsetBits // offsetBits=6bit，setBits=3bit, tag=50-3-6=41
  def sampleTablePcHashBits = pcHashBits
  def sampleTableTimeSampleBits = nlParams.timeSampleCounterBits
  def sampleTableReplacementPolicy = nlParams.sampleTableReplacementPolicy
  def sampleTableUpdatePort = nlParams.sampleTableUpdatePort
  def sampleTableInsertPort = nlParams.sampleTableInsertPort

  // pattern 
  def patternTableSetBits = log2Ceil(nlParams.patternTableSets)
  def patternTableWaysBits = log2Ceil(nlParams.patternTableWays)
  def patternTablePcHashBits = pcHashBits
  def patternTableSatBits = nlParams.patternTableSatBits
  def maxSat = (1.U << patternTableSatBits) - 1.U 
  def patternTableReplacementPolicy = nlParams.patternTableReplacementPolicy
  def patternTableTrainPort = nlParams.patternTableTrainPort
  def patternTablePrefetchPort = nlParams.patternTablePrefetchPort
  def patternTableUpdatePort = nlParams.patternTableUpdatePort
  def patternTableInsertPort = nlParams.patternTableInsertPort  
}

//NL
abstract class NLBundle(implicit val p: Parameters) extends Bundle with HasNLParams

abstract class NLModule(implicit val p: Parameters) extends Module with HasNLParams {
  
  // ==================== Helper function: Address parsing====================
  def getBlockAddr(addr: UInt): UInt = {
    addr >> offsetBits //This offsetBit is determined by the block size of the L2 cache.
  }
  def getfullVAddr(addr: UInt): UInt = {
    addr << offsetBits
  }
  
  def getSampleTableSet(blockAddr: UInt): UInt = {
    blockAddr(sampleTableSetBits - 1, 0)
  }
  
  def getSampleTableTag(blockAddr: UInt): UInt = {
    blockAddr(blockAddrBits - offsetBits - 1, sampleTableSetBits)
  }
  
  def getPcHash(addr: UInt): UInt = {
    addr
  }
  
  def getSampleTableTagAndSet(blockAddr: UInt): (UInt, UInt) = {
    val sampleTag = getSampleTableTag(blockAddr)
    val sampleSet = getSampleTableSet(blockAddr)
    (sampleTag,sampleSet)
  }
}
class TrainData(implicit p: Parameters ) extends NLBundle{
    val addr = UInt(blockAddrBits.W)
    val pc = UInt(blockAddrBits.W)
}
 
class SampleTableEntryField(implicit p: Parameters) extends NLBundle{
    val tag = UInt(sampleTableTagBits.W)
    val sampleTime = UInt(sampleTableTimeSampleBits.W)
    val pcHash = UInt(sampleTablePcHashBits.W)
    val touched = Bool()
    val valid = Bool() 
}

class SampleTableWriteReq(implicit p: Parameters) extends NLBundle {
  val en = Bool()
  val setIdx = UInt(sampleTableSetBits.W)
  val wayMask = UInt(nlParams.sampleTableWays.W)
  val entry = new SampleTableEntryField()
}

//sample
class SampleTrain(implicit p: Parameters) extends NLBundle {
    val addr = UInt(blockAddrBits.W)
    val pc = UInt(blockAddrBits.W)
    val timeSample = UInt(timeSampleCounterBits.W)
}

class PatternTableEntryField(implicit p: Parameters) extends NLBundle{
     val sat = UInt(patternTableSatBits.W)
     val valid = Bool()
}
//Pattern
class PatternTrain(implicit p: Parameters) extends NLBundle {
    val pcHash = UInt(patternTablePcHashBits.W)
    val touched = Bool() 
}

class PatternReq(implicit p: Parameters) extends NLBundle {
    val pc = UInt(blockAddrBits.W)    
    val addr = UInt(blockAddrBits.W) 
}

class PatternResp(implicit p: Parameters) extends NLBundle {
    val nextAddr = UInt(blockAddrBits.W)  
}
//chiselDB interface
class SampleDb(implicit p: Parameters) extends NLBundle {
  val trainEn = Bool()
  val pc = UInt(blockAddrBits.W)
  val addr = UInt(blockAddrBits.W)
  val timeSample = UInt(timeSampleCounterBits.W)

  val hit = Bool()
  val plru_state = UInt(3.W)
 
  val updateEn = Bool()
  val updateIdx = UInt(sampleTableSetBits.W)
  val updateMask = UInt(nlParams.sampleTableWays.W)
  val updateData = new SampleTableEntryField()
  val timeSampleDelta = UInt(timeSampleCounterBits.W)

  val insertEn = Bool()
  val insertIdx = UInt(sampleTableSetBits.W)
  val insertMask = UInt(nlParams.sampleTableWays.W)
  val insertData = new SampleTableEntryField()
  val victimData = new SampleTableEntryField()//victim entry
}

class PatternDb(implicit p: Parameters) extends NLBundle {
 val hit = Bool()
 val hitData = new PatternTableEntryField()
 val sat = UInt(patternTableSatBits.W)

 val trainEn = Bool()
 val trainData = new SampleTableEntryField()

 val updateEn = Bool()
 val updateIdx = UInt(patternTableSetBits.W)
 val updateData = new PatternTableEntryField()

 val insertEn = Bool()
 val insertIdx = UInt(patternTableSetBits.W)
 val insertData = new PatternTableEntryField()
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
  val st = Module(new SetAssociativeMemory(
      gen  = new SampleTableEntryField(),
      sets = sampleTableSets,
      ways = nlParams.sampleTableWays,
      numReadPorts = nlParams.sampleTableRPortNum,
      numWritePorts = nlParams.sampleTableWPortNum,
      shouldReset = true
  ))

  // Replacer: SetAssocReplacer maintains per-set PLRU state internally
  val replacer = new SetAssocReplacer(sampleTableSets, nlParams.sampleTableWays, sampleTableReplacementPolicy)

  // Local port aliases for readability
  private val updatePort = sampleTableUpdatePort
  private val insertPort = sampleTableInsertPort
  io.train.ready := true.B

  // ==================== Stage 0: Read Request ====================
  val s0_valid = io.train.fire
  val s0_timeSample = io.train.bits.timeSample
  val s0_addr = io.train.bits.addr
  val s0_pc = io.train.bits.pc

  // Insert part: insert when timeSample reaches a multiple of timeSampleRate
  val s0_insertEn = s0_valid && !s0_timeSample(timeSampleRateBits - 1, 0).orR
  val s0_insertBlockAddr = getBlockAddr(s0_addr)
  val s0_insertIdx = getSampleTableSet(s0_insertBlockAddr)
  st.io.r(insertPort).req.setIdx := s0_insertIdx

  // Update part: check previous block address
  val s0_updateBlockAddr = s0_insertBlockAddr - 1.U
  val s0_updateIdx = getSampleTableSet(s0_updateBlockAddr)
  st.io.r(updatePort).req.setIdx := s0_updateIdx

  // ==================== Stage 1: Process Read Data ====================
  val s1_valid = RegNext(s0_valid, false.B)
  val s1_timeSample = RegEnable(s0_timeSample, s0_valid)
  val s1_blockAddr = RegEnable(s0_insertBlockAddr, s0_valid)
  val s1_pc = RegEnable(s0_pc, s0_valid)

  // -------------------- Update Route（uptRt） --------------------
  val s1_updateBlockAddr = s1_blockAddr - 1.U
  val s1_updateREntries = RegNext(st.io.r(updatePort).resp.data)
  val (s1_updateTag, s1_updateIdx) = getSampleTableTagAndSet(s1_updateBlockAddr)

  // Hit check
  val s1_updateHitVec = VecInit(s1_updateREntries.map(e => e.valid && e.tag === s1_updateTag))
  val s1_updateHit = s1_updateHitVec.asUInt.orR
  val s1_updateHitWayIdx = PriorityEncoder(s1_updateHitVec.asUInt)
  val s1_updateHitWayOH = UIntToOH(s1_updateHitWayIdx)
  val s1_updateHitEntry = s1_updateREntries(s1_updateHitWayIdx)

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

  val s1_updateReq = Wire(new SampleTableWriteReq())
  s1_updateReq.en := s1_updateEn
  s1_updateReq.setIdx := s1_updateIdx
  s1_updateReq.wayMask := s1_updateHitWayOH
  s1_updateReq.entry := s1_updateEntry

  // Update PLRU state on hit
  when(s1_valid && s1_updateHit) {
    replacer.access(s1_updateIdx, s1_updateHitWayIdx)
  }

  // -------------------- Insert Route（insRt） --------------------
  val s1_insertEn = RegNext(s0_insertEn)
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

  val s1_insertReq = Wire(new SampleTableWriteReq())
  s1_insertReq.en := s1_insertEn
  s1_insertReq.setIdx := s1_insertIdx
  s1_insertReq.wayMask := s1_insertWayOH
  s1_insertReq.entry := s1_insertEntry

  // Output to pattern table
  io.resp.valid := s1_insertEn && s1_victimEntry.valid
  io.resp.bits.pcHash := s1_victimEntry.pcHash
  io.resp.bits.touched := s1_victimEntry.touched

  // ==================== Stage 2: Writeback ====================
  val s2_updateReq = RegNext(s1_updateReq)
  val s2_insertReq = RegNext(s1_insertReq)

  // Update writeback
  st.io.w(updatePort).en := s2_updateReq.en
  st.io.w(updatePort).req.setIdx := s2_updateReq.setIdx
  st.io.w(updatePort).req.wayMask := s2_updateReq.wayMask
  st.io.w(updatePort).req.data := VecInit(Seq.fill(nlParams.sampleTableWays)(s2_updateReq.entry))

  // Insert writeback
  st.io.w(insertPort).en := s2_insertReq.en
  st.io.w(insertPort).req.setIdx := s2_insertReq.setIdx
  st.io.w(insertPort).req.wayMask := s2_insertReq.wayMask
  st.io.w(insertPort).req.data := VecInit(Seq.fill(nlParams.sampleTableWays)(s2_insertReq.entry))

  // ==================== Debug & Performance ====================
  val dbTable = ChiselDB.createTable(s"NLsampledb", new SampleDb, basicDB = false)
  val dbData = Wire(new SampleDb())
  dbTable.log(dbData, dbData.trainEn && (dbData.hit || dbData.insertEn || (dbData.timeSample < (1.U << 14))), s"Nlsample", clock, reset)

  dbData.trainEn := s1_valid
  dbData.addr := s1_blockAddr
  dbData.pc := s1_pc
  dbData.timeSample := s1_timeSample
  dbData.hit := s1_updateHit
  dbData.plru_state := 0.U  // PLRU state now managed internally by SetAssocReplacer
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
  val pt = Module(new FullyAssociativeMemory(
      gen = new PatternTableEntryField(),
      keyWidth = patternTablePcHashBits,
      numEntries = nlParams.patternTableSets,
      numReadPorts = nlParams.patternTableRPortNum,
      numWritePorts = nlParams.patternTableWPortNum,
      shouldReset = true
  ))

  val replacer = ReplacementPolicy.fromString(patternTableReplacementPolicy, nlParams.patternTableSets)
  val replaceState = RegInit(0.U(replacer.nBits.W))

  // Local port aliases
  private val trainPort = patternTableTrainPort
  private val prefetchPort = patternTablePrefetchPort
  private val updatePort = patternTableUpdatePort
  private val insertPort = patternTableInsertPort

  io.train.ready := true.B
  io.req.ready := true.B

  // ==================== Stage 0: Read Request ====================
  val s0_trainValid = io.train.fire
  val s0_trainPcHash = io.train.bits.pcHash
  val s0_trainTouched = io.train.bits.touched
  pt.io.r(trainPort).req.key := s0_trainPcHash

  val s0_reqValid = io.req.fire
  val s0_reqPc = io.req.bits.pc
  val s0_reqAddr = io.req.bits.addr
  val s0_reqPcHash = getPcHash(s0_reqPc)
  pt.io.r(prefetchPort).req.key := s0_reqPcHash
  
  // ==================== Stage 1: Process Read Data ====================
  // -------------------- Train Part --------------------
  val s1_trainValid = RegNext(s0_trainValid, false.B)
  val s1_trainPcHash = RegEnable(s0_trainPcHash, s0_trainValid)
  val s1_trainTouched = RegEnable(s0_trainTouched, s0_trainValid)
  val s1_trainResp = RegEnable(pt.io.r(trainPort).resp, s0_trainValid)

  val s1_updateEn = WireInit(false.B)
  val s1_updateIdx = WireInit(0.U(patternTableSetBits.W))
  val s1_insertEn = WireInit(false.B)
  val s1_insertIdx = WireInit(0.U(patternTableSetBits.W))
  val s1_newEntry = WireInit(s1_trainResp.data)

  when(s1_trainValid) {
    when(s1_trainResp.hit) {
      s1_updateEn := true.B
      s1_newEntry.valid := true.B
      s1_updateIdx := s1_trainResp.hitIdx

      val currentSat = s1_trainResp.data.sat
      when(s1_trainTouched) {
        s1_newEntry.sat := Mux(currentSat === maxSat, maxSat, currentSat + 1.U)
      }.otherwise {
        s1_newEntry.sat := Mux(currentSat === 0.U, 0.U, currentSat - 1.U)
      }
    }.otherwise {
      s1_insertEn := s1_trainTouched
      s1_newEntry.valid := s1_trainTouched
      s1_insertIdx := replacer.get_replace_way(replaceState)
      s1_newEntry.sat := 1.U
    }
  }

  // Update PLRU state
  val nextState = replacer.get_next_state(replaceState, Mux(s1_trainResp.hit, s1_trainResp.hitIdx, s1_insertIdx))
  replaceState := nextState

  // -------------------- Prefetch Part --------------------
  val s1_reqValid = RegNext(s0_reqValid, false.B)
  val s1_reqPc = RegEnable(s0_reqPc, s0_reqValid)
  val s1_reqAddr = RegEnable(s0_reqAddr, s0_reqValid)
  val s1_reqResp = RegEnable(pt.io.r(prefetchPort).resp, s0_reqValid)

  val s1_prefetchAddr = s1_reqAddr + 1.U
  val s1_reqHit = s1_reqValid && s1_reqResp.hit
  //getPPN need full addr
  val s1_crossPage = getPPN(getfullVAddr(s1_reqAddr)) =/= getPPN(getfullVAddr(s1_prefetchAddr))
  val s1_needPrefetch = s1_reqHit && (s1_reqResp.data.sat === maxSat) && !s1_crossPage

  // ==================== Stage 2: Writeback ====================
  val s2_updateEn = RegNext(s1_updateEn, false.B)
  val s2_updateIdx = RegEnable(s1_updateIdx, s1_updateEn)
  val s2_insertEn = RegNext(s1_insertEn, false.B)
  val s2_insertIdx = RegEnable(s1_insertIdx, s1_insertEn)
  val s2_newKey = RegEnable(s1_trainPcHash, s1_updateEn || s1_insertEn)
  val s2_newEntry = RegEnable(s1_newEntry, s1_updateEn || s1_insertEn)

  // Update writeback
  pt.io.w(updatePort).en := s2_updateEn
  pt.io.w(updatePort).req.valid := true.B
  pt.io.w(updatePort).req.key := s2_newKey
  pt.io.w(updatePort).req.idx := s2_updateIdx
  pt.io.w(updatePort).req.data := s2_newEntry

  // Insert writeback
  pt.io.w(insertPort).en := s2_insertEn
  pt.io.w(insertPort).req.valid := true.B
  pt.io.w(insertPort).req.key := s2_newKey
  pt.io.w(insertPort).req.idx := s2_insertIdx
  pt.io.w(insertPort).req.data := s2_newEntry

  // Output
  io.resp.valid := RegNext(s1_needPrefetch, false.B)
  io.resp.bits.nextAddr := RegEnable(s1_prefetchAddr, s1_needPrefetch)

  // ==================== Debug & Performance ====================
  val dbTable = ChiselDB.createTable(s"NLpatterndb", new PatternDb, basicDB = false)
  val dbData = Wire(new PatternDb())
  dbTable.log(dbData, dbData.trainEn && (dbData.hit || dbData.insertEn), s"Nlpattern", clock, reset)

  dbData.sat := s1_trainResp.data.sat
  dbData.hit := s1_trainResp.hit
  dbData.hitData := s1_trainResp.data
  dbData.trainEn := s1_trainValid
  dbData.trainData.pcHash := s1_trainPcHash
  dbData.trainData.valid := true.B
  dbData.trainData.tag := 0.U
  dbData.trainData.sampleTime := 0.U
  dbData.trainData.touched := s1_trainTouched
  dbData.updateEn := s1_updateEn
  dbData.updateIdx := s1_updateIdx
  dbData.updateData := s1_newEntry
  dbData.insertEn := s1_insertEn
  dbData.insertIdx := s1_insertIdx
  dbData.insertData := s1_newEntry

  XSPerfAccumulate("nlPatternTrainTimes", s1_trainValid)
  XSPerfAccumulate("nlPatternTrainMulHitTimes", s1_trainResp.multHit || s1_reqResp.multHit)
  XSPerfAccumulate("nlPatternTrainReplaceTimes", s1_insertEn)
  XSPerfAccumulate("nlPatternUpdateTimes", s1_trainValid && s1_trainResp.hit)
  XSPerfAccumulate("nlPatternUpdateTouchedTrueTimes", s1_trainValid && s1_trainResp.hit && s1_trainTouched)
  XSPerfAccumulate("nlPatternUpdateTouchedFalseTimes", s1_trainValid && s1_trainResp.hit && !s1_trainTouched)
  XSPerfAccumulate("nlPatternPcHitTimes", s1_reqResp.hit && s1_reqValid)
  XSPerfAccumulate("nlPatternPcHitValidEntryTimes", s1_reqHit)
  XSPerfAccumulate("nlPatternCrossPageTimes", s1_reqHit && (s1_reqResp.data.sat === maxSat) && s1_crossPage)
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq3Times", s1_reqHit && (s1_reqResp.data.sat === 3.U))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq2Times", s1_reqHit && (s1_reqResp.data.sat === 2.U))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq1Times", s1_reqHit && (s1_reqResp.data.sat === 1.U))
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq0Times", s1_reqHit && (s1_reqResp.data.sat === 0.U))
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

  val prefetcherSample  = Module(new NextLineSample())
  val prefetcherPattern = Module(new NextLinePattern())
  val prefetchQueue     = Module(new OverwriteQueue( 
          gen = new PatternResp ,
          entries = nlParams.nlPrefetchQueueEntries,
          foreverFlow = false,
          flow = true))
  
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
  XSPerfHistogram("grant_grantack_period", prefetchQueue.io.count, io.req.ready, 0, nlParams.nlPrefetchQueueEntries, 1)

  XSPerfAccumulate("nlTimeSampleCountResetTimes",(!timeSampleCounter.orR) & shouldTrain)

}

