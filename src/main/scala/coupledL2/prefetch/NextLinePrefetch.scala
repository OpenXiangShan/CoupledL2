package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, MemReqSource, XSPerfAccumulate, XSPerfHistogram, sram}
import coupledL2.utils.{FullyAssociativeMemory, OverwriteQueue, SetAssociativeMemory}
import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.ReplacementPolicy
import huancun.{TPmetaReq, TPmetaResp}
import freechips.rocketchip.util.SeqToAugmentedSeq
import freechips.rocketchip.rocket.CSR.X

//define Next-Line Prefetcher base parameters
case class NLParameters(
    L2SliceNum: Int = 4, //L2 cache slice number
    nlPrefetchQueueEntries: Int = 64,

    //timeSample
    timeSampleCounterBits: Int = 64, //Sampling counter bit width
    timeSampleRate: Int = 256, //Sampling rate
    timeSampleMinDistance :Int = 4, //Minimum sampling distance

    //Sample Table config 
    sampleTableWays: Int = 4, 
    sampleTableTouchedBits: Int = 1,
    sampleTableReplacementPolicy: String = "plru",
    
    //Pattern Table config 
    patternTableWays: Int = 1,
    patternTableSets: Int = 64, 
    patternTableSatBits: Int = 2,
    patternTableReplacementPolicy: String = "plru",

    //Table  port id
    sampleTableUpdatePort: Int = 0 ,
    sampleTableReplacePort: Int = 1,
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

  def vaddrBits = fullVAddrBits
  def tablePcTagBits = vaddrBits  //The PcTag field of sampleTable is the same 50 bits as the tag field of patternTable.

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
  def sampleTableTagBits = vaddrBits - sampleTableSetBits - offsetBits // offsetBits=6bit，setBits=3bit, tag=50-3-6=41
  def sampleTablePcTagBits = tablePcTagBits
  def sampleTableTimeSampleBits = nlParams.timeSampleCounterBits
  def sampleTableReplacementPolicy = nlParams.sampleTableReplacementPolicy
  def sampleTableUpdatePort = nlParams.sampleTableUpdatePort
  def sampleTableReplacePort = nlParams.sampleTableReplacePort

  // pattern 
  def patternTableSetBits = log2Ceil(nlParams.patternTableSets)
  def patternTableWaysBits = log2Ceil(nlParams.patternTableWays)
  def patternTableTagBits = tablePcTagBits
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
  
  def getSampleTableSet(blockAddr: UInt): UInt = {
    blockAddr(sampleTableSetBits - 1, 0)
  }
  
  def getSampleTableTag(blockAddr: UInt): UInt = {
    blockAddr(vaddrBits - offsetBits - 1, sampleTableSetBits)
  }
  
  def getPatternTableTag(addr: UInt): UInt = {
    addr
  }
  
  def parseTrainData(blockAddr: UInt): (UInt, UInt) = {
    val sampleTag = getSampleTableTag(blockAddr)
    val sampleSet = getSampleTableSet(blockAddr)
    (sampleTag,sampleSet)
  }
}
class TrainData(implicit p: Parameters ) extends NLBundle{
    val addr = UInt(vaddrBits.W)
    val pc = UInt(vaddrBits.W)
}
 
class SampleTableEntryField(implicit p: Parameters) extends NLBundle{
    val tag = UInt(sampleTableTagBits.W)
    val sampleTime = UInt(sampleTableTimeSampleBits.W)
    val pcTag = UInt(sampleTablePcTagBits.W)
    val touched = Bool()
    val valid = Bool() 
}

class SampleTableWriteReq(implicit p: Parameters) extends NLBundle {
  val en = Bool()
  val setIdx = UInt(sampleTableSetBits.W)
  val wayMask = UInt(nlParams.sampleTableWays.W)
  val entry = new SampleTableEntryField()
}

class SampleTableStateWriteReq(implicit p: Parameters) extends NLBundle {
  val en = Bool()
  val setIdx = UInt(sampleTableSetBits.W)
  val wayMask = UInt(1.W)  
  val state = UInt((nlParams.sampleTableWays-1).W)  
}

//sample
class SampleTrain(implicit p: Parameters) extends NLBundle {
    val addr = UInt(vaddrBits.W)
    val pc = UInt(vaddrBits.W)
    val timeSample = UInt(timeSampleCounterBits.W)
}

class PatternTableEntryField(implicit p: Parameters) extends NLBundle{
     val sat = UInt(patternTableSatBits.W)
     val valid = Bool()
}
//Pattern
class PatternTrain(implicit p: Parameters) extends NLBundle {
    val pcTag = UInt(patternTableTagBits.W)
    val touched = Bool() 
}

class PatternReq(implicit p: Parameters) extends NLBundle {
    val pc = UInt(vaddrBits.W)    
    val addr = UInt(vaddrBits.W) 
}

class PatternResp(implicit p: Parameters) extends NLBundle {
    val nextAddr = UInt(vaddrBits.W)  
}
//chiselDB interface
class SampleDb(implicit p: Parameters) extends NLBundle {
  val trainEn = Bool()
  val pc = UInt(vaddrBits.W)
  val addr = UInt(vaddrBits.W)
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

class NlDb(implicit p: Parameters) extends NLBundle {
  val addr = UInt(vaddrBits.W)
  val pc = UInt(vaddrBits.W)
  val timeSample = UInt(timeSampleCounterBits.W)
  val sampleHit = Bool()
  val patternHit = Bool()
  val patternSat = UInt(3.W)
}

class SamplePatternDb(implicit p: Parameters) extends NLBundle {
  val sample = new SampleDb()
  val pattern = new PatternDb()
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

  // Sample Table
  val sampleTable = Module(new SetAssociativeMemory(
      gen  = new SampleTableEntryField(),
      sets = sampleTableSets,
      ways = nlParams.sampleTableWays,
      numReadPorts = 2,
      numWritePorts = 2,
      shouldReset = true  //Require initial reset to ensure that valid is all zeros.
  ))

  //replacer
  val sampleTableReplacer = ReplacementPolicy.fromString(sampleTableReplacementPolicy, nlParams.sampleTableWays)
  val sampleTableReplaceStateRegs = Module(new SetAssociativeMemory(
      gen = UInt(sampleTableReplacer.nBits.W),
      sets = sampleTableSets,
      ways = 1,
      numReadPorts = 2,
      numWritePorts = 2,
      shouldReset = true  
  ))
  io.train.ready := true.B
  /***************Stage 0: *******************/
  val s0_valid = io.train.fire  
  val s0_timeSample = io.train.bits.timeSample
  val s0_addr = io.train.bits.addr 
  val s0_pc = io.train.bits.pc
 //****************replace part******************//
  //When [timeSampleRateBits-1,0] of timeSample is 0, that is, finish round of timeSampleRate,
  //and a new sample arrives at this time, it is inserted into the table.
  val s0_sampleTableReplaceEn = s0_valid & (!s0_timeSample(timeSampleRateBits-1,0).orR)
  //Parse address
  val s0_sampleTableReplaceBlockAddr  = getBlockAddr(s0_addr)
  val s0_sampleTableReplaceIdx = getSampleTableSet(s0_sampleTableReplaceBlockAddr )
  sampleTable.io.r(sampleTableReplacePort).req.setIdx := s0_sampleTableReplaceIdx   
  sampleTableReplaceStateRegs.io.r(sampleTableReplacePort).req.setIdx := s0_sampleTableReplaceIdx 
  
  //****************update part******************//
  val s0_sampleTableUpdateBlockAddr = s0_sampleTableReplaceBlockAddr  -1.U
  val s0_sampleTableUpdateIdx = getSampleTableSet(s0_sampleTableUpdateBlockAddr)

  sampleTable.io.r(sampleTableUpdatePort).req.setIdx := s0_sampleTableUpdateIdx 
  sampleTableReplaceStateRegs.io.r(sampleTableUpdatePort).req.setIdx := s0_sampleTableUpdateIdx 

  /***************Stage 1: handle  data of Sample Table reading *******************/
  val s1_valid = RegNext(s0_valid, false.B)
  val s1_timeSample = RegEnable(s0_timeSample, s0_valid)
  val s1_blockAddr = RegEnable(s0_sampleTableReplaceBlockAddr, s0_valid)
  val s1_pc = RegEnable(s0_pc, s0_valid)
  val s1_sampleTableReplacBlockAddr = s1_blockAddr
  val s1_sampleTableUpdateBlockAddr = s1_sampleTableReplacBlockAddr - 1.U

  //****************update part******************//
  val s1_sampleTableUpdateEntries = RegNext(sampleTable.io.r(sampleTableUpdatePort).resp.data)  // Vec(ways, SampleTableEntryField)
  val s1_sampleTableUpdataState = RegInit(0.U(sampleTableReplacer.nBits.W))
  val s1_SampleTableUpdatedHitEntry = Wire(new SampleTableEntryField)
  val s1_sampleTableUpdateEn = Wire(Bool())
  val s1_sampleTableUpdatedEntry = Wire(new SampleTableEntryField)
  val (s1_sampleTableUpdateTag, s1_sampleTableUpdateIdx) = parseTrainData(s1_sampleTableUpdateBlockAddr)
  s1_sampleTableUpdataState := sampleTableReplaceStateRegs.io.r(sampleTableUpdatePort).resp.data(0)

  // UpdateData Hit check:
  val s1_sampleTableUpdateHitVec = VecInit(s1_sampleTableUpdateEntries.map(entry => entry.valid && (entry.tag === s1_sampleTableUpdateTag)))
  val sampleTableUpdateHit = s1_sampleTableUpdateHitVec.asUInt.orR  // example: (1010)===>1
  val s1_sampleTableUpdateHitWayIdx = PriorityEncoder(s1_sampleTableUpdateHitVec.asUInt)// example: vec(0):0,vec(1)=1,vec(2)=0,vec(3)=1 ===>1 
  val s1_sampleTableUpdateHitWayOH = UIntToOH(s1_sampleTableUpdateHitWayIdx)    // example: 1===>0010
  val s1_sampleTableUpdataNextState = sampleTableReplacer.get_next_state(s1_sampleTableUpdataState,s1_sampleTableUpdateHitWayIdx) 
  s1_SampleTableUpdatedHitEntry := s1_sampleTableUpdateEntries(s1_sampleTableUpdateHitWayIdx)

  // check if update
  val timeSampleDelta = s1_timeSample - s1_SampleTableUpdatedHitEntry.sampleTime
  val inRange = (timeSampleDelta < timeSampleMaxDistance.U) && (timeSampleMinDistance.U < timeSampleDelta)
  when(s1_valid && sampleTableUpdateHit && inRange ) {//if hit and update condition ,then update   
    s1_sampleTableUpdateEn := true.B
    s1_sampleTableUpdatedEntry := s1_SampleTableUpdatedHitEntry
    s1_sampleTableUpdatedEntry.touched := true.B
  }.otherwise{//if not satisfied, do not update
    s1_sampleTableUpdateEn := false.B
    s1_sampleTableUpdatedEntry := 0.U.asTypeOf(new SampleTableEntryField)
  }

  val s1_sampleTableUpdateReq = Wire(new SampleTableWriteReq())
  s1_sampleTableUpdateReq.en := s1_sampleTableUpdateEn
  s1_sampleTableUpdateReq.setIdx := s1_sampleTableUpdateIdx
  s1_sampleTableUpdateReq.wayMask := s1_sampleTableUpdateHitWayOH
  s1_sampleTableUpdateReq.entry := s1_sampleTableUpdatedEntry

  val s1_sampleTableUpdateStateReq = Wire(new SampleTableStateWriteReq())
  s1_sampleTableUpdateStateReq.en := s1_valid & sampleTableUpdateHit //follow Gem5 design 
  s1_sampleTableUpdateStateReq.setIdx := s1_sampleTableUpdateIdx
  s1_sampleTableUpdateStateReq.wayMask := (s1_valid & sampleTableUpdateHit).asUInt// The state reg has only 1 way (ways=1), so the wayMask is always 1.
  s1_sampleTableUpdateStateReq.state := s1_sampleTableUpdataNextState

  //***************replace part*******************//
  val s1_sampleTableReplaceEn = RegNext(s0_sampleTableReplaceEn)
  val s1_sampleTableReplaceEntries = RegEnable(sampleTable.io.r(sampleTableReplacePort).resp.data, s0_sampleTableReplaceEn)  // Vec(ways, SampleTableEntryField)
  val s1_sampleTableReplaceState = RegEnable(sampleTableReplaceStateRegs.io.r(sampleTableReplacePort).resp.data(0), s0_sampleTableReplaceEn)
  val (s1_sampleTableReplaceTag, s1_sampleTableReplaceIdx) = parseTrainData(s1_sampleTableReplacBlockAddr)
  val s1_patternTableTag = getPatternTableTag(s1_pc)
  s1_sampleTableReplaceState := sampleTableReplaceStateRegs.io.r(sampleTableReplacePort).resp.data(0)

  //caculate victim way id
  val s1_sampleTableReplaceWayIdx = sampleTableReplacer.get_replace_way(s1_sampleTableReplaceState)
  val s1_sampleTableReplaceNextState = sampleTableReplacer.get_next_state(s1_sampleTableReplaceState,s1_sampleTableReplaceWayIdx)
  
  //get victim way data
  val s1_sampleTableReplaceWayOH = UIntToOH(s1_sampleTableReplaceWayIdx)
  val s1_sampleTableVictimEntry = s1_sampleTableReplaceEntries(s1_sampleTableReplaceWayIdx) 

  //set insert entry data
  val s1_sampleTableReplaceEntry = Wire(new SampleTableEntryField)  
  s1_sampleTableReplaceEntry.valid := true.B
  s1_sampleTableReplaceEntry.tag := s1_sampleTableReplaceTag
  s1_sampleTableReplaceEntry.sampleTime := s1_timeSample
  s1_sampleTableReplaceEntry.pcTag := s1_patternTableTag
  s1_sampleTableReplaceEntry.touched := false.B
  
  // Encapsulate the two write requests of the Sample Table
  val s1_sampleTableReplaceReq = Wire(new SampleTableWriteReq())
  s1_sampleTableReplaceReq.en := s1_sampleTableReplaceEn
  s1_sampleTableReplaceReq.setIdx := s1_sampleTableReplaceIdx
  s1_sampleTableReplaceReq.wayMask := s1_sampleTableReplaceWayOH
  s1_sampleTableReplaceReq.entry := s1_sampleTableReplaceEntry

  // Encapsulate replace state table write request
  val s1_sampleTableReplaceStateReq = Wire(new SampleTableStateWriteReq())
  s1_sampleTableReplaceStateReq.en := s1_sampleTableReplaceEn
  s1_sampleTableReplaceStateReq.setIdx := s1_sampleTableReplaceIdx
  s1_sampleTableReplaceStateReq.wayMask := s1_sampleTableReplaceEn.asUInt
  s1_sampleTableReplaceStateReq.state := s1_sampleTableReplaceNextState

  //Data sent to the pattern table
  io.resp.valid := s1_sampleTableReplaceEn && s1_sampleTableVictimEntry.valid
  io.resp.bits.pcTag := s1_sampleTableVictimEntry.pcTag 
  io.resp.bits.touched := s1_sampleTableVictimEntry.touched 

  /***************Stage 2: writeback*******************/
  val s2_sampleTableUpdateReq = RegNext(s1_sampleTableUpdateReq)
  val s2_sampleTableUpdateStateReq = RegNext(s1_sampleTableUpdateStateReq)
  val s2_sampleTableReplaceReq = RegNext(s1_sampleTableReplaceReq)
  val s2_sampleTableReplaceStateReq = RegNext(s1_sampleTableReplaceStateReq)

  // sampleTable update writeback
  sampleTable.io.w(sampleTableUpdatePort).en := s2_sampleTableUpdateReq.en
  sampleTable.io.w(sampleTableUpdatePort).req.setIdx := s2_sampleTableUpdateReq.setIdx
  sampleTable.io.w(sampleTableUpdatePort).req.wayMask := s2_sampleTableUpdateReq.wayMask   
  sampleTable.io.w(sampleTableUpdatePort).req.data := VecInit(Seq.fill(nlParams.sampleTableWays)(s2_sampleTableUpdateReq.entry))

  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).en := s2_sampleTableUpdateStateReq.en
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.setIdx := s2_sampleTableUpdateStateReq.setIdx
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.wayMask := s2_sampleTableUpdateStateReq.wayMask   
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.data := VecInit(Seq.fill(1)(s2_sampleTableUpdateStateReq.state))

  // sampleTable replace writeback
  sampleTable.io.w(sampleTableReplacePort).en := s2_sampleTableReplaceReq.en
  sampleTable.io.w(sampleTableReplacePort).req.setIdx := s2_sampleTableReplaceReq.setIdx
  sampleTable.io.w(sampleTableReplacePort).req.wayMask := s2_sampleTableReplaceReq.wayMask
  sampleTable.io.w(sampleTableReplacePort).req.data := VecInit(Seq.fill(nlParams.sampleTableWays)(s2_sampleTableReplaceReq.entry))

  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).en := s2_sampleTableReplaceStateReq.en
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.setIdx := s2_sampleTableReplaceStateReq.setIdx
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.wayMask := s2_sampleTableReplaceStateReq.wayMask   
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.data := VecInit(Seq.fill(1)(s2_sampleTableReplaceStateReq.state))

  //db
  val dbTable = ChiselDB.createTable(s"NLsampledb", new SampleDb, basicDB = false)
  val dbData = Wire(new SampleDb())
  dbTable.log(dbData, dbData.trainEn & (dbData.hit || dbData.insertEn || (dbData.timeSample<(1.U<<14))), s"Nlsample", clock, reset)

  dbData.trainEn := s1_valid
  dbData.addr := s1_blockAddr
  dbData.pc := s1_pc
  dbData.timeSample := s1_timeSample
 
  dbData.hit := sampleTableUpdateHit  
  dbData.plru_state := s1_sampleTableReplaceNextState

  dbData.updateEn := s1_sampleTableUpdateEn
  dbData.updateIdx := s1_sampleTableUpdateIdx
  dbData.updateMask := s1_sampleTableUpdateHitWayOH
  dbData.updateData := s1_sampleTableUpdatedEntry
  dbData.timeSampleDelta := timeSampleDelta

  dbData.insertEn := s1_sampleTableReplaceEn
  dbData.insertIdx := s1_sampleTableReplaceIdx
  dbData.insertMask := s1_sampleTableReplaceWayOH
  dbData.insertData := s1_sampleTableReplaceEntry
  dbData.victimData := s1_sampleTableVictimEntry

  // detailed time delta for analysis
  XSPerfAccumulate("nlSampleTrainTimes",s0_valid)
  XSPerfAccumulate("nlSampleInsertTimes", s0_sampleTableReplaceEn)

  //update analysis
  XSPerfAccumulate("nlSampleUpdateReqNotHitTimes",s1_valid & !sampleTableUpdateHit)
  XSPerfAccumulate("nlSampleUpdateReqHitOverBoardTimes",s1_valid & sampleTableUpdateHit & !inRange)
  XSPerfAccumulate("nlSampleUpdateTimes",s1_sampleTableUpdateEn) 
  XSPerfAccumulate("nlSampleStateUpdateTimes",s1_valid & sampleTableUpdateHit)
  XSPerfAccumulate("nlSampleUpdateReqMulHitTimes",s1_valid & PopCount(s1_sampleTableUpdateHitVec)>1.U)

  //victim data analysis
  XSPerfAccumulate("nlSampleVictimTouchedTrueTimes",s1_sampleTableReplaceEn & s1_sampleTableVictimEntry.touched)
  XSPerfAccumulate("nlSampleVictimTouchedFalseTimes",s1_sampleTableReplaceEn & !s1_sampleTableVictimEntry.touched)
}

class NextLinePattern(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PatternTrain))
    val req   = Flipped(DecoupledIO(new PatternReq))
    val resp  = DecoupledIO(new PatternResp)
})

  //pattern Table
  val patternTable = Module(new FullyAssociativeMemory(
      gen = new PatternTableEntryField(),
      keyWidth = patternTableTagBits,
      numEntries = nlParams.patternTableSets,
      numReadPorts = 2,
      numWritePorts = 2,
      shouldReset = true
  ))
  val patternTableReplacer = ReplacementPolicy.fromString(patternTableReplacementPolicy, nlParams.patternTableSets)
  
  val patternTableReplaceState = RegInit(0.U(patternTableReplacer.nBits.W))

  // default all ready
  io.train.ready := true.B
  io.req.ready   := true.B
  
  /***************Stage 0: read Pattern Table*******************/
  val s0_trainValid = io.train.fire 
  val s0_trainPcTag = io.train.bits.pcTag
  val s0_trainTouched = io.train.bits.touched
  patternTable.io.r(patternTableTrainPort).req.key := s0_trainPcTag
  
  val s0_reqValid = io.req.fire  
  val s0_reqPc    = io.req.bits.pc
  val s0_reqAddr  = io.req.bits.addr
  val s0_reqPcTag = getPatternTableTag(s0_reqPc)
  patternTable.io.r(patternTablePrefetchPort).req.key := s0_reqPcTag
  
  /***************Stage 1: process train and prefetch requests*******************/
  /************** train part*******************/
  val s1_trainValid         = RegNext(s0_trainValid, false.B)
  val s1_trainPcTag         = RegEnable(s0_trainPcTag, s0_trainValid)
  val s1_trainTouched       = RegEnable(s0_trainTouched, s0_trainValid)
  val s1_trainResp          = RegEnable(patternTable.io.r(patternTableTrainPort).resp, s0_trainValid)
  
  val s1_patternUpdateEn  = WireInit(false.B)
  val s1_patternUpdateIdx = WireInit(0.U(patternTableSetBits.W))

  val s1_patternInsertEn  = WireInit(false.B)
  val s1_patternInsertIdx = WireInit(0.U(patternTableSetBits.W))
  
  val s1_patternNewEntry  = WireInit(s1_trainResp.data)
 
  //update or insert
  when(s1_trainValid) {
    when(s1_trainResp.hit) { //If the training data is hit, will update entry.

      s1_patternUpdateEn := true.B 
      s1_patternNewEntry.valid := true.B
      s1_patternUpdateIdx := s1_trainResp.hitIdx

      val currentSat = s1_trainResp.data.sat
      when(s1_trainTouched) {// If prefetch hit, increase the saturation counter (if not at max)
        s1_patternNewEntry.sat := Mux(currentSat === maxSat, maxSat, currentSat + 1.U)
      }.otherwise {
        s1_patternNewEntry.sat := Mux(currentSat === 0.U, 0.U, currentSat - 1.U)
      }
      
    }.otherwise {//If the training data is NOT hit, insert a new entry.
      s1_patternInsertEn := s1_trainTouched 
     
      s1_patternNewEntry.valid := s1_trainTouched
      s1_patternInsertIdx := patternTableReplacer.get_replace_way(patternTableReplaceState)
      s1_patternNewEntry.sat := 1.U  //The initial confidence is 1
    }
  }

  // update the PLRU state of the set
  val trainUpdateState = patternTableReplacer.get_next_state(patternTableReplaceState, Mux(s1_trainResp.hit, 
                                                                                         s1_trainResp.hitIdx, s1_patternInsertIdx))
                                                                                              
  patternTableReplaceState := trainUpdateState

  //************Prefetch part***************//
  val s1_reqValid = RegNext(s0_reqValid, false.B)
  val s1_reqPc = RegEnable(s0_reqPc, s0_reqValid)
  val s1_reqAddr = RegEnable(s0_reqAddr, s0_reqValid)
  val s1_reqResp = RegEnable(patternTable.io.r(patternTablePrefetchPort).resp, s0_reqValid)

  val s1_prefetchAddr = s1_reqAddr + blockBytes.U
  val s1_reqHitValidEntry = s1_reqValid && s1_reqResp.hit 
  val s1_crossPage = getPPN(s1_reqAddr) =/= getPPN(s1_prefetchAddr)
  val s1_needPrefetch = s1_reqHitValidEntry & (s1_reqResp.data.sat === maxSat) & !s1_crossPage
  /***************Stage 2: write Pattern Table*******************/
  val s2_patternUpdateEn = RegNext(s1_patternUpdateEn, false.B)
  val s2_patternUpdateIdx = RegEnable(s1_patternUpdateIdx, s1_patternUpdateEn)
 
  val s2_patternInsertEn = RegNext(s1_patternInsertEn, false.B)
  val s2_patternInsertIdx = RegEnable(s1_patternInsertIdx, s1_patternInsertEn)
  
  val s2_patternNewKey = RegEnable(s1_trainPcTag, s1_patternUpdateEn || s1_patternInsertEn)
  val s2_patternNewEntry = RegEnable(s1_patternNewEntry, s1_patternUpdateEn || s1_patternInsertEn)

  // update
  patternTable.io.w(patternTableUpdatePort).en := s2_patternUpdateEn
  patternTable.io.w(patternTableUpdatePort).req.valid := true.B  
  patternTable.io.w(patternTableUpdatePort).req.key := s2_patternNewKey
  patternTable.io.w(patternTableUpdatePort).req.idx := s2_patternUpdateIdx
  patternTable.io.w(patternTableUpdatePort).req.data := s2_patternNewEntry
  
  // insert
  patternTable.io.w(patternTableInsertPort).en := s2_patternInsertEn
  patternTable.io.w(patternTableInsertPort).req.valid := true.B  
  patternTable.io.w(patternTableInsertPort).req.key := s2_patternNewKey
  patternTable.io.w(patternTableInsertPort).req.idx := s2_patternInsertIdx
  patternTable.io.w(patternTableInsertPort).req.data := s2_patternNewEntry

  //prefetcher initiate a prefetch request
  io.resp.valid := RegNext(s1_needPrefetch, false.B) 
  io.resp.bits.nextAddr := RegEnable(s1_prefetchAddr, s1_needPrefetch)

  //db  
  val dbTable = ChiselDB.createTable(s"NLpatterndb", new PatternDb, basicDB = false)
  val dbData = Wire(new PatternDb())
  dbTable.log(dbData, dbData.trainEn & (dbData.hit || dbData.insertEn ), s"Nlsample", clock, reset)
  dbData.sat := s1_trainResp.data.sat
  dbData.hit := s1_trainResp.hit
  dbData.hitData := s1_trainResp.data

  dbData.trainEn := s1_trainValid

  dbData.trainData.pcTag := s1_trainPcTag
  dbData.trainData.valid := true.B 
  dbData.trainData.tag   := 0.U 
  dbData.trainData.sampleTime := 0.U
  dbData.trainData.touched    := s1_trainTouched

  dbData.updateEn   := s1_patternUpdateEn
  dbData.updateIdx  := s1_patternUpdateIdx
  dbData.updateData := s1_patternNewEntry

  dbData.insertEn   := s1_patternInsertEn
  dbData.insertIdx  := s1_patternInsertIdx 
  dbData.insertData := s1_patternNewEntry


  XSPerfAccumulate("nlPatternTrainTimes",s1_trainValid) 
  XSPerfAccumulate("nlPatternTrainMulHitTimes",s1_trainResp.multHit||s1_reqResp.multHit)
  XSPerfAccumulate("nlPatternTrainReplaceTimes",s1_patternInsertEn)

  //update analysis
  XSPerfAccumulate("nlPatternUpdateTimes",s1_trainValid && s1_trainResp.hit)
  XSPerfAccumulate("nlPatternUpdateTouchedTrueTimes",s1_trainValid & s1_trainResp.hit & s1_trainTouched)
  XSPerfAccumulate("nlPatternUpdateTouchedFalseTimes",s1_trainValid & s1_trainResp.hit & !s1_trainTouched)

  //pc prefetch analysis
  XSPerfAccumulate("nlPatternPcHitTimes",s1_reqResp.hit && s1_reqValid)
  XSPerfAccumulate("nlPatternPcHitValidEntryTimes",s1_reqHitValidEntry)
  XSPerfAccumulate("nlPatternCrossPageTimes", s1_reqHitValidEntry & (s1_reqResp.data.sat === maxSat) & s1_crossPage)

  //saturation
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq3Times",s1_reqHitValidEntry & (s1_reqResp.data.sat === 3.U) )
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq2Times",s1_reqHitValidEntry & (s1_reqResp.data.sat === 2.U) )
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq1Times",s1_reqHitValidEntry & (s1_reqResp.data.sat === 1.U) )
  XSPerfAccumulate("nlPatternPcHitValidEntrySatEq0Times",s1_reqHitValidEntry & (s1_reqResp.data.sat === 0.U) )
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
  prefetcherSample.io.train.bits.addr := io.train.bits.addr
  prefetcherSample.io.train.bits.pc := io.train.bits.pc.getOrElse(0.U)
  prefetcherSample.io.train.bits.timeSample := timeSampleCounter

  // Sample ---> Pattern 
  prefetcherPattern.io.train <> prefetcherSample.io.resp

  //io.train ---> pattern.req 
  val shouldQuery = shouldTrain
  prefetcherPattern.io.req.valid := shouldQuery
  prefetcherPattern.io.req.bits.addr := io.train.bits.addr
  prefetcherPattern.io.req.bits.pc := io.train.bits.pc.getOrElse(0.U)

  //pattern.resp ---> prefetchQueue.in
  prefetchQueue.io.enq <> prefetcherPattern.io.resp

  // ========== prefetchQueue.out --->io.req ==========
  val nextAddr = prefetchQueue.io.deq.bits.nextAddr
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

