package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, MemReqSource}
import coupledL2.utils.{MultiPortRegFile,FullyAssociativeMemory}
import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.ReplacementPolicy
import huancun.{TPmetaReq, TPmetaResp}
import utility.{sram,XSPerfAccumulate}
import freechips.rocketchip.util.SeqToAugmentedSeq
import freechips.rocketchip.rocket.CSR.X
import utility.XSPerfAccumulate

//define Next-Line Prefetcher base parameters
case class NLParameters(
    L2SliceNum:Int = 4, //L2 cache slice number
    timeSampleCounterBits:Int = 64, //Sampling counter bit width
    timeSampleRate : Int = 256, //Sampling rate
    timeSampleMinDistance :Int = 4, //Minimum sampling distance

    //zzq Sample Table config 
    //use 4 way，then there are 8 blocks in way 
    sampleTableWays: Int = 4, 
    // sampleTableSets: Int = 32,

    //zzq offset bits in a block
    //sample entry  tag bits ,it used to distinguish with other entry
  
    sampleTableTouchedBits: Int = 1,
    sampleTableReplacementPolicy: String = "plru",
    
    //zzq Pattern Table config 
    patternTableWays: Int = 1,
    patternTableSets: Int = 64,
   
     //zzq offset bits in a block
    
    patternTableSatBits: Int = 2,
    patternTableReplacementPolicy: String = "plru",

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

//define Next-Line Prefetcher usefull parameters
trait HasNLParams extends HasCoupledL2Parameters {
  val nlParams = prefetchers.find {
    case p: NLParameters => true 
    case _ => false
  }.get.asInstanceOf[NLParameters]

  def vaddrBits = fullVAddrBits
  def tablePcTagBits        = vaddrBits  //The PcTag field of sampleTable is the same 50 bits as the tag field of patternTable.

  //timeSampleCounter 
  def timeSampleCounterBits = nlParams.timeSampleCounterBits
  def timeSampleCounterMax  = ((BigInt(1) << timeSampleCounterBits) - 1).U(timeSampleCounterBits.W)
  def timeSampleRateBits    = log2Ceil(nlParams.timeSampleRate)
  def timeSampleMinDistance = nlParams.timeSampleMinDistance
  def timeSampleMaxDistance = blocks * nlParams.L2SliceNum / 2 //it Calculate how many bytes the L2 cache is. then div 2
  
  //sample
  def sampleTableBlocks    = timeSampleMaxDistance / nlParams.timeSampleRate 
  def sampleTableSets      = sampleTableBlocks / nlParams.sampleTableWays 
  def sampleTableSetBits   = log2Ceil(sampleTableSets)
  def sampleTableWaysBits  = log2Ceil(nlParams.sampleTableWays)
  def sampleTableTagBits   = vaddrBits - sampleTableSetBits - offsetBits // offsetBits=6bit，setBits=3bit, tag=50-3-6=41
  def sampleTablePcTagBits = tablePcTagBits
  def sampleTableTimeSampleBits = nlParams.timeSampleCounterBits
  def sampleTableReplacementPolicy = nlParams.sampleTableReplacementPolicy
  def sampleTableUpdatePort     = nlParams.sampleTableUpdatePort
  def sampleTableReplacePort    = nlParams.sampleTableReplacePort

  // pattern 
  def patternTableSetBits  = log2Ceil(nlParams.patternTableSets)
  def patternTableWaysBits = log2Ceil(nlParams.patternTableWays)
  
  def patternTableTagBits  = tablePcTagBits
  def patternTableSatBits  = nlParams.patternTableSatBits
  def maxSat = (1.U << patternTableSatBits) - 1.U 

  def patternTableReplacementPolicy = nlParams.patternTableReplacementPolicy
  def patternTableTrainPort     = nlParams.patternTableTrainPort
  def patternTablePrefetchPort  = nlParams.patternTablePrefetchPort
  
  def patternTableUpdatePort  = nlParams.patternTableUpdatePort
  def patternTableInsertPort  = nlParams.patternTableInsertPort  
 
}

//NL
abstract class NLBundle(implicit val p: Parameters) extends Bundle with HasNLParams

abstract class NLModule(implicit val p: Parameters) extends Module with HasNLParams {
  
  // ==================== Helper function: Address parsing====================
  def getBlockAddr(addr: UInt): UInt = {
    addr >> offsetBits //This offsetBit is determined by the block size of the L2 cache.
  }
  
  def getSampleTableSet(addr: UInt): UInt = {
    val blockAddr = getBlockAddr(addr)
    blockAddr(sampleTableSetBits - 1, 0)
  }
  
  def getSampleTableTag(addr: UInt): UInt = {
    val blockAddr = getBlockAddr(addr)
    blockAddr(vaddrBits - offsetBits - 1, sampleTableSetBits)
  }
  
  def getPatternTableTag(addr: UInt): UInt = {
    addr
  }
  
  def parseTrainData(addr: UInt, pc: UInt): (UInt, UInt, UInt) = {
    val sampleSet = getSampleTableSet(addr)
    val sampleTag = getSampleTableTag(addr)
    val patternTag = getPatternTableTag(pc)
    (sampleSet, sampleTag, patternTag)
  }
}
class TrainData(implicit p: Parameters ) extends NLBundle{
    val addr = UInt(vaddrBits.W)
    val pc   = UInt(vaddrBits.W)
}


class SampleTableEntryField(implicit p: Parameters) extends NLBundle{
    val tag        = UInt(sampleTableTagBits.W)
    val sampleTime = UInt(sampleTableTimeSampleBits.W)
    val pcTag      = UInt(sampleTablePcTagBits.W)
    val touched    = Bool()
    val valid      = Bool() 
}


class SampleTableWriteReq(implicit p: Parameters) extends NLBundle {
  val en       = Bool()
  val setIdx   = UInt(sampleTableSetBits.W)
  val wayMask  = UInt(nlParams.sampleTableWays.W)
  val entry    = new SampleTableEntryField()
}


class SampleTableReplaceStateWriteReq(implicit p: Parameters) extends NLBundle {
  val en      = Bool()
  val setIdx  = UInt(sampleTableSetBits.W)
  val wayMask = UInt(1.W)  
  val state   = UInt((nlParams.sampleTableWays-1).W)  
}

//sample
class SampleTrain(implicit p: Parameters) extends NLBundle {
    val addr       = UInt(vaddrBits.W)
    val pc         = UInt(vaddrBits.W)
    val timeSample = UInt(timeSampleCounterBits.W)
}

class PatternTableEntryField(implicit p: Parameters) extends NLBundle{
     val sat  = UInt(patternTableSatBits.W)
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
    val needPrefetch = Bool()
    val nextAddr = UInt(vaddrBits.W)  
}

/*
```
vaddr (50bit):
┌────────────────────┬─────────┬────────┐
│    Sample Tag      │   Set   │ Offset │
│    (41 bit)        │  (3 bit)│ (6 bit)│
└────────────────────┴─────────┴────────┘
     ↑                  ↑         ↑
     getSampleTableTag  │         getBlockAddr
                        getSampleTableSet
```
*/
class NextLineSample(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new SampleTrain)) 
    val resp  = DecoupledIO(new PatternTrain) 

  })

  // Sample Table
  val sampleTable= Module(new MultiPortRegFile(
      gen  = new SampleTableEntryField(),
      sets = sampleTableSets,
      ways = nlParams.sampleTableWays,
      numReadPorts = 2,
      numWritePorts = 2,
      shouldReset = true  //Require initial reset to ensure that valid is all zeros.
  ))

  //replacer
  val sampleTableReplacer = ReplacementPolicy.fromString(sampleTableReplacementPolicy,nlParams.sampleTableWays)
  val sampleTableReplaceStateRegs = Module(new MultiPortRegFile(
      gen = UInt(sampleTableReplacer.nBits.W),
      sets = sampleTableSets,
      ways = 1,
      numReadPorts = 2,
      numWritePorts = 2,
      shouldReset = true  
  ))

 
  io.train.ready := true.B
  /***************Stage 0: *******************/
  val s0_valid      = io.train.fire  
  
  val s0_timeSample = io.train.bits.timeSample
  val s0_addr = io.train.bits.addr 
  val s0_pc   = io.train.bits.pc
  
  //When [timeSampleRateBits-1,0] of timeSample is 0, that is, finish round of timeSampleRate,
  //and a new sample arrives at this time, it is inserted into the table.
  val s0_sampleTableReplaceEn = s0_valid & (!s0_timeSample(timeSampleRateBits-1,0).orR)

  //Parse address
  val s0_sampleTableSet = getSampleTableSet(s0_addr)
  val s0_sampleTableTag = getSampleTableTag(s0_addr)

  val s0_sampleTableUpdateIdx = s0_sampleTableSet -1.U 
  val s0_sampleTableReplaceIdx = s0_sampleTableSet 

  // read sampleTable Plru data table
  sampleTable.io.r(sampleTableUpdatePort).req.setIdx  := s0_sampleTableUpdateIdx 
  sampleTable.io.r(sampleTableReplacePort).req.setIdx := s0_sampleTableReplaceIdx        


  // read sampleTable Plru state table
  sampleTableReplaceStateRegs.io.r(sampleTableUpdatePort).req.setIdx := s0_sampleTableUpdateIdx 
  sampleTableReplaceStateRegs.io.r(sampleTableReplacePort).req.setIdx := s0_sampleTableReplaceIdx
  
  /***************Stage 1: handle  data of Sample Table reading *******************/
  val s1_valid                 = RegNext(s0_valid, false.B)
  val s1_timeSample            = RegNext(s0_timeSample)
  val s1_addr                  = RegNext(s0_addr)
  val s1_pc                    = RegNext(s0_pc)

  val (s1_sampleTableSet, s1_sampleTableTag, s1_patternTableTag) = parseTrainData(s1_addr, s1_pc)

  //****************update part******************//
  val s1_sampleTableUpdateEntries = RegNext(sampleTable.io.r(sampleTableUpdatePort).resp.data)  // Vec(ways, SampleTableEntryField)
  val s1_sampleTableUpdataState = RegInit(0.U(sampleTableReplacer.nBits.W))
  val s1_SampleTableUpdatedHitEntry  = Wire(new SampleTableEntryField)
  val s1_sampleTableUpdateEn         = Wire(Bool())
  val s1_SampleTableUpdatedEntry     = Wire(new SampleTableEntryField)

  s1_sampleTableUpdataState     := sampleTableReplaceStateRegs.io.r(sampleTableUpdatePort).resp.data(0)
  
  // UpdateData Hit check:
  val s1_sampleTableUpdateHitVec     = VecInit(s1_sampleTableUpdateEntries.map(entry => entry.valid && (entry.tag === s1_sampleTableTag)))
  val sampleTableUpdateHit           = s1_sampleTableUpdateHitVec.asUInt.orR  // example: (1000)===>1
  val s1_sampleTableUpdateHitWayOH   = s1_sampleTableUpdateHitVec.asUInt      // example: 1000===>8
  val s1_sampleTableUpdateHitWayIdx  = OHToUInt(s1_sampleTableUpdateHitWayOH) // example: 8===>3 

  val s1_sampleTableUpdataNextState  = sampleTableReplacer.get_next_state(s1_sampleTableUpdataState,s1_sampleTableUpdateHitWayIdx) 
  s1_SampleTableUpdatedHitEntry     := s1_sampleTableUpdateEntries(s1_sampleTableUpdateHitWayIdx)

  
  // check if update
  val timeSampleDelta  = s1_timeSample -s1_SampleTableUpdatedHitEntry.sampleTime
  val realate = timeSampleDelta < timeSampleMaxDistance.U && timeSampleMinDistance.U < timeSampleDelta
  when(s1_valid & sampleTableUpdateHit & realate ) {//if hit and update condition ,then update   
    s1_sampleTableUpdateEn := true.B
    s1_SampleTableUpdatedEntry := s1_SampleTableUpdatedHitEntry
    s1_SampleTableUpdatedEntry.touched := true.B
    
  }.otherwise{//if not satisfied, do not update
    s1_sampleTableUpdateEn := false.B
    s1_SampleTableUpdatedEntry := 0.U.asTypeOf(new SampleTableEntryField)
      
  }

  //***************replace part*******************//
  val s1_sampleTableReplaceEn     = RegNext(s0_sampleTableReplaceEn)
  val s1_sampleTableReplaceEntries = RegNext(sampleTable.io.r(sampleTableReplacePort).resp.data)  // Vec(ways, SampleTableEntryField)
  val s1_sampleTableReplaceState = RegInit(0.U(sampleTableReplacer.nBits.W))

  s1_sampleTableReplaceState := sampleTableReplaceStateRegs.io.r(sampleTableReplacePort).resp.data(0)

  //caculate victim way id
  val s1_sampleTableReplaceWayIdx = sampleTableReplacer.get_replace_way(s1_sampleTableReplaceState)
  val s1_sampleTableReplaceNextState = sampleTableReplacer.get_next_state(s1_sampleTableReplaceState,s1_sampleTableUpdateHitWayIdx)
  
  //get victim way data
  val s1_sampleTableReplaceWayOH = UIntToOH(s1_sampleTableReplaceWayIdx)
  val s1_sampleTableVictimEntry = s1_sampleTableReplaceEntries(s1_sampleTableReplaceWayIdx) 

  //set insert entry data
  val s1_sampleTableReplaceEntry = Wire(new SampleTableEntryField)  
  s1_sampleTableReplaceEntry.valid := true.B
  s1_sampleTableReplaceEntry.tag := s1_sampleTableTag
  s1_sampleTableReplaceEntry.sampleTime := s1_timeSample
  s1_sampleTableReplaceEntry.pcTag := getPatternTableTag(s1_pc)
  s1_sampleTableReplaceEntry.touched := false.B
  

  // Encapsulate the two write requests of the Sample Table
  val s1_sampleTableUpdateReq      = Wire(new SampleTableWriteReq())
  s1_sampleTableUpdateReq.en       := s1_sampleTableUpdateEn
  s1_sampleTableUpdateReq.setIdx   := s1_sampleTableSet
  s1_sampleTableUpdateReq.wayMask  := s1_sampleTableUpdateHitWayOH
  s1_sampleTableUpdateReq.entry    := s1_SampleTableUpdatedEntry

  val s1_sampleTableReplaceReq      = Wire(new SampleTableWriteReq())
  s1_sampleTableReplaceReq.en      := s1_sampleTableReplaceEn
  s1_sampleTableReplaceReq.setIdx  := s1_sampleTableSet
  s1_sampleTableReplaceReq.wayMask := s1_sampleTableReplaceWayOH
  s1_sampleTableReplaceReq.entry   := s1_sampleTableReplaceEntry

  // Encapsulate replace state table write request
  val s1_sampleTableUpdateStateReq = Wire(new SampleTableReplaceStateWriteReq())
  s1_sampleTableUpdateStateReq.en      := s1_sampleTableUpdateEn
  s1_sampleTableUpdateStateReq.setIdx  := s1_sampleTableSet
  s1_sampleTableUpdateStateReq.wayMask := s1_sampleTableUpdateHitWayOH
  s1_sampleTableUpdateStateReq.state   := s1_sampleTableUpdataNextState

  val s1_sampleTableReplaceStateReq = Wire(new SampleTableReplaceStateWriteReq())
  s1_sampleTableReplaceStateReq.en      := s1_sampleTableReplaceEn
  s1_sampleTableReplaceStateReq.setIdx  := s1_sampleTableSet
  s1_sampleTableReplaceStateReq.wayMask := s1_sampleTableReplaceWayOH
  s1_sampleTableReplaceStateReq.state   := s1_sampleTableReplaceNextState

  //Data sent to the pattern table
  io.resp.valid        := s1_sampleTableReplaceEn && s1_sampleTableVictimEntry.valid
  io.resp.bits.pcTag   := s1_sampleTableVictimEntry.pcTag 
  io.resp.bits.touched := s1_sampleTableVictimEntry.touched 

  /***************Stage 2: writeback*******************/
  val s2_valid = RegNext(s1_valid, false.B)

  val s2_sampleTableUpdateReq       = RegNext(s1_sampleTableUpdateReq)
  val s2_sampleTableUpdateStateReq  = RegNext(s1_sampleTableUpdateStateReq)
  val s2_sampleTableReplaceReq      = RegNext(s1_sampleTableReplaceReq)
  val s2_sampleTableReplaceStateReq = RegNext(s1_sampleTableReplaceStateReq)

  // sampleTable update writebacek
  sampleTable.io.w(sampleTableUpdatePort).en          := s2_valid && s2_sampleTableUpdateReq.en
  sampleTable.io.w(sampleTableUpdatePort).req.setIdx  := s2_sampleTableUpdateReq.setIdx
  sampleTable.io.w(sampleTableUpdatePort).req.wayMask := s2_sampleTableUpdateReq.wayMask   
  sampleTable.io.w(sampleTableUpdatePort).req.data    := VecInit(Seq.fill(nlParams.sampleTableWays)(s2_sampleTableUpdateReq.entry))

  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).en          := s2_valid && s2_sampleTableUpdateStateReq.en
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.setIdx  := s2_sampleTableUpdateStateReq.setIdx
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.wayMask := s2_sampleTableUpdateStateReq.wayMask   
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.data    := VecInit(Seq.fill(1)(s2_sampleTableUpdateStateReq.state))

  // sampleTable replace writeback
  sampleTable.io.w(sampleTableReplacePort).en          := s2_valid && s2_sampleTableReplaceReq.en
  sampleTable.io.w(sampleTableReplacePort).req.setIdx  := s2_sampleTableReplaceReq.setIdx
  sampleTable.io.w(sampleTableReplacePort).req.wayMask := s2_sampleTableReplaceReq.wayMask
  sampleTable.io.w(sampleTableReplacePort).req.data    := VecInit(Seq.fill(nlParams.sampleTableWays)(s2_sampleTableReplaceReq.entry))

  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).en          := s2_valid && s2_sampleTableReplaceStateReq.en
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.setIdx  := s2_sampleTableReplaceStateReq.setIdx
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.wayMask := s2_sampleTableReplaceStateReq.wayMask   
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.data    := VecInit(Seq.fill(1)(s2_sampleTableReplaceStateReq.state))


  XSPerfAccumulate("sampleTable_train_times",s0_valid)
  
  XSPerfAccumulate("sampleTable_insert_times", s0_sampleTableReplaceEn)

  //update analysis
  XSPerfAccumulate("sampleTable_update_req_not_hit_times",s1_valid & !sampleTableUpdateHit)
  XSPerfAccumulate("sampleTable_update_req_hit_over_board_times",s1_valid & sampleTableUpdateHit & !realate)
  XSPerfAccumulate("sampleTable_update_times",s1_sampleTableUpdateEn) 

  //victim data analysis
  XSPerfAccumulate("sampleTable_victim_touched_true_times",s1_sampleTableReplaceEn & s1_sampleTableVictimEntry.touched)
  XSPerfAccumulate("sampleTable_victim_touched_false_times",s1_sampleTableReplaceEn & !s1_sampleTableVictimEntry.touched)

}





class NextLinePattern(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PatternTrain))
    val req = Flipped(DecoupledIO(new PatternReq))
    val resp = DecoupledIO(new PatternResp)
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
  io.req.ready := true.B
  
  /***************Stage 0: read Pattern Table*******************/
  val s0_trainValid = io.train.fire 
  val s0_trainPcTag = io.train.bits.pcTag
  val s0_trainTouched = io.train.bits.touched
  
  val s0_reqValid = io.req.fire  
  val s0_reqPc    = io.req.bits.pc
  val s0_reqAddr  = io.req.bits.addr
  val s0_reqPcTag = getPatternTableTag(s0_reqPc)
  
  // Issue two types of read requests to patternTable
  patternTable.io.r(patternTableTrainPort).req.key := s0_trainPcTag
  patternTable.io.r(patternTablePrefetchPort).req.key := s0_reqPcTag
  

  /***************Stage 1: process train and prefetch requests*******************/
  /************** train part*******************/
  val s1_trainValid         = RegNext(s0_trainValid, false.B)
  val s1_trainPcTag         = RegNext(s0_trainPcTag)
  val s1_trainTouched       = RegNext(s0_trainTouched)
  val s1_trainResp          = RegNext(patternTable.io.r(patternTableTrainPort).resp)
  
  val s1_patternUpdateEn = WireInit(false.B)
  val s1_patternUpdateIdx = WireInit(0.U(log2Ceil(nlParams.patternTableSets).W))

  val s1_patternInsertEn = WireInit(false.B)
  val s1_patternInsertIdx = WireInit(0.U(log2Ceil(nlParams.patternTableSets).W))
  val s1_patternNewEntry = WireInit(s1_trainResp.data)
 
  //update or insert
  when(s1_trainValid) {
    when(s1_trainResp.hit) { //If the training data is hit, the route will be updated.

      s1_patternUpdateEn := true.B 
      s1_patternNewEntry.valid := true.B
      s1_patternUpdateIdx := s1_trainResp.hitIdx

      val currentSat = s1_trainResp.data.sat
      when(s1_trainTouched) {// If prefetch hit, increase the saturation counter (if not at max)
        s1_patternNewEntry.sat := Mux(currentSat === maxSat, maxSat, currentSat + 1.U)
      }.otherwise {
        s1_patternNewEntry.sat := Mux(currentSat === 0.U, 0.U, currentSat - 1.U)
      }
      
    }.otherwise {//If the training data is hit, the route will be updated.
      s1_patternInsertEn := s1_trainTouched 
     
      s1_patternNewEntry.valid := s1_trainTouched
      s1_patternInsertIdx := patternTableReplacer.get_replace_way(patternTableReplaceState)
      s1_patternNewEntry.sat := 1.U  //The initial confidence is 1
    }
  }

  //************Prefetch part***************//
  val s1_reqValid     = RegNext(s0_reqValid, false.B)
  val s1_reqPc        = RegNext(s0_reqPc)
  val s1_reqAddr      = RegNext(s0_reqAddr)
  val s1_reqResp = RegNext(patternTable.io.r(patternTablePrefetchPort).resp)

  val s1_prefetchReadIdx = s1_reqResp.hitIdx
  val s1_reqHitValidEntry = s1_reqValid && s1_reqResp.hit && 
                           s1_reqResp.data.valid
  val s1_needPrefetch = s1_reqHitValidEntry & (s1_reqResp.data.sat === ((1.U << patternTableSatBits) - 1.U))
  
 

  // update the PLRU state of the set
  val readUpdateState  = Mux(s1_reqValid, patternTableReplacer.get_next_state(patternTableReplaceState, s1_prefetchReadIdx), patternTableReplaceState)
  
  val trainUpdateState = Mux(s1_trainValid, patternTableReplacer.get_next_state(readUpdateState, Mux(s1_trainResp.hit, 
                                                                                                                      s1_trainResp.hitIdx, s1_patternInsertIdx)), 
                                                                                                readUpdateState)
  patternTableReplaceState := trainUpdateState
  

  /***************Stage 2: write Pattern Table*******************/
  val s2_patternUpdateEn    = RegNext(s1_patternUpdateEn, false.B)
  val s2_patternUpdateIdx   = RegNext(s1_patternUpdateIdx)
 
  val s2_patternInsertEn  = RegNext(s1_patternInsertEn, false.B)
  val s2_patternInsertIdx = RegNext(s1_patternInsertIdx)
  
  val s2_patternNewKey    = RegNext(s1_trainPcTag)
  val s2_patternNewEntry  = RegNext(s1_patternNewEntry)

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
  patternTable.io.w(patternTableInsertPort).req.data :=  s2_patternNewEntry

  //prefetcher initiate a prefetch request
  io.resp.valid             := RegNext(s1_needPrefetch, false.B) 
  io.resp.bits.needPrefetch := RegNext(s1_needPrefetch, false.B)
  io.resp.bits.nextAddr     := RegNext(s1_reqAddr + blockBytes.U, 0.U)


  XSPerfAccumulate("NextLinePattern_train_times",s1_trainValid) 
  //replace analysis
  XSPerfAccumulate("NextLinePattern_train_replace_times",s1_patternInsertEn)


  //update analysis
  XSPerfAccumulate("NextLinePattern_train_update_times",s1_trainValid && s1_trainResp.hit)
  XSPerfAccumulate("NextLinePattern_train_data_touched_ture_times",s1_trainValid & s1_trainResp.hit & s1_trainTouched)
  XSPerfAccumulate("NextLinePattern_train_data_touched_false_times",s1_trainValid & s1_trainResp.hit & !s1_trainTouched)

 

  //pc prefetch analysis
  XSPerfAccumulate("NextLinePattern_pc_hit_times",s1_reqResp.hit && s1_reqValid)
  XSPerfAccumulate("NextLinePattern_pc_hit_validEntry",s1_reqHitValidEntry)

  XSPerfAccumulate("NextLinePattern_pc_hit_satEq3_times",s1_reqHitValidEntry&(s1_reqResp.data.sat===3.U) )
  XSPerfAccumulate("NextLinePattern_pc_hit_satEq2_times",s1_reqHitValidEntry&(s1_reqResp.data.sat===2.U) )
  XSPerfAccumulate("NextLinePattern_pc_hit_satEq1_times",s1_reqHitValidEntry&(s1_reqResp.data.sat===1.U) )
  XSPerfAccumulate("NextLinePattern_pc_hit_satEq0_times",s1_reqHitValidEntry&(s1_reqResp.data.sat===0.U) )


}



class NextLinePrefetchIdeal(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val enable = Input(Bool()) //enable NL prefetcher
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val resp = Flipped(DecoupledIO(new PrefetchResp)) //

    val req = DecoupledIO(new PrefetchReq) //Next line prefetcher Request access L2
  })


val validTrain  = io.enable && io.train.fire &&io.train.bits.reqsource === MemReqSource.CPULoadData.id.U  //Only use load requests for training and prediction
val shouldTrain = validTrain && (!io.train.bits.hit || io.train.bits.prefetched)

// Helper function: Address parsing
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

  
  io.train.ready := prefetcherSample.io.train.ready && prefetcherPattern.io.train.ready
  io.resp.ready := true.B  

  // io.train ---> sample.train        
  prefetcherSample.io.train.valid := shouldTrain
  prefetcherSample.io.train.bits.addr := io.train.bits.addr
  prefetcherSample.io.train.bits.pc := io.train.bits.pc.getOrElse(0.U)
  prefetcherSample.io.train.bits.timeSample := timeSampleCounter

  // Sample -> Pattern 
  prefetcherPattern.io.train <> prefetcherSample.io.resp

  //io.train --> pattern.req 
  val shouldQuery = shouldTrain
  prefetcherPattern.io.req.valid := shouldQuery
  prefetcherPattern.io.req.bits.addr := io.train.bits.addr
  prefetcherPattern.io.req.bits.pc := io.train.bits.pc.getOrElse(0.U)

  // ========== io.req ==========
  
  prefetcherPattern.io.resp.ready := !io.enable //dont use io.resp
  val nextAddr = prefetcherPattern.io.resp.bits.nextAddr 
  
  io.req.valid := io.enable && prefetcherPattern.io.resp.valid && 
                  prefetcherPattern.io.resp.bits.needPrefetch 
  
  //fullTagBits=[cacheTagBits,bankBits]
  //set=512，That is, a slice contains 512 rows, with offsetBits = 6 bits and bankBits = 2 bits.
  val (sendingTag, sendingSet, _) = parseFullAddress(nextAddr)
  println(s"[elab] fullAddressBits = $fullAddressBits")

  //cache address is [cacheTag,cacheSet,cacheBank,cacheOffset]
  io.req.bits.tag := sendingTag //sendingTag=[cacheTag,cacheset[cacheBit-1:cacheBit-2]]
  io.req.bits.set := sendingSet //sendingSet=[cacheSet[cacheBit-3:0],cacheBank] 
  io.req.bits.vaddr.foreach(_ := 0.U)  
  io.req.bits.needT := true.B  
  io.req.bits.source := 0.U  
  io.req.bits.pfSource := MemReqSource.Prefetch2L2NL.id.U  

  // ========== performance counter==========
  XSPerfAccumulate("total_train_times", io.enable && io.train.fire)//nl accept req times
  XSPerfAccumulate("store_train_times", io.enable && io.train.fire && io.train.bits.reqsource === MemReqSource.CPUStoreData.id.U)
  XSPerfAccumulate("atomi_train_times", io.enable && io.train.fire && io.train.bits.reqsource === MemReqSource.CPUAtomicData.id.U)
  XSPerfAccumulate("load_miss_times", validTrain & !io.train.bits.hit)
  XSPerfAccumulate("load_hit_prefetched_times",validTrain&  io.train.bits.prefetched)
  XSPerfAccumulate("load_miss_and_hit_prefetched_times",validTrain& !io.train.bits.hit&  io.train.bits.prefetched)

  
  XSPerfAccumulate("transmit_prefetch_req_times",prefetcherPattern.io.resp.valid && prefetcherPattern.io.resp.bits.needPrefetch && 
                  io.enable)
  
  XSPerfAccumulate("timeSampleCount_reset_times",(!timeSampleCounter.orR) & shouldTrain)

}



