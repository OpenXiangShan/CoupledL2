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


class SampleTrain(implicit p: Parameters) extends NLBundle {
    val addr       = UInt(vaddrBits.W)
    val pc         = UInt(vaddrBits.W)
    val timeSample = UInt(timeSampleCounterBits.W)
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
