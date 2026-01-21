package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, MemReqSource}
import coupledL2.utils.{MultiPortRegFile,FullyAssociativeMemory,BankedSRAM,OverwriteQueue}
import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.ReplacementPolicy
import huancun.{TPmetaReq, TPmetaResp}
import utility.{sram, XSPerfAccumulate}
import freechips.rocketchip.util.SeqToAugmentedSeq
import freechips.rocketchip.rocket.CSR.X

//define interface
class PatternTableEntryField(implicit p: Parameters) extends NLBundle{
     val sat  = UInt(patternTableSatBits.W)
     val valid = Bool()
}

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
