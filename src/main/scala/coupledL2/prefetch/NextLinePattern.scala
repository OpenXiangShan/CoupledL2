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

//接口定义 
class PatternTableEntryField(implicit p: Parameters) extends NLBundle{
     val sat  = UInt(patternTableSatBits.W)
     val valid = Bool()
}

// Pattern Table 训练数据
class PatternTrain(implicit p: Parameters) extends NLBundle {
    val pcTag = UInt(patternTableTagBits.W)
    val touched = Bool()  // samptle的touched字段
}

class PatternReq(implicit p: Parameters) extends NLBundle {
    val pc = UInt(vaddrBits.W)    // 使用虚拟地址位宽
    val addr = UInt(vaddrBits.W)  // 使用虚拟地址位宽
}

//访问pattern table的请求给出的响应
class PatternResp(implicit p: Parameters) extends NLBundle {
    val needPrefetch = Bool()
    val nextAddr = UInt(vaddrBits.W)  // 使用虚拟地址位宽
}

/*
PC (50位):
┌────────────────────────┬────────────┐
│     高位 (忽略)         │ Pattern Tag│
│                        │  (15位)    │
└────────────────────────┴────────────┘
                              ↑
                        getPatternTableTag
*/
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

  // 默认就绪
  io.train.ready := true.B
  io.req.ready := true.B
  
  // ==================== 训练逻辑 ====================
  /***************Stage 0: 读取 Pattern Table*******************/
  val s0_trainValid = io.train.fire //外部有一个训练数据传入
  val s0_trainPcTag = io.train.bits.pcTag
  val s0_trainTouched = io.train.bits.touched
  
  val s0_reqValid = io.req.fire  //外部有一个预取请求传入
  val s0_reqPc    = io.req.bits.pc
  val s0_reqAddr  = io.req.bits.addr
  val s0_reqPcTag = getPatternTableTag(s0_reqPc)
  
  // 对patternTable发起2种读请求
  patternTable.io.r(patternTableTrainPort).req.key := s0_trainPcTag
  patternTable.io.r(patternTablePrefetchPort).req.key := s0_reqPcTag
  

  /***************Stage 1: 处理训练和预取请求*******************/
  val s1_trainValid         = RegNext(s0_trainValid, false.B)
  val s1_trainPcTag         = RegNext(s0_trainPcTag)
  val s1_trainTouched       = RegNext(s0_trainTouched)
  val s1_trainResp          = RegNext(patternTable.io.r(patternTableTrainPort).resp)
  
  val s1_reqValid     = RegNext(s0_reqValid, false.B)
  val s1_reqPc        = RegNext(s0_reqPc)
  val s1_reqAddr      = RegNext(s0_reqAddr)
  val s1_prefetchResp = RegNext(patternTable.io.r(patternTablePrefetchPort).resp)

  //预取读取pattern table的响应
  val s1_prefetchReadIdx = s1_prefetchResp.hitIdx

  // 训练逻辑: 更新或插入表项
  val s1_patternUpdateEn = Wire(Bool())
  val s1_patternUpdateIdx = Wire(UInt(log2Ceil(nlParams.patternTableSets).W))
  
  val s1_patternInsertEn = Wire(Bool())
  val s1_patternInsertIdx = Wire(UInt(log2Ceil(nlParams.patternTableSets).W))

  val s1_patternNewEntry = Wire(new PatternTableEntryField())
  
  //设置数据初始值为：全部flase，值不变
  s1_patternUpdateEn := false.B
  s1_patternInsertEn := false.B
  
  s1_patternUpdateIdx := 0.U
  s1_patternInsertIdx := 0.U

  s1_patternNewEntry := s1_trainResp.data
 
  //更新与插入逻辑
  when(s1_trainValid) {//是否是一个有效的训练数据
    when(s1_trainResp.hit) { //训练数据命中，则是更新路线

      s1_patternUpdateEn := true.B // 使能更新
      s1_patternNewEntry.valid := true.B
      s1_patternUpdateIdx := s1_trainResp.hitIdx

      val currentSat = s1_trainResp.data.sat
      when(s1_trainTouched) {
        // 预取命中，增加饱和计数器（如果未达到最大值）
        s1_patternNewEntry.sat := Mux(currentSat === maxSat, maxSat, currentSat + 1.U)
      }.otherwise {
        // 预取未命中，减少饱和计数器（如果大于0）
        s1_patternNewEntry.sat := Mux(currentSat === 0.U, 0.U, currentSat - 1.U)
      }
      
      
    }.otherwise {// 训练数据未命中，则插入路线
      s1_patternInsertEn := s1_trainTouched //插入使能
     
      s1_patternNewEntry.valid := s1_trainTouched
      s1_patternInsertIdx := patternTableReplacer.get_replace_way(patternTableReplaceState)
      s1_patternNewEntry.sat := 1.U  // 初始置信度为1
    }
  }

  // 预取逻辑: 判断是否需要预取
  val s1_reqHitValidEntry = s1_reqValid && s1_prefetchResp.hit && 
                           s1_prefetchResp.data.valid
  val s1_needPrefetch = s1_reqHitValidEntry & (s1_prefetchResp.data.sat === ((1.U << patternTableSatBits) - 1.U))
  
 

  //更新组相连的plru状态信息
  val readUpdateState  = Mux(s1_reqValid, patternTableReplacer.get_next_state(patternTableReplaceState, s1_prefetchReadIdx), patternTableReplaceState)

  val trainUpdateState = Mux(s1_trainValid, patternTableReplacer.get_next_state(readUpdateState, Mux(s1_trainResp.hit, 
                                                                              s1_trainResp.hitIdx, 
                                                                              s1_patternInsertIdx)), readUpdateState)
  patternTableReplaceState := trainUpdateState
  

  /***************Stage 2: 写回 Pattern Table*******************/
  val s2_patternUpdateEn    = RegNext(s1_patternUpdateEn, false.B)
  val s2_patternUpdateIdx   = RegNext(s1_patternUpdateIdx)
 
  
  val s2_patternInsertEn  = RegNext(s1_patternInsertEn, false.B)
  val s2_patternInsertIdx = RegNext(s1_patternInsertIdx)
  
  val s2_patternNewKey    = RegNext(s1_trainPcTag)
  val s2_patternNewEntry  = RegNext(s1_patternNewEntry)

  // 更新现有表项
  patternTable.io.w(patternTableUpdatePort).en := s2_patternUpdateEn
  patternTable.io.w(patternTableUpdatePort).req.valid := true.B  // 标记为有效
  patternTable.io.w(patternTableUpdatePort).req.key := s2_patternNewKey
  patternTable.io.w(patternTableUpdatePort).req.idx := s2_patternUpdateIdx
  patternTable.io.w(patternTableUpdatePort).req.data := s2_patternNewEntry
  

  // 插入新表项
  patternTable.io.w(patternTableInsertPort).en := s2_patternInsertEn
  patternTable.io.w(patternTableInsertPort).req.valid := true.B  // 标记为有效
  patternTable.io.w(patternTableInsertPort).req.key := s2_patternNewKey
  patternTable.io.w(patternTableInsertPort).req.idx := s2_patternInsertIdx
  patternTable.io.w(patternTableInsertPort).req.data :=  s2_patternNewEntry

  //发起预取请求
  io.resp.valid             := RegNext(s1_needPrefetch, false.B) //设置初始值为false.B
  io.resp.bits.needPrefetch := RegNext(s1_needPrefetch, false.B)
  io.resp.bits.nextAddr     := RegNext(s1_reqAddr + blockBytes.U, 0.U)

  //patternTable训练分析
  XSPerfAccumulate("NextLinePattern_train_times",s1_trainValid) //patternTable收到的多少个训练请求
  //replace分析
  XSPerfAccumulate("NextLinePattern_train_replace_times",s1_patternInsertEn)//训练数据在patternTable中未命中的次数（插入新表项的次数）


  //update分析
  XSPerfAccumulate("NextLinePattern_train_update_times",s1_trainValid && s1_trainResp.hit)//训练数据在patternTable中命中的次数
  XSPerfAccumulate("NextLinePattern_train_data_touched_ture_times",s1_trainValid & s1_trainResp.hit & s1_trainTouched)//发过来的训练数据的tuouched是true的次数
  XSPerfAccumulate("NextLinePattern_train_data_touched_false_times",s1_trainValid & s1_trainResp.hit & !s1_trainTouched)//发过来的训练数据的tuouched是true的次数

 

  //pc预测分析
  XSPerfAccumulate("NextLinePattern_pc_hit_times",s1_prefetchResp.hit && s1_reqValid)//pc预取请求在patternTable中命中的次数
  XSPerfAccumulate("NextLinePattern_pc_hit_validEntry",s1_reqHitValidEntry)//命中有效块的次数

  //命中有效块的时候Sat值的情况
  XSPerfAccumulate("NextLinePattern_pc_hit_satEq3_times",s1_reqHitValidEntry&(s1_prefetchResp.data.sat===3.U) )
  XSPerfAccumulate("NextLinePattern_pc_hit_satEq2_times",s1_reqHitValidEntry&(s1_prefetchResp.data.sat===2.U) )
  XSPerfAccumulate("NextLinePattern_pc_hit_satEq1_times",s1_reqHitValidEntry&(s1_prefetchResp.data.sat===1.U) )
  XSPerfAccumulate("NextLinePattern_pc_hit_satEq0_times",s1_reqHitValidEntry&(s1_prefetchResp.data.sat===0.U) )


}
