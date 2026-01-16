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

//接口定义 
//定义Sample table的entry内部数据结构
class SampleTableEntryField(implicit p: Parameters) extends NLBundle{
    val tag        = UInt(sampleTableTagBits.W)
    val sampleTime = UInt(sampleTableTimeSampleBits.W)
    val pcTag      = UInt(sampleTablePcTagBits.W)
    val touched    = Bool()
    val valid      = Bool() 
}

// Sample Table 写请求封装
class SampleTableWriteReq(implicit p: Parameters) extends NLBundle {
  val en       = Bool()
  val setIdx   = UInt(sampleTableSetBits.W)
  val wayMask  = UInt(nlParams.sampleTableWays.W)//有几路则这个掩码就有几位
  val entry    = new SampleTableEntryField()
}

// Sample Table 替换状态写请求封装
class SampleTableReplaceStateWriteReq(implicit p: Parameters) extends NLBundle {
  val en      = Bool()
  val setIdx  = UInt(sampleTableSetBits.W)
  val wayMask = UInt(1.W)  // 替换状态只有1路
  // PLRU 的状态位宽是 ways-1, 对于 4-way 是 3 bits
  val state   = UInt((nlParams.sampleTableWays-1).W)  // 固定为3位，适用于4路PLRU
}


class SampleTrain(implicit p: Parameters) extends NLBundle {
    val addr       = UInt(vaddrBits.W)
    val pc         = UInt(vaddrBits.W)
    val timeSample = UInt(timeSampleCounterBits.W)
}

/*
```
虚拟地址 (50位):
┌────────────────────┬─────────┬────────┐
│    Sample Tag      │   Set   │ Offset │
│    (53位)          │  (5位)  │ (6位)  │
└────────────────────┴─────────┴────────┘
     ↑                  ↑         ↑
     getSampleTableTag  │         getBlockAddr
                        getSampleTableSet
```

*/
class NextLineSample(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new SampleTrain)) //输入训练数据
    val resp  = DecoupledIO(new PatternTrain) //输出被对本训练数据的响应

  })

  // Sample Table
  val sampleTable= Module(new MultiPortRegFile(
      gen  = new SampleTableEntryField(),
      sets = sampleTableSets,
      ways = nlParams.sampleTableWays,
      numReadPorts = 2,
      numWritePorts = 2,
      shouldReset = true  //要求初始复位，保证valid全0
  ))

  //定义替换算法器
  val sampleTableReplacer = ReplacementPolicy.fromString(sampleTableReplacementPolicy,nlParams.sampleTableWays)
  val sampleTableReplaceStateRegs = Module(new MultiPortRegFile(
      gen = UInt(sampleTableReplacer.nBits.W),
      sets = sampleTableSets,
      ways = 1,
      numReadPorts = 2,
      numWritePorts = 2,
      shouldReset = true  
  ))

  // ==================== 训练逻辑 ====================
  io.train.ready := true.B
  /***************Stage 0: 根据训练地址读取 Sample Table*******************/
  val s0_valid      = io.train.fire  // valid && ready 握手成功
  
  // 先定义基础信号
  val s0_timeSample = io.train.bits.timeSample
  val s0_addr = io.train.bits.addr 
  val s0_pc   = io.train.bits.pc
  
  //当timeSample的[timeSampleRateBits-1,0]为0，即经过一个轮timeSampleRate，并且此时来了一个新采样，则插入表中
  val s0_sampleTableReplaceEn = s0_valid & (!s0_timeSample(timeSampleRateBits-1,0).orR)

  // 解析地址
  val s0_sampleTableSet = getSampleTableSet(s0_addr)
  val s0_sampleTableTag = getSampleTableTag(s0_addr)

  val s0_sampleTableUpdateSet = s0_sampleTableSet -1.U //更新请求访问Table的set
  val s0_sampleTableReplaceSet = s0_sampleTableSet //替换请求访问Table的set

  // 发起 Sample Table 读请求 (读取前一个块和当前块)
  sampleTable.io.r(sampleTableUpdatePort).req.setIdx  := s0_sampleTableUpdateSet // 前一个块
  sampleTable.io.r(sampleTableReplacePort).req.setIdx := s0_sampleTableReplaceSet        // 当前块


  // 发起 Sample Table 的plru替换状态读请求
  sampleTableReplaceStateRegs.io.r(sampleTableUpdatePort).req.setIdx := s0_sampleTableUpdateSet 
  sampleTableReplaceStateRegs.io.r(sampleTableReplacePort).req.setIdx := s0_sampleTableReplaceSet
  
  /***************Stage 1: 处理Sample Table的数据*******************/
  // Stage 1: 处理 Sample Table 读响应
  val s1_valid                 = RegNext(s0_valid, false.B)
  val s1_timeSample            = RegNext(s0_timeSample)
  val s1_addr                  = RegNext(s0_addr)
  val s1_pc                    = RegNext(s0_pc)

  //update
  val s1_sampleTableUpdateEntries = RegNext(sampleTable.io.r(sampleTableUpdatePort).resp.data)  // Vec(ways, SampleTableEntryField)
  val s1_sampleTableUpdataState = RegInit(0.U(sampleTableReplacer.nBits.W))

  //replace
  val s1_sampleTableReplaceEn     = RegNext(s0_sampleTableReplaceEn)
  val s1_sampleTableReplaceEntries = RegNext(sampleTable.io.r(sampleTableReplacePort).resp.data)  // Vec(ways, SampleTableEntryField)
  val s1_sampleTableReplaceState = RegInit(0.U(sampleTableReplacer.nBits.W))


  // 重新解析 Stage 1 的地址
  val (s1_sampleTableSet, s1_sampleTableTag, s1_patternTableTag) = 
    parseTrainData(s1_addr, s1_pc)
  //update
  s1_sampleTableUpdataState     := sampleTableReplaceStateRegs.io.r(sampleTableUpdatePort).resp.data(0)
  
  // UpdateData的 Hit 检测: 检查每个 way 是否命中
  val s1_sampleTableUpdateHitVec     = VecInit(s1_sampleTableUpdateEntries.map(entry => entry.valid && (entry.tag === s1_sampleTableTag)))
  val sampleTableUpdateHit           = s1_sampleTableUpdateHitVec.asUInt.orR  // 任意一个 way 命中则为 true (1000)===>1
  val s1_sampleTableUpdateHitWayOH   = s1_sampleTableUpdateHitVec.asUInt  // One-Hot 编码的命中 way 1000===>8
  val s1_sampleTableUpdateHitWayIdx  = OHToUInt(s1_sampleTableUpdateHitWayOH)  // 命中的 way 索引 (0-3) 8===>3 
  val s1_SampleTableUpdatedHitEntry  = Wire(new SampleTableEntryField)
  val s1_sampleTableUpdataNextState  = sampleTableReplacer.get_next_state(s1_sampleTableUpdataState,s1_sampleTableUpdateHitWayIdx) 
  s1_SampleTableUpdatedHitEntry     := s1_sampleTableUpdateEntries(s1_sampleTableUpdateHitWayIdx)

  //判断是否要更新
  val s1_sampleTableUpdateEn         = Wire(Bool())
  val s1_SampleTableUpdatedEntry     = Wire(new SampleTableEntryField)
  
  val timeSampleDelta  = s1_timeSample -s1_SampleTableUpdatedHitEntry.sampleTime
  val realate = timeSampleDelta < timeSampleMaxDistance.U && timeSampleMinDistance.U < timeSampleDelta
  when(s1_valid & sampleTableUpdateHit & realate ) {//如果命中,并且满足更新条件则更新   
    s1_sampleTableUpdateEn := true.B
    s1_SampleTableUpdatedEntry := s1_SampleTableUpdatedHitEntry
    s1_SampleTableUpdatedEntry.touched := true.B
    
  }.otherwise{//如果不满足则不更新
    s1_sampleTableUpdateEn := false.B
    s1_SampleTableUpdatedEntry := 0.U.asTypeOf(new SampleTableEntryField)
      
  }

  //replace Data 处理 
  s1_sampleTableReplaceState := sampleTableReplaceStateRegs.io.r(sampleTableReplacePort).resp.data(0)

  //使用替换算法计算Victim way id
  val s1_sampleTableReplaceWayIdx = sampleTableReplacer.get_replace_way(s1_sampleTableReplaceState)
  val s1_sampleTableReplaceNextState = sampleTableReplacer.get_next_state(s1_sampleTableReplaceState,s1_sampleTableUpdateHitWayIdx)
  
  //获取Victim way data
  val s1_sampleTableReplaceWayOH = UIntToOH(s1_sampleTableReplaceWayIdx)
  val s1_sampleTableVictimEntry = s1_sampleTableReplaceEntries(s1_sampleTableReplaceWayIdx) 

  //设置插入数据
  val s1_sampleTableReplaceEntry = Wire(new SampleTableEntryField)  
  s1_sampleTableReplaceEntry.valid := true.B
  s1_sampleTableReplaceEntry.tag := s1_sampleTableTag
  s1_sampleTableReplaceEntry.sampleTime := s1_timeSample
  s1_sampleTableReplaceEntry.pcTag := getPatternTableTag(s1_pc)
  s1_sampleTableReplaceEntry.touched := false.B
  

  // 封装 Sample Table 的两个写请求
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

  //封装replace state table 写请求
  val s1_sampleTableUpdateStateReq = Wire(new SampleTableReplaceStateWriteReq())
  s1_sampleTableUpdateStateReq.en := s1_sampleTableUpdateEn
  s1_sampleTableUpdateStateReq.setIdx := s1_sampleTableSet
  s1_sampleTableUpdateStateReq.wayMask := s1_sampleTableUpdateHitWayOH
  s1_sampleTableUpdateStateReq.state := s1_sampleTableUpdataNextState

  val s1_sampleTableReplaceStateReq = Wire(new SampleTableReplaceStateWriteReq())
  s1_sampleTableReplaceStateReq.en := s1_sampleTableReplaceEn
  s1_sampleTableReplaceStateReq.setIdx := s1_sampleTableSet
  s1_sampleTableReplaceStateReq.wayMask := s1_sampleTableReplaceWayOH
  s1_sampleTableReplaceStateReq.state := s1_sampleTableReplaceNextState

  //发送给pattern table的数据
  io.resp.valid        := s1_sampleTableReplaceEn && s1_sampleTableVictimEntry.valid //出现replace，并且victim数据是valid则给pattern传入训练数据
  io.resp.bits.pcTag   := s1_sampleTableVictimEntry.pcTag //将踢出的表项的pcTag传入
  io.resp.bits.touched := s1_sampleTableVictimEntry.touched //将踢出表项的touched传入

    /***************Stage 2: 进行数据写回*******************/
  val s2_valid = RegNext(s1_valid, false.B)

  // 使用 Bundle 进行 Stage 1 -> Stage 2 流水
  val s2_sampleTableUpdateReq = RegNext(s1_sampleTableUpdateReq)
  val s2_sampleTableUpdateStateReq = RegNext(s1_sampleTableUpdateStateReq)
  val s2_sampleTableReplaceReq = RegNext(s1_sampleTableReplaceReq)
  val s2_sampleTableReplaceStateReq = RegNext(s1_sampleTableReplaceStateReq)

  // sampleTable 的更新写回
  sampleTable.io.w(sampleTableUpdatePort).en := s2_valid && s2_sampleTableUpdateReq.en
  sampleTable.io.w(sampleTableUpdatePort).req.setIdx := s2_sampleTableUpdateReq.setIdx
  sampleTable.io.w(sampleTableUpdatePort).req.wayMask := s2_sampleTableUpdateReq.wayMask   
  sampleTable.io.w(sampleTableUpdatePort).req.data := VecInit(Seq.fill(nlParams.sampleTableWays)(s2_sampleTableUpdateReq.entry))

  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).en := s2_valid && s2_sampleTableUpdateStateReq.en
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.setIdx := s2_sampleTableUpdateStateReq.setIdx
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.wayMask := s2_sampleTableUpdateStateReq.wayMask   
  sampleTableReplaceStateRegs.io.w(sampleTableUpdatePort).req.data := VecInit(Seq.fill(1)(s2_sampleTableUpdateStateReq.state))

  // sampleTable 的替换写回
  sampleTable.io.w(sampleTableReplacePort).en := s2_valid && s2_sampleTableReplaceReq.en
  sampleTable.io.w(sampleTableReplacePort).req.setIdx := s2_sampleTableReplaceReq.setIdx
  sampleTable.io.w(sampleTableReplacePort).req.wayMask := s2_sampleTableReplaceReq.wayMask
  sampleTable.io.w(sampleTableReplacePort).req.data := VecInit(Seq.fill(nlParams.sampleTableWays)(s2_sampleTableReplaceReq.entry))

  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).en := s2_valid && s2_sampleTableReplaceStateReq.en
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.setIdx := s2_sampleTableReplaceStateReq.setIdx
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.wayMask := s2_sampleTableReplaceStateReq.wayMask   
  sampleTableReplaceStateRegs.io.w(sampleTableReplacePort).req.data := VecInit(Seq.fill(1)(s2_sampleTableReplaceStateReq.state))

  //
  XSPerfAccumulate("sampleTable_train_times",s0_valid)//sampleTable的训练次数
  
  //replace分析
  XSPerfAccumulate("sampleTable_insert_times", s0_sampleTableReplaceEn)//sampleTable采用到新数据

  //更新分析
  XSPerfAccumulate("sampleTable_update_req_not_hit_times",s1_valid & !sampleTableUpdateHit)
  XSPerfAccumulate("sampleTable_update_req_hit_over_board_times",s1_valid & sampleTableUpdateHit & !realate)
  XSPerfAccumulate("sampleTable_update_times",s1_sampleTableUpdateEn) //sampleTable更新数据

  //sampleTable给patternTable的训练数据和victim data
  XSPerfAccumulate("sampleTable_victim_touched_true_times",s1_sampleTableReplaceEn & s1_sampleTableVictimEntry.touched)
  XSPerfAccumulate("sampleTable_victim_touched_false_times",s1_sampleTableReplaceEn & !s1_sampleTableVictimEntry.touched)

}
