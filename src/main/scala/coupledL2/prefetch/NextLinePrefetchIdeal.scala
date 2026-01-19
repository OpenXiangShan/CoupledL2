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
    tablePcTagBits: Int = 15, //data field in block
    timeSampleCounterBits:Int = 14, //采样计数器位宽
    timeSampleRate : Int = 256, //采样率
    timeSampleMinDistance :Int = 4, //采样最小距离

    //zzq Sample Table config 
    //use 4 way，then there are 8 blocks in way 
    sampleTableWays: Int = 4, 
    // sampleTableSets: Int = 32,

    //zzq offset bits in a block
    //sample entry  tag bits ,it used to distinguish with other entry
  
    sampleTableTouchedBits: Int = 1,
    sampleTableSampleTimeBits: Int =14,
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
  override val hasPrefetchBit: Boolean = true //表示
  override val hasPrefetchSrc: Boolean = true //表示预取器知道预取请求的来源
  override val inflightEntries: Int = 16 // changed in sv48
}

//define Next-Line Prefetcher usefull parameters
trait HasNLParams extends HasCoupledL2Parameters {
  val nlParams = prefetchers.find {
    case p: NLParameters => true 
    case _ => false
  }.get.asInstanceOf[NLParameters]

  def vaddrBits = fullVAddrBits
  def timeSampleCounterBits = nlParams.timeSampleCounterBits
  def timeSampleRateBits    = log2Ceil(nlParams.timeSampleRate)
  // def tablePcTagBits        = vaddrBits - log2Ceil(nlParams.patternTableSets) - offsetBits //sampleTable的PcTag字段和patternTable的tag字段是一样的50-6-6=38bit
  def tablePcTagBits        = vaddrBits  //sampleTable的PcTag字段和patternTable的tag字段是一样的50bit
  def timeSampleMinDistance = nlParams.timeSampleMinDistance
  //block = set=512*way=8=4096 bytes
  //blockBytes = 64 bytes 64*4096/2=2^10*2^7
  def timeSampleMaxDistance = blocks * nlParams.L2SliceNum / 2 //计算出L2cache是多少个字节，然后除2就是max的值了,这个数
  //
  def sampleTableBlocks     = timeSampleMaxDistance / nlParams.timeSampleRate //8192/256=32个采样点
  def sampleTableSets      = sampleTableBlocks / nlParams.sampleTableWays //32/4=8个set
  def sampleTableSetBits   = log2Ceil(sampleTableSets)
  def sampleTableWaysBits  = log2Ceil(nlParams.sampleTableWays)
  def sampleTableTagBits   = vaddrBits - sampleTableSetBits - offsetBits // offsetBits=6bit，setBits=3bit, tag=50-3-6=41
  def sampleTablePcTagBits = tablePcTagBits
  def sampleTableTimeSampleBits = nlParams.timeSampleCounterBits

  // pattern 
  def patternTableSetBits  = log2Ceil(nlParams.patternTableSets)
  def patternTableWaysBits = log2Ceil(nlParams.patternTableWays)
  
  def patternTableTagBits  = tablePcTagBits
  def patternTableSatBits  = nlParams.patternTableSatBits
  def maxSat = (1.U << patternTableSatBits) - 1.U //饱和计数器最大值

  //
  def sampleTableReplacementPolicy = nlParams.sampleTableReplacementPolicy
  def patternTableReplacementPolicy = nlParams.patternTableReplacementPolicy

  def sampleTableUpdatePort     = nlParams.sampleTableUpdatePort
  def sampleTableReplacePort    = nlParams.sampleTableReplacePort
  def patternTableTrainPort     = nlParams.patternTableTrainPort
  def patternTablePrefetchPort  = nlParams.patternTablePrefetchPort
  
  def patternTableUpdatePort  = nlParams.patternTableUpdatePort
  def patternTableInsertPort  = nlParams.patternTableInsertPort
  
 
}

abstract class NLBundle(implicit val p: Parameters) extends Bundle with HasNLParams

abstract class NLModule(implicit val p: Parameters) extends Module with HasNLParams {
  
  // ==================== 辅助函数: 地址解析 ====================
  
  /** 获取块地址 (去除块内偏移)
    * @param addr 完整地址|tag|set|Cacheoffset|
    * @return 块地址 (右移 offsetBits)|tag+set|
    */
  def getBlockAddr(addr: UInt): UInt = {
    addr >> offsetBits //这个offsetBit是L2cache的块大小决定的
  }
  
  /** 从地址中提取 Sample Table 的 set 索引
    * @param addr 完整地址|tag|set|
    * @return Sample Table 的 set 索引|set|
    */
  def getSampleTableSet(addr: UInt): UInt = {
    val blockAddr = getBlockAddr(addr)
    blockAddr(sampleTableSetBits - 1, 0)
  }
  
  /** 从地址中提取 Sample Table 的 tag
    * @param addr 完整地址|tag|SampleSet|Cacheoffset|
    * @return Sample Table 的 tag |tag|
    * blockAddr 位宽 = vaddrBits - offsetBits
    * tag = blockAddr 的高位部分（去除 set 位）
    */
  def getSampleTableTag(addr: UInt): UInt = {
    val blockAddr = getBlockAddr(addr)
    // blockAddr 的位宽是 (vaddrBits - offsetBits)
    // tag 从 blockAddr 的最高位开始，去掉低 sampleTableSetBits 位
    blockAddr(vaddrBits - offsetBits - 1, sampleTableSetBits)
  }
  
  
  /** 从 PC 中提取 Pattern Table 的 tag
    * @param pc 程序计数器地址
    * @return Pattern Table 的 tag (使用 PC 的低位)
    * blockAddr 位宽 = vaddrBits - offsetBits
    * tag = blockAddr 的高位部分（去除 set 位）
    */
  def getPatternTableTag(addr: UInt): UInt = {
    // val blockAddr = getBlockAddr(addr)
    // // blockAddr 的位宽是 (vaddrBits - offsetBits)
    // // tag 从 blockAddr 的最高位开始，去掉低 patternTableSetBits 位
    // blockAddr(vaddrBits - offsetBits - 1, patternTableSetBits)

    addr
  }
  
  /** 综合解析: 从地址和 PC 中提取所有需要的字段
    * @param addr 访问地址
    * @param pc 程序计数器
    * @return (sampleSet, sampleTag, patternSet, patternTag)
    */
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


class NextLinePrefetchIdeal(implicit p: Parameters) extends NLModule {
  val io = IO(new Bundle() {
    val enable = Input(Bool()) //enable NL prefetcher
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val resp = Flipped(DecoupledIO(new PrefetchResp)) //

    val req = DecoupledIO(new PrefetchReq) //Next line prefetcher Request access L2
  })


val validTrain  = io.enable && io.train.fire &&io.train.bits.reqsource === MemReqSource.CPULoadData.id.U  //只使用时load请求进行训练和预测
val shouldTrain = validTrain && (!io.train.bits.hit || io.train.bits.prefetched)

// 采样计数器(用于采样训练)
val timeSampleCounter = RegInit(0.U(timeSampleCounterBits.W))
//更新timeSampleCounter的值
  when(shouldTrain) {
    when(timeSampleCounter === ((1.U << timeSampleCounterBits) - 1.U)) {
      timeSampleCounter := 0.U  // 溢出时归零
    }.otherwise {
      timeSampleCounter := timeSampleCounter + 1.U
    }
  }

  val prefetcherSample = Module(new NextLineSample())
  val prefetcherPattern = Module(new NextLinePattern())

  // ========== 修复 1: ready 只连接 Sample ==========
  io.train.ready := prefetcherSample.io.train.ready && prefetcherPattern.io.train.ready
  
  // ========== 修复: io.resp 未使用,设置默认值 ==========
  io.resp.ready := true.B  // 总是准备好接收响应

  // ========== 修复 2: 训练流程 ==========
  // Sample 训练,是下面两种情况之一，则更新 Sample Table
  // 1. miss 时
  // 2. hit，且是hit预取的，
  
  
                  
  prefetcherSample.io.train.valid := shouldTrain
  prefetcherSample.io.train.bits.addr := io.train.bits.addr
  prefetcherSample.io.train.bits.pc := io.train.bits.pc.getOrElse(0.U)
  prefetcherSample.io.train.bits.timeSample := timeSampleCounter

  // ========== 修复 3: Sample -> Pattern 训练通道 ==========
  prefetcherPattern.io.train <> prefetcherSample.io.resp

  // ========== 修复 4: Pattern 查询用于预取决策 ==========
  // 所有访问（hit 和 miss）都查询 Pattern Table 来决定是否预取
  val shouldQuery = shouldTrain
  prefetcherPattern.io.req.valid := shouldQuery
  prefetcherPattern.io.req.bits.addr := io.train.bits.addr
  prefetcherPattern.io.req.bits.pc := io.train.bits.pc.getOrElse(0.U)

  // ========== 发起 预取请求 ==========
  
  prefetcherPattern.io.resp.ready := !io.enable //这个传入的resp信号我不使用，所以设置一直接收
  val nextAddr = prefetcherPattern.io.resp.bits.nextAddr // 注意: 如果 nextAddr 是虚拟地址，会有问题！
  
  io.req.valid := io.enable && prefetcherPattern.io.resp.valid && 
                  prefetcherPattern.io.resp.bits.needPrefetch 
  
  //这里使用fullTagBits=[cacheTagBits,bankBits]
  //set=512，即一个slice里面有512行，offsetBits=6bit, bankBits=2bit
  val (sendingTag, sendingSet, _) = parseFullAddress(nextAddr)
  // print the compile-time parameter during elaboration (Scala side)
  println(s"[elab] fullAddressBits = $fullAddressBits")

  //cache地址是[cacheTag,cacheSet,cacheBank,cacheOffset]
  io.req.bits.tag := sendingTag //sendingTag=[cacheTag,cacheset[cacheBit-1:cacheBit-2]]
  io.req.bits.set := sendingSet //sendingSet=[cacheSet[cacheBit-3:0],cacheBank] ,经过SinkA会将这些重组成正确的
  io.req.bits.vaddr.foreach(_ := 0.U)  // NL 预取器不使用 vaddr
  io.req.bits.needT := true.B  // 预取默认不需要独占权限
  io.req.bits.source := 0.U  // 由 L2 分配
  io.req.bits.pfSource := MemReqSource.Prefetch2L2NL.id.U  

  //训练数据分析
  XSPerfAccumulate("total_train_times", io.enable && io.train.fire)//nl接收到req的总次数
  XSPerfAccumulate("load_miss_times", validTrain & !io.train.bits.hit)//nl接收到req是load miss的次数
  XSPerfAccumulate("load_hit_prefetched_times",validTrain&  io.train.bits.prefetched)//nl接收到的req里面load在cache命中预取器的次数
  
  //预取分析
  XSPerfAccumulate("transmit_prefetch_req_times",prefetcherPattern.io.resp.valid && prefetcherPattern.io.resp.bits.needPrefetch && 
                  io.enable)//nl发起的预取请求个数
  //其他分析
  XSPerfAccumulate("timeSampleCount_reset_times",(!timeSampleCounter.orR) & shouldTrain)

}



