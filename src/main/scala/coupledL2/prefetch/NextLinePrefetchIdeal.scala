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



