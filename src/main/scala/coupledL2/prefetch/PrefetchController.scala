package coupledL2.prefetch

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import utility.mbist.MbistPipeline
import coupledL2._

class PrefetchReplaceDemandBundle()(implicit p: Parameters) extends L2Bundle {
  val demandAddr = UInt(fullAddressBits.W)
  val pfReqSrc = UInt(MemReqSource.reqSourceBits.W)
}

class DemandRefillBundle()(implicit p: Parameters) extends L2Bundle {
  val isDemand = Bool()
  val isPrefetch = Bool()
  val addr = UInt(fullAddressBits.W)
  val latency = UInt(timestampBits.W)
}

class L2PfFeedbackCtrl(implicit p: Parameters) extends PrefetchBundle {
  val streamDegree = UInt(degreeBits.W)
  val strideDegree = UInt(degreeBits.W)
  val bertiDegree = UInt(degreeBits.W)
  val smsDegree = UInt(degreeBits.W)
  val vbopDegree = UInt(degreeBits.W)
  val pbopDegree = UInt(degreeBits.W)
  val tpDegree = UInt(degreeBits.W)
}

class PrefetchFeedbackBundle(implicit p: Parameters) extends PrefetchBundle {
  val pfReplaceDemand = Valid(new PrefetchReplaceDemandBundle())
  val dataRefill = Valid(new DemandRefillBundle()) // for pollution hit and latency update
  val dirResult = Valid(new DirResult()) // for cache hit
  val pfStatInMSHR = new PfStatInMSHRBundle() // for mshr hit
}
class PrefetchControllerIO(implicit p: Parameters) extends PrefetchBundle {
  // FIXME lyq: epoch 方式：isDemand, isDemandTrain(miss/pfhit)
  val isDemandTrain = Input(Bool())
  val pfFeedbackVec = Input(Vec(banks, new PrefetchFeedbackBundle()))
  val l2PfFbCtrl = Output(new L2PfFeedbackCtrl)
}

class PrefetchController(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchControllerIO)

  // prefetch number
  private val PF_STREAM = 0
  private val PF_STRIDE = 1
  private val PF_BERTI  = 2
  private val PF_SMS    = 3
  private val PF_VBOP   = 4
  private val PF_PBOP   = 5
  private val PF_TP     = 6
  private val PF_NUM    = 7

  private def pfIdxFromReqSource(src: UInt): UInt = {
    MuxLookup(src, PF_NUM.U)(Seq(
      MemReqSource.Prefetch2L2Stream.id.U -> PF_STREAM.U,
      MemReqSource.Prefetch2L2Stride.id.U -> PF_STRIDE.U,
      MemReqSource.Prefetch2L2Berti.id.U  -> PF_BERTI.U,
      MemReqSource.Prefetch2L2SMS.id.U    -> PF_SMS.U,
      MemReqSource.Prefetch2L2BOP.id.U    -> PF_VBOP.U,
      MemReqSource.Prefetch2L2PBOP.id.U   -> PF_PBOP.U,
      MemReqSource.Prefetch2L2TP.id.U     -> PF_TP.U
    ))
  }

  private def pfIdxFromPfSource(src: UInt): UInt = {
    MuxLookup(src, PF_NUM.U)(Seq(
      PfSource.Stream.id.U -> PF_STREAM.U,
      PfSource.Stride.id.U -> PF_STRIDE.U,
      PfSource.Berti.id.U  -> PF_BERTI.U,
      PfSource.SMS.id.U    -> PF_SMS.U,
      PfSource.BOP.id.U    -> PF_VBOP.U,
      PfSource.PBOP.id.U   -> PF_PBOP.U,
      PfSource.TP.id.U     -> PF_TP.U
    ))
  }

  // latency calculation
  def avgLatency(old: UInt, newLatency: UInt): UInt = {
    // moving average with alpha = 0.5
    Mux(old === 0.U, newLatency, (old + newLatency) >> 1)
  }

  def _signedExtend(x: UInt, width: Int): SInt = {
    if (x.getWidth >= width) {
      x.asSInt
    } else {
      Cat(Fill(width - x.getWidth, x.head(1)), x).asSInt
    }
  }
  def _zeroExtend(x: UInt, width: Int): SInt = {
    if (x.getWidth >= width) {
      x.asSInt
    } else {
      Cat(Fill(width - x.getWidth, 0.U(1.W)), x).asSInt
    }
  }

  // PHT: Pollition Holding Table
  private val PHT_ENTRIES = 256
  private val PHT_TAG_BITS = 6
  private val PHT_INDEX_BITS = log2Ceil(PHT_ENTRIES)
  private val PHT_OFFSET_BITS = offsetBits + bankBits
  private def phtTagSrcWidth = fullAddressBits - PHT_OFFSET_BITS - PHT_INDEX_BITS
  private def phtTagPadWidth = ((phtTagSrcWidth + PHT_TAG_BITS - 1) / PHT_TAG_BITS) * PHT_TAG_BITS
  private def phtIndexOf(addr: UInt): UInt = {
    val blkAddr = addr(fullAddressBits - 1, PHT_OFFSET_BITS)
    blkAddr(PHT_INDEX_BITS - 1, 0)
  }
  private def phtTagOf(addr: UInt): UInt = {
    val src = addr(fullAddressBits - 1, PHT_OFFSET_BITS + PHT_INDEX_BITS)
    val srcPad = if (phtTagPadWidth == phtTagSrcWidth) src else Cat(0.U((phtTagPadWidth - phtTagSrcWidth).W), src)
    (0 until (phtTagPadWidth / PHT_TAG_BITS)).map { i =>
      srcPad((i + 1) * PHT_TAG_BITS - 1, i * PHT_TAG_BITS)
    }.reduce(_ ^ _)
  }

  // ========== structure and io ==========
  val peVec = RegInit(VecInit(Seq.fill(PF_NUM)(0.S(peBits.W))))
  val latencyAvg = RegInit(0.U(timestampBits.W))
  val phtValid = RegInit(VecInit(Seq.fill(banks)(VecInit(Seq.fill(PHT_ENTRIES)(false.B)))))
  val phtTag = RegInit(VecInit(Seq.fill(banks)(VecInit(Seq.fill(PHT_ENTRIES)(0.U(PHT_TAG_BITS.W))))))
  val phtPfId = RegInit(VecInit(Seq.fill(banks)(VecInit(Seq.fill(PHT_ENTRIES)(0.U(log2Ceil(PF_NUM).W))))))
  // TODO lyq: 一些信息感觉不需要分 bank 存储？因为地址是不会重合的？

  // io alias
  val pfReplaceDemand = io.pfFeedbackVec.map(x => x.pfReplaceDemand)
  val dataRefill = io.pfFeedbackVec.map(x => x.dataRefill)
  val dirResult = io.pfFeedbackVec.map(x => x.dirResult)
  val pfStatInMSHR = io.pfFeedbackVec.map(x => x.pfStatInMSHR)

  // ========== latency update ==========
  // TODO lyq: timing
  latencyAvg := dataRefill.map(x => 
    Mux(x.valid, avgLatency(latencyAvg, x.bits.latency), latencyAvg)
  ).reduce(_ + _) >> bankBits

  // ========== pht lookup and update ==========
  val p0_phtHitVec = Wire(Vec(banks, Bool()))
  val p0_phtHitLatencyVec = Wire(Vec(banks, UInt(timestampBits.W)))
  val p0_phtHitPfIdx = Wire(Vec(banks, UInt(log2Ceil(PF_NUM).W)))
  val p1_phtHitVec = RegNext(p0_phtHitVec)
  val p1_phtHitLatencyVec = Wire(Vec(banks, UInt(timestampBits.W)))
  val p1_phtHitPfIdx = Wire(Vec(banks, UInt(log2Ceil(PF_NUM).W)))

  for (s <- 0 until banks) {
    val rValid = dataRefill(s).valid && dataRefill(s).bits.isDemand
    val rIdx = phtIndexOf(dataRefill(s).bits.addr)
    val rTag = phtTagOf(dataRefill(s).bits.addr)

    p0_phtHitVec(s) := rValid && phtValid(s)(rIdx) && phtTag(s)(rIdx) === rTag
    p0_phtHitLatencyVec(s) := dataRefill(s).bits.latency
    p0_phtHitPfIdx(s) := phtPfId(s)(rIdx)
    
    val p1_rIdx = RegEnable(rIdx, p0_phtHitVec(s))
    p1_phtHitLatencyVec(s) := RegEnable(dataRefill(s).bits.latency, p0_phtHitVec(s))
    p1_phtHitPfIdx(s) := RegEnable(phtPfId(s)(rIdx), p0_phtHitVec(s))

    val wValid = pfReplaceDemand(s).valid
    val wIdx = phtIndexOf(pfReplaceDemand(s).bits.demandAddr)
    val wTag = phtTagOf(pfReplaceDemand(s).bits.demandAddr)
    val wPfidx = pfIdxFromReqSource(pfReplaceDemand(s).bits.pfReqSrc)

    when (wValid) {
      phtValid(s)(wIdx) := true.B
      phtTag(s)(wIdx) := wTag
      phtPfId(s)(wIdx) := wPfidx
    }.elsewhen (p1_phtHitVec(s)) {
      phtValid(s)(p1_rIdx) := false.B
    }
  }

  // ========== PE calculation ==========
  def isHitPfInCache(r: ValidIO[DirResult], i: Int): Bool = {
    r.valid && r.bits.replacerInfo.channel === 1.U && 
      MemReqSource.isCPUReq(r.bits.replacerInfo.reqSource) && r.bits.hit &&
      r.bits.meta.prefetch.getOrElse(false.B) && 
      pfIdxFromPfSource(r.bits.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U)) === i.U
  }

  for (i <- 0 until PF_NUM) {
    val peDeltaSliceVec = RegInit(VecInit(Seq.fill(banks)(0.S(peBits.W))))

    // pe0: get the pe of each slice
    for (s <- 0 until banks) {
      val mshrHitDelta = Mux(
        pfStatInMSHR(s).hitPf && pfIdxFromReqSource(pfStatInMSHR(s).hitPfReqSrc) === i.U, 
        _zeroExtend(pfStatInMSHR(s).hitPfLatency, peBits), 
        0.S(peBits.W)
      )
      val cacheHitDelta =
        Mux(isHitPfInCache(dirResult(s), i), _zeroExtend(latencyAvg, peBits), 0.S(peBits.W))
      val phtDelta = 
        Mux(p1_phtHitVec(s) && p1_phtHitPfIdx(s) === i.U, _zeroExtend(p1_phtHitLatencyVec(s), peBits), 0.S(peBits.W))
      peDeltaSliceVec(s) := mshrHitDelta + cacheHitDelta - phtDelta
    }
    
    // pe1: get the pe sum of all slices
    peVec(i) := peVec(i) + peDeltaSliceVec.reduce(_ + _)
  }

  // ========== control engine ==========
  private val epochEdge = 256
  private val epochBits = log2Ceil(epochEdge)
  val activeVec = RegInit(VecInit(Seq.fill(PF_NUM)(true.B)))
  val aggressiveVec = RegInit(VecInit(Seq.fill(PF_NUM)(1.U(degreeBits.W))))
  val demandCnt = RegInit(0.U(epochBits.W))

  private def pfDegree(idx: Int): UInt = Mux(activeVec(idx), aggressiveVec(idx), 0.U)

  val epochEnd = io.isDemandTrain && demandCnt === (epochEdge - 1).U
  when (epochEnd) {
    demandCnt := 0.U
  }.elsewhen(io.isDemandTrain) {
    demandCnt := demandCnt + 1.U
  }

  when (epochEnd) {
    for (i <- 0 until PF_NUM) {
      val peEval = peVec(i)
      when (activeVec(i)) {
        when (peEval > 0.S) {
          aggressiveVec(i) := Mux(aggressiveVec(i) === 3.U, 3.U, aggressiveVec(i) + 1.U)
        }.elsewhen (peEval < 0.S) {
          when (aggressiveVec(i) > 0.U) {
            aggressiveVec(i) := aggressiveVec(i) - 1.U
          }.otherwise {
            activeVec(i) := false.B
            aggressiveVec(i) := 3.U
          }
        }
      }.otherwise {
        aggressiveVec(i) := Mux(aggressiveVec(i) === 0.U, 0.U, aggressiveVec(i) - 1.U)
        when (aggressiveVec(i) === 0.U) {
          activeVec(i) := true.B
          aggressiveVec(i) := 1.U
        }
      }
      peVec(i) := 0.S
    }
  }

  io.l2PfFbCtrl.streamDegree := pfDegree(PF_STREAM)
  io.l2PfFbCtrl.strideDegree := pfDegree(PF_STRIDE)
  io.l2PfFbCtrl.bertiDegree := pfDegree(PF_BERTI)
  io.l2PfFbCtrl.smsDegree := pfDegree(PF_SMS)
  io.l2PfFbCtrl.vbopDegree := pfDegree(PF_VBOP)
  io.l2PfFbCtrl.pbopDegree := pfDegree(PF_PBOP)
  io.l2PfFbCtrl.tpDegree := pfDegree(PF_TP)

  XSPerfAccumulate("epochCount", epochEnd)
  XSPerfAccumulate("pfReplaceDemand", PopCount(pfReplaceDemand.map(x => x.valid)))
  XSPerfAccumulate("dataRefill", PopCount(dataRefill.map(x => x.valid)))
  XSPerfAccumulate("parttenDefined", PopCount((0 until PF_NUM).map(i => !(activeVec(i) && aggressiveVec(i) =/= 1.U))))

}
