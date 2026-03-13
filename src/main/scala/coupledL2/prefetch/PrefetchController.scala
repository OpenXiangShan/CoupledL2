package coupledL2.prefetch

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import utility.mbist.MbistPipeline
import coupledL2._

class ReplaceBundle()(implicit p: Parameters) extends L2Bundle {
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
  val victimPAddr = UInt(fullAddressBits.W)
  val victimPfSource = UInt(PfSource.pfSourceBits.W)
}

class DemandRefillBundle()(implicit p: Parameters) extends L2Bundle {
  val isDemand = Bool()
  val isPrefetch = Bool()
  val pfReqSrc = UInt(MemReqSource.reqSourceBits.W)
  val addr = UInt(fullAddressBits.W)
  val latency = UInt(timestampBits.W)
}

class BusContentionBundle()(implicit p: Parameters) extends L2Bundle {
  val delayHit = Bool()
  val busHit = Bool()
  val bankHit = Bool()
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
  val replaceRecord = Valid(new ReplaceBundle())
  val dataRefill = Valid(new DemandRefillBundle()) // for pollution hit and latency update
  val dirResult = Valid(new DirResult()) // for cache hit
  val pfStatInMSHR = new PfStatInMSHRBundle() // for mshr hit
  val busContention = Valid(new BusContentionBundle())
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
  private val PF_NAME_VEC = Seq("Stream", "Stride", "Berti", "SMS", "VBOP", "PBOP", "TP")

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
    // if newLatency > 500.U, it may occur ddr flush, causing to long and instable latency
    Mux(old === 0.U, newLatency, Mux(newLatency > 500.U, old, old + newLatency) >> 1)
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
  val epochID = RegInit(0.U(64.W))
  val peVec = RegInit(VecInit(Seq.fill(PF_NUM)(0.S(peBits.W))))
  val latencyAvg = RegInit(0.U(timestampBits.W))
  val latencyLastEpoch = RegInit(0.U(timestampBits.W))
  val phtValid = RegInit(VecInit(Seq.fill(banks)(VecInit(Seq.fill(PHT_ENTRIES)(false.B)))))
  val phtTag = RegInit(VecInit(Seq.fill(banks)(VecInit(Seq.fill(PHT_ENTRIES)(0.U(PHT_TAG_BITS.W))))))
  val phtPfId = RegInit(VecInit(Seq.fill(banks)(VecInit(Seq.fill(PHT_ENTRIES)(0.U(log2Ceil(PF_NUM).W))))))
  // TODO lyq: 一些信息感觉不需要分 bank 存储？因为地址是不会重合的？

  // io alias
  val replaceRecord = io.pfFeedbackVec.map(x => x.replaceRecord)
  val dataRefill = io.pfFeedbackVec.map(x => x.dataRefill)
  val dirResult = io.pfFeedbackVec.map(x => x.dirResult)
  val pfStatInMSHR = io.pfFeedbackVec.map(x => x.pfStatInMSHR)
  val busContention = io.pfFeedbackVec.map(x => x.busContention)
  val pfReplaceDemandValid = replaceRecord.map(x =>
    x.valid && MemReqSource.isL2Prefetch(x.bits.reqSource) && x.bits.victimPfSource === PfSource.NoWhere.id.U
  )

  // ========== latency update ==========
  // TODO lyq: timing
  latencyAvg := dataRefill.map(x => 
    Mux(x.valid, avgLatency(latencyAvg, x.bits.latency), latencyAvg)
  ).reduce(_ + _) >> bankBits

  // record for debug //
  val refillRecordTable = ChiselDB.createTable("RefillRecordTable", new DemandRefillBundle, basicDB = true)
  dataRefill.foreach { r =>
    refillRecordTable.log(
      data = r.bits,
      en = r.valid,
      site = "L2PrefetchController",
      clock, reset
    )
  }

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

    val wValid = pfReplaceDemandValid(s)
    val wIdx = phtIndexOf(replaceRecord(s).bits.victimPAddr)
    val wTag = phtTagOf(replaceRecord(s).bits.victimPAddr)
    val wPfidx = pfIdxFromReqSource(replaceRecord(s).bits.reqSource)

    when (wValid) {
      phtValid(s)(wIdx) := true.B
      phtTag(s)(wIdx) := wTag
      phtPfId(s)(wIdx) := wPfidx
    }.elsewhen (p1_phtHitVec(s)) {
      phtValid(s)(p1_rIdx) := false.B
    }
  }

  // ========== PE calculation ==========
  // at least ratio=1/MultipeLog useful
  private val prefetchYieldMultipeLog = log2Ceil(8)
  private val l1PrefetchCacheHitMultipeLog = log2Ceil(4)

  def isPfLateInCache(r: ValidIO[DirResult], i: Int): Bool = {
    r.valid && r.bits.replacerInfo.channel === 1.U &&
      r.bits.hit && pfIdxFromReqSource(r.bits.replacerInfo.reqSource) === i.U
  }

  def isHitPfInCache(r: ValidIO[DirResult], i: Int): Bool = {
    r.valid && r.bits.replacerInfo.channel === 1.U &&
      r.bits.hit && r.bits.meta.prefetch.getOrElse(false.B) && 
      pfIdxFromPfSource(r.bits.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U)) === i.U
  }
  
  def isDemandHitPfInCache(r: ValidIO[DirResult], i: Int): Bool = {
    isHitPfInCache(r, i) && MemReqSource.isCPUReq(r.bits.replacerInfo.reqSource)
  }

  def isL1PrefetchHitPfInCache(r: ValidIO[DirResult], i: Int): Bool = {
    isHitPfInCache(r, i) && MemReqSource.isL1Prefetch(r.bits.replacerInfo.reqSource)
  }

  // if timing is bad, there can add pipelines arbitrarily.
  val statPeOverflowVec = WireInit(VecInit(Seq.fill(PF_NUM)(false.B)))

  val statMshrHitVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statDemandCacheHitVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statL1PrefetchCacheHitVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statPollutionHoldVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statPfMshrHoldVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statPfReqBufferHoldVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statPfLateInMshrVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statPfLateInCacheVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statPfUselessVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statTnocVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statTbusVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))
  val statTbankVec = Wire(Vec(PF_NUM, Vec(banks, Bool())))

  val deltaMshrHitVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaDemandCacheHitVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaL1PrefetchCacheHitVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaPollutionHoldVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaPfMshrHoldVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaPfReqBufferHoldVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaPfLateInMshrVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaPfLateInCacheVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaPfUselessVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaTnocVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaTbusVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))
  val deltaTbankVec = Wire(Vec(PF_NUM, Vec(banks, SInt(peBits.W))))

  for (i <- 0 until PF_NUM) {
    val peDeltaSliceVec = RegInit(VecInit(Seq.fill(banks)(0.S(peBits.W))))

    // pe0: get the pe of each slice
    for (s <- 0 until banks) {
      statMshrHitVec(i)(s) := pfStatInMSHR(s).hitPf &&
        pfIdxFromReqSource(pfStatInMSHR(s).hitPfReqSrc) === i.U
      statDemandCacheHitVec(i)(s) := isDemandHitPfInCache(dirResult(s), i)
      statL1PrefetchCacheHitVec(i)(s) := isL1PrefetchHitPfInCache(dirResult(s), i)
      statPollutionHoldVec(i)(s) := p1_phtHitVec(s) && p1_phtHitPfIdx(s) === i.U
      statPfMshrHoldVec(i)(s) := dataRefill(s).valid && dataRefill(s).bits.isPrefetch &&
        pfIdxFromReqSource(dataRefill(s).bits.pfReqSrc) === i.U
      statPfReqBufferHoldVec(i)(s) := pfStatInMSHR(s).pfReleaseFromReqBuffer && 
        pfIdxFromReqSource(pfStatInMSHR(s).reqBufferPfReqSrc) === i.U
      statPfLateInMshrVec(i)(s) := pfStatInMSHR(s).pfLate &&
        pfIdxFromReqSource(pfStatInMSHR(s).pfLateReqSrc) === i.U
      statPfLateInCacheVec(i)(s) := isPfLateInCache(dirResult(s), i)
      statPfUselessVec(i)(s) := replaceRecord(s).valid &&
        pfIdxFromPfSource(replaceRecord(s).bits.victimPfSource) === i.U
      statTnocVec(i)(s) := busContention(s).valid && busContention(s).bits.delayHit
      statTbusVec(i)(s) := busContention(s).valid && busContention(s).bits.busHit
      statTbankVec(i)(s) := busContention(s).valid && busContention(s).bits.bankHit

      deltaMshrHitVec(i)(s) := Mux(
        statMshrHitVec(i)(s),
        _zeroExtend(pfStatInMSHR(s).hitPfLatency, peBits), 
        0.S(peBits.W)
      )
      deltaDemandCacheHitVec(i)(s) := Mux(
        statDemandCacheHitVec(i)(s),
        _zeroExtend(latencyAvg, peBits),
        0.S(peBits.W)
      )
      deltaL1PrefetchCacheHitVec(i)(s) := Mux(
        statL1PrefetchCacheHitVec(i)(s),
        _zeroExtend(latencyAvg >> l1PrefetchCacheHitMultipeLog, peBits),
        0.S(peBits.W)
      )
      deltaPollutionHoldVec(i)(s) := Mux(
        statPollutionHoldVec(i)(s),
        // _zeroExtend(p1_phtHitLatencyVec(s), peBits),
        _zeroExtend(latencyAvg, peBits), // to avoid long latency because of ddr flush 
        0.S(peBits.W)
      )
      deltaPfMshrHoldVec(i)(s) := Mux(
        statPfMshrHoldVec(i)(s),
        // _zeroExtend(dataRefill(s).bits.latency, peBits) >> prefetchYieldMultipeLog,
        _zeroExtend(latencyAvg >> prefetchYieldMultipeLog, peBits), // to avoid long latency because of ddr flush 
        0.S(peBits.W)
      )
      deltaPfReqBufferHoldVec(i)(s) := Mux(
        statPfReqBufferHoldVec(i)(s),
        _zeroExtend(pfStatInMSHR(s).reqBufferHoldLatency >> prefetchYieldMultipeLog, peBits) + 1.S, // to avoid 0
        0.S(peBits.W)
      )
      deltaPfLateInMshrVec(i)(s) := Mux(
        statPfLateInMshrVec(i)(s),
        1.S(peBits.W),
        0.S(peBits.W)
      )
      deltaPfLateInCacheVec(i)(s) := Mux(
        statPfLateInCacheVec(i)(s),
        1.S(peBits.W),
        0.S(peBits.W)
      )
      deltaPfUselessVec(i)(s) := Mux(
        statPfUselessVec(i)(s),
        _zeroExtend(latencyAvg, peBits),
        0.S(peBits.W)
      )
      deltaTnocVec(i)(s) := Mux(
        statTnocVec(i)(s),
        estTnoc.S(peBits.W),
        0.S(peBits.W)
      )
      deltaTbusVec(i)(s) := Mux(
        statTbusVec(i)(s),
        estTbus.S(peBits.W),
        0.S(peBits.W)
      )
      deltaTbankVec(i)(s) := Mux(
        statTbankVec(i)(s),
        estTbank.S(peBits.W),
        0.S(peBits.W)
      )
      
      // PE Equation 0: I-POP origin
      peDeltaSliceVec(s) := deltaDemandCacheHitVec(i)(s) - deltaPollutionHoldVec(i)(s) -
        deltaTnocVec(i)(s) - deltaTbusVec(i)(s) - deltaTbankVec(i)(s)

      // PE Equation 1
      // peDeltaSliceVec(s) := deltaMshrHitVec(i)(s) + deltaDemandCacheHitVec(i)(s) + deltaL1PrefetchCacheHitVec(i)(s) - 
      //   deltaPollutionHoldVec(i)(s) - deltaPfMshrHoldVec(i)(s) - deltaPfReqBufferHoldVec(i)(s)

      // PE Equation 2
      // peDeltaSliceVec(s) := deltaMshrHitVec(i)(s) + deltaDemandCacheHitVec(i)(s) + deltaL1PrefetchCacheHitVec(i)(s) - 
      //   deltaPollutionHoldVec(i)(s) - deltaPfMshrHoldVec(i)(s) - deltaPfReqBufferHoldVec(i)(s) -
      //   deltaPfLateInMshrVec(i)(s) - deltaPfLateInCacheVec(i)(s)

      // PE Equation 3
      // peDeltaSliceVec(s) := deltaMshrHitVec(i)(s) + deltaDemandCacheHitVec(i)(s) + deltaL1PrefetchCacheHitVec(i)(s) - 
      //   deltaPollutionHoldVec(i)(s) - deltaPfReqBufferHoldVec(i)(s) - deltaPfUselessVec(i)(s)
    }
    
    // pe1: get the pe sum of all slices
    val peDeltaSum = peDeltaSliceVec.reduce(_ + _)
    val peDeltaSumNarrow = peDeltaSum(peBits - 1, 0).asSInt
    val peNext = peVec(i) + peDeltaSumNarrow
    peVec(i) := peNext
    statPeOverflowVec(i) := peDeltaSumNarrow(peBits - 1) === peVec(i)(peBits - 1) && peNext(peBits - 1) =/= peVec(i)(peBits - 1)
  }

  // record for debug //
  class LatencyAttributeBundle extends Bundle {
    val epochID = UInt(64.W)
    val deltaMshrHit = UInt(peBits.W)
    val deltaDemandCacheHit = UInt(peBits.W)
    val deltaL1PrefetchCacheHit = UInt(peBits.W)
    val deltaPollutionHold = UInt(peBits.W)
    val deltaPfMshrHold = UInt(peBits.W)
    val deltaPfReqBufferHold = UInt(peBits.W)
    val deltaPfLateInMshr = UInt(peBits.W)
    val deltaPfLateInCache = UInt(peBits.W)
    val deltaPfUseless = UInt(peBits.W)
    val deltaTnoc = UInt(peBits.W)
    val deltaTbus = UInt(peBits.W)
    val deltaTbank = UInt(peBits.W)
  }
  for (i <- 0 until PF_NUM) {
    val latencyAttribute = Wire(new LatencyAttributeBundle())
    latencyAttribute.epochID := epochID
    latencyAttribute.deltaMshrHit := deltaMshrHitVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaDemandCacheHit := deltaDemandCacheHitVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaL1PrefetchCacheHit := deltaL1PrefetchCacheHitVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaPollutionHold := deltaPollutionHoldVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaPfMshrHold := deltaPfMshrHoldVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaPfReqBufferHold := deltaPfReqBufferHoldVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaPfLateInMshr := deltaPfLateInMshrVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaPfLateInCache := deltaPfLateInCacheVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaPfUseless := deltaPfUselessVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaTnoc := deltaTnocVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaTbus := deltaTbusVec(i).reduce(_ + _).asUInt
    latencyAttribute.deltaTbank := deltaTbankVec(i).reduce(_ + _).asUInt
    val w = statMshrHitVec(i).asUInt.orR ||
      statDemandCacheHitVec(i).asUInt.orR ||
      statL1PrefetchCacheHitVec(i).asUInt.orR ||
      statPollutionHoldVec(i).asUInt.orR ||
      statPfMshrHoldVec(i).asUInt.orR ||
      statPfReqBufferHoldVec(i).asUInt.orR ||
      statPfLateInMshrVec(i).asUInt.orR ||
      statPfLateInCacheVec(i).asUInt.orR ||
      statPfUselessVec(i).asUInt.orR ||
      statTnocVec(i).asUInt.orR ||
      statTbusVec(i).asUInt.orR ||
      statTbankVec(i).asUInt.orR
    val latencyAttributeTable = ChiselDB.createTable(s"LatencyAttributeTable_${PF_NAME_VEC(i)}", new LatencyAttributeBundle, basicDB = true)
    latencyAttributeTable.log(latencyAttribute, w, "L2PrefetchController", clock, reset)
  }

  // ========== control engine ==========
  // becase ddr flush may cause long latency, so epochEdge can not be too large.
  private val epochEdge = 256
  private val epochBits = log2Ceil(epochEdge)
  private def latencyDownThreshold(x: UInt): UInt = x >> 1 // ratio of last latency or other constant
  private def maxDegree = (1 << degreeBits) - 1

  val activeVec = RegInit(VecInit(Seq.fill(PF_NUM)(true.B)))
  val levelVec = RegInit(VecInit(Seq.fill(PF_NUM)(0.U(degreeBits.W))))
  val demandCnt = RegInit(0.U(epochBits.W))

  private def pfDegree(idx: Int): UInt = Mux(
    activeVec(idx),
    1.U, // Mux(levelVec(idx) === maxDegree.U, maxDegree.U, levelVec(idx) + 1.U),
    0.U
  )

  val epochEnd = io.isDemandTrain && demandCnt === (epochEdge - 1).U
  val epochEndReg = RegNext(epochEnd)
  when (epochEnd) {
    demandCnt := 0.U
  }.elsewhen(io.isDemandTrain) {
    demandCnt := demandCnt + 1.U
  }

  val latencyDown = latencyAvg < latencyLastEpoch && 
    ((latencyLastEpoch - latencyAvg) > latencyDownThreshold(latencyLastEpoch))
  val statPfHitLagActiveVec = WireInit(VecInit(Seq.fill(PF_NUM)(false.B)))
  val statLatencyDownActiveVec = WireInit(VecInit(Seq.fill(PF_NUM)(false.B)))
  when (epochEnd) {
    for (i <- 0 until PF_NUM) {
      val peEval = peVec(i)
      when (activeVec(i)) {
        when (peEval > 0.S) {
          levelVec(i) := Mux(levelVec(i) === maxDegree.U, maxDegree.U, levelVec(i) + 1.U)
        }.elsewhen (peEval < 0.S) {
          when (levelVec(i) === 0.U) {
            activeVec(i) := false.B
            levelVec(i) := maxDegree.U
          }.otherwise {
            levelVec(i) := levelVec(i) - 1.U
          }
        }
      }.otherwise {
        when (peEval > 0.S) { // prefetches from previous epoches hit at current epoch
          activeVec(i) := true.B
          levelVec(i) := 1.U
          statPfHitLagActiveVec(i) := true.B
        }.elsewhen (levelVec(i) === 0.U) {
          when(latencyDown) { // latency has downtrend, try to active this prefetcher
            activeVec(i) := true.B
            levelVec(i) := 0.U
            statLatencyDownActiveVec(i) := true.B
          }
        }.otherwise {
          levelVec(i) := levelVec(i) - 1.U
        }
      }
      peVec(i) := 0.S
    }
    latencyLastEpoch := latencyAvg
    epochID := epochID + 1.U
  }

  io.l2PfFbCtrl.streamDegree := pfDegree(PF_STREAM)
  io.l2PfFbCtrl.strideDegree := pfDegree(PF_STRIDE)
  io.l2PfFbCtrl.bertiDegree := pfDegree(PF_BERTI)
  io.l2PfFbCtrl.smsDegree := pfDegree(PF_SMS)
  io.l2PfFbCtrl.vbopDegree := pfDegree(PF_VBOP)
  io.l2PfFbCtrl.pbopDegree := pfDegree(PF_PBOP)
  io.l2PfFbCtrl.tpDegree := pfDegree(PF_TP)

  // record for debug //
  class EpochRecordBundle extends Bundle {
    val epochID = UInt(64.W)
    val latencyCurr = UInt(timestampBits.W)
    val pe = Vec(PF_NUM, UInt(peBits.W))
  }
  val epochRecord = Wire(new EpochRecordBundle())
  epochRecord.epochID := epochID
  epochRecord.latencyCurr := latencyAvg
  for (i <- 0 until PF_NUM) {
    epochRecord.pe(i) := peVec(i).asUInt
  }
  val epochRecordTable = ChiselDB.createTable("EpochRecordTable", new EpochRecordBundle(), basicDB = true)
  epochRecordTable.log(epochRecord, epochEnd, "L2PrefetchController", clock, reset)

  XSPerfAccumulate("epochCount", epochEnd)
  XSPerfAccumulate("replaceCount", PopCount(replaceRecord.map(x => x.valid)))
  XSPerfAccumulate("pfReplaceDemand", PopCount(pfReplaceDemandValid))
  XSPerfAccumulate("dataRefill", PopCount(dataRefill.map(x => x.valid)))
  XSPerfAccumulate("epoch_latencyDownActiveCountTotal", PopCount(statLatencyDownActiveVec))
  XSPerfAccumulate("epoch_pfHitLagActiveCountTotal", PopCount(statPfHitLagActiveVec))
  XSPerfAccumulate("other_peVecOverflowCountTotal", PopCount(statPeOverflowVec))
  XSPerfAccumulate("stat_mshrHitTotal", statMshrHitVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_demandCacheHitTotal", statDemandCacheHitVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_l1PrefetchCacheHitTotal", statL1PrefetchCacheHitVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_pollutionHoldTotal", statPollutionHoldVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_pfMshrHoldTotal", statPfMshrHoldVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_pfReqBufferHoldTotal", statPfReqBufferHoldVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_pfLateInMshrTotal", statPfLateInMshrVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_pfLateInCacheTotal", statPfLateInCacheVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_pfUselessTotal", statPfUselessVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_tnocTotal", statTnocVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_tbusTotal", statTbusVec.map(x => PopCount(x)).reduce(_ + _))
  XSPerfAccumulate("stat_tbankTotal", statTbankVec.map(x => PopCount(x)).reduce(_ + _))
  for (i <- 0 until PF_NUM) {
    XSPerfAccumulate(s"epoch_activeEpochCount${PF_NAME_VEC(i)}", epochEndReg && activeVec(i))
    XSPerfAccumulate(s"epoch_inactiveEpochCount${PF_NAME_VEC(i)}", epochEndReg && !activeVec(i))
    XSPerfAccumulate(s"epoch_latencyDownActiveCount${PF_NAME_VEC(i)}", statLatencyDownActiveVec(i))
    XSPerfAccumulate(s"epoch_pfHitLagActiveCount${PF_NAME_VEC(i)}", statPfHitLagActiveVec(i))
    for (k <- 0 until (1 << degreeBits)) {
      XSPerfAccumulate(s"epoch_partten${PF_NAME_VEC(i)}_0_${k}", epochEndReg && !activeVec(i) && levelVec(i) === k.U)
      XSPerfAccumulate(s"epoch_partten${PF_NAME_VEC(i)}_1_${k}", epochEndReg && activeVec(i) && levelVec(i) === k.U)
    }

    XSPerfAccumulate(s"stat_mshrHitCount${PF_NAME_VEC(i)}", PopCount(statMshrHitVec(i)))
    XSPerfAccumulate(s"stat_demandCacheHitCount${PF_NAME_VEC(i)}", PopCount(statDemandCacheHitVec(i)))
    XSPerfAccumulate(s"stat_l1PrefetchCacheHitCount${PF_NAME_VEC(i)}", PopCount(statL1PrefetchCacheHitVec(i)))
    XSPerfAccumulate(s"stat_pollutionHoldCount${PF_NAME_VEC(i)}", PopCount(statPollutionHoldVec(i)))
    XSPerfAccumulate(s"stat_pfMshrHoldCount${PF_NAME_VEC(i)}", PopCount(statPfMshrHoldVec(i)))
    XSPerfAccumulate(s"stat_pfReqBufferHoldCount${PF_NAME_VEC(i)}", PopCount(statPfReqBufferHoldVec(i)))
    XSPerfAccumulate(s"stat_pfLateInMshrCount${PF_NAME_VEC(i)}", PopCount(statPfLateInMshrVec(i)))
    XSPerfAccumulate(s"stat_pfLateInCacheCount${PF_NAME_VEC(i)}", PopCount(statPfLateInCacheVec(i)))
    XSPerfAccumulate(s"stat_pfUselessCount${PF_NAME_VEC(i)}", PopCount(statPfUselessVec(i)))
    XSPerfAccumulate(s"stat_tnocCount${PF_NAME_VEC(i)}", PopCount(statTnocVec(i)))
    XSPerfAccumulate(s"stat_tbusCount${PF_NAME_VEC(i)}", PopCount(statTbusVec(i)))
    XSPerfAccumulate(s"stat_tbankCount${PF_NAME_VEC(i)}", PopCount(statTbankVec(i)))

    XSPerfAccumulate(s"delta_mshrHitSum${PF_NAME_VEC(i)}", deltaMshrHitVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_demandCacheHitSum${PF_NAME_VEC(i)}", deltaDemandCacheHitVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_l1PrefetchCacheHitSum${PF_NAME_VEC(i)}", deltaL1PrefetchCacheHitVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_pollutionHoldSum${PF_NAME_VEC(i)}", deltaPollutionHoldVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_pfMshrHoldSum${PF_NAME_VEC(i)}", deltaPfMshrHoldVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_pfReqBufferHoldSum${PF_NAME_VEC(i)}", deltaPfReqBufferHoldVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_pfLateInMshrSum${PF_NAME_VEC(i)}", deltaPfLateInMshrVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_pfLateInCacheSum${PF_NAME_VEC(i)}", deltaPfLateInCacheVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_pfUselessSum${PF_NAME_VEC(i)}", deltaPfUselessVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_tnocSum${PF_NAME_VEC(i)}", deltaTnocVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_tbusSum${PF_NAME_VEC(i)}", deltaTbusVec(i).reduce(_ + _).asUInt)
    XSPerfAccumulate(s"delta_tbankSum${PF_NAME_VEC(i)}", deltaTbankVec(i).reduce(_ + _).asUInt)

    XSPerfAccumulate(s"other_peOverflowCount${PF_NAME_VEC(i)}", statPeOverflowVec(i))
  }

}
