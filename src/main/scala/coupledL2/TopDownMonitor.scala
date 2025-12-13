/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package coupledL2

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.prefetch.PfSource
import coupledL2.utils._
import utility._

// TODO: Accommodate CHI
class TopDownMonitor()(implicit p: Parameters) extends L2Module {
  val banks = 1 << bankBits
  val io = IO(new Bundle() {
    val dirResult = Vec(banks, Flipped(ValidIO(new DirResult)))
    val msStatus = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus))))
    val msAlloc = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRAllocStatus))))
    val pfSent = Vec(banks, Flipped(ValidIO(UInt(MemReqSource.reqSourceBits.W))))
    val pfHitInMSHR = Vec(banks, Flipped(ValidIO(UInt(PfSource.pfSourceBits.W))))
    val pfLateInMSHR = Vec(banks, Flipped(ValidIO(UInt(MemReqSource.reqSourceBits.W))))
    val debugTopDown = new Bundle {
      val robTrueCommit = Input(UInt(64.W))
      val robHeadPaddr = Flipped(Valid(UInt(36.W)))
      val l2MissMatch = Output(Bool())
    }
  })

  /* ====== PART ONE ======
   * Check whether the Addr given by core is a Miss in Cache
   */
  val addrMatchVec = io.msStatus.zipWithIndex.map {
    case(slice, i) =>
      slice.map {
        ms =>
          val msBlockAddr = if(bankBits == 0) Cat(ms.bits.reqTag, ms.bits.set)
            else Cat(ms.bits.reqTag, ms.bits.set, i.U(bankBits-1, 0))
          val pBlockAddr  = (io.debugTopDown.robHeadPaddr.bits >> 6.U).asUInt
          val isMiss = ms.valid && ms.bits.is_miss

          io.debugTopDown.robHeadPaddr.valid && (msBlockAddr === pBlockAddr) && isMiss
      }
  }

  io.debugTopDown.l2MissMatch := Cat(addrMatchVec.flatten).orR
  XSPerfAccumulate(s"RobBlockBy${cacheParams.name}Miss", io.debugTopDown.l2MissMatch)

  /* ====== PART TWO ======
   * Count the parallel misses, and divide them into CPU/Prefetch
   */
  def allValidMatchVec[T <: Data](vec: Vec[Vec[ValidIO[T]]])(cond: T => Bool): IndexedSeq[Bool] = {
    vec.flatMap{
      case slice =>
        slice.map {
          ms => ms.valid && cond(ms.bits)
        }
    }
  }

  val missVecCPU  = allValidMatchVec(io.msStatus)(s => s.fromA && s.is_miss && !s.is_prefetch)
  val missVecPref = allValidMatchVec(io.msStatus)(s => s.fromA && s.is_miss &&  s.is_prefetch)
  // val missVecAll = allValidMatchVec(io.msStatus)(s => s.fromA && s.is_miss)
  val totalMSHRs = banks * mshrsAll
  XSPerfHistogram("mshr_cycles_CPU" , PopCount(missVecCPU), true.B, 0, totalMSHRs, 1)
  XSPerfHistogram("mshr_cycles_Prefetch", PopCount(missVecPref), true.B, 0, totalMSHRs, 1)
  XSPerfHistogram("mshr_cycles_All" , PopCount(missVecCPU)+PopCount(missVecPref), true.B, 0, totalMSHRs, 1)

  // count the miss times
  val missCountCPU = allValidMatchVec(io.msAlloc)(s => s.fromA && s.is_miss && !s.is_prefetch)
  val missCountPref = allValidMatchVec(io.msAlloc)(s => s.fromA && s.is_miss && s.is_prefetch)
  XSPerfAccumulate("mshr_count_CPU", PopCount(missCountCPU))
  XSPerfAccumulate("mshr_count_Prefetch", PopCount(missCountPref))
  XSPerfAccumulate("mshr_count_All", PopCount(missCountCPU) + PopCount(missCountPref))

  /* ====== PART THREE ======
   * Distinguish req sources and count num & miss
   */
  // For num of Probes and Releases, please check busPMU counters

  // we calculate at the point when directory returns result (dirResult.valid)
  // we add reqSource in replacerInfo, set in dirRead in ReqArb, pass it through Directory and get it in DirResult
  // to help us distinguish whether it is an A-channel req
  def dirResultMatchVec(cond: DirResult => Bool): IndexedSeq[Bool] = {
    io.dirResult.map {
      r => r.valid && r.bits.replacerInfo.channel === 1.U && cond(r.bits)
    }
  }

  for (i <- 0 until MemReqSource.ReqSourceCount.id) {
    val sourceMatchVec = dirResultMatchVec(r => r.replacerInfo.reqSource === i.U)
    val sourceMatchVecMiss = dirResultMatchVec(r => r.replacerInfo.reqSource === i.U && !r.hit)

    val sourceName = MemReqSource.apply(i).toString
    XSPerfAccumulate(s"E2_${cacheParams.name}AReqSource_${sourceName}_Total", PopCount(sourceMatchVec))
    XSPerfAccumulate(s"E2_${cacheParams.name}AReqSource_${sourceName}_Miss", PopCount(sourceMatchVecMiss))
  }

  /* ====== MISC ======
   * Some performance counters need to be aggregated among slices. For convenience, they are defined here
   */
  val pfTypes = Seq(
    ("BOP", MemReqSource.Prefetch2L2BOP.id.U, PfSource.BOP.id.U),
    ("PBOP", MemReqSource.Prefetch2L2PBOP.id.U, PfSource.PBOP.id.U),
    ("SMS", MemReqSource.Prefetch2L2SMS.id.U, PfSource.SMS.id.U),
    ("Stride", MemReqSource.Prefetch2L2Stride.id.U, PfSource.Stride.id.U),
    ("Stream", MemReqSource.Prefetch2L2Stream.id.U, PfSource.Stream.id.U),
    ("TP", MemReqSource.Prefetch2L2TP.id.U, PfSource.TP.id.U),
    ("Berti", MemReqSource.Prefetch2L2Berti.id.U, PfSource.Berti.id.U)
  )

  // sent/useful vector
  val l2pfSentVec = pfTypes.map { case (_, reqSrc, _) => io.pfSent.map(r => r.valid && r.bits === reqSrc) }
  val l2pfSentToPipeVec = pfTypes.map { case (_, reqSrc, _) => dirResultMatchVec(r => r.replacerInfo.reqSource === reqSrc) }
  val l2pfHitInCacheVec = pfTypes.map { case (_, _, pfSrc) =>
    dirResultMatchVec(r => MemReqSource.isCPUReq(r.replacerInfo.reqSource) && r.hit &&
      r.meta.prefetch.getOrElse(false.B) && r.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) === pfSrc)
  }
  val l2pfHitInMSHRVec = pfTypes.map { case (_, _, pfSrc) =>
    io.pfHitInMSHR.map(r => r.valid && r.bits === pfSrc)
  }
  val l2pfLateInCache = pfTypes.map { case (_, reqSrc, _) =>
    dirResultMatchVec(r => MemReqSource.isL2Prefetch(r.replacerInfo.reqSource) && r.hit &&
      !r.meta.prefetch.getOrElse(false.B) && r.replacerInfo.reqSource === reqSrc)
  }
  val l2pfLateInMSHR = pfTypes.map { case (_, reqSrc, _) =>
    io.pfLateInMSHR.map(r => r.valid && r.bits === reqSrc)
  }
  val l2pfHitVec = l2pfHitInCacheVec.zip(l2pfHitInMSHRVec).map { case (c, m) => PopCount(c) + PopCount(m) }
  val l2pfLateVec = l2pfLateInCache.zip(l2pfLateInMSHR).map { case (c, m) => PopCount(c) + PopCount(m) }
  val l2demandMiss = dirResultMatchVec(
    r => MemReqSource.isCPUReq(r.replacerInfo.reqSource) && !r.hit
  )
  val l2prefetchMiss = dirResultMatchVec(
    r => MemReqSource.isL2Prefetch(r.replacerInfo.reqSource) && !r.hit
  )
  val l1prefetchMiss = dirResultMatchVec(
    r => MemReqSource.isL1Prefetch(r.replacerInfo.reqSource) && !r.hit
  )

  // PF Accuracy/Coverage/Late Accumulate/Rolling
  XSPerfAccumulate("l2demandMiss", PopCount(l2demandMiss))
  XSPerfAccumulate("l1prefetchMiss", PopCount(l1prefetchMiss))
  XSPerfAccumulate("l2prefetchMiss", PopCount(l2prefetchMiss))
  XSPerfAccumulate("l2prefetchSent", PopCount(l2pfSentVec.flatten))
  XSPerfAccumulate("l2prefetchSentToPipe", PopCount(l2pfSentToPipeVec.flatten))
  XSPerfAccumulate("l2prefetchHit", l2pfHitVec.reduce(_ + _))
  XSPerfAccumulate("l2prefetchHitInCache", PopCount(l2pfHitInCacheVec.flatten))
  XSPerfAccumulate("l2prefetchHitInMSHR", PopCount(l2pfHitInMSHRVec.flatten))
  XSPerfAccumulate("l2prefetchLate", l2pfLateVec.reduce(_ + _))
  XSPerfAccumulate("l2prefetchLateInCache", PopCount(l2pfLateInCache.flatten))
  XSPerfAccumulate("l2prefetchLateInMSHR", PopCount(l2pfLateInMSHR.flatten))
  XSPerfRolling("L2PrefetchAccuracy", l2pfHitVec.reduce(_ + _), PopCount(l2pfSentVec.flatten), 1000, io.debugTopDown.robTrueCommit, clock, reset)
  XSPerfRolling("L2PrefetchLate", l2pfLateVec.reduce(_ + _), PopCount(l2pfSentVec.flatten), 1000, io.debugTopDown.robTrueCommit, clock, reset)
  XSPerfRolling("L2PrefetchCoverage", l2pfHitVec.reduce(_ + _), l2pfHitVec.reduce(_ + _) + PopCount(l2demandMiss), 1000, io.debugTopDown.robTrueCommit, clock, reset)
  for ((x, i) <- pfTypes.zipWithIndex) {
    val name = x._1
    XSPerfAccumulate(s"l2prefetchSent$name", PopCount(l2pfSentVec(i)))
    XSPerfAccumulate(s"l2prefetchSentToPipe$name", PopCount(l2pfSentToPipeVec(i)))
    XSPerfAccumulate(s"l2prefetchHit$name", l2pfHitVec(i))
    XSPerfAccumulate(s"l2prefetchHitInCache$name", PopCount(l2pfHitInCacheVec(i)))
    XSPerfAccumulate(s"l2prefetchHitInMSHR$name", PopCount(l2pfHitInMSHRVec(i)))
    XSPerfAccumulate(s"l2prefetchLate$name", l2pfLateVec(i))
    XSPerfAccumulate(s"l2prefetchLateInCache$name", PopCount(l2pfLateInCache(i)))
    XSPerfAccumulate(s"l2prefetchLateInMSHR$name", PopCount(l2pfLateInMSHR(i)))
    XSPerfRolling(s"L2PrefetchAccuracy$name", l2pfHitVec(i), PopCount(l2pfSentVec(i)), 1000, io.debugTopDown.robTrueCommit, clock, reset)
    XSPerfRolling(s"L2PrefetchLate$name", l2pfLateVec(i), PopCount(l2pfSentVec(i)), 1000, io.debugTopDown.robTrueCommit, clock, reset)
    XSPerfRolling(s"L2PrefetchCoverage$name", l2pfHitVec(i), l2pfHitVec(i) + PopCount(l2demandMiss), 1000, io.debugTopDown.robTrueCommit, clock, reset)
  }

}
