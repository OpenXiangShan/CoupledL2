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
import utility.MemReqSource

class TopDownMonitor()(implicit p: Parameters) extends L2Module {
  val banks = 1 << bankBits
  val io = IO(new Bundle() {
    val dirResult = Vec(banks, Flipped(ValidIO(new DirResult)))
    val msStatus  = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus))))
    val latePF    = Vec(banks, Input(Bool()))
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
  XSPerfAccumulate(cacheParams, s"${cacheParams.name}MissMatch_${cacheParams.hartId}", io.debugTopDown.l2MissMatch)

  /* ====== PART TWO ======
   * Count the parallel misses, and divide them into CPU/Prefetch
   */
  def allMSHRMatchVec(cond: MSHRStatus => Bool): IndexedSeq[Bool] = {
    io.msStatus.zipWithIndex.flatMap {
      case (slice, i) =>
        slice.map {
          ms => ms.valid && cond(ms.bits)
        }
    }
  }

  val missVecCPU  = allMSHRMatchVec(s => s.fromA && s.is_miss && !s.is_prefetch)
  val missVecPref = allMSHRMatchVec(s => s.fromA && s.is_miss &&  s.is_prefetch)
  // val missVecAll = allMSHRMatchVec(s => s.fromA && s.is_miss)

  val totalMSHRs = banks * mshrsAll
  XSPerfHistogram(cacheParams, "parallel_misses_CPU" , PopCount(missVecCPU), true.B, 0, totalMSHRs, 1)
  XSPerfHistogram(cacheParams, "parallel_misses_Pref", PopCount(missVecPref), true.B, 0, totalMSHRs, 1)
  XSPerfHistogram(cacheParams, "parallel_misses_All" , PopCount(missVecCPU)+PopCount(missVecPref), true.B, 0, 32, 1)

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

  def reqFromCPU(r: DirResult): Bool = {
    r.replacerInfo.reqSource === MemReqSource.CPULoadData.id.U ||
    r.replacerInfo.reqSource === MemReqSource.CPUStoreData.id.U
  }

  for (i <- 0 until MemReqSource.ReqSourceCount.id) {
    val sourceMatchVec = dirResultMatchVec(r => r.replacerInfo.reqSource === i.U)
    val sourceMatchVecMiss = dirResultMatchVec(r => r.replacerInfo.reqSource === i.U && !r.hit)

    val sourceName = MemReqSource.apply(i).toString
    XSPerfAccumulate(cacheParams, s"E2_${cacheParams.name}AReqSource_${sourceName}_Total", PopCount(sourceMatchVec))
    XSPerfAccumulate(cacheParams, s"E2_${cacheParams.name}AReqSource_${sourceName}_Miss", PopCount(sourceMatchVecMiss))
  }

  /* ====== MISC ======
   * Some performance counters need to be aggregated among slices. For convenience, they are defined here
   */
  // prefetch accuracy calculation
  val l2prefetchSent = dirResultMatchVec(
    r =>  !r.hit &&
      (r.replacerInfo.reqSource === MemReqSource.Prefetch2L2BOP.id.U ||
       r.replacerInfo.reqSource === MemReqSource.Prefetch2L2PBOP.id.U ||
       r.replacerInfo.reqSource === MemReqSource.Prefetch2L2SMS.id.U ||
       r.replacerInfo.reqSource === MemReqSource.Prefetch2L2Stride.id.U ||
       r.replacerInfo.reqSource === MemReqSource.Prefetch2L2Stream.id.U ||
       r.replacerInfo.reqSource === MemReqSource.Prefetch2L2TP.id.U)
  )
  val l2prefetchSentBOP = dirResultMatchVec(
    r => !r.hit && r.replacerInfo.reqSource === MemReqSource.Prefetch2L2BOP.id.U
  )
  val l2prefetchSentPBOP = dirResultMatchVec(
    r => !r.hit && r.replacerInfo.reqSource === MemReqSource.Prefetch2L2PBOP.id.U
  )
  val l2prefetchSentSMS = dirResultMatchVec(
    r => !r.hit && r.replacerInfo.reqSource === MemReqSource.Prefetch2L2SMS.id.U
  )
  val l2prefetchSentStride = dirResultMatchVec(
    r => !r.hit && r.replacerInfo.reqSource === MemReqSource.Prefetch2L2Stride.id.U
  )
  val l2prefetchSentStream = dirResultMatchVec(
    r => !r.hit && r.replacerInfo.reqSource === MemReqSource.Prefetch2L2Stream.id.U
  )
  val l2prefetchSentTP = dirResultMatchVec(
    r => !r.hit && r.replacerInfo.reqSource === MemReqSource.Prefetch2L2TP.id.U
  )

  val l2prefetchUseful = dirResultMatchVec(
    r => reqFromCPU(r) && r.hit && r.meta.prefetch.getOrElse(false.B)
  )
  val l2prefetchUsefulBOP = dirResultMatchVec(
    r => reqFromCPU(r) && r.hit &&
      r.meta.prefetch.getOrElse(false.B) && r.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) === PfSource.BOP.id.U
  )
  val l2prefetchUsefulPBOP = dirResultMatchVec(
    r => reqFromCPU(r) && r.hit &&
      r.meta.prefetch.getOrElse(false.B) && r.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) === PfSource.PBOP.id.U
  )
  val l2prefetchUsefulSMS = dirResultMatchVec(
    r => reqFromCPU(r) && r.hit &&
      r.meta.prefetch.getOrElse(false.B) && r.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) === PfSource.SMS.id.U
  )
  val l2prefetchUsefulStride = dirResultMatchVec(
    r => reqFromCPU(r) && r.hit &&
      r.meta.prefetch.getOrElse(false.B) && r.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) === PfSource.Stride.id.U
  )
  val l2prefetchUsefulStream = dirResultMatchVec(
    r => reqFromCPU(r) && r.hit &&
      r.meta.prefetch.getOrElse(false.B) && r.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) === PfSource.Stream.id.U
  )
  val l2prefetchUsefulTP = dirResultMatchVec(
    r => reqFromCPU(r) && r.hit &&
      r.meta.prefetch.getOrElse(false.B) && r.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) === PfSource.TP.id.U
  )

  val l2demandRequest = dirResultMatchVec(
    r => reqFromCPU(r)
  )
  val l2prefetchLate = io.latePF

  // PF Accuracy
  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracy",
    PopCount(l2prefetchUseful), PopCount(l2prefetchSent),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracyBOP",
    PopCount(l2prefetchUsefulBOP), PopCount(l2prefetchSentBOP),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracyPBOP",
    PopCount(l2prefetchUsefulPBOP), PopCount(l2prefetchSentPBOP),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracySMS",
    PopCount(l2prefetchUsefulSMS), PopCount(l2prefetchSentSMS),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracyTP",
    PopCount(l2prefetchUsefulTP), PopCount(l2prefetchSentTP),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracyStride",
    PopCount(l2prefetchUsefulStride), PopCount(l2prefetchSentStride),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracyStream",
    PopCount(l2prefetchUsefulStream), PopCount(l2prefetchSentStream),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracyTP",
    PopCount(l2prefetchUsefulTP), PopCount(l2prefetchSentTP),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )

  // PF Late
  XSPerfRolling(
    cacheParams, "L2PrefetchLate",
    PopCount(l2prefetchLate), PopCount(l2prefetchUseful),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )

  // PF Coverage
  XSPerfRolling(
    cacheParams, "L2PrefetchCoverage",
    PopCount(l2prefetchUseful), PopCount(l2demandRequest),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchCoverageBOP",
    PopCount(l2prefetchUsefulBOP), PopCount(l2demandRequest),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchCoveragePBOP",
    PopCount(l2prefetchUsefulPBOP), PopCount(l2demandRequest),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchCoverageSMS",
    PopCount(l2prefetchUsefulSMS), PopCount(l2demandRequest),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchCoverageTP",
    PopCount(l2prefetchUsefulTP), PopCount(l2demandRequest),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchCoverageStride",
    PopCount(l2prefetchUsefulStride), PopCount(l2demandRequest),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchCoverageStream",
    PopCount(l2prefetchUsefulStream), PopCount(l2demandRequest),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchCoverageTP",
    PopCount(l2prefetchUsefulTP), PopCount(l2demandRequest),
    1000, io.debugTopDown.robTrueCommit, clock, reset
  )

  XSPerfAccumulate(cacheParams, "l2prefetchSent", PopCount(l2prefetchSent))
  XSPerfAccumulate(cacheParams, "l2prefetchSentBOP", PopCount(l2prefetchSentBOP))
  XSPerfAccumulate(cacheParams, "l2prefetchSentSMS", PopCount(l2prefetchSentSMS))
  XSPerfAccumulate(cacheParams, "l2prefetchSentStride", PopCount(l2prefetchSentStride))
  XSPerfAccumulate(cacheParams, "l2prefetchSentStream", PopCount(l2prefetchSentStream))
  XSPerfAccumulate(cacheParams, "l2prefetchSentTP", PopCount(l2prefetchSentTP))
  XSPerfAccumulate(cacheParams, "l2prefetchUseful", PopCount(l2prefetchUseful))
  XSPerfAccumulate(cacheParams, "l2prefetchUsefulBOP", PopCount(l2prefetchUsefulBOP))
  XSPerfAccumulate(cacheParams, "l2prefetchUsefulSMS", PopCount(l2prefetchUsefulSMS))
  XSPerfAccumulate(cacheParams, "l2prefetchUsefulStride", PopCount(l2prefetchUsefulStride))
  XSPerfAccumulate(cacheParams, "l2prefetchUsefulStream", PopCount(l2prefetchUsefulStream))
  XSPerfAccumulate(cacheParams, "l2prefetchUsefulTP", PopCount(l2prefetchUsefulTP))
  XSPerfAccumulate(cacheParams, "l2demandRequest", PopCount(l2demandRequest))
  XSPerfAccumulate(cacheParams, "l2prefetchLate", PopCount(l2prefetchLate))
}
