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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.utils.{XSPerfAccumulate, XSPerfHistogram, XSPerfRolling}

class TopDownMonitor()(implicit p: Parameters) extends L2Module {
  val banks = 1 << bankBits
  val io = IO(new Bundle() {
    val dirResult = Vec(banks, Flipped(ValidIO(new DirResult)))
    val msStatus  = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus))))
    val latePF    = Vec(banks, Input(Bool()))
  })

  /* ====== PART ONE ======
   * Check whether the Addr given by core is a Miss in Cache
   */
  for (hartId <- cacheParams.hartIds) {
    val perfName = s"${cacheParams.name}MissMatch_${hartId}"

    val pAddr = WireInit(0.U.asTypeOf(Valid(UInt(36.W)))) // TODO: hand written to match PAddrBits in SoC.scala
    ExcitingUtils.addSink(pAddr, s"rob_head_paddr_${hartId}", ExcitingUtils.Perf)

    val addrMatchVec = io.msStatus.zipWithIndex.map {
      case(slice, i) =>
        slice.map {
          ms =>
            val msBlockAddr = if(bankBits == 0) Cat(ms.bits.reqTag, ms.bits.set)
              else Cat(ms.bits.reqTag, ms.bits.set, i.U(bankBits-1, 0))
            val pBlockAddr  = (pAddr.bits >> 6.U).asUInt
            val isMiss = ms.valid && ms.bits.is_miss

            pAddr.valid && (msBlockAddr === pBlockAddr) && isMiss
        }
    }

    val addrMatch = Cat(addrMatchVec.flatten).orR

    XSPerfAccumulate(cacheParams, perfName, addrMatch)
    ExcitingUtils.addSource(addrMatch, perfName, ExcitingUtils.Perf)
    ExcitingUtils.addSink(WireDefault(addrMatch), perfName, ExcitingUtils.Perf)
  }

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
  // count releases
  val releaseCnt = allMSHRMatchVec(s => s.will_free && s.fromC)
  XSPerfAccumulate(cacheParams, s"${cacheParams.name}C_ReleaseCnt_Total", PopCount(releaseCnt))

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
    XSPerfAccumulate(cacheParams, s"E2_${cacheParams.name}AReqSource_${sourceName}_Total", PopCount(sourceMatchVec))
    XSPerfAccumulate(cacheParams, s"E2_${cacheParams.name}AReqSource_${sourceName}_Miss", PopCount(sourceMatchVecMiss))
  }

  /* ====== MISC ======
   * Some performance counters need to be aggregated among slices. For convenience, they are defined here
   */
  // prefetch accuracy calculation
  val l2prefetchSent = dirResultMatchVec(
    r => (r.replacerInfo.reqSource === MemReqSource.L2Prefetch.id.U) && !r.hit
  )
  val l2prefetchUseful = dirResultMatchVec(
    r => (r.replacerInfo.reqSource === MemReqSource.CPULoadData.id.U
      || r.replacerInfo.reqSource === MemReqSource.CPUStoreData.id.U) &&
      r.hit &&
      r.meta.prefetch.getOrElse(false.B)
  )
  val l2demandRequest = dirResultMatchVec(
    r => (r.replacerInfo.reqSource === MemReqSource.CPULoadData.id.U
      || r.replacerInfo.reqSource === MemReqSource.CPUStoreData.id.U)
  )
  val l2prefetchLate = io.latePF

  XSPerfRolling(
    cacheParams, "L2PrefetchAccuracy",
    PopCount(l2prefetchUseful), PopCount(l2prefetchSent),
    1000, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchLate",
    PopCount(l2prefetchLate), PopCount(l2prefetchUseful),
    1000, clock, reset
  )
  XSPerfRolling(
    cacheParams, "L2PrefetchCoverage",
    PopCount(l2prefetchUseful), PopCount(l2demandRequest),
    1000, clock, reset
  )
}
