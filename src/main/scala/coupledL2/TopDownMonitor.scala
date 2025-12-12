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
    val msStatus  = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus))))
    val msAlloc   = Vec(banks, Vec(mshrsAll, Flipped(ValidIO(new MSHRAllocStatus))))
    val latePF    = Vec(banks, Flipped(ValidIO(UInt(PfSource.pfSourceBits.W))))
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
  XSPerfHistogram("miss_cycles_CPU" , PopCount(missVecCPU), true.B, 0, totalMSHRs, 1)
  XSPerfHistogram("miss_cycles_Prefetch", PopCount(missVecPref), true.B, 0, totalMSHRs, 1)
  XSPerfHistogram("miss_cycles_All" , PopCount(missVecCPU)+PopCount(missVecPref), true.B, 0, totalMSHRs, 1)

  // count the miss times
  val missCountCPU = allValidMatchVec(io.msAlloc)(s => s.fromA && s.is_miss && !s.is_prefetch)
  val missCountPref = allValidMatchVec(io.msAlloc)(s => s.fromA && s.is_miss && s.is_prefetch)
  XSPerfAccumulate("miss_count_CPU", PopCount(missCountCPU))
  XSPerfAccumulate("miss_count_Prefetch", PopCount(missCountPref))
  XSPerfAccumulate("miss_count_All", PopCount(missCountCPU) + PopCount(missCountPref))

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
  val l2prefetchSentVec = pfTypes.map { case (_, reqSrc, _) => dirResultMatchVec(r => r.replacerInfo.reqSource === reqSrc) }
  val l2prefetchUsefulVec = pfTypes.map { case (_, _, pfSrc) =>
    dirResultMatchVec(r => reqFromCPU(r) && r.hit &&
      r.meta.prefetch.getOrElse(false.B) && r.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U) === pfSrc)
  }
  val l2prefetchLateVec = pfTypes.map { case (_, _, pfSrc) =>
    io.latePF.map(r => r.valid && r.bits === pfSrc)
  }

  // to summary
  val l2prefetchSent = dirResultMatchVec(
    r => MemReqSource.isL2Prefetch(r.replacerInfo.reqSource)
  )
  val l2prefetchUseful = dirResultMatchVec(
    r => reqFromCPU(r) && r.hit && r.meta.prefetch.getOrElse(false.B)
  )
  val l2demandMiss = dirResultMatchVec(
    r => reqFromCPU(r) && !r.hit
  )
  val l2prefetchLate = io.latePF.map(_.valid)
  // TODO: get difference prefetchSrc for detailed analysis
  // FIXME lyq: it's abnormal l2prefetchLate / l2prefetchUseful is more than 1

  // PF Accuracy/Coverage/Late Accumulate/Rolling
  XSPerfAccumulate("l2prefetchSent", PopCount(l2prefetchSent))
  XSPerfAccumulate("l2prefetchUseful", PopCount(l2prefetchUseful))
  XSPerfAccumulate("l2demandMiss", PopCount(l2demandMiss))
  XSPerfAccumulate("l2prefetchLate", PopCount(l2prefetchLate))
  XSPerfRolling("L2PrefetchAccuracy", PopCount(l2prefetchUseful), PopCount(l2prefetchSent), 1000, io.debugTopDown.robTrueCommit, clock, reset)
  XSPerfRolling("L2PrefetchCoverage", PopCount(l2prefetchUseful), PopCount(l2prefetchUseful) + PopCount(l2demandMiss), 1000, io.debugTopDown.robTrueCommit, clock, reset)
  XSPerfRolling("L2PrefetchLate", PopCount(l2prefetchLate), PopCount(l2prefetchUseful), 1000, io.debugTopDown.robTrueCommit, clock, reset)
  for ((name, _, _, sent, useful, late) <- pfTypes zip l2prefetchSentVec zip l2prefetchUsefulVec zip l2prefetchLateVec map { case (((a, b), c), d) => (a._1, a._2, a._3, b, c, d) }) {
    XSPerfAccumulate(s"l2prefetchSent$name", PopCount(sent))
    XSPerfAccumulate(s"l2prefetchUseful$name", PopCount(useful))
    XSPerfAccumulate(s"l2prefetchLate$name", PopCount(late))
    XSPerfRolling(s"L2PrefetchAccuracy$name", PopCount(useful), PopCount(sent), 1000, io.debugTopDown.robTrueCommit, clock, reset)
    XSPerfRolling(s"L2PrefetchCoverage$name", PopCount(useful), PopCount(useful) + PopCount(l2demandMiss), 1000, io.debugTopDown.robTrueCommit, clock, reset)
  }

}
