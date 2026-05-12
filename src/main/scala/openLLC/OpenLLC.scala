/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package openLLC

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2chi.{PortIO, CHIIssue}

class OpenLLC(implicit p: Parameters) extends LLCModule with HasClientInfo {
  private val sizeBytes = cacheParams.toCacheParams.capacity.toDouble 
  private val sizeStr = sizeBytesToStr(sizeBytes)
  private val clientParam = cacheParams.clientCaches.head.toCacheParams
  // Display info
  println(s"====== ${inclusion} CHI-CHI ${cacheParams.name} ($sizeStr * $banks-bank)  ======")
  println(s"bankBits: ${bankBits}")
  println(s"sets:${cacheParams.sets} ways:${cacheParams.ways} blockBytes:${cacheParams.blockBytes}")
  println(s"[snoop filter] size:${sizeBytesToStr(clientSets * clientWays * clientParam.blockBytes.toDouble)} * $banks-bank")
  println(s"[snoop filter] sets:${clientSets} ways:${clientWays}")

  val io = IO(new Bundle {
    val rn = Vec(numRNs, Flipped(new PortIO))
    val sn = new NoSnpPortIO
    val nodeID = Input(UInt())
    val debugTopDown = new Bundle() {
      val robHeadPaddr = Vec(cacheParams.hartIds.length, Flipped(Valid(UInt(48.W))))
      val addrMatch = Vec(cacheParams.hartIds.length, Output(Bool()))
    }
    val l3Miss = Output(Bool())
  })

  println(s"CHI Issue Version: ${p(CHIIssue)}")
  println(s"CHI REQ Flit Width: ${io.rn(0).tx.req.flit.getWidth}")
  println(s"CHI RSP Flit Width: ${io.rn(0).tx.rsp.flit.getWidth}")
  println(s"CHI SNP Flit Width: ${io.rn(0).rx.snp.flit.getWidth}")
  println(s"CHI DAT Flit Width: ${io.rn(0).rx.dat.flit.getWidth}")
  println(s"CHI Port Width: ${io.rn(0).getWidth}")

  val mmioDiverger = Module(new MMIODiverger())
  val mmioMerger = Module(new MMIOMerger())
  val rnXbar = Module(new RNXbar())
  val snXbar = Module(new SNXbar())
  val snLinkMonitor = Module(new SNLinkMonitor())
  val topDown = topDownOpt.map(_ => Module(new TopDownMonitor()))
  val slices = (0 until banks).map { i =>
    val slice = Module(new Slice())
    slice.io.in <> rnXbar.io.out(i)
    rnXbar.io.snpMasks(i) := slice.io.snpMask 
    snXbar.io.in(i) <> slice.io.out
    slice
  }

  for (i <- 0 until numRNs) {
    val rnLinkMonitor = Module(new RNLinkMonitor())
    rnLinkMonitor.io.out <> io.rn(i)
    rnLinkMonitor.io.entranceID := i.U
    rnLinkMonitor.io.nodeID := io.nodeID
    mmioDiverger.io.in(i) <> rnLinkMonitor.io.in
    rnXbar.io.in(i) <> mmioDiverger.io.out.cache(i)
  }

  mmioMerger.io.in.cache <> snXbar.io.out
  mmioMerger.io.in.uncache <> mmioDiverger.io.out.uncache
  snLinkMonitor.io.in <> mmioMerger.io.out
  snLinkMonitor.io.nodeID := io.nodeID

  io.sn <> snLinkMonitor.io.out

  topDown match {
    case Some(t) =>
      t.io.msStatus.zip(slices).foreach {
        case (in, s) => in := s.io.msStatus.get
      }
      t.io.debugTopDown <> io.debugTopDown
    case None => io.debugTopDown.addrMatch.foreach(_ := false.B)
  }

  io.l3Miss := RegNext(Cat(slices.map(_.io.l3Miss)).orR)
}
