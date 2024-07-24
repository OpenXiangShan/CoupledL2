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
import coupledL2.tl2chi.{PortIO}

class OpenLLC(implicit p: Parameters) extends LazyModule with HasOpenLLCParameters with HasClientInfo {

  class OpenLLCImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    private val sizeBytes = cacheParams.toCacheParams.capacity.toDouble 
    private val sizeStr = sizeBytesToStr(sizeBytes)
    private val clientParam = cacheParams.clientCaches.head.toCacheParams
    private val singleCore = cacheParams.clientCaches.size == 1
    private val inclusion = if (singleCore) "Exclusive" else "Non-inclusive"
    // Display info
    println(s"====== ${inclusion} CHI-CHI ${cacheParams.name} ($sizeStr * $banks-bank)  ======")
    println(s"bankBits: ${bankBits}")
    println(s"sets:${cacheParams.sets} ways:${cacheParams.ways} blockBytes:${cacheParams.blockBytes}")
    println(s"[snoop filter] size:${sizeBytesToStr(clientSets * clientWays * clientParam.blockBytes.toDouble)}")
    println(s"[snoop filter] sets:${clientSets} ways:${clientWays}")

    val io = IO(new Bundle {
      val rn = Vec(numRNs, Flipped(new PortIO))
      val sn = new NoSnpPortIO
      val nodeID = Input(UInt())
    })

    val rnXbar = Module(new RNXbar())
    val snXbar = Module(new SNXbar())
    val snLinkMonitor = Module(new DownwardsLinkMonitor())

    for (i <- 0 until numRNs) {
      val rnLinkMonitor = Module(new UpwardsLinkMonitor())
      rnLinkMonitor.io.out <> io.rn(i)
      rnLinkMonitor.io.entranceID := i.U
      rnLinkMonitor.io.nodeID := io.nodeID
      rnXbar.io.in(i) <> rnLinkMonitor.io.in
    }

    for (j <- 0 until banks) {
      val slice = Module(new Slice())
      slice.io.in <> rnXbar.io.out(j)
      rnXbar.io.snpMasks(j) := slice.io.snpMask 
      snXbar.io.in(j) <> slice.io.out
    }
    
    snLinkMonitor.io.in <> snXbar.io.out
    snLinkMonitor.io.nodeID := io.nodeID

    io.sn <> snLinkMonitor.io.out

    dontTouch(io)
  }
  lazy val module = new OpenLLCImp(this)
}
