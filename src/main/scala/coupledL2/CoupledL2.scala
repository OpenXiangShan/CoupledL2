/** *************************************************************************************
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
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package coupledL2

import chisel3._
import chisel3.util._
import utility.{FastArbiter, ParallelMax, ParallelPriorityMux, Pipeline, RegNextN}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import scala.math.max
import coupledL2.prefetch._
import coupledL2.utils.XSPerfAccumulate
import huancun.{TPmetaReq, TPmetaResp, BankBitsKey}

trait HasCoupledL2Parameters {
  val p: Parameters
  // val tl2tlParams: HasTLL2Parameters = p(L2ParamKey)
  def enableCHI = p(EnableCHI)
  def cacheParams = p(L2ParamKey)

  def XLEN = 64
  def blocks = cacheParams.sets * cacheParams.ways
  def blockBytes = cacheParams.blockBytes
  def beatBytes = cacheParams.channelBytes.d.get
  def beatSize = blockBytes / beatBytes

  def wayBits = log2Ceil(cacheParams.ways)
  def setBits = log2Ceil(cacheParams.sets)
  def offsetBits = log2Ceil(blockBytes)
  def beatBits = offsetBits - log2Ceil(beatBytes)
  def stateBits = MetaData.stateBits
  def chiOpt = if (enableCHI) Some(true) else None
  def aliasBitsOpt = if(cacheParams.clientCaches.isEmpty) None
                  else cacheParams.clientCaches.head.aliasBitsOpt
  // vaddr without offset bits
  def vaddrBitsOpt = if(cacheParams.clientCaches.isEmpty) None
                  else cacheParams.clientCaches.head.vaddrBitsOpt
  def fullVAddrBits = vaddrBitsOpt.getOrElse(0) + offsetBits
  // from L1 load miss cache require
  def isKeywordBitsOpt = if(cacheParams.clientCaches.isEmpty) None
                  else cacheParams.clientCaches.head.isKeywordBitsOpt

  def pageOffsetBits = log2Ceil(cacheParams.pageBytes)

  def bufBlocks = 4 // hold data that flows in MainPipe
  def bufIdxBits = log2Up(bufBlocks)

  def releaseBufWPorts = 3 // sinkC & mainPipe s5 & mainPipe s3 (nested)

  def mmioBridgeSize = cacheParams.mmioBridgeSize

  // Prefetch
  def prefetchers = cacheParams.prefetch
  def prefetchOpt = if(prefetchers.nonEmpty) Some(true) else None
  def hasBOP = prefetchers.exists(_.isInstanceOf[BOPParameters])
  def hasReceiver = prefetchers.exists(_.isInstanceOf[PrefetchReceiverParams])
  def hasTPPrefetcher = prefetchers.exists(_.isInstanceOf[TPParameters])
  def hasPrefetchBit = prefetchers.exists(_.hasPrefetchBit) // !! TODO.test this
  def hasPrefetchSrc = prefetchers.exists(_.hasPrefetchSrc)
  def topDownOpt = if(cacheParams.elaboratedTopDown) Some(true) else None

  def enableHintGuidedGrant = true

  def hintCycleAhead = 3 // how many cycles the hint will send before grantData

  def edgeIn = p(EdgeInKey)
  def edgeOut = p(EdgeOutKey)
  def bankBits = p(BankBitsKey)

  def clientBits = edgeIn.client.clients.count(_.supports.probe)
  def sourceIdBits = edgeIn.bundle.sourceBits // ids of L1
  def msgSizeBits = edgeIn.bundle.sizeBits
  def sourceIdAll = 1 << sourceIdBits

  def hartIdLen: Int = p(MaxHartIdBits)

  def mshrsAll = cacheParams.mshrs
  def idsAll = 256// ids of L2 //TODO: Paramterize like this: max(mshrsAll * 2, sourceIdAll * 2)
  def mshrBits = log2Up(idsAll)
  // id of 0XXXX refers to mshrId
  // id of 1XXXX refers to reqs that do not enter mshr
  // require(isPow2(idsAll))

  def grantBufSize = mshrsAll
  def grantBufInflightSize = mshrsAll //TODO: lack or excessive? !! WARNING

  // width params with bank idx (used in prefetcher / ctrl unit)
  def fullAddressBits = edgeIn.bundle.addressBits
  def fullTagBits = fullAddressBits - setBits - offsetBits
  // width params without bank idx (used in slice)
  def addressBits = fullAddressBits - bankBits
  def tagBits = fullTagBits - bankBits

  def outerSinkBits = edgeOut.bundle.sinkBits

  def sam = cacheParams.sam

  def getClientBitOH(sourceId: UInt): UInt = {
    if (clientBits == 0) {
      0.U
    } else {
      Cat(
        edgeIn.client.clients
          .filter(_.supports.probe)
          .map(c => {
            c.sourceId.contains(sourceId).asInstanceOf[Bool]
          })
          .reverse
      )
    }
  }

  def getSourceId(client: UInt): UInt = {
    if (clientBits == 0) {
      0.U
    } else {
      Mux1H(
        client,
        edgeIn.client.clients
          .filter(_.supports.probe)
          .map(c => c.sourceId.start.U)
      )
    }
  }

  def parseFullAddress(x: UInt): (UInt, UInt, UInt) = {
    val offset = x // TODO: check address mapping
    val set = offset >> offsetBits
    val tag = set >> setBits
    (tag(fullTagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
  }

  def parseAddress(x: UInt): (UInt, UInt, UInt) = {
    val offset = x
    val set = offset >> (offsetBits + bankBits)
    val tag = set >> setBits
    (tag(tagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
  }

  def restoreAddress(x: UInt, idx: Int) = {
    restoreAddressUInt(x, idx.U)
  }

  def restoreAddressUInt(x: UInt, idx: UInt) = {
    if(bankBits == 0){
      x
    } else {
      val high = x >> offsetBits
      val low = x(offsetBits - 1, 0)
      Cat(high, idx(bankBits - 1, 0), low)
    }
  }

  def getPPN(x: UInt): UInt = {
    x(x.getWidth - 1, pageOffsetBits)
  }

  def arb[T <: Bundle](in: Seq[DecoupledIO[T]], out: DecoupledIO[T], name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) { a <> req }
    out <> arb.io.out
  }

  def fastArb[T <: Bundle](in: Seq[DecoupledIO[T]], out: DecoupledIO[T], name: Option[String] = None): Unit = {
    val arb = Module(new FastArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) { a <> req }
    out <> arb.io.out
  }

  def odOpGen(r: UInt) = {
    val grantOp = GrantData
    val opSeq = Seq(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, grantOp, Grant)
    val opToA = VecInit(opSeq)(r)
    opToA
  }

  def sizeBytesToStr(sizeBytes: Double): String = sizeBytes match {
    case _ if sizeBytes >= 1024 * 1024 => (sizeBytes / 1024 / 1024) + "MB"
    case _ if sizeBytes >= 1024        => (sizeBytes / 1024) + "KB"
    case _                            => "B"
  }
  
  def print_bundle_fields(fs: Seq[BundleFieldBase], prefix: String) = {
    if(fs.nonEmpty){
      println(fs.map{f => s"$prefix/${f.key.name}: (${f.data.getWidth}-bit)"}.mkString("\n"))
    }
  }
}

abstract class CoupledL2Base(implicit p: Parameters) extends LazyModule with HasCoupledL2Parameters {

  val xfer = TransferSizes(blockBytes, blockBytes)
  val atom = TransferSizes(1, cacheParams.channelBytes.d.get)
  val access = TransferSizes(1, blockBytes)

  val pf_recv_node: Option[BundleBridgeSink[PrefetchRecv]] =
    if(hasReceiver) Some(BundleBridgeSink(Some(() => new PrefetchRecv))) else None
}
