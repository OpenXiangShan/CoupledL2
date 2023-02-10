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
import coupledL2.utils.{FastArbiter}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.Parameters
import scala.math.max

trait HasCoupledL2Parameters {
  val p: Parameters
  val cacheParams = p(L2ParamKey)

  val blocks = cacheParams.sets * cacheParams.ways
  val blockBytes = cacheParams.blockBytes
  val beatBytes = cacheParams.channelBytes.d.get
  val beatSize = blockBytes / beatBytes

  val wayBits = log2Ceil(cacheParams.ways)
  val setBits = log2Ceil(cacheParams.sets)
  val offsetBits = log2Ceil(blockBytes)
  val beatBits = offsetBits - log2Ceil(beatBytes)
  val stateBits = MetaData.stateBits
  val aliasBits = 2

  val mshrsAll = 16
  val idsAll = 128 // TODO: parameterize this?
  val mshrBits = log2Up(idsAll)

  val bufBlocks = 8 // hold data that flows in MainPipe
  val bufIdxBits = log2Up(bufBlocks)

  // 1 cycle for sram read, and latch for another cycle
  val sramLatency = 2

  val releaseBufWPorts = 3 // sinkC and mainpipe s5, s6

  val useFIFOGrantBuffer = true

  val hintCycleAhead = 2 // how many cycles the hint will send before grantData

  lazy val edgeIn = p(EdgeInKey)
  lazy val edgeOut = p(EdgeOutKey)

  lazy val clientBits = edgeIn.client.clients.count(_.supports.probe)
  lazy val sourceIdBits = edgeIn.bundle.sourceBits
  lazy val msgSizeBits = edgeIn.bundle.sizeBits
  lazy val sourceIdAll = 1 << sourceIdBits
  // id of 0XXXX refers to mshrid
  // id of 1XXXX refers to reqs that do not enter mshr
  // require(isPow2(idsAll))
  // require(idsAll >= mshrsAll * 2)
  // require(idsAll >= sourceIdAll * 2)

  // width params with bank idx (used in prefetcher / ctrl unit)
  lazy val fullAddressBits = edgeOut.bundle.addressBits
  lazy val fullTagBits = fullAddressBits - setBits - offsetBits
  // width params without bank idx (used in slice)
  // TODO: consider bankbits
  lazy val addressBits = fullAddressBits
  lazy val tagBits = fullTagBits

  lazy val outerSinkBits = edgeOut.bundle.sinkBits

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

  def parseAddress(x: UInt): (UInt, UInt, UInt) = {
    val offset = x
    val bankBits = 0  // TODO: fix bankBits
    val set = offset >> (offsetBits + bankBits)
    val tag = set >> setBits
    (tag(tagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
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
}

trait DontCareInnerLogic { this: Module =>
  override def IO[T <: Data](iodef: T): T = {
    val p = chisel3.experimental.IO.apply(iodef)
    p <> DontCare
    p
  }
}

class CoupledL2(implicit p: Parameters) extends LazyModule with HasCoupledL2Parameters {

  val xfer = TransferSizes(blockBytes, blockBytes)
  val atom = TransferSizes(1, cacheParams.channelBytes.d.get)
  val access = TransferSizes(1, blockBytes)

  val clientPortParams = (m: TLMasterPortParameters) => TLMasterPortParameters.v2(
    Seq(
      TLMasterParameters.v2(
        name = cacheParams.name,
        supports = TLSlaveToMasterTransferSizes(
          probe = xfer
        ),
        sourceId = IdRange(0, idsAll)
      )
    ),
    channelBytes = cacheParams.channelBytes,
    minLatency = 1,
    echoFields = cacheParams.echoField,
    requestFields = cacheParams.reqField,
    responseKeys = cacheParams.respKey
  )

  val managerPortParams = (m: TLSlavePortParameters) => TLSlavePortParameters.v1(
    m.managers.map { m =>
      m.v2copy(
        regionType = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
        supports = TLMasterToSlaveTransferSizes(
          acquireB = xfer,
          acquireT = if (m.supportsAcquireT) xfer else TransferSizes.none,
          arithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
          logical = if (m.supportsAcquireT) atom else TransferSizes.none,
          get = access,
          putFull = if (m.supportsAcquireT) access else TransferSizes.none,
          putPartial = if (m.supportsAcquireT) access else TransferSizes.none,
          hint = access
        ),
        fifoId = None
      )
    },
    beatBytes = 32,
    minLatency = 2,
    responseFields = cacheParams.respField,
    requestKeys = cacheParams.reqKey,
    endSinkId = idsAll
  )

  val node = TLAdapterNode(
    clientFn = clientPortParams,
    managerFn = managerPortParams
  )

  lazy val module = new LazyModuleImp(this) {
    val banks = node.in.size

    def print_bundle_fields(fs: Seq[BundleFieldBase], prefix: String) = {
      if(fs.nonEmpty){
        println(fs.map{f => s"$prefix/${f.key.name}: (${f.data.getWidth}-bit)"}.mkString("\n"))
      }
    }
    print_bundle_fields(node.in.head._2.bundle.requestFields, "usr")
    print_bundle_fields(node.in.head._2.bundle.echoFields, "echo")

    node.edges.in.headOption.foreach { n =>
      n.client.clients.zipWithIndex.foreach {
        case (c, i) =>
          println(s"\t${i} <= ${c.name}")
      }
    }

    val slices = node.in.zip(node.out).zipWithIndex.map {
      case (((in, edgeIn), (out, edgeOut)), i) =>
        require(in.params.dataBits == out.params.dataBits)
        val rst_L2 = reset
        val slice = withReset(rst_L2) { 
          Module(new Slice()(p.alterPartial {
            case EdgeInKey  => edgeIn
            case EdgeOutKey => edgeOut
          })) 
        }
        slice.io.in <> in
        out <> slice.io.out

        slice
    }

  }

}
