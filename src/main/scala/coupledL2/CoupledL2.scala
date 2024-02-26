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
import utility.{FastArbiter, Pipeline}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.max
import coupledL2.prefetch._
import coupledL2.utils.XSPerfAccumulate
import huancun.{TPmetaReq, TPmetaResp}
import huancun.{AliasField, PrefetchField}
import huancun.{AliasKey, PrefetchKey}
import utility.{LookupTree,ParallelPriorityMux}
import utility.ReqSourceField

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
  val aliasBitsOpt = if(cacheParams.clientCaches.isEmpty) None
                  else cacheParams.clientCaches.head.aliasBitsOpt
  val vaddrBitsOpt = if(cacheParams.clientCaches.isEmpty) None
                  else cacheParams.clientCaches.head.vaddrBitsOpt
  // from L1 load miss cache require
  val isKeywordBitsOpt = if(cacheParams.clientCaches.isEmpty) None
                  else cacheParams.clientCaches.head.isKeywordBitsOpt
         
  val pageOffsetBits = log2Ceil(cacheParams.pageBytes)

  val bufBlocks = 4 // hold data that flows in MainPipe
  val bufIdxBits = log2Up(bufBlocks)

  val releaseBufWPorts = 3 // sinkC & mainPipe s5 & mainPipe s3 (nested)

  // Prefetch
  val prefetchOpt = cacheParams.prefetch
  val hasPrefetchBit = prefetchOpt.nonEmpty && prefetchOpt.get.hasPrefetchBit
  val hasPrefetchSrc = prefetchOpt.nonEmpty && prefetchOpt.get.hasPrefetchSrc
  val topDownOpt = if(cacheParams.elaboratedTopDown) Some(true) else None

  val enableHintGuidedGrant = true

  val hintCycleAhead = 3 // how many cycles the hint will send before grantData

  lazy val edgeIn = p(EdgeInKey)
  lazy val edgeOut = p(EdgeOutKey)
  lazy val bankBits = p(BankBitsKey)

  lazy val clientBits = edgeIn.client.clients.count(_.supports.probe)
  lazy val sourceIdBits = edgeIn.bundle.sourceBits // ids of L1
  lazy val msgSizeBits = edgeIn.bundle.sizeBits
  lazy val sourceIdAll = 1 << sourceIdBits

  val mshrsAll = cacheParams.mshrs
  val idsAll = 256// ids of L2 //TODO: Paramterize like this: max(mshrsAll * 2, sourceIdAll * 2)
  val mshrBits = log2Up(idsAll)
  // id of 0XXXX refers to mshrId
  // id of 1XXXX refers to reqs that do not enter mshr
  // require(isPow2(idsAll))

  val grantBufSize = mshrsAll
  val grantBufInflightSize = mshrsAll //TODO: lack or excessive? !! WARNING

  // width params with bank idx (used in prefetcher / ctrl unit)
  lazy val fullAddressBits = edgeOut.bundle.addressBits
  lazy val fullTagBits = fullAddressBits - setBits - offsetBits
  // width params without bank idx (used in slice)
  lazy val addressBits = fullAddressBits - bankBits
  lazy val tagBits = fullTagBits - bankBits

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

  def getPPN(x: UInt): UInt = {
    x(x.getWidth - 1, pageOffsetBits)
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
trait HasTlBundleParameters{
  val nSets = 256
  val pageSize = 4 * 1024
  val VAddrBits = 39
  val blockOffBits = 6
  val nBlockBytes = 64

  val setBytes = nSets * nBlockBytes
  val aliasBitsOptSel = if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None

  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField(),
    VaddrField(VAddrBits - blockOffBits), 
  ) ++ aliasBitsOptSel.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Seq(
    IsKeywordField()
  )

  //Icache 
  val reqFieldsL1i: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField()
  ) ++ aliasBitsOptSel.map(AliasField)

  val l1dTlBundleParameters = TLBundleParameters(
    addressBits = 36,
    dataBits    = 256,
    sourceBits  = 7,
    sinkBits    = 8,
    sizeBits    = 3,
    requestFields = reqFields,
    echoFields = echoFields,
    responseFields = Nil,
    hasBCE = true)

  val l1iTlBundleParameters = TLBundleParameters(
    addressBits = 36,
    dataBits    = 256,
    sourceBits  = 7,
    sinkBits    = 8,
    sizeBits    = 3,
    requestFields = reqFieldsL1i,
//    requestFields = reqFields,
    echoFields = Nil,
    responseFields = Nil,
    hasBCE = false)

  val ptwTlBundleParameters = TLBundleParameters(
    addressBits = 36,
    dataBits    = 256,
    sourceBits  = 7,
    sinkBits    = 8,
    sizeBits    = 3,
    requestFields =  Seq(ReqSourceField()),
//    requestFields = reqFields,
    echoFields = Nil,
    responseFields = Nil,
    hasBCE = false)

  def getBank(x: UInt): UInt = {
    (LookupTree(x, List(
      "b100".U -> 0x1.U, //0001
      "b101".U -> 0x2.U, //0010
      "b110".U -> 0x4.U, //0100
      "b111".U -> 0x8.U, //1000
    ))).asUInt
  }

  def getStall(x: UInt, y:UInt, z:UInt, last:UInt, hint:UInt): UInt = {
   /*
    line-up for each x/y/z from 4 slices MUST follow rules in order:
    1. any slice is last beat, go first
    2. no slice is last beat, then any slcie has hint 3 cycles before should go
    3. no slice is last beat + no slice has hint 3 cycles before, then go with priority of slice0->4 

    slice/slave       x(l1d)      y(l1i)     z(ptw)
     0              *_x_0         *_y_0     *_z_0  
     1              *_x_1         *_y_1     *_z_1  
     2              *_x_2         *_y_2     *_z_2  
     3              *_x_3         *_y_3     *_z_3  
     */
    val stall_r = Wire(UInt(4.W))

    //grant request of 2nd beats
    val x_last = x & last 
    val y_last = y & last
    val z_last = z & last
    //grant request of hint 3 cycles before, only for x(l1d)
    val x_hint = x & hint & ~x_last
    //grant request which is stalled last cycle
    val x_wait = x & stall_r & ~x_hint & ~x_last
    val y_wait = y & stall_r & ~y_last
    val z_wait = z & stall_r & ~z_last
    //grant request of not last or hint
    val x_norm = x & ~x_hint & ~x_last & ~x_wait
    val y_norm = y & ~y_last & ~y_wait
    val z_norm = z & ~z_last & ~z_wait

    //to x(l1d) line up 
    val norm_x_0 = x_norm(0) && (x_last(3,0).orR || x_hint(3,0).orR || x_wait(3,0).orR)
    val norm_x_1 = x_norm(1) && (x_last(3,0).orR || x_hint(3,0).orR || x_wait(3,0).orR || x_norm(0))
    val norm_x_2 = x_norm(2) && (x_last(3,0).orR || x_hint(3,0).orR || x_wait(3,0).orR || x_norm(1,0).orR)
    val norm_x_3 = x_norm(3) && (x_last(3,0).orR || x_hint(3,0).orR || x_wait(3,0).orR || x_norm(2,0).orR)

    val wait_x_0 = x_wait(0) && (x_last(3,0).orR || x_hint(3,0).orR)
    val wait_x_1 = x_wait(1) && (x_last(3,0).orR || x_hint(3,0).orR || x_wait(0))
    val wait_x_2 = x_wait(2) && (x_last(3,0).orR || x_hint(3,0).orR || x_wait(1,0).orR)
    val wait_x_3 = x_wait(3) && (x_last(3,0).orR || x_hint(3,0).orR || x_wait(2,0).orR)

    val hint_x_0 = x_hint(0) &&  x_last(3,0).orR
    val hint_x_1 = x_hint(1) && (x_last(3,0).orR || x_hint(0))
    val hint_x_2 = x_hint(2) && (x_last(3,0).orR || x_hint(1,0).orR)
    val hint_x_3 = x_hint(3) && (x_last(3,0).orR || x_hint(2,0).orR)

    val last_x_0 = false.B
    val last_x_1 = x_last(1) && x_last(0)
    val last_x_2 = x_last(2) && x_last(1,0).orR
    val last_x_3 = x_last(3) && x_last(2,0).orR

    val stall_x_0 = last_x_0 || hint_x_0 || wait_x_0 || norm_x_0 
    val stall_x_1 = last_x_1 || hint_x_1 || wait_x_1 || norm_x_1
    val stall_x_2 = last_x_2 || hint_x_2 || wait_x_2 || norm_x_2
    val stall_x_3 = last_x_3 || hint_x_3 || wait_x_3 || norm_x_3
    //to y(l1i) line up
    val norm_y_0 = y_norm(0) && (y_last(3,0).orR || y_wait(3,0).orR)  
    val norm_y_1 = y_norm(1) && (y_last(3,0).orR || y_wait(3,0).orR || y_norm(0)) 
    val norm_y_2 = y_norm(2) && (y_last(3,0).orR || y_wait(3,0).orR || y_norm(1,0).orR)
    val norm_y_3 = y_norm(3) && (y_last(3,0).orR || y_wait(3,0).orR || y_norm(2,0).orR)

    val wait_y_0 = y_wait(0) &&  y_last(3,0).orR
    val wait_y_1 = y_wait(1) && (y_last(3,0).orR || y_wait(0))
    val wait_y_2 = y_wait(2) && (y_last(3,0).orR || y_wait(1,0).orR)
    val wait_y_3 = y_wait(3) && (y_last(3,0).orR || y_wait(2,0).orR)

    val last_y_0 = false.B
    val last_y_1 = y_last(1) && y_last(0)
    val last_y_2 = y_last(2) && y_last(1,0).orR
    val last_y_3 = y_last(3) && y_last(2,0).orR

    val stall_y_0 = last_y_0 || wait_y_0 || norm_y_0 
    val stall_y_1 = last_y_1 || wait_y_1 || norm_y_1
    val stall_y_2 = last_y_2 || wait_y_2 || norm_y_2
    val stall_y_3 = last_y_3 || wait_y_3 || norm_y_3
    //to z(ptw) line up
    val norm_z_0 = z_norm(0) && (z_last(3,0).orR || z_wait(3,0).orR)  
    val norm_z_1 = z_norm(1) && (z_last(3,0).orR || z_wait(3,0).orR || z_norm(0)) 
    val norm_z_2 = z_norm(2) && (z_last(3,0).orR || z_wait(3,0).orR || z_norm(1,0).orR)
    val norm_z_3 = z_norm(3) && (z_last(3,0).orR || z_wait(3,0).orR || z_norm(2,0).orR)

    val wait_z_0 = z_wait(0) &&  z_last(3,0).orR
    val wait_z_1 = z_wait(1) && (z_last(3,0).orR || z_wait(0))
    val wait_z_2 = z_wait(2) && (z_last(3,0).orR || z_wait(1,0).orR)
    val wait_z_3 = z_wait(3) && (z_last(3,0).orR || z_wait(2,0).orR)

    val last_z_0 = false.B
    val last_z_1 = z_last(1) && z_last(0)
    val last_z_2 = z_last(2) && z_last(1,0).orR
    val last_z_3 = z_last(3) && z_last(2,0).orR

    val stall_z_0 = last_z_0 || wait_z_0 || norm_z_0
    val stall_z_1 = last_z_1 || wait_z_1 || norm_z_1
    val stall_z_2 = last_z_2 || wait_z_2 || norm_z_2
    val stall_z_3 = last_z_3 || wait_z_3 || norm_z_3

    //final line up- each slice may go to x/y/z, so combine them all 
    val stall_0 = stall_x_0 || stall_y_0 || stall_z_0
    val stall_1 = stall_x_1 || stall_y_1 || stall_z_1
    val stall_2 = stall_x_2 || stall_y_2 || stall_z_2
    val stall_3 = stall_x_3 || stall_y_3 || stall_z_3

    val stall = VecInit(Seq(stall_0, stall_1, stall_2, stall_3)).asUInt
    stall_r := RegNext(stall)
    Reverse(stall)
  }

} //end of HasTlBundleParameters

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

  val pf_recv_node: Option[BundleBridgeSink[PrefetchRecv]] = prefetchOpt match {
    case Some(_: PrefetchReceiverParams) =>
      Some(BundleBridgeSink(Some(() => new PrefetchRecv)))
    case _ => None
  }
  val tpmeta_source_node = prefetchOpt match {
    case Some(param: PrefetchReceiverParams) =>
      if (param.hasTPPrefetcher) Some(BundleBridgeSource(() => DecoupledIO(new TPmetaReq))) else None
    case _ => None
  }
  val tpmeta_sink_node = prefetchOpt match {
    case Some(param: PrefetchReceiverParams) =>
      if (param.hasTPPrefetcher) Some(BundleBridgeSink(Some(() => ValidIO(new TPmetaResp)))) else None
    case _ => None
  }

  class CoupledL2Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasTlBundleParameters{
    val banks = node.in.size
    val bankBits = if (banks == 1) 0 else log2Up(banks)
    val io = IO(new Bundle {
    //  val l2_hint = Valid(UInt(32.W))
      val l2_hint = ValidIO(new L2ToL1Hint())
      val debugTopDown = new Bundle {
        val robHeadPaddr = Vec(cacheParams.hartIds.length, Flipped(Valid(UInt(36.W))))
        val l2MissMatch = Vec(cacheParams.hartIds.length, Output(Bool()))
      }
      //l1i/l1d/ptw inteface with L2
      val l1iBus = Flipped(TLBundle(l1iTlBundleParameters))
      val ptwBus = Flipped(TLBundle(ptwTlBundleParameters))
      val l1dBus = Flipped(TLBundle(l1dTlBundleParameters))

    })

     dontTouch(io.l1dBus)
    dontTouch(io.l1iBus)
    dontTouch(io.ptwBus)

    /*
     L2 Arbiter of A channel
     1. from 3 masters(l1d/l1i/ptw) to 4 slaves(L2 4 slices) 
     2. fix priority : l1d > ptw > l1i (TODO: ptw ?? l1i who first)
     3. address[7:6] as 0~3 bank index for interleave
     4. valid/ready protocal 
     */
    val reqPriBank = Wire(Vec(4, UInt(3.W)))
    val reqValidsBank = Wire(Vec(4, UInt(3.W)))
    val coreReq = Wire(Vec(4, new TLBundle(l1dTlBundleParameters)))

    coreReq(0) := 0.U.asTypeOf(new TLBundle(l1dTlBundleParameters))
    coreReq(1) := 0.U.asTypeOf(new TLBundle(l1dTlBundleParameters))
    coreReq(2) := 0.U.asTypeOf(new TLBundle(l1dTlBundleParameters))
    coreReq(3) := 0.U.asTypeOf(new TLBundle(l1dTlBundleParameters))

    for (i <- 0 to 3) {
      reqPriBank(i) := VecInit(Seq(
        io.l1dBus.a.valid && (io.l1dBus.a.bits.address(7,6)=== i.U),
        io.ptwBus.a.valid && (io.ptwBus.a.bits.address(7,6)=== i.U),
        io.l1iBus.a.valid && (io.l1iBus.a.bits.address(7,6)=== i.U)
      )).asUInt
    }

    //reqValidsBank is one-hot signal bit[2:0] = {l1i, ptw, l1d} -> priority l1d > ptw > l1i 
    for (i <- 0 to 3) {
      reqValidsBank(i) := VecInit(Seq(
        reqPriBank(i)(0),
        reqPriBank(i)(1) && ~reqPriBank(i)(0),
        reqPriBank(i)(2) && ~reqPriBank(i)(1) && ~reqPriBank(i)(0)
      )).asUInt
    }


    io.ptwBus.a.ready := !Cat(reqPriBank.map {req => req(1,0).andR}).orR
    io.l1iBus.a.ready := !Cat(reqPriBank.map {req => req(1,0).orR && req(2)}).orR


    /*    io.l1iBus.a.ready := !(reqPriBank(0)(1,0).andR |
                           reqPriBank(1)(1,0).andR |
                           reqPriBank(2)(1,0).andR |
                           reqPriBank(3)(1,0).andR)


    io.ptwBus.a.ready := !(reqPriBank(0)(1,0).orR && reqPriBank(0)(2) |
                           reqPriBank(1)(1,0).orR && reqPriBank(1)(2) |
                           reqPriBank(2)(1,0).orR && reqPriBank(2)(2) |
                           reqPriBank(3)(1,0).orR && reqPriBank(3)(2)) 
 */
    /*
     align the l1i/ptw A channel in case has user defined area
     */
    val l1iReq = Wire(new TLBundle(l1dTlBundleParameters))
    l1iReq := 0.U.asTypeOf(new TLBundle(l1dTlBundleParameters))
    l1iReq.a.bits.address := io.l1iBus.a.bits.address
    l1iReq.a.bits.opcode := io.l1iBus.a.bits.opcode
    l1iReq.a.bits.param := io.l1iBus.a.bits.param
    l1iReq.a.bits.size:= io.l1iBus.a.bits.size
    l1iReq.a.bits.source:= io.l1iBus.a.bits.source
    l1iReq.a.bits.mask:= io.l1iBus.a.bits.mask
    l1iReq.a.bits.data:= io.l1iBus.a.bits.data
    l1iReq.a.bits.corrupt:= io.l1iBus.a.bits.corrupt
    l1iReq.a.bits.user.lift(PrefetchKey).foreach(_ := io.l1iBus.a.bits.user.lift(PrefetchKey).getOrElse(false.B))
    l1iReq.a.bits.user.lift(AliasKey).foreach(_ := io.l1iBus.a.bits.user.lift(AliasKey).getOrElse(false.B))
    l1iReq.a.bits.user.lift(utility.ReqSourceKey).foreach(_ := io.l1iBus.a.bits.user.lift(utility.ReqSourceKey).getOrElse(false.B))
    l1iReq.a.bits.user.lift(VaddrKey).foreach(_ := 0.U)

    val ptwReq = Wire(new TLBundle(l1dTlBundleParameters))
    ptwReq := 0.U.asTypeOf(new TLBundle(l1dTlBundleParameters))
    ptwReq.a.bits.address := io.ptwBus.a.bits.address
    ptwReq.a.bits.opcode := io.ptwBus.a.bits.opcode
    ptwReq.a.bits.param := io.ptwBus.a.bits.param
    ptwReq.a.bits.size:= io.ptwBus.a.bits.size
    ptwReq.a.bits.source:= io.ptwBus.a.bits.source
    ptwReq.a.bits.mask:= io.ptwBus.a.bits.mask
    ptwReq.a.bits.data:= io.ptwBus.a.bits.data
    ptwReq.a.bits.corrupt:= io.ptwBus.a.bits.corrupt
    ptwReq.a.bits.user.lift(utility.ReqSourceKey).foreach(_ := io.ptwBus.a.bits.user.lift(utility.ReqSourceKey).getOrElse(false.B))
    ptwReq.a.bits.user.lift(AliasKey).foreach(_ := 0.U)
    ptwReq.a.bits.user.lift(VaddrKey).foreach(_ := 0.U)
    ptwReq.a.bits.user.lift(VaddrKey).foreach(_ := 0.U)

    val releaseReq = getBank(Cat(io.l1dBus.c.valid,io.l1dBus.c.bits.address(7,6)))
    val grantAckReq = getBank(Cat(io.l1dBus.e.valid,io.l1dBus.e.bits.sink(7,6)))

    //A/C/E cahnnel connect 
    coreReq.zipWithIndex.foreach{ 
      case(req,i) =>
        req.a.valid := reqValidsBank(i).orR
        req.a.bits := ParallelPriorityMux (reqValidsBank(i), Seq(io.l1dBus.a.bits, ptwReq.a.bits, l1iReq.a.bits))
        req.a.bits.source := ParallelPriorityMux (reqValidsBank(i),
          Seq(
            io.l1dBus.a.bits.source(6,0),
            ptwReq.a.bits.source(6,0) + "d80".U,
            l1iReq.a.bits.source(6,0) + "d64".U))

        req.c.valid := releaseReq(i)
        req.c.bits := io.l1dBus.c.bits

        req.e.valid := grantAckReq(i)
        req.e.bits := io.l1dBus.e.bits
        req.e.bits.sink := Cat("b00".U(2.W), io.l1dBus.e.bits.sink(5,0))
    }

    // Display info
    val sizeBytes = cacheParams.toCacheParams.capacity.toDouble
    def sizeBytesToStr(sizeBytes: Double): String = sizeBytes match {
      case _ if sizeBytes >= 1024 * 1024 => (sizeBytes / 1024 / 1024) + "MB"
      case _ if sizeBytes >= 1024        => (sizeBytes / 1024) + "KB"
      case _                            => "B"
    }
    val sizeStr = sizeBytesToStr(sizeBytes)
    val prefetch = "prefetch: " + cacheParams.prefetch
    println(s"====== Inclusive ${cacheParams.name} ($sizeStr * $banks-bank) $prefetch ======")
    println(s"bankBits: ${bankBits}")
    println(s"replacement: ${cacheParams.replacement}")
    println(s"replace policy: ${cacheParams.releaseData}")
    println(s"sets:${cacheParams.sets} ways:${cacheParams.ways} blockBytes:${cacheParams.blockBytes}")
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
          println(s"\t${i} <= ${c.name};" +
            s"\tsourceRange: ${c.sourceId.start}~${c.sourceId.end}")
      }
    }

    // connection between prefetcher and the slices
    val pftParams: Parameters = p.alterPartial {
      case EdgeInKey => node.in.head._2
      case EdgeOutKey => node.out.head._2
      case BankBitsKey => bankBits
    }
    val prefetcher = prefetchOpt.map(_ => Module(new Prefetcher()(pftParams)))
    val prefetchTrains = prefetchOpt.map(_ => Wire(Vec(banks, DecoupledIO(new PrefetchTrain()(pftParams)))))
    val prefetchResps = prefetchOpt.map(_ => Wire(Vec(banks, DecoupledIO(new PrefetchResp()(pftParams)))))
    val prefetchReqsReady = WireInit(VecInit(Seq.fill(banks)(false.B)))
    prefetchOpt.foreach {
      _ =>
        fastArb(prefetchTrains.get, prefetcher.get.io.train, Some("prefetch_train"))
        prefetcher.get.io.req.ready := Cat(prefetchReqsReady).orR
        fastArb(prefetchResps.get, prefetcher.get.io.resp, Some("prefetch_resp"))
    }
    pf_recv_node match {
      case Some(x) =>
        prefetcher.get.io.recv_addr.valid := x.in.head._1.addr_valid
        prefetcher.get.io.recv_addr.bits.addr := x.in.head._1.addr
        prefetcher.get.io.recv_addr.bits.pfSource := x.in.head._1.pf_source
        prefetcher.get.io_l2_pf_en := x.in.head._1.l2_pf_en
      case None =>
        prefetcher.foreach{
          p =>
            p.io.recv_addr := 0.U.asTypeOf(p.io.recv_addr)
            p.io_l2_pf_en := false.B
        }
    }

    tpmeta_source_node match {
      case Some(x) =>
        x.out.head._1 <> prefetcher.get.tpio.tpmeta_port.get.req
      case None =>
    }
    tpmeta_sink_node match {
      case Some(x) =>
        prefetcher.get.tpio.tpmeta_port.get.resp <> x.in.head._1
      case None =>
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
    def bank_eq(set: UInt, bankId: Int, bankBits: Int): Bool = {
      if(bankBits == 0) true.B else set(bankBits - 1, 0) === bankId.U
    }

    def RegNextN[T <: Data](data: T, n: Int): T = {
      if(n == 1)
        RegNext(data)
      else
        RegNextN(data, n - 1)
    }

    val hint_chosen = Wire(UInt(node.in.size.W))
    val hint_fire = Wire(Bool())
    val release_sourceD_condition = Wire(Vec(node.in.size, Bool()))

    val slices = node.in.zip(node.out).zipWithIndex.map {
      case (((in, edgeIn), (out, edgeOut)), i) =>
        require(in.params.dataBits == out.params.dataBits)
        val rst_L2 = reset
        val slice = withReset(rst_L2) {
          Module(new Slice()(p.alterPartial {
            case EdgeInKey  => edgeIn
            case EdgeOutKey => edgeOut
            case BankBitsKey => bankBits
            case SliceIdKey => i
          }))
        }
        val sourceD_can_go = RegNextN(!hint_fire || i.U === OHToUInt(hint_chosen), hintCycleAhead - 1)
        release_sourceD_condition(i) := sourceD_can_go && !slice.io.in.d.valid
//        slice.io.in <> in
        if(enableHintGuidedGrant) {
          // If the hint of slice X is selected in T cycle, then in T + 3 cycle we will try our best to select the grant of slice X.
          // If slice X has no grant in T + 3 cycle, it means that the hint of T cycle is wrong, so relax the restriction on grant selection.
          // Timing will be worse if enabled
          in.d.valid := slice.io.in.d.valid && (sourceD_can_go || Cat(release_sourceD_condition).orR)
          slice.io.in.d.ready := in.d.ready && (sourceD_can_go || Cat(release_sourceD_condition).orR)
        }
        in.b.bits.address := restoreAddress(slice.io.in.b.bits.address, i)
        out <> slice.io.out
        out.a.bits.address := restoreAddress(slice.io.out.a.bits.address, i)
        out.c.bits.address := restoreAddress(slice.io.out.c.bits.address, i)
        slice.io.sliceId := i.U

        slice.io.prefetch.zip(prefetcher).foreach {
          case (s, p) =>
            s.req.valid := p.io.req.valid && bank_eq(p.io.req.bits.set, i, bankBits)
            s.req.bits := p.io.req.bits
            prefetchReqsReady(i) := s.req.ready && bank_eq(p.io.req.bits.set, i, bankBits)
            val train = Pipeline(s.train)
            val resp = Pipeline(s.resp)
            prefetchTrains.get(i) <> train
            prefetchResps.get(i) <> resp
            // restore to full address
            if(bankBits != 0){
              val train_full_addr = Cat(
                train.bits.tag, train.bits.set, i.U(bankBits.W), 0.U(offsetBits.W)
              )
              val (train_tag, train_set, _) = s.parseFullAddress(train_full_addr)
              val resp_full_addr = Cat(
                resp.bits.tag, resp.bits.set, i.U(bankBits.W), 0.U(offsetBits.W)
              )
              val (resp_tag, resp_set, _) = s.parseFullAddress(resp_full_addr)
              prefetchTrains.get(i).bits.tag := train_tag
              prefetchTrains.get(i).bits.set := train_set
              prefetchResps.get(i).bits.tag := resp_tag
              prefetchResps.get(i).bits.set := resp_set
            }
        }

        slice
    }

        /*
     L2 Arbiter of D channel  
     1. from 4 sink(L2 4 slices to 3 source(l1d/l1i/ptw) 
     2. D channel use fix priority : slice0 -> slice1 > slice2 > slice3 
     3. use source range to idenify source
        l1d: sourceRange 0~35
        lii: sourceRange 64~78
        ptw: sourceRange 80~87
     4. use valid/ready protocal 
     */
    for (i <- 0 to 3) {
      slices(i).io.in.a <> coreReq(i).a
      slices(i).io.in.c <> coreReq(i).c
      slices(i).io.in.e <> coreReq(i).e

    }
/*    slices(0).io.in.a <> coreReq(0).a
    slices(0).io.in.c <> coreReq(0).c
    slices(0).io.in.e <> coreReq(0).e

    slices(1).io.in.a <> coreReq(1).a
    slices(1).io.in.c <> coreReq(1).c
    slices(1).io.in.e <> coreReq(1).e

    slices(2).io.in.a <> coreReq(2).a
    slices(2).io.in.c <> coreReq(2).c
    slices(2).io.in.e <> coreReq(2).e

    slices(3).io.in.a <> coreReq(3).a
    slices(3).io.in.c <> coreReq(3).c
    slices(3).io.in.e <> coreReq(3).e
 */
    //A ready
    io.l1dBus.a.ready := Cat(slices.zipWithIndex.map{
      case(s,i) => s.io.in.a.ready && reqValidsBank(i)(0)}).orR
    io.l1dBus.c.ready := Cat(slices.zipWithIndex.map{
      case(s,i) => s.io.in.c.ready && releaseReq(i)}).orR
    io.l1iBus.a.ready := Cat(slices.zipWithIndex.map{
      case(s,i) => s.io.in.a.ready && reqValidsBank(i)(2)}).orR
    io.ptwBus.a.ready := Cat(slices.zipWithIndex.map{
      case(s,i) => s.io.in.a.ready && reqValidsBank(i)(1)}).orR

/*  io.l1dBus.a.ready :=
    reqValidsBank0(0) & slices(0).io.in.a.ready |
    reqValidsBank1(0) & slices(1).io.in.a.ready |
    reqValidsBank2(0) & slices(2).io.in.a.ready |
    reqValidsBank3(0) & slices(3).io.in.a.ready

    io.l1iBus.a.ready :=
    reqValidsBank0(1) & slices(0).io.in.a.ready |
    reqValidsBank1(1) & slices(1).io.in.a.ready |
    reqValidsBank2(1) & slices(2).io.in.a.ready |
    reqValidsBank3(1) & slices(3).io.in.a.ready

    io.ptwBus.a.ready :=
    reqValidsBank0(2) & slices(0).io.in.a.ready |
    reqValidsBank1(2) & slices(1).io.in.a.ready |
    reqValidsBank2(2) & slices(2).io.in.a.ready |
    reqValidsBank3(2) & slices(3).io.in.a.ready
    //C ready
    io.l1dBus.c.ready :=
    releaseReq(0) & slices(0).io.in.c.ready |
    releaseReq(1) & slices(1).io.in.c.ready |
    releaseReq(2) & slices(2).io.in.c.ready |
    releaseReq(3) & slices(3).io.in.c.ready
 */

    val l1dGrantValids = slices.map{
      slice => slice.io.in.d.valid && (slice.io.in.d.bits.source(6,0) <= "d35".U)}.asUInt
    val l1iGrantValids = slices.map{
      slice =>
      slice.io.in.d.valid && (slice.io.in.d.bits.source(6,0) <= "d78".U) && (slice.io.in.d.bits.source(6,0) >= "d64".U)}.asUInt
    val ptwGrantValids = slices.map{
      slice =>
      slice.io.in.d.valid && (slice.io.in.d.bits.source(6,0) <= "d87".U) && (slice.io.in.d.bits.source(6,0) >= "d80".U)}.asUInt



    //l1d
/*    val l1dGrantValids = VecInit(Seq(
      slices(0).io.in.d.valid && (slices(0).io.in.d.bits.source(6,0) <= "d35".U),
      slices(1).io.in.d.valid && (slices(1).io.in.d.bits.source(6,0) <= "d35".U),
      slices(2).io.in.d.valid && (slices(2).io.in.d.bits.source(6,0) <= "d35".U),
      slices(3).io.in.d.valid && (slices(3).io.in.d.bits.source(6,0) <= "d35".U),
    )).asUInt


    //l1i
    val l1iGrantValids = VecInit(Seq(
      slices(0).io.in.d.valid && (slices(0).io.in.d.bits.source(6,0) <= "d78".U) && (slices(0).io.in.d.bits.source(6,0) >= "d64".U),
      slices(1).io.in.d.valid && (slices(1).io.in.d.bits.source(6,0) <= "d78".U) && (slices(1).io.in.d.bits.source(6,0) >= "d64".U),
      slices(2).io.in.d.valid && (slices(2).io.in.d.bits.source(6,0) <= "d78".U) && (slices(2).io.in.d.bits.source(6,0) >= "d64".U),
      slices(3).io.in.d.valid && (slices(3).io.in.d.bits.source(6,0) <= "d78".U) && (slices(3).io.in.d.bits.source(6,0) >= "d64".U),
    )).asUInt
    //ptw
    val ptwGrantValids = VecInit(Seq(
      slices(0).io.in.d.valid && (slices(0).io.in.d.bits.source(6,0) <= "d87".U) && (slices(0).io.in.d.bits.source(6,0) >= "d80".U),
      slices(1).io.in.d.valid && (slices(1).io.in.d.bits.source(6,0) <= "d87".U) && (slices(1).io.in.d.bits.source(6,0) >= "d80".U),
      slices(2).io.in.d.valid && (slices(2).io.in.d.bits.source(6,0) <= "d87".U) && (slices(2).io.in.d.bits.source(6,0) >= "d80".U),
      slices(3).io.in.d.valid && (slices(3).io.in.d.bits.source(6,0) <= "d87".U) && (slices(3).io.in.d.bits.source(6,0) >= "d80".U),
    )).asUInt
 */
    /*Stall
     1. fix priority slice0->slice1->slice2->slice3
     2. Any slice at last beats of grant(GrantData) will stall other higher priority slices
     */
    val grant_last = slices.map{ slice => {
                       val (_, last, _, _) = node.in.head._2.count(slice.io.in.d)
                       slice.io.in.d.valid && last &&
                       ((slice.io.in.d.bits.opcode === GrantData) || (slice.io.in.d.bits.opcode === AccessAckData))
                      }}.asUInt
    val hintValidsPre = slices.map{slice => slice.io.l1Hint.valid}.asUInt
    val hintValids1 = RegNext(hintValidsPre)
    val hintValids2 = RegNext(hintValids1)
    val hintValids  = RegNext(hintValids2)

    val hintValidsPreRev = Reverse(slices.map{slice => slice.io.l1Hint.valid}.asUInt)
    val hintValids1Rev = RegNext(hintValidsPreRev)
    val hintValids2Rev = RegNext(hintValids1Rev)
    val hintValidsRev  = RegNext(hintValids2Rev)





    //    val hintValids = RegNextN(Cat(slices.map(_.io.l1Hint.valid)).asUInt, hintCycleAhead -1)

    //slice0 stall becuase of 2nd beats of slices1/2/3
/*    val l1dGrantFireStall0 = l1dGrantValids(0) && ( l1dGrantValids(1) && grant_last(1) ||
                                                    l1dGrantValids(2) && grant_last(2) ||
                                                    l1dGrantValids(3) && grant_last(3)) 
    val l1iGrantFireStall0 = l1iGrantValids(0) && ( l1iGrantValids(1) && grant_last(1)  ||
                                                    l1iGrantValids(2) && grant_last(2)  ||
                                                    l1iGrantValids(3) && grant_last(3)) 
    val ptwGrantFireStall0 = ptwGrantValids(0) && ( ptwGrantValids(1) && grant_last(1)  ||
                                                    ptwGrantValids(2) && grant_last(2)  ||
                                                    ptwGrantValids(3) && grant_last(3)) 

    //slice1 stall becuase of 2nd beats of other slice2/3
    val l1dGrantFireStall1 = l1dGrantValids(1) && ( l1dGrantValids(2) && grant_last(2) ||
                                                    l1dGrantValids(3) && grant_last(3))
    val l1iGrantFireStall1 = l1iGrantValids(1) && ( l1iGrantValids(2) && grant_last(2) ||
                                                    l1iGrantValids(3) && grant_last(3))
    val ptwGrantFireStall1 = ptwGrantValids(1) && ( ptwGrantValids(2) && grant_last(2) ||
                                                    ptwGrantValids(3) && grant_last(3))

    //slice2 stall becuase of 2nd beats of other slice3
    val l1dGrantFireStall2 =l1dGrantValids(2) && (l1dGrantValids(3) && grant_last(3))
    val l1iGrantFireStall2 =l1iGrantValids(2) && (l1iGrantValids(3) && grant_last(3))
    val ptwGrantFireStall2 =ptwGrantValids(2) && (ptwGrantValids(3) && grant_last(3))

    //combine stall to all sinks (l1d/l1i/ptw)
    val grantFireStall0 = l1dGrantFireStall0 || l1iGrantFireStall0 || ptwGrantFireStall0
    val grantFireStall1 = l1dGrantFireStall1 || l1iGrantFireStall1 || ptwGrantFireStall1
    val grantFireStall2 = l1dGrantFireStall2 || l1iGrantFireStall2 || ptwGrantFireStall2
 */
//    val stall = getStall( l1dGrantValids, l1iGrantValids, ptwGrantValids, grant_last, hintValids)
    val stall = getStall(
      Reverse(l1dGrantValids),
      Reverse(l1iGrantValids),
      Reverse(ptwGrantValids),
      Reverse(grant_last),
      hintValidsRev)



/*    val sliceStallSlice1 =
      l1dGrantValids(1) && ~grant_last(1) && l1dGrantValids(0) |
      l1iGrantValids(1) && ~grant_last(1) && l1iGrantValids(0) |
      ptwGrantValids(1) && ~grant_last(1) && ptwGrantValids(0) 

    val sliceStallSlice2 =
      l1dGrantValids(2) && ~grant_last(2) && l1dGrantValids(1,0).orR |
      l1iGrantValids(2) && ~grant_last(2) && l1iGrantValids(1,0).orR |
      ptwGrantValids(2) && ~grant_last(2) && ptwGrantValids(1,0).orR 

    val sliceStallSlice3 =
      l1dGrantValids(3) && ~grant_last(3) && l1dGrantValids(2,0).orR |
      l1iGrantValids(3) && ~grant_last(3) && l1iGrantValids(2,0).orR |
      ptwGrantValids(3) && ~grant_last(3) && ptwGrantValids(2,0).orR 

//    val sliceStall = Cat (  grantFireStall0,
//                            sliceStallSlice1 || grantFireStall1,
//                            sliceStallSlice2 || grantFireStall2,
//                            sliceStallSlice3 )



    val sliceStall0 = grantFireStall0
    val sliceStall1 = sliceStallSlice1 || grantFireStall1
    val sliceStall2 = sliceStallSlice2 || grantFireStall2 
    val sliceStall3 = sliceStallSlice3

//    slices.zipWithIndex.foreach {
//      case (slice,i) => slice.io.in.d.ready := ~sliceStall(i)
//    }
 */
    slices(0).io.in.d.ready := ~stall(0)
    slices(1).io.in.d.ready := ~stall(1)
    slices(2).io.in.d.ready := ~stall(2)
    slices(3).io.in.d.ready := ~stall(3)

//    slices(0).io.in.d.ready := ~sliceStall0
//    slices(1).io.in.d.ready := ~sliceStall1
//    slices(2).io.in.d.ready := ~sliceStall2
//    slices(3).io.in.d.ready := ~sliceStall3


//    val l1dGrantPri = Wire(UInt(4.W))
//    val l1iGrantPri = Wire(UInt(4.W))
//    val ptwGrantPri = Wire(UInt(4.W))
    //final lineup
//    for (i <- 0 until 3) {
//      l1dGrantPri(i) := l1dGrantValids(i) && ~sliceStall(i)
//      l1iGrantPri(i) := l1iGrantValids(i) && ~sliceStall(i)
//      ptwGrantPri(i) := ptwGrantValids(i) && ~sliceStall(i)
//    }


    val l1dGrantPri = l1dGrantValids & ~stall
    val l1iGrantPri = l1iGrantValids & ~stall
    val ptwGrantPri = ptwGrantValids & ~stall

/*    val l1dGrantPri = VecInit(Seq(
      l1dGrantValids(0) && !sliceStall0,
      l1dGrantValids(1) && !sliceStall1,
      l1dGrantValids(2) && !sliceStall2,
      l1dGrantValids(3) && !sliceStall3 
    )).asUInt

    val l1iGrantPri = VecInit(Seq(
      l1iGrantValids(0) && !sliceStall0,
      l1iGrantValids(1) && !sliceStall1,
      l1iGrantValids(2) && !sliceStall2,
      l1iGrantValids(3) && !sliceStall3 
    )).asUInt

    val ptwGrantPri = VecInit(Seq(
      ptwGrantValids(0) && !sliceStall0,
      ptwGrantValids(1) && !sliceStall1,
      ptwGrantValids(2) && !sliceStall2,
      ptwGrantValids(3) && !sliceStall3 
    )).asUInt
 */

    //TL-D register-out for timing
    io.l1dBus.d.valid := RegNext(l1dGrantPri.orR)
    io.l1dBus.d.bits :=  RegNext(ParallelPriorityMux(l1dGrantPri, slices.map(_.io.in.d.bits)))
    io.l1dBus.d.bits.sink :=  RegNext(ParallelPriorityMux(l1dGrantPri,
      slices.zipWithIndex.map{case(slice,i) =>  Cat(i.U(2.W), slice.io.in.d.bits.sink(5,0))}))


    io.l1iBus.d.valid := RegNext(l1iGrantPri.orR)
    io.l1iBus.d.bits :=  RegNext(ParallelPriorityMux(l1iGrantPri, slices.map(_.io.in.d.bits)))
    io.l1iBus.d.bits.source :=  RegNext(ParallelPriorityMux(l1iGrantPri,
      slices.map(_.io.in.d.bits.source(6,0) - "d64".U)))

    io.ptwBus.d.valid := RegNext(ptwGrantPri.orR)
    io.ptwBus.d.bits :=  RegNext(ParallelPriorityMux(ptwGrantPri, slices.map(_.io.in.d.bits)))
    io.ptwBus.d.bits.source :=  RegNext(ParallelPriorityMux(ptwGrantPri,
      slices.map(_.io.in.d.bits.source(6,0) - "d80".U)))

    //hint
    val sliceHint = VecInit(Seq(
      slices(0).io.l1Hint.valid,
      slices(1).io.l1Hint.valid,
      slices(2).io.l1Hint.valid,
      slices(3).io.l1Hint.valid,
    )).asUInt

    io.l2_hint.valid := RegNext(sliceHint.orR)
    io.l2_hint.bits.sourceId := RegNext(ParallelPriorityMux(sliceHint, slices.map(_.io.l1Hint.bits.sourceId)))
    io.l2_hint.bits.isKeyword := RegNext(ParallelPriorityMux(sliceHint, slices.map(_.io.l1Hint.bits.isKeyword)))


    //    io.l2_hint.valid := RegNext(hintValids.orR)
//    io.l2_hint.bits := RegNext(ParallelPriorityMux(hintValids, slices.map(_.io.l1Hint.bits.sourceId)))

    //TL-D
/*    io.l1dBus.d.valid := l1dGrantPri.orR
    io.l1dBus.d.bits :=  ParallelPriorityMux(l1dGrantPri, slices.map(_.io.in.d.bits))
    io.l1dBus.d.bits.sink :=  ParallelPriorityMux(l1dGrantPri, 
      slices.zipWithIndex.map{case(slice,i) =>  Cat(i.U(2.W), slice.io.in.d.bits.sink(5,0))}) 



    io.l1iBus.d.valid := l1iGrantPri.orR
    io.l1iBus.d.bits :=  ParallelPriorityMux(l1iGrantPri, slices.map(_.io.in.d.bits))
    io.l1iBus.d.bits.source :=  ParallelPriorityMux(l1iGrantPri,
      slices.map(_.io.in.d.bits.source(6,0) - "d64".U))

    io.ptwBus.d.valid := ptwGrantPri.orR
    io.ptwBus.d.bits :=  ParallelPriorityMux(ptwGrantPri, slices.map(_.io.in.d.bits))
    io.ptwBus.d.bits.source :=  ParallelPriorityMux(ptwGrantPri, 
      slices.map(_.io.in.d.bits.source(6,0) - "d80".U))
 */
    //TL-E
    io.l1dBus.e.ready := true.B

/*    io.l1dBus.d.valid := l1dGrantPri.orR
    io.l1dBus.d.bits :=  ParallelPriorityMux(l1dGrantPri, Seq(
      slices(0).io.in.d.bits,
      slices(1).io.in.d.bits,
      slices(2).io.in.d.bits,
      slices(3).io.in.d.bits))
    io.l1dBus.d.bits.sink :=  ParallelPriorityMux(l1dGrantPri, Seq(
      Cat("b00".U(2.W), slices(0).io.in.d.bits.sink(5,0)),
      Cat("b01".U(2.W), slices(1).io.in.d.bits.sink(5,0)),
      Cat("b10".U(2.W), slices(2).io.in.d.bits.sink(5,0)),
      Cat("b11".U(2.W), slices(3).io.in.d.bits.sink(5,0))))

    io.l1iBus.d.valid := l1iGrantPri.orR
    io.l1iBus.d.bits :=  ParallelPriorityMux(l1iGrantPri, Seq(
      slices(0).io.in.d.bits,
      slices(1).io.in.d.bits,
      slices(2).io.in.d.bits,
      slices(3).io.in.d.bits))
    io.l1iBus.d.bits.source :=  ParallelPriorityMux(l1iGrantPri, Seq(
      slices(0).io.in.d.bits.source(6,0) - "d64".U,
      slices(1).io.in.d.bits.source(6,0) - "d64".U,
      slices(2).io.in.d.bits.source(6,0) - "d64".U,
      slices(3).io.in.d.bits.source(6,0) - "d64".U))

    io.ptwBus.d.valid := ptwGrantPri.orR
    io.ptwBus.d.bits :=  ParallelPriorityMux(ptwGrantPri, Seq(
      slices(0).io.in.d.bits,
      slices(1).io.in.d.bits,
      slices(2).io.in.d.bits,
      slices(3).io.in.d.bits))
    io.ptwBus.d.bits.source :=  ParallelPriorityMux(ptwGrantPri, Seq(
      slices(0).io.in.d.bits.source(6,0) - "d80".U,
      slices(1).io.in.d.bits.source(6,0) - "d80".U,
      slices(2).io.in.d.bits.source(6,0) - "d80".U,
      slices(3).io.in.d.bits.source(6,0) - "d80".U))
 */


    //TL-B
    val l1dSnoopValids = slices.map(_.io.in.b.valid).asUInt
    io.l1dBus.b.valid := l1dSnoopValids.orR
    io.l1dBus.b.bits :=  ParallelPriorityMux(l1dSnoopValids, slices.map(_.io.in.b.bits))
    io.l1dBus.b.bits.address :=  ParallelPriorityMux(l1dSnoopValids, 
      slices.zipWithIndex.map{case(slice,i) => restoreAddress(slice.io.in.b.bits.address, i)}) 

    slices(0).io.in.b.ready := true.B
    slices(1).io.in.b.ready := ~l1dSnoopValids(0)
    slices(2).io.in.b.ready := ~l1dSnoopValids(1,0).orR
    slices(3).io.in.b.ready := ~l1dSnoopValids(2,0).orR


    /*    val l1dSnoopValids = VecInit(Seq(
      slices(0).io.in.b.valid,
      slices(1).io.in.b.valid,
      slices(2).io.in.b.valid,
      slices(3).io.in.b.valid
    )).asUInt
 
    io.l1dBus.b.valid := l1dSnoopValids.orR
    io.l1dBus.b.bits :=  ParallelPriorityMux(l1dSnoopValids, Seq(
      slices(0).io.in.b.bits,
      slices(1).io.in.b.bits,
      slices(2).io.in.b.bits,
      slices(3).io.in.b.bits))
    io.l1dBus.b.bits.address :=  ParallelPriorityMux(l1dSnoopValids, Seq(
      restoreAddress(slices(0).io.in.b.bits.address, 0),
      restoreAddress(slices(1).io.in.b.bits.address, 1),
      restoreAddress(slices(2).io.in.b.bits.address, 2),
      restoreAddress(slices(3).io.in.b.bits.address, 3)))
 */







    val l1Hint_arb = Module(new Arbiter(new L2ToL1Hint, slices.size))
    val slices_l1Hint = slices.zipWithIndex.map {
      case (s, i) => Pipeline(s.io.l1Hint, depth = 1, pipe = false, name = Some(s"l1Hint_buffer_$i"))
    }
    val (client_sourceId_match_oh, client_sourceId_start) = node.in.head._2.client.clients
                                                          .map(c => {
                                                                (c.sourceId.contains(l1Hint_arb.io.out.bits.sourceId).asInstanceOf[Bool], c.sourceId.start.U)
                                                              })
                                                          .unzip
    l1Hint_arb.io.in <> VecInit(slices_l1Hint)
//    io.l2_hint.valid := l1Hint_arb.io.out.fire
//    io.l2_hint.bits.sourceId := l1Hint_arb.io.out.bits.sourceId - Mux1H(client_sourceId_match_oh, client_sourceId_start)
//    io.l2_hint.bits.isKeyword := l1Hint_arb.io.out.bits.isKeyword
    // always ready for grant hint
    l1Hint_arb.io.out.ready := true.B

    hint_chosen := l1Hint_arb.io.chosen
    hint_fire := io.l2_hint.valid

    val topDown = topDownOpt.map(_ => Module(new TopDownMonitor()(p.alterPartial {
      case EdgeInKey => node.in.head._2
      case EdgeOutKey => node.out.head._2
      case BankBitsKey => bankBits
    })))
    topDown match {
      case Some(t) =>
        t.io.msStatus.zip(slices).foreach {
          case (in, s) => in := s.io.msStatus.get
        }
        t.io.dirResult.zip(slices).foreach {
          case (res, s) => res := s.io.dirResult.get
        }
        t.io.latePF.zip(slices).foreach {
          case (in, s) => in := s.io.latePF.get
        }
        t.io.debugTopDown <> io.debugTopDown
      case None => io.debugTopDown.l2MissMatch.foreach(_ := false.B)
    }

    XSPerfAccumulate(cacheParams, "hint_fire", io.l2_hint.valid)
    val grant_fire = slices.map{ slice => {
                        val (_, _, grant_fire_last, _) = node.in.head._2.count(slice.io.in.d)
                        slice.io.in.d.fire && grant_fire_last && slice.io.in.d.bits.opcode === GrantData
                      }}
    XSPerfAccumulate(cacheParams, "grant_data_fire", PopCount(VecInit(grant_fire)))
  }

  lazy val module = new CoupledL2Imp(this)
}
