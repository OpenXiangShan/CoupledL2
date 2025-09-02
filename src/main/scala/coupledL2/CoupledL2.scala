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
import utility._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Field, Parameters}

import scala.math.max
import coupledL2.prefetch._
import huancun.{BankBitsKey, TPmetaReq, TPmetaResp}
import utility.mbist.{MbistInterface, MbistPipeline}
import utility.sram.{SramBroadcastBundle, SramHelper}

trait HasCoupledL2Parameters {
  val p: Parameters
  // val tl2tlParams: HasTLL2Parameters = p(L2ParamKey)
  def enableCHI = p(EnableCHI)
  def cacheParams = p(L2ParamKey)
  def EnableNewClint = cacheParams.EnableNewClint

  def XLEN = 64
  def blocks = cacheParams.sets * cacheParams.ways
  def blockBytes = cacheParams.blockBytes
  def blockBits = blockBytes * 8
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

  // ECC
  // tag(data)BankSplit refers to tag(data) splits before ECC encode
  // tag(data)SRAMSplit refers to tag(data) splits of tag(data)Array SRAM
  // *NOTICE*
  // tag width = 31(1 MB L2), requires padding when split
  // currently, not split tag if SRAM's split requirement cannot meet(L2 size changes)
  // encDataBank width = 137(bakSplit = 4), requires padding when extra SRAM split
  def enableECC = cacheParams.enableTagECC || cacheParams.enableDataECC
  def enableTagECC = cacheParams.enableTagECC
  def tagBankSplit = 1
  def tagSRAMSplit = 2
  def tagBankBits = tagBits / tagBankSplit
  def encTagBankBits = cacheParams.tagCode.width(tagBankBits)
  def enableTagSRAMSplit = encTagBankBits % (tagSRAMSplit / tagBankSplit) == 0
  def eccTagBankBits = encTagBankBits - tagBankBits
  def enableDataECC = cacheParams.enableDataECC
  def dataBankSplit = 4
  def dataSRAMSplit = 8
  def wordBits = 64
  def bankWords = blockBits / wordBits / dataBankSplit
  def dataBankBits = wordBits * bankWords
  def encBankBits = cacheParams.dataCode.width(dataBankBits)
  def encDataPadBits = 4 // recaculate if any split changes

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

  // Hardware Performance Monitor
  def numPCntHc: Int = 12

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
    (ZeroExt(tag, fullTagBits), set(setBits - 1, 0), offset(offsetBits - 1, 0))
  }

  def parseAddress(x: UInt): (UInt, UInt, UInt) = {
    val offset = x
    val set = offset >> (offsetBits + bankBits)
    val tag = set >> setBits
    (ZeroExt(tag, tagBits), set(setBits - 1, 0), offset(offsetBits - 1, 0))
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
    val opSeq = Seq(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, grantOp, Grant, 0.U, 0.U, 0.U, 0.U, CBOAck, CBOAck, CBOAck)
    val opToA = VecInit(opSeq)(r)
    opToA
  }

  def sizeBytesToStr(sizeBytes: Double): String = sizeBytes match {
    case _ if sizeBytes >= 1024 * 1024 => s"${sizeBytes / 1024 / 1024}MB"
    case _ if sizeBytes >= 1024        => s"${sizeBytes / 1024}KB"
    case _                            => "B"
  }

  def print_bundle_fields(fs: Seq[BundleFieldBase], prefix: String) = {
    if(fs.nonEmpty){
      println(fs.map{f => s"$prefix/${f.key.name}: (${f.data.getWidth}-bit)"}.mkString("\n"))
    }
  }

  def bank_eq(set: UInt, bankId: Int, bankBits: Int): Bool = {
    if(bankBits == 0) true.B else set(bankBits - 1, 0) === bankId.U
  }
}

abstract class CoupledL2Base(implicit p: Parameters) extends LazyModule with HasCoupledL2Parameters {

  val xfer = TransferSizes(blockBytes, blockBytes)
  val atom = TransferSizes(1, cacheParams.channelBytes.d.get)
  val access = TransferSizes(1, blockBytes)

  val pf_recv_node: Option[BundleBridgeSink[PrefetchRecv]] =
    if(hasReceiver) Some(BundleBridgeSink(Some(() => new PrefetchRecv))) else None

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

  val node = TLAdapterNode(
    clientFn = clientPortParams,
    managerFn = managerPortParams
  )

  val tpmeta_source_node = if(hasTPPrefetcher) Some(BundleBridgeSource(() => DecoupledIO(new TPmetaReq(hartIdLen, node.in.head._2.bundle.addressBits, offsetBits)))) else None
  val tpmeta_sink_node = if(hasTPPrefetcher) Some(BundleBridgeSink(Some(() => ValidIO(new TPmetaResp(hartIdLen, node.in.head._2.bundle.addressBits, offsetBits))))) else None

  abstract class BaseCoupledL2Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasPerfEvents {
    val banks = node.in.size
    val bankBits = log2Ceil(banks)
    val l2TlbParams: Parameters = p.alterPartial {
      case EdgeInKey => node.in.head._2
      case EdgeOutKey => node.out.head._2
      case BankBitsKey => bankBits
    }
    val l2ECCParams: Parameters = p.alterPartial {
      case EdgeInKey => node.in.head._2
      // case EdgeOutKey => node.out.head._2
      // case BankBitsKey => bankBits
    } // currently only EdgeInKey is used

    require(banks == node.in.size)

    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val pfCtrlFromCore = Input(new PrefetchCtrlFromCore)
    //  val l2_hint = Valid(UInt(32.W))
      val l2_hint = ValidIO(new L2ToL1Hint())
      val l2_tlb_req = new L2ToL1TlbIO(nRespDups = 1)(l2TlbParams)
      val debugTopDown = new Bundle {
        val robTrueCommit = Input(UInt(64.W))
        val robHeadPaddr = Flipped(Valid(UInt(36.W)))
        val l2MissMatch = Output(Bool())
      }
      val l2Miss = Output(Bool())
      val error = Output(new L2CacheErrorInfo()(l2ECCParams))
      val l2Flush = Option.when(cacheParams.enableL2Flush) (Input(Bool()))
      val l2FlushDone = Option.when(cacheParams.enableL2Flush) (Output(Bool()))
      val dft = Option.when(cacheParams.hasDFT)(Input(new SramBroadcastBundle))
      val dft_reset = Option.when(cacheParams.hasMbist)(Input(new DFTResetSignals()))
    })

    // Display info
    val sizeBytes = cacheParams.toCacheParams.capacity.toDouble
    val sizeStr = sizeBytesToStr(sizeBytes)
    println(s"====== Inclusive TL-${if (enableCHI) "CHI" else "TL"} ${cacheParams.name} ($sizeStr * $banks-bank)  ======")
    println(s"prefetch: ${cacheParams.prefetch}")
    println(s"bankBits: ${bankBits}")
    println(s"replacement: ${cacheParams.replacement}")
    println(s"replace policy: ${cacheParams.releaseData}")
    println(s"sets:${cacheParams.sets} ways:${cacheParams.ways} blockBytes:${cacheParams.blockBytes}")
    print_bundle_fields(node.in.head._2.bundle.requestFields, "usr")
    print_bundle_fields(node.in.head._2.bundle.echoFields, "echo")

    println(s"Cacheable:")
    node.edges.in.headOption.foreach { n =>
      n.client.clients.zipWithIndex.foreach {
        case (c, i) =>
          println(s"\t${i} <= ${c.name};" +
            s"\tsourceRange: ${c.sourceId.start}~${c.sourceId.end}")
      }
    }

    // Connection between prefetcher and the slices
    val pftParams: Parameters = p.alterPartial {
      case EdgeInKey => node.in.head._2
      case EdgeOutKey => node.out.head._2
      case BankBitsKey => bankBits
    }
    val prefetcher = prefetchOpt.map(_ => Module(new Prefetcher()(pftParams)))
    val prefetchTrains = prefetchOpt.map(_ => Wire(Vec(banks, DecoupledIO(new PrefetchTrain()(pftParams)))))
    val prefetchResps = prefetchOpt.map(_ => Wire(Vec(banks, DecoupledIO(new PrefetchResp()(pftParams)))))
    val prefetchReqsReady = WireInit(VecInit(Seq.fill(banks)(false.B)))
    io.l2_tlb_req <> DontCare // TODO: l2_tlb_req should be Option
    prefetchOpt.foreach {
      _ =>
        fastArb(prefetchTrains.get, prefetcher.get.io.train, Some("prefetch_train"))
        prefetcher.get.io.req.ready := Cat(prefetchReqsReady).orR
        prefetcher.get.hartId := io.hartId
        prefetcher.get.pfCtrlFromCore := io.pfCtrlFromCore
        fastArb(prefetchResps.get, prefetcher.get.io.resp, Some("prefetch_resp"))
        prefetcher.get.io.tlb_req <> io.l2_tlb_req
    }
    pf_recv_node match {
      case Some(x) =>
        prefetcher.get.io.recv_addr.valid := x.in.head._1.addr_valid
        prefetcher.get.io.recv_addr.bits.addr := x.in.head._1.addr
        prefetcher.get.io.recv_addr.bits.pfSource := x.in.head._1.pf_source
      case None =>
        prefetcher.foreach{
          p =>
            p.io.recv_addr := 0.U.asTypeOf(p.io.recv_addr)
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

    // ** WARNING:TODO: this depends on where the latch is
    // ** if Hint latched in slice, while D-Channel latched in XSTile
    // ** we need only [hintCycleAhead - 1] later
    val sliceAhead = hintCycleAhead - 1

    val hintChosen = Wire(UInt(banks.W))
    val hintFire = Wire(Bool())

    // if Hint indicates that this slice should fireD, yet no D resp comes out of this slice
    // then we releaseSourceD, enabling io.d.ready for other slices
    // TODO: if Hint for single slice is 100% accurate, may consider remove this
    val releaseSourceD = Wire(Vec(banks, Bool()))
    val allCanFire = (RegNextN(!hintFire, sliceAhead) && RegNextN(!hintFire, sliceAhead + 1)) || Cat(releaseSourceD).orR

    val slices = node.in.zip(node.out).zipWithIndex.map {
      case (((in, edgeIn), (out, edgeOut)), i) =>
        require(in.params.dataBits == out.params.dataBits)
        val rst_L2 = reset
        val slice = withReset(rst_L2) {
          if (enableCHI) {
            Module(new tl2chi.Slice()(p.alterPartial {
              case EdgeInKey => edgeIn
              case EdgeOutKey => edgeOut
              case BankBitsKey => bankBits
              case SliceIdKey => i
            }))
          } else {
            Module(new tl2tl.Slice()(p.alterPartial {
              case EdgeInKey => edgeIn
              case EdgeOutKey => edgeOut
              case BankBitsKey => bankBits
              case SliceIdKey => i
            }))
          }
        }
        slice.io.in <> in
        if (enableHintGuidedGrant) {
          // If the hint of slice X is selected at cycle T, then at cycle (T + 3) & (T + 4)
          // we will try our best to select the grant of slice X.
          // If slice X has no grant then, it means that the hint at cycle T is wrong,
          // so we relax the restriction on grant selection.
          val sliceCanFire = RegNextN(hintFire && i.U === hintChosen, sliceAhead) ||
            RegNextN(hintFire && i.U === hintChosen, sliceAhead + 1)

          releaseSourceD(i) := sliceCanFire && !slice.io.in.d.valid

          in.d.valid := slice.io.in.d.valid && (sliceCanFire || allCanFire)
          slice.io.in.d.ready := in.d.ready && (sliceCanFire || allCanFire)
        }
        in.b.bits.address := restoreAddress(slice.io.in.b.bits.address, i)
        slice.io.sliceId := i.U

        slice.io.error.ready := enableECC.asBool // TODO: fix the datapath as optional

        slice.io.l2Flush.foreach(_ := io.l2Flush.getOrElse(false.B))

        slice.io.prefetch.zip(prefetcher).foreach {
          case (s, p) =>
            s.req.valid := p.io.req.valid && bank_eq(Cat(p.io.req.bits.tag, p.io.req.bits.set), i, bankBits)
            s.req.bits := p.io.req.bits
            prefetchReqsReady(i) := s.req.ready && bank_eq(Cat(p.io.req.bits.tag, p.io.req.bits.set), i, bankBits)
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
            s.tlb_req.req.valid := false.B
            s.tlb_req.req.bits := DontCare
            s.tlb_req.req_kill := DontCare
            s.tlb_req.resp.ready := true.B
        }

        slice
    }

    val perfEvents = Seq(("noEvent", 0.U)) ++ slices.zipWithIndex.map {
      case (slide, slide_idx) =>
        slide.getPerfEvents.map{case (str, idx) => ("Slice" + slide_idx.toString + "_" + str, idx)}
    }.flatten
    generatePerfEvent()

    // ECC error
    if (enableECC) {
      val l2ECCArb = Module(new Arbiter(new L2CacheErrorInfo()(l2ECCParams), slices.size))
      val slices_l2ECC = slices.zipWithIndex.map {
        case (s, i) =>
          val sliceError = Wire(DecoupledIO(new L2CacheErrorInfo()(l2ECCParams)))
          sliceError := s.io.error
          sliceError.bits.address := restoreAddress(s.io.error.bits.address, i)
          sliceError
      }
      l2ECCArb.io.in <> VecInit(slices_l2ECC)
      l2ECCArb.io.out.ready := true.B
      io.error.valid := l2ECCArb.io.out.fire && l2ECCArb.io.out.bits.valid
      io.error.address := l2ECCArb.io.out.bits.address
    } else {
      io.error.valid := false.B
      io.error.address := 0.U.asTypeOf(io.error.address)
    }

    //L2 Flush Done
    io.l2FlushDone.foreach(_ :=  VecInit(slices.zipWithIndex.map { case (s, i) => s.io.l2FlushDone.getOrElse(false.B)}).reduce(_&_) )

    // Refill hint
    if (enableHintGuidedGrant) {
      // for timing consideration, hint should latch one cycle before sending to L1
      // instead of adding a Pipeline/Queue to latch here, we just set hintQueue in GrantBuf & CustomL1Hint "flow=false"
      val l1HintArb = Module(new Arbiter(new L2ToL1Hint(), slices.size))
      val slices_l1Hint = slices.zipWithIndex.map {
        case (s, i) => s.io.l1Hint
      }
      // should only Hint for DCache
      val (sourceIsDcache, dcacheSourceIdStart) = node.in.head._2.client.clients
        .filter(_.supports.probe)
        .map(c => {
          (c.sourceId.contains(l1HintArb.io.out.bits.sourceId).asInstanceOf[Bool], c.sourceId.start.U)
        }).head

      l1HintArb.io.in <> VecInit(slices_l1Hint)
      io.l2_hint.valid := l1HintArb.io.out.fire && sourceIsDcache
      io.l2_hint.bits.sourceId := l1HintArb.io.out.bits.sourceId - dcacheSourceIdStart
      io.l2_hint.bits.isKeyword := l1HintArb.io.out.bits.isKeyword
      // continuous hints can only be sent every two cycle, since GrantData takes two cycles
      l1HintArb.io.out.ready := !RegNext(io.l2_hint.valid, false.B)

      hintChosen := l1HintArb.io.chosen // ! THIS IS NOT ONE-HOT !
      hintFire := io.l2_hint.valid
    }

    // Outer interface connection
    slices.zip(node.out).zipWithIndex.foreach {
      case ((slice, (out, _)), i) =>
        slice match {
          case slice: tl2tl.Slice =>
            out <> slice.io.out
            out.a.bits.address := restoreAddress(slice.io.out.a.bits.address, i)
            out.c.bits.address := restoreAddress(slice.io.out.c.bits.address, i)
          case slice: tl2chi.Slice =>
        }
    }

    // ==================== TopDown ====================
    val topDown = topDownOpt.map(_ => Module(new TopDownMonitor()(p.alterPartial {
      case EdgeInKey => node.in.head._2
      case EdgeOutKey => node.out.head._2
      case BankBitsKey => bankBits
    })))
    topDown match {
      case Some(t) =>
        t.io.msStatus.zip(slices).foreach {
          case (in, s) =>
            s match {
              case slice: tl2tl.Slice => in := slice.io_msStatus.get
              case slice: tl2chi.Slice => in := slice.io_msStatus.get
            }
        }
        t.io.dirResult.zip(slices).foreach {
          case (res, s) => res := s.io.dirResult.get
        }
        t.io.latePF.zip(slices).foreach {
          case (in, s) => in := s.io.latePF.get
        }
        t.io.debugTopDown <> io.debugTopDown
      case None => io.debugTopDown.l2MissMatch := false.B
    }

    io.l2Miss := RegNext(slices.map(_.io.l2Miss).reduce(_ || _))

    // ==================== XSPerf Counters ====================
    val grant_data_fire = slices.map { slice =>
      val (first, _, _, _) = node.in.head._2.count(slice.io.in.d)
      slice.io.in.d.fire && first && slice.io.in.d.bits.opcode === GrantData
    }
    XSPerfAccumulate("grant_data_fire", PopCount(VecInit(grant_data_fire)))

    val hint_source = io.l2_hint.bits.sourceId

    val grant_data_source = ParallelPriorityMux(slices.map {
      s => (s.io.in.d.fire, s.io.in.d.bits.source)
    })

    val hintPipe2 = Module(new Pipeline(UInt(32.W), 2))
    hintPipe2.io.in.valid := io.l2_hint.valid
    hintPipe2.io.in.bits := hint_source
    hintPipe2.io.out.ready := true.B

    val hintPipe1 = Module(new Pipeline(UInt(32.W), 1))
    hintPipe1.io.in.valid := io.l2_hint.valid
    hintPipe1.io.in.bits := hint_source
    hintPipe1.io.out.ready := true.B

    val accurateHint = grant_data_fire.orR && hintPipe2.io.out.valid && hintPipe2.io.out.bits === grant_data_source
    XSPerfAccumulate("accurate3Hints", accurateHint)

    val okHint = grant_data_fire.orR && hintPipe1.io.out.valid && hintPipe1.io.out.bits === grant_data_source
    XSPerfAccumulate("ok2Hints", okHint)

    private val sigFromSrams = Option.when(cacheParams.hasDFT)(SramHelper.genBroadCastBundleTop())
    private val cg = Option.when(cacheParams.hasMbist)(utility.ClockGate.genTeSrc)
    if (cacheParams.hasMbist) {
      cg.get.cgen := io.dft.get.cgen
    }
    sigFromSrams.foreach { sig => sig := DontCare }
    sigFromSrams.zip(io.dft).foreach {
      case (sig, dft) =>
        if (cacheParams.hasMbist) {
          sig.ram_hold := dft.ram_hold
          sig.ram_bypass := dft.ram_bypass
          sig.ram_bp_clken := dft.ram_bp_clken
          sig.ram_aux_clk := dft.ram_aux_clk
          sig.ram_aux_ckbp := dft.ram_aux_ckbp
          sig.ram_mcp_hold := dft.ram_mcp_hold
          sig.cgen := dft.cgen
        }
        if (cacheParams.hasSramCtl) {
          sig.ram_ctl := dft.ram_ctl
        }
    }

    private val mbistPl = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "L2Cache", cacheParams.hasMbist)
    private val l2MbistIntf = if (cacheParams.hasMbist) {
      val params = mbistPl.get.nodeParams
      val intf = Some(Module(new MbistInterface(
        params = Seq(params),
        ids = Seq(mbistPl.get.childrenIds),
        name = s"MbistIntfL2",
        pipelineNum = 1
      )))
      intf.get.toPipeline.head <> mbistPl.get.mbist
      if (cacheParams.hartId == 0) mbistPl.get.registerCSV(intf.get.info, "MbistL2")
      intf.get.mbist := DontCare
      dontTouch(intf.get.mbist)
      //TODO: add mbist controller connections here
      intf
    } else {
      None
    }
  }
}
