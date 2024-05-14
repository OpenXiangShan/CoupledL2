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

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import utility.{FastArbiter, Pipeline, ParallelPriorityMux, RegNextN}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import scala.math.max
import coupledL2._
import coupledL2.tl2chi.CHIOpcode.RSPOpcodes._
import coupledL2.prefetch._
import coupledL2.utils.XSPerfAccumulate

abstract class TL2CHIL2Bundle(implicit val p: Parameters) extends Bundle
  with HasCoupledL2Parameters
  with HasCHIMsgParameters
abstract class TL2CHIL2Module(implicit val p: Parameters) extends Module
  with HasCoupledL2Parameters
  with HasCHIMsgParameters

class TL2CHICoupledL2(implicit p: Parameters) extends CoupledL2Base {

  /**
    * Make diplomacy happy:
    * To implement multi-bank L2, a BankBinder must be placed downstream of L2,
    * therefore a TLAdapterNode is implemented here.
    */
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

  val addressRange = AddressSet(0x00000000L, 0xfffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL)) // TODO: parameterize this
  val managerParameters = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = addressRange,
      regionType = RegionType.CACHED,
      supportsAcquireT = xfer,
      supportsAcquireB = xfer,
      supportsArithmetic = atom,
      supportsLogical = atom,
      supportsGet = access,
      supportsPutFull = access,
      supportsPutPartial = access,
      supportsHint = access,
      fifoId = None
    )),
    beatBytes = 32,
    minLatency = 2,
    responseFields = cacheParams.respField,
    requestKeys = cacheParams.reqKey,
    endSinkId = idsAll // TODO: Confirm this
  )
  val managerNode = TLManagerNode(Seq(managerParameters))

  val mmioBridge = LazyModule(new MMIOBridge)
  val mmioNode = mmioBridge.mmioNode

  class CoupledL2Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val banks = node.in.size
    val bankBits = log2Ceil(banks)
    val l2TlbParams: Parameters = p.alterPartial {
      case EdgeInKey => node.in.head._2
      case EdgeOutKey => node.out.head._2
      case BankBitsKey => bankBits
    }

    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val l2_hint = ValidIO(new L2ToL1Hint())
      val l2_tlb_req = new L2ToL1TlbIO(nRespDups = 1)(l2TlbParams)
      val debugTopDown = new Bundle {
        val robTrueCommit = Input(UInt(64.W))
        val robHeadPaddr = Flipped(Valid(UInt(36.W)))
        val l2MissMatch = Output(Bool())
      }
      val chi = new PortIO
      val nodeID = Input(UInt())
    })

    // Display info
    val sizeBytes = cacheParams.toCacheParams.capacity.toDouble
    val sizeStr = sizeBytesToStr(sizeBytes)
    val prefetch = "prefetch: " + cacheParams.prefetch
    println(s"====== Inclusive TL-CHI ${cacheParams.name} ($sizeStr * $banks-bank) $prefetch ======")
    println(s"bankBits: ${bankBits}")
    println(s"replacement: ${cacheParams.replacement}")
    println(s"sets:${cacheParams.sets} ways:${cacheParams.ways} blockBytes:${cacheParams.blockBytes}")
    print_bundle_fields(node.in.head._2.bundle.requestFields, "usr")
    print_bundle_fields(node.in.head._2.bundle.echoFields, "echo")
    println(s"CHI REQ Width: ${(new CHIREQ).getWidth}")
    println(s"CHI RSP Width: ${(new CHIRSP).getWidth}")
    println(s"CHI SNP Width: ${(new CHISNP).getWidth}")
    println(s"CHI DAT Width: ${(new CHIDAT).getWidth}")
    println(s"CHI Port Width: ${io.chi.getWidth}")

    println(s"Cacheable:")
    node.edges.in.headOption.foreach { n =>
      n.client.clients.zipWithIndex.foreach {
        case (c, i) =>
          println(s"\t${i} <= ${c.name};" +
            s"\tsourceRange: ${c.sourceId.start}~${c.sourceId.end}")
      }
    }
    println(s"MMIO:")
    mmioNode.edges.in.headOption.foreach { n =>
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
        fastArb(prefetchResps.get, prefetcher.get.io.resp, Some("prefetch_resp"))
        prefetcher.get.io.tlb_req <> io.l2_tlb_req
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

    // TODO: Remove this to utility or HasCoupledL2Parameters
    def bank_eq(set: UInt, bankId: Int, bankBits: Int): Bool = {
      if(bankBits == 0) true.B else set(bankBits - 1, 0) === bankId.U
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

    val slices = node.in.zipWithIndex.map {
      case ((in, edgeIn), i) =>
        val rst_L2 = reset
        val slice = withReset(rst_L2) {
          Module(new Slice()(p.alterPartial {
            case EdgeInKey => edgeIn
            case BankBitsKey => bankBits
            case SliceIdKey => i
          }))
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
            s.tlb_req.req.valid := false.B
            s.tlb_req.req.bits := DontCare
            s.tlb_req.req_kill := DontCare
            s.tlb_req.resp.ready := true.B
        }

        slice
    }

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

    /**
      * TxnID space arrangement:
      * If this is a cacheable request:
      * +----------------+-----------+---------------+
      * |    0.U(1.W)    |  SliceID  |  Inner TxnID  |
      * +----------------+-----------+---------------+
      * Otherwise this is an MMIO request:
      * +----------------+-----------+---------------+
      * |    1.U(1.W)    |        Inner TxnID        |
      * +----------------+---------------------------+  
      *
      */
    def setSliceID(txnID: UInt, sliceID: UInt, mmio: Bool): UInt = {
      Mux(
        mmio,
        Cat(1.U(1.W), txnID.tail(1)),
        Cat(0.U(1.W), if (banks <= 1) txnID.tail(1) else Cat(sliceID(bankBits - 1, 0), txnID.tail(bankBits + 1)))
      )
    }
    def getSliceID(txnID: UInt): UInt = if (banks <= 1) 0.U else txnID.tail(1).head(bankBits)
    def restoreTXNID(txnID: UInt): UInt = {
      val mmio = txnID.head(1).asBool
      Mux(
        mmio || (banks <= 1).B,
        Cat(0.U(1.W), txnID.tail(1)),
        Cat(0.U(1.W), 0.U(bankBits.W), txnID.tail(bankBits + 1))
      )
    }
    val mmio = mmioBridge.module

    // TXREQ
    val txreq_arb = Module(new Arbiter(new CHIREQ, slices.size + 1)) // plus 1 for MMIO
    val txreq = Wire(DecoupledIO(new CHIREQ))
    slices.zip(txreq_arb.io.in.init).foreach { case (s, in) => in <> s.io.out.tx.req }
    txreq_arb.io.in.last <> mmio.io.tx.req
    txreq <> txreq_arb.io.out
    txreq.bits.txnID := setSliceID(txreq_arb.io.out.bits.txnID, txreq_arb.io.chosen, mmio.io.tx.req.fire)

    // TXRSP
    val txrsp = Wire(DecoupledIO(new CHIRSP))
    arb(slices.map(_.io.out.tx.rsp), txrsp, Some("txrsp"))

    // TXDAT
    val txdat = Wire(DecoupledIO(new CHIDAT))
    arb(slices.map(_.io.out.tx.dat) :+ mmio.io.tx.dat, txdat, Some("txdat"))

    // RXSNP
    val rxsnp = Wire(DecoupledIO(new CHISNP))
    val rxsnpSliceID = if (banks <= 1) 0.U else (rxsnp.bits.addr >> (offsetBits - 3))(bankBits - 1, 0)
    slices.zipWithIndex.foreach { case (s, i) =>
      s.io.out.rx.snp.valid := rxsnp.valid && rxsnpSliceID === i.U
      s.io.out.rx.snp.bits := rxsnp.bits
    }
    rxsnp.ready := Cat(slices.zipWithIndex.map { case (s, i) => s.io.out.rx.snp.ready && rxsnpSliceID === i.U }).orR

    // RXRSP
    val rxrsp = Wire(DecoupledIO(new CHIRSP))
    val rxrspIsMMIO = rxrsp.bits.txnID.head(1).asBool
    val isPCrdGrant = rxrsp.valid && (rxrsp.bits.opcode === PCrdGrant)
    val pArb = Module(new RRArbiter(UInt(), banks))
    /*
     when PCrdGrant, give credit to one Slice that:
     1. got RetryAck and not Reissued
     2. match srcID and PCrdType
     3. use Round-Robin arbiter if multi-Slice match
     */
    val matchPCrdGrant = Wire(Vec(banks, UInt(mshrsAll.W)))
    slices.zipWithIndex.foreach { case (s, i) =>
      matchPCrdGrant(i) := VecInit(s.io.waitPCrdInfo.map(p =>
      p.valid && isPCrdGrant &&
      p.srcID.get === rxrsp.bits.srcID &&
      p.pCrdType.get === rxrsp.bits.pCrdType
      )).asUInt
    }
    val pCrdIsWait = VecInit(matchPCrdGrant.map(_.asUInt.orR)).asUInt

    pArb.io.in.zipWithIndex.foreach {
      case (in, i) =>
        in.valid := pCrdIsWait(i)
        in.bits := 0.U
    }
    pArb.io.out.ready := true.B
    val pCrdSliceID = pArb.io.chosen 
//    val pCrdSliceID = PriorityEncoder(pCrdIsWait)
//    val rxrspSliceID = getSliceID(rxrsp.bits.txnID)
    val rxrspSliceID = Mux(isPCrdGrant, pCrdSliceID, getSliceID(rxrsp.bits.txnID))
    slices.zipWithIndex.foreach { case (s, i) =>
      s.io.out.rx.rsp.valid := rxrsp.valid && rxrspSliceID === i.U && !rxrspIsMMIO
      s.io.out.rx.rsp.bits := rxrsp.bits
      s.io.out.rx.rsp.bits.txnID := restoreTXNID(rxrsp.bits.txnID)
    }
    mmio.io.rx.rsp.valid := rxrsp.valid && rxrspIsMMIO
    mmio.io.rx.rsp.bits := rxrsp.bits
    mmio.io.rx.rsp.bits.txnID := restoreTXNID(rxrsp.bits.txnID)
    rxrsp.ready := Mux(
      rxrspIsMMIO,
      mmio.io.rx.rsp.ready,
      Cat(slices.zipWithIndex.map { case (s, i) => s.io.out.rx.rsp.ready && rxrspSliceID === i.U }).orR
    )

    // RXDAT
    val rxdat = Wire(DecoupledIO(new CHIDAT))
    val rxdatIsMMIO = rxdat.bits.txnID.head(1).asBool
    val rxdatSliceID = getSliceID(rxdat.bits.txnID)
    slices.zipWithIndex.foreach { case (s, i) =>
      s.io.out.rx.dat.valid := rxdat.valid && rxdatSliceID === i.U && !rxdatIsMMIO
      s.io.out.rx.dat.bits := rxdat.bits
      s.io.out.rx.dat.bits.txnID := restoreTXNID(rxdat.bits.txnID)
    }
    mmio.io.rx.dat.valid := rxdat.valid && rxdatIsMMIO
    mmio.io.rx.dat.bits := rxdat.bits
    mmio.io.rx.dat.bits.txnID := restoreTXNID(rxdat.bits.txnID)
    rxdat.ready := Mux(
      rxdatIsMMIO,
      mmio.io.rx.dat.ready,
      Cat(slices.zipWithIndex.map { case (s, i) => s.io.out.rx.dat.ready && rxdatSliceID === i.U}).orR
    )

    val linkMonitor = Module(new LinkMonitor)
    linkMonitor.io.in.tx.req <> txreq
    linkMonitor.io.in.tx.rsp <> txrsp
    linkMonitor.io.in.tx.dat <> txdat
    rxsnp <> linkMonitor.io.in.rx.snp
    rxrsp <> linkMonitor.io.in.rx.rsp
    rxdat <> linkMonitor.io.in.rx.dat
    io.chi <> linkMonitor.io.out
    linkMonitor.io.nodeID := io.nodeID

    // ==================== TopDown ====================
    val topDown = topDownOpt.map(_ => Module(new TopDownMonitor()(p.alterPartial {
      case EdgeInKey => node.in.head._2
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
      case None => io.debugTopDown.l2MissMatch := false.B
    }

    // ==================== XSPerf Counters ====================
    val grant_data_fire = slices.map { slice => {
      val (first, _, _, _) = node.in.head._2.count(slice.io.in.d)
      slice.io.in.d.fire && first && slice.io.in.d.bits.opcode === GrantData
    }
    }
    XSPerfAccumulate(cacheParams, "grant_data_fire", PopCount(VecInit(grant_data_fire)))

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
    XSPerfAccumulate(cacheParams, "accurate3Hints", accurateHint)

    val okHint = grant_data_fire.orR && hintPipe1.io.out.valid && hintPipe1.io.out.bits === grant_data_source
    XSPerfAccumulate(cacheParams, "ok2Hints", okHint)
  }

  lazy val module = new CoupledL2Imp(this)
}
