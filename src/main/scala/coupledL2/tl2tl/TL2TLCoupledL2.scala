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

package coupledL2.tl2tl

import chisel3._
import chisel3.util._
import utility.{FastArbiter, Pipeline}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import scala.math.max
import coupledL2._
import coupledL2.prefetch._
import coupledL2.utils.XSPerfAccumulate
import huancun.{TPmetaReq, TPmetaResp}
import scala.languageFeature.implicitConversions

trait HasTL2TLCoupledL2Parameters extends HasCoupledL2Parameters{
  val tl2tlParams: HasTLL2Parameters = p(L2ParamKey)

  lazy val edgeOut = p(EdgeOutKey)

  lazy val outerSinkBits = edgeOut.bundle.sinkBits
}

abstract class TL2TLL2Bundle(implicit val p: Parameters) extends Bundle
  with HasTL2TLCoupledL2Parameters
abstract class TL2TLL2Module(implicit val p: Parameters) extends Module
  with HasTL2TLCoupledL2Parameters

class TL2TLCoupledL2(implicit p: Parameters) extends CoupledL2Base
  with HasTL2TLCoupledL2Parameters {
  
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

  class CoupledL2Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {

    val banks = node.in.size
    val bankBits = if (banks == 1) 0 else log2Up(banks)
    val io = IO(new Bundle {
    //  val l2_hint = Valid(UInt(32.W))
      val l2_hint = ValidIO(new L2ToL1Hint())
      val debugTopDown = new Bundle {
        val robHeadPaddr = Vec(cacheParams.hartIds.length, Flipped(Valid(UInt(36.W))))
        val l2MissMatch = Vec(cacheParams.hartIds.length, Output(Bool()))
      }
    })

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
    println(s"replace policy: ${tl2tlParams.releaseData}")
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
        slice.io.in <> in
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
    io.l2_hint.valid := l1Hint_arb.io.out.fire
    io.l2_hint.bits.sourceId := l1Hint_arb.io.out.bits.sourceId - Mux1H(client_sourceId_match_oh, client_sourceId_start)
    io.l2_hint.bits.isKeyword := l1Hint_arb.io.out.bits.isKeyword
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