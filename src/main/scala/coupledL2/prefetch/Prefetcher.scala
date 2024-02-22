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

package coupledL2.prefetch

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import coupledL2._
import coupledL2.utils.{XSPerfAccumulate, XSPerfHistogram}

class PrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val pfSource = UInt(MemReqSource.reqSourceBits.W)

  def isBOP:Bool = pfSource === MemReqSource.Prefetch2L2BOP.id.U
  def isSMS:Bool = pfSource === MemReqSource.Prefetch2L2SMS.id.U
  def isTP:Bool = pfSource === MemReqSource.Prefetch2L2TP.id.U
  def fromL2:Bool =
    pfSource === MemReqSource.Prefetch2L2BOP.id.U ||
    pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
    pfSource === MemReqSource.Prefetch2L2TP.id.U
}

class PrefetchResp(implicit p: Parameters) extends PrefetchBundle {
  // val id = UInt(sourceIdBits.W)
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchTrain(implicit p: Parameters) extends PrefetchBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
  val hit = Bool()
  val prefetched = Bool()
  val pfsource = UInt(PfSource.pfSourceBits.W)
  val reqsource = UInt(MemReqSource.reqSourceBits.W)

  def addr: UInt = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  val train = Flipped(DecoupledIO(new PrefetchTrain))
  val req = DecoupledIO(new PrefetchReq)
  val resp = Flipped(DecoupledIO(new PrefetchResp))
  val recv_addr = Flipped(ValidIO(new Bundle() {
    val addr = UInt(64.W)
    val pfSource = UInt(MemReqSource.reqSourceBits.W)
  }))
}

class PrefetchQueue(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(new PrefetchReq))
    val deq = DecoupledIO(new PrefetchReq)
  })
  /*  Here we implement a queue that
   *  1. is pipelined  2. flows
   *  3. always has the latest reqs, which means the queue is always ready for enq and deserting the eldest ones
   */
  val queue = RegInit(VecInit(Seq.fill(inflightEntries)(0.U.asTypeOf(new PrefetchReq))))
  val valids = RegInit(VecInit(Seq.fill(inflightEntries)(false.B)))
  val idxWidth = log2Up(inflightEntries)
  val head = RegInit(0.U(idxWidth.W))
  val tail = RegInit(0.U(idxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last

  when(!empty && io.deq.ready) {
    valids(head) := false.B
    head := head + 1.U
  }

  when(io.enq.valid) {
    queue(tail) := io.enq.bits
    valids(tail) := !empty || !io.deq.ready // true.B
    tail := tail + (!empty || !io.deq.ready).asUInt
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
  }

  io.enq.ready := true.B
  io.deq.valid := !empty || io.enq.valid
  io.deq.bits := Mux(empty, io.enq.bits, queue(head))

  // The reqs that are discarded = enq - deq
  XSPerfAccumulate(cacheParams, "prefetch_queue_enq",         io.enq.fire)
  XSPerfAccumulate(cacheParams, "prefetch_queue_enq_fromBOP", io.enq.fire && io.enq.bits.isBOP)
  XSPerfAccumulate(cacheParams, "prefetch_queue_enq_fromSMS", io.enq.fire && io.enq.bits.isSMS)
  XSPerfAccumulate(cacheParams, "prefetch_queue_enq_fromTP",  io.enq.fire && io.enq.bits.isTP)

  XSPerfAccumulate(cacheParams, "prefetch_queue_deq",         io.deq.fire)
  XSPerfAccumulate(cacheParams, "prefetch_queue_deq_fromBOP", io.deq.fire && io.deq.bits.isBOP)
  XSPerfAccumulate(cacheParams, "prefetch_queue_deq_fromSMS", io.deq.fire && io.deq.bits.isSMS)
  XSPerfAccumulate(cacheParams, "prefetch_queue_deq_fromTP",  io.deq.fire && io.deq.bits.isTP)

  XSPerfHistogram(cacheParams, "prefetch_queue_entry", PopCount(valids.asUInt),
    true.B, 0, inflightEntries, 1)
}

class Prefetcher(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO)
  val tpio = IO(new Bundle() {
    val tpmeta_port = prefetchOpt.map(_ => new tpmetaPortIO)
  })
  /* io_l2_pf_en:
   * chicken bits for whether L2 prefetchers are enabled
   * it will control BOP and TP prefetchers
   */
  val io_l2_pf_en = IO(Input(Bool()))

  prefetchOpt.get match {
    case bop: BOPParameters =>
      val pft = Module(new BestOffsetPrefetch)
      val pftQueue = Module(new PrefetchQueue)
      val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
      pft.io.train <> io.train
      pft.io.resp <> io.resp
      pftQueue.io.enq <> pft.io.req
      pipe.io.in <> pftQueue.io.deq
      io.req <> pipe.io.out
    case receiver: PrefetchReceiverParams =>
      val pfRcv = Module(new PrefetchReceiver())
      val bop = Module(new BestOffsetPrefetch()(p.alterPartial({
        case tl2tl.L2ParamKey => p(tl2tl.L2ParamKey).copy(prefetch = Some(BOPParameters()))
      })))
      val tp = Module(new TemporalPrefetch()(p.alterPartial({
        case tl2tl.L2ParamKey => p(tl2tl.L2ParamKey).copy(prefetch = Some(TPParameters()))
      })))
      val pftQueue = Module(new PrefetchQueue)
      val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
      val l2_pf_en = RegNextN(io_l2_pf_en, 2, Some(true.B))

      // prefetch from upper level
      pfRcv.io.recv_addr := ValidIODelay(io.recv_addr, 2)
      pfRcv.io.train.valid := false.B
      pfRcv.io.train.bits := 0.U.asTypeOf(new PrefetchTrain)
      pfRcv.io.resp.valid := false.B
      pfRcv.io.resp.bits := 0.U.asTypeOf(new PrefetchResp)
      assert(!pfRcv.io.req.valid ||
       pfRcv.io.req.bits.pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
       pfRcv.io.req.bits.pfSource === MemReqSource.Prefetch2L2Stream.id.U ||
       pfRcv.io.req.bits.pfSource === MemReqSource.Prefetch2L2Stride.id.U
      )

      // prefetch from local prefetchers: BOP & TP
      bop.io.train <> io.train
      bop.io.resp <> io.resp
      tp.io.train <> io.train
      tp.io.resp <> io.resp

      // send to prq
      pftQueue.io.enq.valid := pfRcv.io.req.valid || (l2_pf_en && (bop.io.req.valid || tp.io.req.valid))
      pftQueue.io.enq.bits := Mux(pfRcv.io.req.valid,
        pfRcv.io.req.bits,
        Mux(bop.io.req.valid,
          bop.io.req.bits,
          tp.io.req.bits
        )
      )
      pfRcv.io.req.ready := true.B
      bop.io.req.ready := true.B
      tp.io.req.ready := !pfRcv.io.req.valid && !bop.io.req.valid
      pipe.io.in <> pftQueue.io.deq
      io.req <> pipe.io.out

      // tpmeta interface
      tp.io.tpmeta_port <> tpio.tpmeta_port.get

      XSPerfAccumulate(cacheParams, "prefetch_req_fromSMS", pfRcv.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_fromBOP", l2_pf_en && bop.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_fromTP", l2_pf_en && tp.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_SMS_other_overlapped",
        pfRcv.io.req.valid && l2_pf_en && (bop.io.req.valid || tp.io.req.valid))
    case _ => assert(cond = false, "Unknown prefetcher")
  }
}
