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

/* virtual address */
trait HasPrefetcherHelper extends HasCircularQueuePtrHelper with HasCoupledL2Parameters {
  // filter
  val TRAIN_FILTER_SIZE = 4
  val REQ_FILTER_SIZE = 16
  val TLB_REPLAY_CNT = 10

  // parameters
  val BLK_ADDR_RAW_WIDTH = 10
  val REGION_SIZE = 1024
  val PAGE_OFFSET = pageOffsetBits
  val VADDR_HASH_WIDTH = 5

  // vaddr:
  // |       tag               |     index     |    offset    |
  // |       block addr                        | block offset |
  // |       region addr       |        region offset         |
  val BLOCK_OFFSET = offsetBits
  val REGION_OFFSET = log2Up(REGION_SIZE)
  val REGION_BLKS = REGION_SIZE / blockBytes
  val INDEX_BITS = log2Up(REGION_BLKS)
  val TAG_BITS = fullVAddrBits - REGION_OFFSET
  val PTAG_BITS = fullAddressBits - REGION_OFFSET
  val BLOCK_ADDR_BITS = fullVAddrBits - BLOCK_OFFSET

  // hash related
  val HASH_TAG_WIDTH = VADDR_HASH_WIDTH + BLK_ADDR_RAW_WIDTH

  def get_tag(vaddr: UInt) = {
    require(vaddr.getWidth == fullVAddrBits)
    vaddr(vaddr.getWidth - 1, REGION_OFFSET)
  }

  def get_ptag(vaddr: UInt) = {
    require(vaddr.getWidth == fullAddressBits)
    vaddr(vaddr.getWidth - 1, REGION_OFFSET)
  }

  def get_index(addr: UInt) = {
    require(addr.getWidth >= REGION_OFFSET)
    addr(REGION_OFFSET - 1, BLOCK_OFFSET)
  }

  def get_index_oh(vaddr: UInt): UInt = {
    UIntToOH(get_index(vaddr))
  }

  def get_block_vaddr(vaddr: UInt): UInt = {
    vaddr(vaddr.getWidth - 1, BLOCK_OFFSET)
  }

  def _vaddr_hash(x: UInt): UInt = {
    val width = VADDR_HASH_WIDTH
    val low = x(width - 1, 0)
    val mid = x(2 * width - 1, width)
    val high = x(3 * width - 1, 2 * width)
    low ^ mid ^ high
  }

  def block_hash_tag(vaddr: UInt): UInt = {
    val blk_addr = get_block_vaddr(vaddr)
    val low = blk_addr(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = blk_addr(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = _vaddr_hash(high)
    Cat(high_hash, low)
  }

  def region_hash_tag(vaddr: UInt): UInt = {
    val region_tag = get_tag(vaddr)
    val low = region_tag(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = region_tag(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = _vaddr_hash(high)
    Cat(high_hash, low)
  }

  def region_to_block_addr(tag: UInt, index: UInt): UInt = {
    Cat(tag, index)
  }

  def toBinary(n: Int): String = n match {
    case 0 | 1 => s"$n"
    case _ => s"${toBinary(n / 2)}${n % 2}"
  }
}

class PrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val pfSource = UInt(MemReqSource.reqSourceBits.W)

  def isBOP:Bool = pfSource === MemReqSource.Prefetch2L2BOP.id.U
  def isPBOP:Bool = pfSource === MemReqSource.Prefetch2L2PBOP.id.U
  def isSMS:Bool = pfSource === MemReqSource.Prefetch2L2SMS.id.U
  def isTP:Bool = pfSource === MemReqSource.Prefetch2L2TP.id.U
  def needAck:Bool = pfSource === MemReqSource.Prefetch2L2BOP.id.U || pfSource === MemReqSource.Prefetch2L2PBOP.id.U
  def fromL2:Bool =
    pfSource === MemReqSource.Prefetch2L2BOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2PBOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
      pfSource === MemReqSource.Prefetch2L2TP.id.U
}

class PrefetchResp(implicit p: Parameters) extends PrefetchBundle {
  // val id = UInt(sourceIdBits.W)
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
  val pfSource = UInt(MemReqSource.reqSourceBits.W)

  def addr = Cat(tag, set, 0.U(offsetBits.W))
  def isBOP: Bool = pfSource === MemReqSource.Prefetch2L2BOP.id.U
  def isPBOP: Bool = pfSource === MemReqSource.Prefetch2L2PBOP.id.U
  def isSMS: Bool = pfSource === MemReqSource.Prefetch2L2SMS.id.U
  def isTP: Bool = pfSource === MemReqSource.Prefetch2L2TP.id.U
  def fromL2: Bool =
    pfSource === MemReqSource.Prefetch2L2BOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2PBOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
      pfSource === MemReqSource.Prefetch2L2TP.id.U
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
  val tlb_req = new L2ToL1TlbIO(nRespDups= 1)
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
  XSPerfAccumulate(cacheParams, "prefetch_queue_enq_fromPBOP", io.enq.fire && io.enq.bits.isPBOP)
  XSPerfAccumulate(cacheParams, "prefetch_queue_enq_fromSMS", io.enq.fire && io.enq.bits.isSMS)
  XSPerfAccumulate(cacheParams, "prefetch_queue_enq_fromTP",  io.enq.fire && io.enq.bits.isTP)

  XSPerfAccumulate(cacheParams, "prefetch_queue_deq",         io.deq.fire)
  XSPerfAccumulate(cacheParams, "prefetch_queue_deq_fromBOP", io.deq.fire && io.deq.bits.isBOP)
  XSPerfAccumulate(cacheParams, "prefetch_queue_deq_fromPBOP", io.deq.fire && io.deq.bits.isPBOP)
  XSPerfAccumulate(cacheParams, "prefetch_queue_deq_fromSMS", io.deq.fire && io.deq.bits.isSMS)
  XSPerfAccumulate(cacheParams, "prefetch_queue_deq_fromTP",  io.deq.fire && io.deq.bits.isTP)

  XSPerfHistogram(cacheParams, "prefetch_queue_entry", PopCount(valids.asUInt),
    true.B, 0, inflightEntries, 1)
  XSPerfAccumulate(cacheParams, "prefetch_queue_empty", empty)
  XSPerfAccumulate(cacheParams, "prefetch_queue_full", full)
}

class Prefetcher(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO)
  val tpio = IO(new Bundle() {
    val tpmeta_port = prefetchOpt match {
      case Some(param: PrefetchReceiverParams) =>
        if (param.hasTPPrefetcher) Some(new tpmetaPortIO()) else None
      case _ => None
    }
  })
  val hartId = IO(Input(UInt(hartIdLen.W)))

  /* io_l2_pf_en:
   * chicken bits for whether L2 prefetchers are enabled
   * it will control BOP and TP prefetchers
   */
  val io_l2_pf_en = IO(Input(Bool()))

  prefetchOpt.get match {
    case bop: BOPParameters =>
      val pft = Module(new VBestOffsetPrefetch)
      val pftQueue = Module(new PrefetchQueue)
      val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
      pft.io.train <> io.train
      pft.io.resp <> io.resp
      pft.io.tlb_req <> io.tlb_req
      pftQueue.io.enq <> pft.io.req
      pipe.io.in <> pftQueue.io.deq
      io.req <> pipe.io.out
    case receiver: PrefetchReceiverParams =>
      val pfRcv = Module(new PrefetchReceiver())
      val pbop = Module(new PBestOffsetPrefetch()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(BOPParameters(
          virtualTrain = false,
          badScore = 1,
          offsetList = Seq(
            -32, -30, -27, -25, -24, -20, -18, -16, -15,
            -12, -10, -9, -8, -6, -5, -4, -3, -2, -1,
            1, 2, 3, 4, 5, 6, 8, 9, 10,
            12, 15, 16, 18, 20, 24, 25, 27, 30
          ))))
      })))
      val vbop = Module(new VBestOffsetPrefetch()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(BOPParameters(
          badScore = 2,
          offsetList = Seq(
            -117,-147,-91,117,147,91,
            -256, -250, -243, -240, -225, -216, -200,
            -192, -180, -162, -160, -150, -144, -135, -128,
            -125, -120, -108, -100, -96, -90, -81, -80,
            -75, -72, -64, -60, -54, -50, -48, -45,
            -40, -36, -32, -30, -27, -25, -24, -20,
            -18, -16, -15, -12, -10, -9, -8, -6,
            -5, -4, -3, -2, -1,
            1, 2, 3, 4, 5, 6, 8,
            9, 10, 12, 15, 16, 18, 20, 24,
            25, 27, 30, 32, 36, 40, 45, 48,
            50, 54, 60, 64, 72, 75, 80, 81,
            90, 96, 100, 108, 120, 125, 128, 135,
            144, 150, 160, 162, 180, 192, 200, 216,
            225, 240, 243, 250/*, 256*/
          )
        )))
      })))
      val tp = Module(new TemporalPrefetch()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(TPParameters()))
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
      pfRcv.io.tlb_req.req.ready := true.B
      pfRcv.io.tlb_req.resp.valid := false.B
      pfRcv.io.tlb_req.resp.bits := DontCare
      assert(!pfRcv.io.req.valid ||
        pfRcv.io.req.bits.pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
        pfRcv.io.req.bits.pfSource === MemReqSource.Prefetch2L2Stream.id.U ||
        pfRcv.io.req.bits.pfSource === MemReqSource.Prefetch2L2Stride.id.U
      )

      // prefetch from local prefetchers: BOP & TP
      vbop.io.train <> io.train
      vbop.io.train.valid := io.train.valid && (io.train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U)
      vbop.io.resp <> io.resp
      vbop.io.resp.valid := io.resp.valid && io.resp.bits.isBOP
      vbop.io.tlb_req <> io.tlb_req
      vbop.io.pbopCrossPage := true.B // pbop.io.pbopCrossPage // let vbop have noting to do with pbop

      pbop.io.train <> io.train
      pbop.io.train.valid := io.train.valid && (io.train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U)
      pbop.io.resp <> io.resp
      pbop.io.resp.valid := io.resp.valid && io.resp.bits.isPBOP
      tp.io.train <> io.train
      tp.io.resp <> io.resp
      tp.io.hartid := hartId

      pfRcv.io.req.ready := true.B
      vbop.io.req.ready := true.B
      pbop.io.req.ready := true.B
      tp.io.req.ready := !pfRcv.io.req.valid && !vbop.io.req.valid
      pipe.io.in <> pftQueue.io.deq
      io.req <> pipe.io.out

      // tpmeta interface
      tp.io.tpmeta_port <> tpio.tpmeta_port.get

      /* pri vbop */
      pftQueue.io.enq.valid := pfRcv.io.req.valid || (l2_pf_en && (vbop.io.req.valid || pbop.io.req.valid || tp.io.req.valid))
      pftQueue.io.enq.bits := ParallelPriorityMux(Seq(
        pfRcv.io.req.valid -> pfRcv.io.req.bits,
        vbop.io.req.valid -> vbop.io.req.bits,
        pbop.io.req.valid -> pbop.io.req.bits,
        tp.io.req.valid -> tp.io.req.bits
      ))
      XSPerfAccumulate(cacheParams, "prefetch_req_fromL1", l2_pf_en && pfRcv.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_fromBOP", l2_pf_en && vbop.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_fromPBOP", l2_pf_en && pbop.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_fromTP", l2_pf_en && tp.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_selectL1", l2_pf_en && pfRcv.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_selectBOP", l2_pf_en && !pfRcv.io.req.valid && vbop.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_selectPBOP", l2_pf_en && !pfRcv.io.req.valid && !vbop.io.req.valid && pbop.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_selectTP", l2_pf_en && !pfRcv.io.req.valid && !vbop.io.req.valid && !pbop.io.req.valid && tp.io.req.valid)
      XSPerfAccumulate(cacheParams, "prefetch_req_SMS_other_overlapped",
        pfRcv.io.req.valid && l2_pf_en && (vbop.io.req.valid || tp.io.req.valid))

      /* pri pbop */
      // pftQueue.io.enq.valid := pfRcv.io.req.valid || (l2_pf_en && (vbop.io.req.valid || pbop.io.req.valid || tp.io.req.valid))
      // pftQueue.io.enq.bits := ParallelPriorityMux(Seq(
      //   pfRcv.io.req.valid -> pfRcv.io.req.bits,
      //   pbop.io.req.valid -> pbop.io.req.bits,
      //   vbop.io.req.valid -> vbop.io.req.bits,
      //   tp.io.req.valid -> tp.io.req.bits
      // ))
      // XSPerfAccumulate(cacheParams, "prefetch_req_fromL1", l2_pf_en && pfRcv.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_fromBOP", l2_pf_en && vbop.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_fromPBOP", l2_pf_en && pbop.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_fromTP", l2_pf_en && tp.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_selectL1", l2_pf_en && pfRcv.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_selectPBOP", l2_pf_en && !pfRcv.io.req.valid && pbop.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_selectBOP", l2_pf_en && !pfRcv.io.req.valid && !pbop.io.req.valid && vbop.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_selectTP", l2_pf_en && !pfRcv.io.req.valid && !vbop.io.req.valid && !pbop.io.req.valid && tp.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_SMS_other_overlapped",
      //   pfRcv.io.req.valid && l2_pf_en && (vbop.io.req.valid || tp.io.req.valid))

      /* solo vbop */
      //  vbop.io.pbopCrossPage := true.B
      //  pftQueue.io.enq.valid := pfRcv.io.req.valid || (l2_pf_en && (vbop.io.req.valid || tp.io.req.valid))
      //  pftQueue.io.enq.bits := ParallelPriorityMux(Seq(
      //    pfRcv.io.req.valid -> pfRcv.io.req.bits,
      //    vbop.io.req.valid -> vbop.io.req.bits,
      //    tp.io.req.valid -> tp.io.req.bits
      //  ))
      //  XSPerfAccumulate(cacheParams, "prefetch_req_fromL1", l2_pf_en && pfRcv.io.req.valid)
      //  XSPerfAccumulate(cacheParams, "prefetch_req_fromBOP", l2_pf_en && vbop.io.req.valid)
      //  XSPerfAccumulate(cacheParams, "prefetch_req_fromTP", l2_pf_en && tp.io.req.valid)
      //  XSPerfAccumulate(cacheParams, "prefetch_req_selectL1", l2_pf_en && pfRcv.io.req.valid)
      //  XSPerfAccumulate(cacheParams, "prefetch_req_selectBOP", l2_pf_en && !pfRcv.io.req.valid && vbop.io.req.valid)
      //  XSPerfAccumulate(cacheParams, "prefetch_req_selectTP", l2_pf_en && !pfRcv.io.req.valid && !vbop.io.req.valid && tp.io.req.valid)
      //  XSPerfAccumulate(cacheParams, "prefetch_req_SMS_other_overlapped",
      //    pfRcv.io.req.valid && l2_pf_en && (vbop.io.req.valid || tp.io.req.valid))

      /* solo pbop */
      // vbop.io.train.valid := false.B
      // vbop.io.resp.valid := false.B
      // pftQueue.io.enq.valid := pfRcv.io.req.valid || (l2_pf_en && (pbop.io.req.valid || tp.io.req.valid))
      // pftQueue.io.enq.bits := ParallelPriorityMux(Seq(
      //   pfRcv.io.req.valid -> pfRcv.io.req.bits,
      //   pbop.io.req.valid -> pbop.io.req.bits,
      //   tp.io.req.valid -> tp.io.req.bits
      // ))
      // XSPerfAccumulate(cacheParams, "prefetch_req_fromL1", l2_pf_en && pfRcv.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_fromPBOP", l2_pf_en && pbop.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_fromTP", l2_pf_en && tp.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_selectL1", l2_pf_en && pfRcv.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_selectPBOP", l2_pf_en && !pfRcv.io.req.valid && pbop.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_selectTP", l2_pf_en && !pfRcv.io.req.valid && !pbop.io.req.valid && tp.io.req.valid)
      // XSPerfAccumulate(cacheParams, "prefetch_req_SMS_other_overlapped",
      //   pfRcv.io.req.valid && l2_pf_en && (vbop.io.req.valid || tp.io.req.valid))

    case _ => assert(cond = false, "Unknown prefetcher")
  }
}
