package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.{HasCoupledL2Parameters, L2ToL1TlbIO}
import utility._
import coupledL2._

case class CDPParameters(
  DetectPipeNum: Int = 4,

  ReqFilterEntryNum: Int = 16,   // how many entries in the prefetch req filter?

  // Replacement
  replacer: Option[String] = Some("setplru"),

  // VPN Table Params
  VpnTableSetNum:       Int = 4,
  VpnTableWayNum:       Int = 4,
  VpnTableSubEntryNum:  Int = 4,
  VpnTableTagBits:      Int = 10,     // should be a val within (0, 18 - log2(VpnTableSubEntryNum)]
  CounterBits:          Int = 10,
  VpnResetPeriod:       Int = 128,    // Every $VpnResetPeriod visits, VPN entries will be reset

  Degree:   Int = 1,      // issue how many prefetch req?

  debug: Boolean = false
) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true
  override val inflightEntries: Int = 16  // ???
}

trait HasCDPParams extends HasPrefetcherHelper with HasCoupledL2Parameters {
  def cdpParams = prefetchers.find {
    case p: CDPParameters => true
    case _ => false
  }.get.asInstanceOf[CDPParameters]

  // helper function
  def get_folded_hash(origin_val: UInt, res_len: Int) = {    // fold $origin_val length value into $res_len
    val totalBits = origin_val.getWidth

    val groupSize = totalBits / res_len
    val remainder = totalBits % res_len

    val paddedBits = if (remainder == 0) totalBits else (groupSize + 1) * res_len
    val padded_val = Cat(0.U((paddedBits - totalBits).W), origin_val)

    val actualGroupSize = paddedBits / res_len
    val groups = Seq.tabulate(res_len) { i =>
      val startBit = i * actualGroupSize
      val endBit = (i + 1) * actualGroupSize - 1
      padded_val(endBit, startBit)
    }

    groups.reduce(_ ^ _)
  }

  val DetectPipeNum = cdpParams.DetectPipeNum

  val Degree    = cdpParams.Degree

  val replType  = cdpParams.replacer

  // VpnTable Params
  val CounterBits   = cdpParams.CounterBits

  def get_vpn2(addr: UInt): UInt = addr(38, 30)
  def get_vpn1(addr: UInt): UInt = addr(29, 21)
  def get_vpn0(addr: UInt): UInt = addr(20, 12)
  def get_offset(addr: UInt): UInt = addr(11, 0)

  val VpnTableSetNum      = cdpParams.VpnTableSetNum
  val VpnTableWayNum      = cdpParams.VpnTableWayNum
  val VpnTableSubEntryNum = cdpParams.VpnTableSubEntryNum

  val VpnResetPeriod      = cdpParams.VpnResetPeriod

  val mainEntryBits = log2Ceil(VpnTableSetNum)
  val subEntryBits  = log2Ceil(VpnTableSubEntryNum)
  val vpnTabTagBits = cdpParams.VpnTableTagBits
  val vpnWayBits    = log2Ceil(VpnTableWayNum)

  // addr => [vpn2, vpn1] => [vpn_addr, sub_idx]
  def get_vpn_addr(addr: UInt) = addr(38, 21 + subEntryBits)

  def get_main_idx(addr: UInt) = {
    // Hash vpn_addr (18 - $subEntryBits bits) to $mainEntryBits
    val vpn_addr = get_vpn_addr(addr)

    get_folded_hash(vpn_addr, mainEntryBits)
  }

  def get_sub_idx(addr: UInt) = addr(20 + subEntryBits, 20)

  def get_vpntab_tag(addr: UInt) = {
    val vpn_addr = get_vpn_addr(addr)
    get_folded_hash(vpn_addr, vpnTabTagBits)
  }

  // Req Filter Params
  val ReqFilterEntryNum = cdpParams.ReqFilterEntryNum
  val ReqFilterTagBits  = fullAddressBits - log2Ceil(blockBytes)
}

abstract class CDPBundle(implicit val p: Parameters) extends Bundle with HasCDPParams
abstract class CDPModule(implicit val p: Parameters) extends Module with HasCDPParams

class CDPDetectTrigger(implicit p: Parameters) extends CDPBundle {
  val cacheblock = UInt(blockBits.W)
  val pfDepth    = UInt(4.W)
  // pf Src
}

class CDPDetectEntry(implicit p: Parameters) extends CDPBundle {
  val half_cacheblock = UInt((blockBits / 2).W)
  val pfDepth         = UInt(4.W)
  // pf Src
}

class CDPTrainTrigger(implicit p: Parameters) extends CDPBundle {
  val vaddr = UInt(fullAddressBits.W)
  // pf Src
}

class VpnTableMetaInfo(implicit p: Parameters) extends CDPBundle {
  val valid = Bool()
  val hot   = Bool()    // indicate whether this 1MB page is frequently visited in the past period

  val prevRefCnt  = UInt(CounterBits.W)
  val refCnt      = UInt(CounterBits.W)
}

class vtQueryReq(implicit p: Parameters) extends CDPBundle {
  val main_idx  = UInt(mainEntryBits.W)
  val sub_idx   = UInt(subEntryBits.W)
}

class vtQueryRsp(implicit p: Parameters) extends CDPBundle {
  val tag_vec   = Vec(VpnTableWayNum, UInt(vpnTabTagBits.W))
  val meta_vec  = Vec(VpnTableWayNum, new VpnTableMetaInfo)
}

class vtTrainReq(implicit p: Parameters) extends CDPBundle {
  val need_alloc  = Bool()    // Allocate a new entry!
  val target_way  = UInt(log2Ceil(VpnTableWayNum).W)

  val main_idx    = UInt(mainEntryBits.W)
  val sub_idx     = UInt(subEntryBits.W)

  val tag         = UInt(vpnTabTagBits.W)
}

class VpnTable(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val query_req = Flipped(Vec(DetectPipeNum + 1, DecoupledIO(new vtQueryReq)))   // +1 for train pipe
    val query_rsp = Vec(DetectPipeNum + 1, ValidIO(new vtQueryRsp))
    val train_req = Flipped(ValidIO(new vtTrainReq))
  })

  val (query_req, query_rsp) = (io.query_req, io.query_rsp)
  val train_req = io.train_req

  val resetCnt = RegInit(0.U(32.W))
  val is_reset = Wire(Bool())

  // TODO: use SRAM ?
  // Tag Array
  val tag_array = RegInit(VecInit(Seq.fill(VpnTableSetNum)(
    VecInit(Seq.fill(VpnTableWayNum)(0.U(vpnTabTagBits.W)))
  )))

  // Meta Info Array
  val meta_array = RegInit(VecInit(Seq.fill(VpnTableSetNum)(
    VecInit(Seq.fill(VpnTableWayNum)(
      VecInit(Seq.fill(VpnTableSubEntryNum)(0.U.asTypeOf(new VpnTableMetaInfo)))
    ))
  )))

  // Query Logic
  for (i <- 0 until DetectPipeNum + 1) {
    val (req, rsp) = (query_req(i), query_rsp(i))

    req.ready := !train_req.valid & !is_reset
    rsp.valid := !train_req.valid & !is_reset

    val (main_idx, sub_idx) = (req.bits.main_idx, req.bits.sub_idx)

    for (j <- 0 until VpnTableWayNum) {
      rsp.bits.tag_vec(j)   := tag_array(main_idx)(j)
      rsp.bits.meta_vec(j)  := meta_array(main_idx)(j)(sub_idx)
    }
  }

  // Train Logic
  when (train_req.valid) {
    val (main_idx, sub_idx) = (train_req.bits.main_idx, train_req.bits.sub_idx)
    val target_way = train_req.bits.target_way

    when (train_req.bits.need_alloc) {
      // Allocate/Replace for a new mainEntry, thus we need to clean all the subEntries

      // use target_way for replacement
      val replace_way = target_way

      // Update Tag
      tag_array(main_idx)(replace_way)  := train_req.bits.tag

      // Update Meta
      for (i <- 0 until VpnTableSubEntryNum) {
        meta_array(main_idx)(replace_way)(sub_idx)  := 0.U.asTypeOf(new VpnTableMetaInfo)
        meta_array(main_idx)(replace_way)(sub_idx).valid  := true.B
        meta_array(main_idx)(replace_way)(sub_idx).refCnt := 1.U
      }
    }.otherwise {
      meta_array(main_idx)(target_way)(sub_idx).refCnt := meta_array(main_idx)(target_way)(sub_idx).refCnt + 1.U
    }
  }

  // Reset Logic (Reset the entries)
  val query_req_fire_vec = query_req.map(_.fire)
  val visit_cnt = PopCount(query_req_fire_vec)

  when (resetCnt < VpnResetPeriod.U){
    resetCnt := resetCnt + visit_cnt
  }

  is_reset := !train_req.valid && resetCnt >= VpnResetPeriod.U
  when (is_reset) {
    resetCnt := 0.U

    // go through every sub entry
    for (i <- 0 until VpnTableSetNum) {
      for (j <- 0 until VpnTableWayNum) {
        for (k <- 0 until VpnTableSubEntryNum) {
          val entry = meta_array(i)(j)(k)

          /**
           * Update Entry:
           *  prevRefCnt -> 0.8 * refCnt + 0.2 * prevRefCnt
           *  refCnt  -> 0
           *  hot -> prevRefCnt > 0 ? 1 : 0
           * */
          // TODO: For better timing, maybe we should pipeline this.
          val nxt_prevRefCnt = ((entry.refCnt * 4.U) + entry.prevRefCnt) >> 8.U

          entry.refCnt      := 0.U
          entry.prevRefCnt  := nxt_prevRefCnt
          entry.hot         := Mux(nxt_prevRefCnt > 0.U, true.B, false.B)
        }
      }
    }
  }

}

class CDPTrainReq(implicit p: Parameters) extends CDPBundle {
  val vaddr = UInt(fullAddressBits.W)
  // pf Src?
}

class CDPDetectReq(implicit p: Parameters) extends CDPBundle {
  val vaddr   = UInt(fullAddressBits.W)
  val pfDepth = UInt(4.W)
  // pf Src?
}

class CDPPrefetchReq(implicit p: Parameters) extends CDPBundle {
  val pfAddr  = UInt(fullAddressBits.W)
  val pfDepth = UInt(4.W)
}

class ReplaceUpt(implicit p: Parameters) extends CDPBundle {
  val way = UInt(vpnWayBits.W)
  val set = UInt(mainEntryBits.W)
}

class ReplaceReq(implicit p: Parameters) extends CDPBundle {
  val set = UInt(mainEntryBits.W)
}

class ReplaceRsp(implicit p: Parameters) extends CDPBundle {
  val way = UInt(vpnWayBits.W)
}

class TrainPipeline(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val train_req = Flipped(DecoupledIO(new CDPTrainReq))

    // to VPN Table
    val vt_query_req = Decoupled(new vtQueryReq)
    val vt_query_rsp = Flipped(ValidIO(new vtQueryRsp))
    val vt_train_req = ValidIO(new vtTrainReq)

    // to Replacer
    val replace_upt = ValidIO(new ReplaceUpt)
    val replace_req = ValidIO(new ReplaceReq)
    val replace_rsp = Flipped(ValidIO(new ReplaceRsp))
  })

  val train_req = io.train_req
  val (vt_query_req, vt_query_rsp, vt_train_req) = (io.vt_query_req, io.vt_query_rsp, io.vt_train_req)
  val (replace_upt, replace_req, replace_rsp) = (io.replace_upt, io.replace_req, io.replace_rsp)

  // Pipeline Control Signals Def
  val s0_valid = Wire(Bool())   // Stage N content valid
  val s1_valid = Wire(Bool())
  val s2_valid = Wire(Bool())
  val s3_valid = Wire(Bool())
  val s4_valid = Wire(Bool())

  val s0_ready = Wire(Bool())   // Stage N ready to receive
  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  val s3_ready = Wire(Bool())
  val s4_ready = Wire(Bool())

  // ----------- s0 -----------
  s0_ready := reset.asBool || s1_ready
  s0_valid := train_req.valid
  train_req.ready := s0_ready

  val s0_main_idx = get_main_idx(train_req.bits.vaddr)
  val s0_sub_idx  = get_sub_idx(train_req.bits.vaddr)
  val s0_tag      = get_vpntab_tag(train_req.bits.vaddr)

  // ----------- s1 -----------
  // search the Vpn Table
  s1_ready := reset.asBool || !s1_valid || s2_ready && vt_query_req.ready && vt_query_rsp.valid
  s1_valid := RegEnable(s0_valid, s1_ready)

  val s1_main_idx = RegEnable(s0_main_idx, s1_ready)
  val s1_sub_idx  = RegEnable(s0_sub_idx, s1_ready)
  val s1_tag      = RegEnable(s0_tag, s1_ready)
  val s1_vt_tab_rsp = vt_query_rsp.bits

  vt_query_req.valid := s1_valid
  vt_query_req.bits.main_idx := s1_main_idx
  vt_query_req.bits.sub_idx  := s1_sub_idx

  // ----------- s2 -----------
  // check whether hit/miss
  s2_ready := reset.asBool || !s2_valid || s3_ready
  s2_valid := RegEnable(s1_valid, s2_ready)

  val s2_main_idx = RegEnable(s1_main_idx, s2_ready)
  val s2_sub_idx  = RegEnable(s1_sub_idx, s2_ready)
  val s2_tag      = RegEnable(s1_tag, s2_ready)
  val s2_vt_tab_rsp = RegEnable(s1_vt_tab_rsp, s2_ready)

  val s2_tag_vec  = s2_vt_tab_rsp.tag_vec
  val s2_meta_vec = s2_vt_tab_rsp.meta_vec

  val s2_hit_vec = s2_tag_vec.zip(s2_meta_vec).map {
    case (t, m) =>
      t === s2_tag && m.valid
  }

  val s2_hit      = s2_hit_vec.reduce(_ || _)
  val s2_hit_idx  = PriorityEncoder(s2_hit_vec)
  assert(PopCount(s2_hit_vec) < 2.U, "Meta multiple hit!")

  // ----------- s3 -----------
  // get plru replacer info & calculate update info
  s3_ready  := reset.asBool || !s3_valid || s4_ready
  s3_valid  := RegEnable(s2_valid, s3_ready)

  val s3_main_idx = RegEnable(s2_main_idx, s3_ready)
  val s3_sub_idx  = RegEnable(s2_sub_idx, s3_ready)
  val s3_tag      = RegEnable(s2_tag, s3_ready)
  val s3_hit      = RegEnable(s2_hit, s3_ready)
  val s3_hit_idx  = RegEnable(s2_hit_idx, s3_ready)

  val s3_update_info = WireInit(0.U.asTypeOf(new vtTrainReq))

  replace_req.valid     := s3_valid
  replace_req.bits.set  := s3_main_idx
  val plru_way = replace_rsp.bits.way  // TODO: update this

  s3_update_info.tag  := s3_tag
  s3_update_info.main_idx := s3_main_idx
  s3_update_info.sub_idx  := s3_sub_idx

  s3_update_info.need_alloc := !s3_hit
  s3_update_info.target_way := Mux(s3_hit, s3_hit_idx, plru_way)

  // ----------- s4 -----------
  // update plru && VpnTable
  s4_ready  := !reset.asBool
  s4_valid  := RegEnable(s3_valid, s4_ready)

  val s4_update_info  = RegEnable(s3_update_info, s4_ready)

  vt_train_req.valid  := s4_valid
  vt_train_req.bits   := s4_update_info

  replace_upt.valid := s4_valid
  replace_upt.bits.set  := s4_update_info.main_idx
  replace_upt.bits.way  := s4_update_info.target_way
}

class DetectPipeline(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val detect_req    = Flipped(DecoupledIO(new CDPDetectReq))

    // to Vpn Table
    val vt_query_req  = DecoupledIO(new vtQueryReq)
    val vt_query_rsp  = Flipped(ValidIO(new vtQueryRsp))

    // to Replacer
    val replace_upt = ValidIO(new ReplaceUpt)

    // Prefetch Req
    val pft_req = DecoupledIO(new CDPPrefetchReq)
  })

  val detect_req  = io.detect_req
  val pft_req     = io.pft_req

  val (vt_query_req, vt_query_rsp) = (io.vt_query_req, io.vt_query_rsp)

  // Pipeline Ctrl Signals
  val s0_valid = Wire(Bool())
  val s1_valid = Wire(Bool())
  val s2_valid = Wire(Bool())
  val s3_valid = Wire(Bool())
  val s4_valid = Wire(Bool())

  val s0_ready = Wire(Bool())
  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  val s3_ready = Wire(Bool())
  val s4_ready = Wire(Bool())

  // ------------------ s0 ------------------
  s0_ready  := !reset.asBool || s1_ready
  s0_valid  := detect_req.valid

  val s0_addr   = detect_req.bits.vaddr
  val s0_depth  = detect_req.bits.pfDepth

  detect_req.ready := s0_ready

  // ------------------ s1 ------------------
  s1_ready  := !reset.asBool || !s1_valid || s2_ready && vt_query_req.ready && vt_query_rsp.valid
  s1_valid  := RegEnable(s0_valid, s1_ready)

  val s1_addr   = RegEnable(s0_addr, s1_ready)
  val s1_depth  = RegEnable(s0_depth, s1_ready)

  val s1_main_idx = get_main_idx(s1_addr)
  val s1_sub_idx  = get_main_idx(s1_addr)

  vt_query_req.valid  := s1_valid
  vt_query_req.bits.main_idx  := s1_main_idx
  vt_query_req.bits.sub_idx   := s1_sub_idx

  val s1_vt_query_rsp = vt_query_rsp.bits

  // ------------------ s2 ------------------
  s2_ready  := !reset.asBool || !s2_valid || s3_ready
  s2_valid  := RegEnable(s1_valid, s2_ready)

  val s2_addr   = RegEnable(s1_addr, s2_ready)
  val s2_depth  = RegEnable(s1_depth, s2_ready)
  val s2_vt_qeury_rsp = RegEnable(s1_vt_query_rsp, s2_ready)

  val s2_tag  = get_vpntab_tag(s2_addr)
  val s2_vt_hit_vec = s2_vt_qeury_rsp.tag_vec.zip(s2_vt_qeury_rsp.meta_vec).map{
    case (t, m) =>
      t === s2_tag && m.valid
  }
  val s2_vt_hit     = s2_vt_hit_vec.reduce(_ || _)
  val s2_vt_hit_idx = PriorityEncoder(s2_vt_hit_vec)
  val s2_vt_hit_hot = s2_vt_qeury_rsp.meta_vec(s2_vt_hit_idx).hot

  val s2_vpn0 = get_vpn0(s2_addr)
  val s2_vpn0_is_nzero    = s2_vpn0 =/= 0.U

  val s2_low_bit  = s2_addr(1, 0)
  val s2_low_bit_is_zero  = s2_low_bit === 0.U

  val s2_high_bit = s2_addr(fullAddressBits - 1, 39)
  val s2_high_bit_is_zero = s2_high_bit === 0.U

  val s2_can_pft  = s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero && s2_vt_hit_hot && s2_depth < 3.U    // TODO: maybe we can move this to s3?

  // ------------------ s3 ------------------
  s3_ready  := !reset.asBool || !s3_valid || s4_ready
  s3_valid  := RegEnable(s2_valid, s3_ready)

  val s3_addr       = RegEnable(s2_addr, s3_ready)    // prefetch target addr
  val s3_vt_hit     = RegEnable(s2_vt_hit, s3_ready)
  val s3_vt_hit_idx = RegEnable(s2_vt_hit_idx, s3_ready)
  val s3_can_pft    = RegEnable(s2_can_pft, s3_ready)
  val s3_depth      = RegEnable(Mux(s2_depth === 0.U, 4.U, s2_depth + 1.U), s3_ready)

  // Update PLRU
  io.replace_upt.valid  := s3_valid && s3_vt_hit
  io.replace_upt.bits.set :=get_main_idx(s3_addr)
  io.replace_upt.bits.way := s3_vt_hit_idx

  // ------------------ s4 ------------------
  s4_ready  := !reset.asBool || !s4_valid || pft_req.ready
  s4_valid  := RegEnable(s3_valid && s3_can_pft, s4_ready)

  val s4_addr   = RegEnable(s3_addr, s4_ready)
  val s4_depth  = RegEnable(s3_depth, s4_ready)

  pft_req.valid := s4_valid
  pft_req.bits.pfAddr   := s4_addr
  pft_req.bits.pfDepth  := s4_depth
}

class PrefetchFilterEntry(implicit p: Parameters) extends CDPBundle {
  val paddr_valid = Bool()
  val pTag  = UInt(ReqFilterTagBits.W)
  val vTag  = UInt(ReqFilterTagBits.W)
  val pfDepth = UInt(4.W)

  def toPrefetchReq(): PrefetchReq = {
    val req = Wire(new PrefetchReq)

    val full_addr = Cat(pTag, 0.U(log2Ceil(blockBytes).W))
    req := DontCare
    req.tag := parseFullAddress(full_addr)._1
    req.set := parseFullAddress(full_addr)._2
    req.pfSource  := MemReqSource.Prefetch2L2CDP.id.U
    req.pfDepth   := pfDepth

    req
  }
}

class PrefetchFilter(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val cdp_pft_req = Flipped(DecoupledIO(new CDPPrefetchReq))
    // tlb req
    val tlb_req = new L2ToL1TlbIO
    // pft req
    val pft_req = DecoupledIO(new PrefetchReq)
  })

  val (cdp_pft_req, pft_req) = (io.cdp_pft_req, io.pft_req)

  val tlb_req = io.tlb_req.req
  val tlb_rsp = io.tlb_req.resp
  val pmp_rsp = io.tlb_req.pmp_resp
  io.tlb_req.req_kill := false.B
  tlb_rsp.ready := true.B

  // check same cacheline?
  def block_addr(addr: UInt) = {
    addr(fullAddressBits - 1, log2Ceil(blockBytes))
  }

  val valids = RegInit(VecInit(Seq.fill(ReqFilterEntryNum)(false.B)))
  val entries = RegInit(VecInit(Seq.fill(ReqFilterEntryNum)(0.U.asTypeOf(new PrefetchFilterEntry))))

  val pf_req_arb = Module(new RRArbiterInit(new PrefetchReq, ReqFilterEntryNum))
  val tlb_req_arb = Module(new RRArbiterInit(new L2TlbReq, ReqFilterEntryNum))

  pf_req_arb.io.out <> pft_req
  tlb_req_arb.io.out <> tlb_req

  val s0_valid = Wire(Bool())
  val s1_valid = Wire(Bool())

  // --------------- s0 -----------------
  // check whether duplicated
  s0_valid  := cdp_pft_req.valid
  cdp_pft_req.ready := true.B
  
  val entry_hit_vec = entries.zip(valids).map{
    case (e, v) =>
      v && e.vTag === block_addr(cdp_pft_req.bits.pfAddr)
  }
  val entry_hit = entry_hit_vec.reduce(_ || _)

  // --------------- s1 -----------------
  // alloc new entry if not hit
  s1_valid := RegEnable(s0_valid && !entry_hit, s0_valid)
  val s1_pft_req = RegEnable(cdp_pft_req.bits, s1_valid)

  val free_entry_vec = valids.map(!_)
  val has_free_entry = free_entry_vec.reduce(_ || _)
  val free_entry_idx = PriorityEncoder(free_entry_vec)

  val idx = free_entry_idx
  val entry = entries(idx)
  when (s1_valid && has_free_entry) {
    val alloc_entry = Wire(new PrefetchFilterEntry)
    alloc_entry := DontCare
    alloc_entry.vTag := block_addr(s1_pft_req.pfAddr)
    alloc_entry.pfDepth := s1_pft_req.pfDepth

    entry := alloc_entry
  }

  assert(!s1_valid || has_free_entry, "Prefetch Filter is full! Consider increasing the number of entries.") // TODO: use plru

  val need_drop = tlb_rsp.bits.miss ||
    // page/access fault
    tlb_rsp.bits.excp.head.pf.ld || tlb_rsp.bits.excp.head.gpf.ld || tlb_rsp.bits.excp.head.af.ld ||
    // uncache
    pmp_rsp.mmio || Pbmt.isUncache(tlb_rsp.bits.pbmt) ||
    // pmp access fault
    pmp_rsp.ld
  
  // Arbiter tlb_reqs
  for (i <- 0 until ReqFilterEntryNum) {
    val entry = entries(i)
    val entry_tlb_req = tlb_req_arb.io.in(i)

    // req tlb
    entry_tlb_req.valid := valids(i) && !entry.paddr_valid
    entry_tlb_req.bits  := DontCare
    entry_tlb_req.bits.vaddr  := Cat(entry.vTag, 0.U(log2Ceil(blockBytes).W))
    entry_tlb_req.bits.cmd    := TlbCmd.read
    entry_tlb_req.bits.isPrefetch := true.B
    entry_tlb_req.bits.size   := 3.U

    // update entry
    val tlb_handled = RegNext(entry_tlb_req.fire)
    when (tlb_handled && tlb_rsp.valid && !need_drop && !tlb_rsp.bits.miss) {
      entry.paddr_valid := true.B
      entry.pTag := block_addr(tlb_rsp.bits.paddr.head)
    }

    when (tlb_handled && tlb_rsp.valid && need_drop && !tlb_rsp.bits.miss) {
      // clean the entry if the req is invalid (e.g., unmapped/uncached)
      valids(i) := false.B
    }
  }

  // Arbiter pf_reqs
  for (i <- 0 until ReqFilterEntryNum) {
    val entry = entries(i)
    val entry_pft_req = pf_req_arb.io.in(i)

    entry_pft_req.valid := valids(i) && entry.paddr_valid
    entry_pft_req.bits := entry.toPrefetchReq()

    when (entry_pft_req.fire) {
      // clear the entry after issuing the prefetch req
      valids(i) := false.B
    }
  }
}

class CDPPrefetcher(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    // TODO: require Slice Num == 4
    val l2_detect_triggers = Flipped(Vec(4, ValidIO(new CDPDetectTrigger)))
    val demand_hit_train_trigger   = Flipped(ValidIO(new CDPTrainTrigger))

    // tlb?
    val tlb_req = new L2ToL1TlbIO

    // prefetch req?
    val pft_req = DecoupledIO(new PrefetchReq)

    // train
    val train = Flipped(DecoupledIO(new PrefetchTrain))
  })

  val enable = io.enable
  val train = io.train

  val l2_triggers = io.l2_detect_triggers
  val l1_demand_hit_train_trigger = io.demand_hit_train_trigger

  val vpn_table         = Module(new VpnTable)
  val train_pipe        = Module(new TrainPipeline)
  val detect_pipe_seq   = Seq.fill(DetectPipeNum)(Module(new DetectPipeline))

  // TODO: ugly...
  // Detect Req
  val detect_trig_queue_seq = Seq.fill(4)(Module(new MIMOQueue(new CDPDetectEntry, 8, 2, 1)))
  val detect_trig_arb = Module(new RRArbiterInit(new CDPDetectEntry, 4))

  // detect_trigs <> detect_trig_queue_seq <> detect_trig_arb <> detect_pipe_seq
  for (i <- 0 until 4) {
    val detect_trig_queue = detect_trig_queue_seq(i)

    val detect_trig = l2_triggers(i)
    
    detect_trig_queue.io.flush := reset.asBool

    detect_trig_queue.io.enq(0).valid := detect_trig.valid
    detect_trig_queue.io.enq(0).bits.half_cacheblock  := detect_trig.bits.cacheblock(blockBits / 2 - 1, 0)
    detect_trig_queue.io.enq(0).bits.pfDepth  := detect_trig.bits.pfDepth

    detect_trig_queue.io.enq(1).valid := detect_trig.valid
    detect_trig_queue.io.enq(1).bits.half_cacheblock  := detect_trig.bits.cacheblock(blockBits - 1, blockBits / 2)
    detect_trig_queue.io.enq(1).bits.pfDepth  := detect_trig.bits.pfDepth

    detect_trig_arb.io.in(i) <> detect_trig_queue_seq(i).io.deq(0)
  }

  for (i <- 0 until DetectPipeNum) {
    val detect_pipe = detect_pipe_seq(i)

    detect_pipe.io.detect_req.valid := detect_trig_arb.io.out.valid
    detect_pipe.io.detect_req.bits.vaddr := detect_trig_arb.io.out.bits.half_cacheblock((i + 1) * 8 - 1, i * 8)
    detect_pipe.io.detect_req.bits.pfDepth := detect_trig_arb.io.out.bits.pfDepth
  }
  detect_trig_arb.io.out.ready := detect_pipe_seq.map(_.io.detect_req.ready).reduce(_ && _)

  // Train Req
  val train_trig_queue = Module(new Queue(new CDPTrainTrigger, 8))
  train_trig_queue.io.enq.valid := train.valid && enable
  train_trig_queue.io.enq.bits.vaddr := train.bits.vaddr.getOrElse(0.U)
  train.ready := train_trig_queue.io.enq.ready
  assert(!train_trig_queue.io.enq.valid | train_trig_queue.io.enq.ready, "l1_demand_hit_train_trigger should always be ready to accept new trigger!")

  train_pipe.io.train_req.valid := train_trig_queue.io.deq.valid
  train_trig_queue.io.deq.ready := train_pipe.io.train_req.ready
  train_pipe.io.train_req.bits.vaddr  := train_trig_queue.io.deq.bits.vaddr

  val vpn_tab_query_req_seq = detect_pipe_seq.map(_.io.vt_query_req) ++ Seq(train_pipe.io.vt_query_req)
  val vpn_tab_query_rsp_seq = detect_pipe_seq.map(_.io.vt_query_rsp) ++ Seq(train_pipe.io.vt_query_rsp)
  require(vpn_tab_query_req_seq.size == vpn_table.io.query_req.size)
  vpn_table.io.query_req.zip(vpn_tab_query_req_seq).foreach{
    case (tab_req, pipe_req) =>
      tab_req.valid := pipe_req.valid
      tab_req.bits := pipe_req.bits
      pipe_req.ready := tab_req.ready
  }
  vpn_table.io.query_rsp.zip(vpn_tab_query_rsp_seq).foreach{
    case (tab_rsp, pipe_rsp) =>
      pipe_rsp.valid := tab_rsp.valid
      pipe_rsp.bits := tab_rsp.bits
  }

  vpn_table.io.train_req.valid  := train_pipe.io.vt_train_req.valid
  vpn_table.io.train_req.bits   := train_pipe.io.vt_train_req.bits
  
  // replacement algorithm
  val replacer = ReplacementPolicy.fromString(replType, VpnTableWayNum, VpnTableSetNum)

  train_pipe.io.replace_rsp.bits  := DontCare
  when (train_pipe.io.replace_req.valid) {
    train_pipe.io.replace_rsp.valid     := true.B
    train_pipe.io.replace_rsp.bits.way  := replacer.way(train_pipe.io.replace_req.bits.set)
  }.otherwise {
    train_pipe.io.replace_rsp.valid := false.B
    train_pipe.io.replace_rsp.bits  := DontCare
  }

  val replAcceessReqs = Seq(train_pipe.io.replace_upt) ++ detect_pipe_seq.map(_.io.replace_upt)
  val touchSets = replAcceessReqs.map(_.bits.set)
  val touchWays = Seq.fill(replAcceessReqs.size)(Wire(ValidIO(UInt(vpnWayBits.W))))
  touchWays.zip(replAcceessReqs).foreach{
    case (way, req) =>
      way.valid := req.valid
      way.bits := req.bits.way
  }
  replacer.access(touchSets, touchWays)

  val pft_req_buffer = Module(new MIMOQueue(new CDPPrefetchReq, 8, DetectPipeNum, 1))
  pft_req_buffer.io.flush := reset.asBool
  for (i <- 0 until DetectPipeNum) {
    pft_req_buffer.io.enq(i).valid      := detect_pipe_seq(i).io.pft_req.valid
    pft_req_buffer.io.enq(i).bits       := detect_pipe_seq(i).io.pft_req.bits
    detect_pipe_seq(i).io.pft_req.ready := pft_req_buffer.io.enq(i).ready
  }

  val pft_req_filter = Module(new PrefetchFilter)
  pft_req_filter.io.cdp_pft_req <> pft_req_buffer.io.deq(0)
  pft_req_filter.io.tlb_req <> io.tlb_req
  pft_req_filter.io.pft_req <> io.pft_req
}