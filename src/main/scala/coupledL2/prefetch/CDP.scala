package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.{HasCoupledL2Parameters, L2ToL1TlbIO}
import utility._
import coupledL2._
import utility.TLLogger.b
import svsim.CommonCompilationSettings.Timescale.Unit.s

case class CDPParameters(
  UseFilteredDetect:  Boolean = true,

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
  EntryBits:            Int = 20,     // Every SubEntry maintain for 2^20 Bits = 1M space

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

  val UseFilteredDetect = cdpParams.UseFilteredDetect

  // helper function
  def get_folded_hash(origin_val: UInt, resultBitWidth: Int): UInt = {    // fold $origin_val length value into $resultBitWidth
    val totalBits = origin_val.getWidth

    // Handle zero-width origin_val to avoid reduce on an empty sequence
    if (totalBits == 0) {
      0.U(resultBitWidth.W)
    } else {
      val groupNum  = if (totalBits % resultBitWidth == 0) totalBits / resultBitWidth else totalBits / resultBitWidth + 1

      val paddedBits = groupNum * resultBitWidth
      val padWidth = paddedBits - totalBits
      val padded_val =
        if (padWidth == 0) origin_val
        else Cat(0.U(padWidth.W), origin_val)

      val groups = Seq.tabulate(groupNum) { i =>
        val startBit = i * resultBitWidth
        val endBit = (i + 1) * resultBitWidth - 1
        padded_val(endBit, startBit)
      }

      groups.reduce(_ ^ _)
    }
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

  val EntryBits     = cdpParams.EntryBits
  val mainEntryBits = log2Ceil(VpnTableSetNum)
  val subEntryBits  = log2Ceil(VpnTableSubEntryNum)
  val vpnTabTagBits = cdpParams.VpnTableTagBits
  val vpnWayBits    = log2Ceil(VpnTableWayNum)

  // vaddr => [ Tag | MainEntryIdx | SubEntryIdx | EntryBits(1M Space) ]
  // vpn_addr => [ Tag | MainEntryIdx | SubEntryIdx ]

  def get_vpn_addr(addr: UInt) = addr(addr.getWidth - 1, EntryBits)  // TODO: parameterize

  def get_main_idx(addr: UInt) = {
    val vpn_addr = get_vpn_addr(addr)

    vpn_addr(subEntryBits + mainEntryBits - 1, subEntryBits)
  }

  def get_sub_idx(addr: UInt) = {
    val vpn_addr = get_vpn_addr(addr)
    vpn_addr(subEntryBits - 1, 0)
  }

  def get_vpntab_tag(addr: UInt) = {
    val vpn_addr = get_vpn_addr(addr)

    val origin_tag = vpn_addr(vpn_addr.getWidth - 1 ,subEntryBits + mainEntryBits) // TODO: parameterize
    get_folded_hash(origin_tag, vpnTabTagBits)
  }

  // Req Filter Params
  val ReqFilterEntryNum = cdpParams.ReqFilterEntryNum
  val ReqFilterTagBits  = fullAddressBits - log2Ceil(blockBytes)
}

abstract class CDPBundle(implicit val p: Parameters) extends Bundle with HasCDPParams
abstract class CDPModule(implicit val p: Parameters) extends Module with HasCDPParams

class CDPDetectTrigger(implicit p: Parameters) extends CDPBundle {
  val cacheblock  = UInt(blockBits.W)
  val pfDepth     = UInt(4.W)
  val pfSource    = UInt(PfSource.pfSourceBits.W)
  val is_hit     = Bool()
  // pf Src
}

class CDPDetectEntry(implicit p: Parameters) extends CDPBundle {
  val half_cacheblock = UInt((blockBits / 2).W)
  val pfDepth         = UInt(4.W)
  val pfSource        = UInt(PfSource.pfSourceBits.W)

  val is_hit  = Bool()
}

class CDPTrainTrigger(implicit p: Parameters) extends CDPBundle {
  val vaddr = UInt(fullAddressBits.W)
  // pf Src?
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

    req.ready := !is_reset
    rsp.valid := !is_reset

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
        when (i.U === sub_idx) {
          meta_array(main_idx)(replace_way)(i)  := 0.U.asTypeOf(new VpnTableMetaInfo)
          meta_array(main_idx)(replace_way)(i).valid  := true.B
          meta_array(main_idx)(replace_way)(i).refCnt := 1.U
        }.otherwise {
          meta_array(main_idx)(replace_way)(i)  := 0.U.asTypeOf(new VpnTableMetaInfo)
        }
      }
    }.otherwise {
      meta_array(main_idx)(target_way)(sub_idx).refCnt := meta_array(main_idx)(target_way)(sub_idx).refCnt + 1.U
    }
  }

  // Reset Logic (Reset the entries)
  when (resetCnt < VpnResetPeriod.U && train_req.valid){
    resetCnt := resetCnt + 1.U
  }

  is_reset := !train_req.valid && resetCnt >= VpnResetPeriod.U
  XSPerfAccumulate("VpnTable_reset", is_reset)
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
          val nxt_prevRefCnt = ((entry.refCnt * 13.U) + (entry.prevRefCnt * 3.U)) >> 4.U

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
  val vaddr     = UInt(fullAddressBits.W)
  val pfDepth   = UInt(4.W)
  val pfSource  = UInt(PfSource.pfSourceBits.W)

  val is_hit    = Bool()
}

class CDPPrefetchReq(implicit p: Parameters) extends CDPBundle {
  val pfAddr  = UInt(fullAddressBits.W)
  val pfDepth = UInt(4.W)

  // Only for monitor
  val pfSource  = UInt(PfSource.pfSourceBits.W)
  val is_hit    = Bool()
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

  val s0_s1_same = Wire(Bool())
  val s0_s2_same = Wire(Bool())
  val s0_s3_same = Wire(Bool())
  val s0_s4_same = Wire(Bool())

  // ----------- s0 -----------
  val same_addr = s0_s1_same || s0_s2_same || s0_s3_same || s0_s4_same

  s0_valid := train_req.valid && !same_addr
  train_req.ready := reset.asBool || !same_addr

  val s0_main_idx = get_main_idx(train_req.bits.vaddr)
  val s0_sub_idx  = get_sub_idx(train_req.bits.vaddr)
  val s0_tag      = get_vpntab_tag(train_req.bits.vaddr)

  // ----------- s1 -----------
  // search the Vpn Table
  s1_valid  := RegNext(s0_valid)

  val s1_main_idx = RegNext(s0_main_idx)
  val s1_sub_idx  = RegNext(s0_sub_idx)
  val s1_tag      = RegNext(s0_tag)
  val s1_vt_tab_rsp = vt_query_rsp.bits

  vt_query_req.valid := s1_valid
  vt_query_req.bits.main_idx := s1_main_idx
  vt_query_req.bits.sub_idx  := s1_sub_idx

  s0_s1_same := s1_valid && s0_main_idx === s1_main_idx && s0_sub_idx === s1_sub_idx && s0_tag === s1_tag

  // ----------- s2 -----------
  // check for hit/miss
  s2_valid := RegNext(s1_valid)

  val s2_main_idx = RegNext(s1_main_idx)
  val s2_sub_idx  = RegNext(s1_sub_idx)
  val s2_tag      = RegNext(s1_tag)
  val s2_vt_tab_rsp = RegNext(s1_vt_tab_rsp)

  val s2_tag_vec  = s2_vt_tab_rsp.tag_vec
  val s2_meta_vec = s2_vt_tab_rsp.meta_vec

  val s2_hit_vec = s2_tag_vec.zip(s2_meta_vec).map {
    case (t, m) =>
      t === s2_tag && m.valid
  }

  val s2_hit      = s2_hit_vec.reduce(_ || _)
  val s2_hit_idx  = PriorityEncoder(s2_hit_vec)
  assert(PopCount(s2_hit_vec) < 2.U, "Meta multiple hit!")

  s0_s2_same := s2_valid && s0_main_idx === s2_main_idx && s0_sub_idx === s2_sub_idx && s0_tag === s2_tag

  // ----------- s3 -----------
  // get plru replacer info & calculate update info
  s3_valid  := RegNext(s2_valid)

  val s3_main_idx = RegNext(s2_main_idx)
  val s3_sub_idx  = RegNext(s2_sub_idx)
  val s3_tag      = RegNext(s2_tag)
  val s3_hit      = RegNext(s2_hit)
  val s3_hit_idx  = RegNext(s2_hit_idx)

  val s3_update_info = WireInit(0.U.asTypeOf(new vtTrainReq))

  replace_req.valid     := s3_valid
  replace_req.bits.set  := s3_main_idx
  val plru_way = replace_rsp.bits.way  // TODO: update this

  s3_update_info.tag  := s3_tag
  s3_update_info.main_idx := s3_main_idx
  s3_update_info.sub_idx  := s3_sub_idx

  s3_update_info.need_alloc := !s3_hit
  s3_update_info.target_way := Mux(s3_hit, s3_hit_idx, plru_way)

  s0_s3_same := s3_valid && s0_main_idx === s3_main_idx && s0_sub_idx === s3_sub_idx && s0_tag === s3_tag

  // ----------- s4 -----------
  // update plru && VpnTable
  s4_valid  := RegNext(s3_valid)

  val s4_update_info  = RegNext(s3_update_info)

  vt_train_req.valid  := s4_valid
  vt_train_req.bits   := s4_update_info

  replace_upt.valid := s4_valid
  replace_upt.bits.set  := s4_update_info.main_idx
  replace_upt.bits.way  := s4_update_info.target_way

  s0_s4_same := s4_valid && s0_main_idx === s4_update_info.main_idx && s0_sub_idx === s4_update_info.sub_idx && s0_tag === s4_update_info.tag
}

class DetectPipeline(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val detect_req    = Flipped(ValidIO(new CDPDetectReq))

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
  val s0_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))
  val s1_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))
  val s2_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))
  val s3_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))
  val s4_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))

  // ------------------ s0 ------------------
  s0_req.valid  := detect_req.valid
  s0_req.bits   := detect_req.bits

  // ------------------ s1 ------------------
  s1_req.valid  := RegNext(s0_req.valid)
  s1_req.bits   := RegNext(s0_req.bits)

  val s1_addr     = s1_req.bits.vaddr
  val s1_main_idx = get_main_idx(s1_addr)
  val s1_sub_idx  = get_sub_idx(s1_addr)

  vt_query_req.valid  := s1_req.valid
  vt_query_req.bits.main_idx  := s1_main_idx
  vt_query_req.bits.sub_idx   := s1_sub_idx

  val s1_vt_query_rsp = vt_query_rsp.bits

  // ------------------ s2 ------------------
  s2_req.valid  := RegNext(s1_req.valid)
  s2_req.bits   := RegNext(s1_req.bits)

  val s2_addr   = s2_req.bits.vaddr
  val s2_depth  = s2_req.bits.pfDepth
  val s2_is_hit = s2_req.bits.is_hit
  
  val s2_vt_query_rsp = RegNext(s1_vt_query_rsp)

  val s2_tag  = get_vpntab_tag(s2_addr)
  val s2_vt_hit_vec = s2_vt_query_rsp.tag_vec.zip(s2_vt_query_rsp.meta_vec).map{
    case (t, m) =>
      t === s2_tag && m.valid
  }
  val s2_vt_hit     = s2_vt_hit_vec.reduce(_ || _)
  val s2_vt_hit_idx = PriorityEncoder(s2_vt_hit_vec)
  val s2_vt_hit_hot = s2_vt_query_rsp.meta_vec(s2_vt_hit_idx).hot

  val s2_vpn0 = get_vpn0(s2_addr)
  val s2_vpn0_is_nzero    = s2_vpn0 =/= 0.U

  val s2_low_bit  = s2_addr(1, 0)
  val s2_low_bit_is_zero  = s2_low_bit === 0.U

  val s2_high_bit = s2_addr(fullAddressBits - 1, 39)
  val s2_high_bit_is_zero = s2_high_bit === 0.U

  // TODO: maybe we should move depth control totally to the entrance?
  val s2_is_hit_can_pft     = s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero && s2_vt_hit && s2_vt_hit_hot   // depth == 2 || 4 is restricted when entering
  val s2_non_hit_can_pft    = s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero && s2_vt_hit && s2_vt_hit_hot && s2_depth < 3.U
  val s2_can_pft  = Mux(s2_is_hit, s2_is_hit_can_pft, s2_non_hit_can_pft)

  // ------------------ s3 ------------------
  s3_req.valid  := RegNext(s2_req.valid)
  s3_req.bits   := RegNext(s2_req.bits)

  val s3_vt_hit     = RegNext(s2_vt_hit)
  val s3_vt_hit_idx = RegNext(s2_vt_hit_idx)
  val s3_can_pft    = RegNext(s2_can_pft)
  val s3_depth      = RegNext(Mux(
    s2_is_hit,
    1.U,      // hit a CDP prefetched block, depth == 2 or 4, reinforce
    Mux(s2_depth === 0.U, 4.U, s2_depth + 1.U)
  ))

  val s3_addr = s3_req.bits.vaddr

  // Update PLRU
  io.replace_upt.valid  := s3_req.valid && s3_vt_hit
  io.replace_upt.bits.set :=get_main_idx(s3_addr)
  io.replace_upt.bits.way := s3_vt_hit_idx

  // ------------------ s4 ------------------
  s4_req.valid  := RegNext(s3_req.valid && s3_can_pft)
  s4_req.bits   := RegNext(s3_req.bits)

  val s4_addr   = s4_req.bits.vaddr
  val s4_depth  = RegNext(s3_depth)

  pft_req.valid := s4_req.valid
  pft_req.bits.pfAddr   := s4_addr
  pft_req.bits.pfDepth  := s4_depth
  pft_req.bits.pfSource := s4_req.bits.pfSource
  pft_req.bits.is_hit   := s4_req.bits.is_hit

  // ------------------ Performance Counter ------------------
  XSPerfAccumulate(s"detect_pft_from_hit", s4_req.valid && s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_hit_cdp", s4_req.valid && s4_req.bits.is_hit && s4_req.bits.pfSource === PfSource.CDP.id.U)
  XSPerfAccumulate(s"detect_pft_from_hit_sms", s4_req.valid && s4_req.bits.is_hit && s4_req.bits.pfSource === PfSource.SMS.id.U)
  XSPerfAccumulate(s"detect_pft_from_hit_bop", s4_req.valid && s4_req.bits.is_hit && (s4_req.bits.pfSource === PfSource.BOP.id.U || s4_req.bits.pfSource === PfSource.PBOP.id.U))
  XSPerfAccumulate(s"detect_pft_from_refill", s4_req.valid && !s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_refill_cpu", s4_req.valid && s4_req.bits.pfSource === PfSource.NoWhere.id.U && !s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_refill_cdp", s4_req.valid && s4_req.bits.pfSource === PfSource.CDP.id.U && !s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_refill_other_pft", s4_req.valid && s4_req.bits.pfSource =/= PfSource.NoWhere.id.U && s4_req.bits.pfSource =/= PfSource.CDP.id.U && !s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_refill_bop", s4_req.valid && (s4_req.bits.pfSource === PfSource.BOP.id.U || s4_req.bits.pfSource === PfSource.PBOP.id.U) && !s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_refill_sms", s4_req.valid && s4_req.bits.pfSource === PfSource.SMS.id.U && !s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_refill_stream", s4_req.valid && s4_req.bits.pfSource === PfSource.Stream.id.U && !s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_refill_stride", s4_req.valid && s4_req.bits.pfSource === PfSource.Stride.id.U && !s4_req.bits.is_hit)
  XSPerfAccumulate(s"detect_pft_from_refill_berti", s4_req.valid && s4_req.bits.pfSource === PfSource.Berti.id.U && !s4_req.bits.is_hit)
  for (i <- 0 until 5) {
    XSPerfAccumulate(s"detect_pft_depth_$i", s4_req.valid && s4_depth === i.U)
  }

  XSPerfAccumulate(s"detect_req_from_hit", s0_req.valid && s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_hit_cdp", s0_req.valid && s0_req.bits.is_hit && s0_req.bits.pfSource === PfSource.CDP.id.U)
  XSPerfAccumulate(s"detect_req_from_hit_sms", s0_req.valid && s0_req.bits.is_hit && s0_req.bits.pfSource === PfSource.SMS.id.U)
  XSPerfAccumulate(s"detect_req_from_hit_bop", s0_req.valid && s0_req.bits.is_hit && (s0_req.bits.pfSource === PfSource.BOP.id.U || s0_req.bits.pfSource === PfSource.PBOP.id.U))
  XSPerfAccumulate(s"detect_req_from_refill", s0_req.valid && !s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_refill_cpu", s0_req.valid && s0_req.bits.pfSource === PfSource.NoWhere.id.U && !s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_refill_cdp", s0_req.valid && s0_req.bits.pfSource === PfSource.CDP.id.U && !s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_refill_other_pft", s0_req.valid && s0_req.bits.pfSource =/= PfSource.NoWhere.id.U && s0_req.bits.pfSource =/= PfSource.CDP.id.U && !s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_refill_bop", s0_req.valid && (s0_req.bits.pfSource === PfSource.BOP.id.U || s0_req.bits.pfSource === PfSource.PBOP.id.U) && !s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_refill_sms", s0_req.valid && s0_req.bits.pfSource === PfSource.SMS.id.U && !s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_refill_stream", s0_req.valid && s0_req.bits.pfSource === PfSource.Stream.id.U && !s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_refill_stride", s0_req.valid && s0_req.bits.pfSource === PfSource.Stride.id.U && !s0_req.bits.is_hit)
  XSPerfAccumulate(s"detect_req_from_refill_berti", s0_req.valid && s0_req.bits.pfSource === PfSource.Berti.id.U && !s0_req.bits.is_hit)
  for (i <- 0 until 5) {
    XSPerfAccumulate(s"detect_req_depth_$i", s0_req.valid && s0_req.bits.pfDepth === i.U)
  }
}

class PrefetchFilterEntry(implicit p: Parameters) extends CDPBundle {
  val paddr_valid = Bool()
  val pTag  = UInt(ReqFilterTagBits.W)
  val vTag  = UInt(ReqFilterTagBits.W)
  val pfDepth = UInt(4.W)
  
  // Only for monitor
  val pfSource = UInt(PfSource.pfSourceBits.W)
  val is_hit   = Bool()

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
  s1_valid := RegNext(s0_valid && !entry_hit)
  val s1_pft_req = RegNext(cdp_pft_req.bits)

  val free_entry_vec = valids.map(!_)
  val has_free_entry = free_entry_vec.reduce(_ || _)
  val free_entry_idx = PriorityEncoder(free_entry_vec)

  val idx = free_entry_idx
  val entry = entries(idx)
  when (s1_valid && has_free_entry) {
    val alloc_entry = WireInit(0.U.asTypeOf(new PrefetchFilterEntry))
    alloc_entry.vTag := block_addr(s1_pft_req.pfAddr)
    alloc_entry.pfDepth   := s1_pft_req.pfDepth
    alloc_entry.pfSource  := s1_pft_req.pfSource
    alloc_entry.is_hit    := s1_pft_req.is_hit

    entry := alloc_entry
    valids(idx) := true.B
  }

  val tlb_s0_fire_vec = VecInit(tlb_req_arb.io.in.map(_.fire))
  val tlb_s1_fire_vec = RegNext(tlb_s0_fire_vec)
  val tlb_s2_fire_vec = RegNext(tlb_s1_fire_vec)

  // --------------- tlb s0: arb the tlb req ---------------
  // Arbiter tlb_reqs
  for (i <- 0 until ReqFilterEntryNum) {
    val entry = entries(i)
    val entry_tlb_req = tlb_req_arb.io.in(i)

    // req tlb
    entry_tlb_req.valid := valids(i) && !entry.paddr_valid && !tlb_s1_fire_vec(i) && !tlb_s2_fire_vec(i)
    entry_tlb_req.bits  := DontCare
    entry_tlb_req.bits.vaddr  := Cat(entry.vTag, 0.U(log2Ceil(blockBytes).W))
    entry_tlb_req.bits.cmd    := TlbCmd.read
    entry_tlb_req.bits.isPrefetch := true.B
    entry_tlb_req.bits.size   := 3.U
  }

  // --------------- tlb s1: recv tlb rsp ---------------
  val tlb_s1_rsp = tlb_rsp

  // --------------- tlb s2: recv pmp rsp & update entry ---------------
  val tlb_s2_rsp_valid  = RegNext(tlb_s1_rsp.valid)
  val tlb_s2_rsp_bits   = RegNext(tlb_s1_rsp.bits)
  val tlb_s2_pmp        = pmp_rsp

  val fire_idx = PriorityEncoder(tlb_s2_fire_vec)
  val has_fire = tlb_s2_fire_vec.reduce(_ || _)

  val need_drop = 
    // page/access fault
    tlb_s2_rsp_bits.excp.head.pf.ld || tlb_s2_rsp_bits.excp.head.gpf.ld || tlb_s2_rsp_bits.excp.head.af.ld ||
    // uncache
    tlb_s2_pmp.mmio || Pbmt.isUncache(tlb_s2_rsp_bits.pbmt) ||
    // pmp access fault
    tlb_s2_pmp.ld

  when (tlb_s2_rsp_valid && !tlb_s2_rsp_bits.miss && has_fire) {
    when (need_drop) {
      // drop the req, i.e., clear the entry
      valids(fire_idx) := false.B
    }.otherwise {
      // update the entry with paddr
      entries(fire_idx).paddr_valid := true.B
      entries(fire_idx).pTag := block_addr(tlb_s2_rsp_bits.paddr.head)
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

  // Performance Counter
  XSPerfAccumulate("drop_dup", s0_valid && entry_hit)
  XSPerfAccumulate("drop_full", s1_valid && !has_free_entry)

  val selected_entry = entries(pf_req_arb.io.chosen)
  
  XSPerfAccumulate("pf_from_hit", pf_req_arb.io.out.valid && selected_entry.is_hit)
  XSPerfAccumulate("pf_from_hit_cdp", pf_req_arb.io.out.valid && selected_entry.is_hit && selected_entry.pfSource === PfSource.CDP.id.U)
  XSPerfAccumulate("pf_from_hit_sms", pf_req_arb.io.out.valid && selected_entry.is_hit && selected_entry.pfSource === PfSource.SMS.id.U)
  XSPerfAccumulate("pf_from_hit_bop", pf_req_arb.io.out.valid && selected_entry.is_hit && (selected_entry.pfSource === PfSource.BOP.id.U || selected_entry.pfSource === PfSource.PBOP.id.U))
  XSPerfAccumulate("pf_from_refill", pf_req_arb.io.out.valid && !selected_entry.is_hit)
  XSPerfAccumulate("pf_from_refill_cpu", pf_req_arb.io.out.valid && selected_entry.pfSource === PfSource.NoWhere.id.U && !selected_entry.is_hit)
  XSPerfAccumulate("pf_from_refill_cdp", pf_req_arb.io.out.valid && selected_entry.pfSource === PfSource.CDP.id.U && !selected_entry.is_hit)
  XSPerfAccumulate("pf_from_refill_other_pft", pf_req_arb.io.out.valid && selected_entry.pfSource =/= PfSource.NoWhere.id.U && selected_entry.pfSource =/= PfSource.CDP.id.U && !selected_entry.is_hit)
  XSPerfAccumulate("pf_from_refill_bop", pf_req_arb.io.out.valid && (selected_entry.pfSource === PfSource.BOP.id.U || selected_entry.pfSource === PfSource.PBOP.id.U) && !selected_entry.is_hit)
  XSPerfAccumulate(s"pf_from_refill_sms", pf_req_arb.io.out.valid && selected_entry.pfSource === PfSource.SMS.id.U && !selected_entry.is_hit)
  XSPerfAccumulate(s"pf_from_refill_stream", pf_req_arb.io.out.valid && selected_entry.pfSource === PfSource.Stream.id.U && !selected_entry.is_hit)
  XSPerfAccumulate(s"pf_from_refill_stride", pf_req_arb.io.out.valid && selected_entry.pfSource === PfSource.Stride.id.U && !selected_entry.is_hit)
  XSPerfAccumulate(s"pf_from_refill_berti", pf_req_arb.io.out.valid && selected_entry.pfSource === PfSource.Berti.id.U && !selected_entry.is_hit)
  for (i <- 0 until 5) {
    XSPerfAccumulate(s"pf_depth_$i", pf_req_arb.io.out.valid && selected_entry.pfDepth === i.U)
  }
}

class CDPPrefetcher(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    // TODO: require Slice Num == 4
    // detect
    val l2_detect_triggers = Flipped(Vec(4, ValidIO(new CDPDetectTrigger)))

    // train
    val train = Flipped(DecoupledIO(new PrefetchTrain))

    // tlb?
    val tlb_req = new L2ToL1TlbIO

    // prefetch req?
    val pft_req = DecoupledIO(new PrefetchReq)
  })

  private val cstEnable = Constantin.createRecord("cdp_enable"+cacheParams.hartId.toString, initValue = 1)

  val enable = io.enable & cstEnable.orR
  val train = io.train

  val l2_triggers = io.l2_detect_triggers

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

    /**
      * Check : Detection Condition
      * Hit Trigger:
          a) Hit a CDP prefetched block, pfDepth == 2 or 4
          b) Hit a SMS/BOP prefetched block (TODO!)

      * Refill Trigger:
          a) Refill a true demanded block
          b) Refill a CDP required block
          c) Refill other prefetcher's block. (TODO: filter)
    */
    val detect_trig_fromCDP = detect_trig.bits.pfSource === PfSource.CDP.id.U
    val detect_trig_fromSMS = detect_trig.bits.pfSource === PfSource.SMS.id.U
    val detect_trig_fromBOP = detect_trig.bits.pfSource === PfSource.BOP.id.U || detect_trig.bits.pfSource === PfSource.PBOP.id.U
    val detect_trig_fromStream  = detect_trig.bits.pfSource === PfSource.Stream.id.U
    val detect_trig_fromStride  = detect_trig.bits.pfSource === PfSource.Stride.id.U
    val detect_trig_fromCPU     = detect_trig.bits.pfSource === PfSource.NoWhere.id.U

    val hit_trigger       = detect_trig.bits.is_hit  &&
      (
        if (UseFilteredDetect) {
          detect_trig_fromCDP && (detect_trig.bits.pfDepth === 2.U || detect_trig.bits.pfDepth === 4.U) || detect_trig_fromSMS || detect_trig_fromBOP
        }
        else {
          detect_trig_fromCDP && (detect_trig.bits.pfDepth === 2.U || detect_trig.bits.pfDepth === 4.U)
        }
      )
    val refill_trigger    = !detect_trig.bits.is_hit && 
      (
        if (UseFilteredDetect) {
          detect_trig_fromCPU || detect_trig_fromCDP || detect_trig_fromStride || detect_trig_fromStream
        }
        else {
          true.B
        }
      )
    
    detect_trig_queue.io.flush := reset.asBool

    detect_trig_queue.io.enq(0).valid := detect_trig.valid && (hit_trigger || refill_trigger)
    detect_trig_queue.io.enq(0).bits.half_cacheblock  := detect_trig.bits.cacheblock(blockBits / 2 - 1, 0)
    detect_trig_queue.io.enq(0).bits.pfDepth  := detect_trig.bits.pfDepth
    detect_trig_queue.io.enq(0).bits.pfSource := detect_trig.bits.pfSource
    detect_trig_queue.io.enq(0).bits.is_hit   := detect_trig.bits.is_hit

    detect_trig_queue.io.enq(1).valid := detect_trig.valid && (hit_trigger || refill_trigger)
    detect_trig_queue.io.enq(1).bits.half_cacheblock  := detect_trig.bits.cacheblock(blockBits - 1, blockBits / 2)
    detect_trig_queue.io.enq(1).bits.pfDepth  := detect_trig.bits.pfDepth
    detect_trig_queue.io.enq(1).bits.pfSource := detect_trig.bits.pfSource
    detect_trig_queue.io.enq(1).bits.is_hit   := detect_trig.bits.is_hit

    detect_trig_arb.io.in(i) <> detect_trig_queue_seq(i).io.deq(0)
  }

  for (i <- 0 until DetectPipeNum) {
    val detect_pipe = detect_pipe_seq(i)

    detect_pipe.io.detect_req.valid := detect_trig_arb.io.out.valid
    detect_pipe.io.detect_req.bits.vaddr    := detect_trig_arb.io.out.bits.half_cacheblock((i + 1) * 64 - 1, i * 64)   // 8 Byte ==> 64 bit
    detect_pipe.io.detect_req.bits.pfDepth  := detect_trig_arb.io.out.bits.pfDepth
    detect_pipe.io.detect_req.bits.pfSource := detect_trig_arb.io.out.bits.pfSource
    detect_pipe.io.detect_req.bits.is_hit   := detect_trig_arb.io.out.bits.is_hit
  }
  detect_trig_arb.io.out.ready := true.B

  // Train Req
  val train_trig_queue = Module(new Queue(new CDPTrainTrigger, 8))
  train_trig_queue.io.enq.valid := train.valid && enable
  train_trig_queue.io.enq.bits.vaddr := train.bits.vaddr.getOrElse(0.U)
  train.ready := train_trig_queue.io.enq.ready
  //assert(!train_trig_queue.io.enq.valid | train_trig_queue.io.enq.ready, "l1_demand_hit_train_trigger should always be ready to accept new trigger!")

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

  // ------------------- Performance Counter -------------------
  XSPerfAccumulate("train_trigger_enq", train_trig_queue.io.enq.fire)
  XSPerfAccumulate("train_trigger_deq", train_trig_queue.io.deq.fire)
  XSPerfAccumulate("train_trigger_discard", train_trig_queue.io.enq.valid && !train_trig_queue.io.enq.ready)
  for (i <- 0 until 4) {
    XSPerfAccumulate(s"l2_trigger_${i}_enq", l2_triggers(i).valid)
  }
  for (i <- 0 until DetectPipeNum) {
    XSPerfAccumulate(s"detect_pipe_${i}_pft_drop", detect_pipe_seq(i).io.pft_req.valid && !detect_pipe_seq(i).io.pft_req.ready)
    XSPerfAccumulate(s"detect_pipe_${i}_detect_enq", detect_pipe_seq(i).io.detect_req.valid)
  }
}