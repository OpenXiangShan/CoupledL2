package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.{HasCoupledL2Parameters, L2ToL1TlbIO}
import utility._
import coupledL2._

case class CDPParameters(
  UseFilteredDetect:  Boolean = true,

  HotThreshold:   Int = 2,

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

  // FilterTable Params
  FilterTableSetNum:  Int = 64,
  FilterTableWayNum:  Int = 4,
  FilterEntryBlks:  Int = 64,         // 64 slots per entry
  FilterEntryGranularity: Int = 4096, // 4KB per slot

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

  val debug = cdpParams.debug

  val UseFilteredDetect = cdpParams.UseFilteredDetect

  val hot_threshold = cdpParams.HotThreshold

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

  // Filter Table Params
  val FilterTableSetNum = cdpParams.FilterTableSetNum
  val FilterTableWayNum = cdpParams.FilterTableWayNum
  val FilterEntryBlks = cdpParams.FilterEntryBlks
  val FilterEntryGranularity = cdpParams.FilterEntryGranularity

  val FilterTableOffsetBits = log2Ceil(FilterEntryBlks)
  val FilterTableSetBits    = log2Ceil(FilterTableSetNum)
  val FilterTableTagBits    = fullAddressBits - log2Ceil(FilterEntryGranularity) - FilterTableSetBits - FilterTableOffsetBits

  def get_filter_addr(addr: UInt) = {
    addr(addr.getWidth - 1, log2Ceil(FilterEntryGranularity))
  }

  def get_filter_offset(addr: UInt) = {
    val filter_addr = get_filter_addr(addr)
    filter_addr(FilterTableOffsetBits - 1, 0)
  }

  def get_filter_set(addr: UInt) = {
    val filter_addr = get_filter_addr(addr)
    filter_addr(FilterTableSetBits + FilterTableOffsetBits - 1, FilterTableOffsetBits)
  }

  def get_filter_tag(addr: UInt) = {
    val filter_addr = get_filter_addr(addr)
    filter_addr(filter_addr.getWidth - 1, FilterTableSetBits + FilterTableOffsetBits)
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
}

class CDPDetectEntry(implicit p: Parameters) extends CDPBundle {
  val half_cacheblock = UInt((blockBits / 2).W)
  val pfDepth         = UInt(4.W)
  val pfSource        = UInt(PfSource.pfSourceBits.W)

  val is_hit  = Bool()
}

class CDPTrainTrigger(implicit p: Parameters) extends CDPBundle {
  val vaddr = UInt(fullAddressBits.W)
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
  val valid_vec = Vec(VpnTableWayNum, Bool())
}

class vtTrainReq(implicit p: Parameters) extends CDPBundle {
  val alloc_main  = Bool()    // allocate a new MainEntry (clear all the SubEntries)
  val alloc_sub   = Bool()    // allocate a new SubEntry (refCnt = 1)
  val target_way  = UInt(log2Ceil(VpnTableWayNum).W)

  val main_idx    = UInt(mainEntryBits.W)
  val sub_idx     = UInt(subEntryBits.W)

  val tag         = UInt(vpnTabTagBits.W)
}

class VpnTable(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val query_req = Flipped(Vec(DetectPipeNum + 1, ValidIO(new vtQueryReq)))   // +1 for train pipe
    val query_rsp = Vec(DetectPipeNum + 1, ValidIO(new vtQueryRsp))
    val train_req = Flipped(ValidIO(new vtTrainReq))
  })

  val (query_req, query_rsp) = (io.query_req, io.query_rsp)
  val train_req = io.train_req

  val refreshCnt = RegInit(0.U(32.W))
  val is_refresh = Wire(Bool())

  // TODO: use SRAM ?
  // Tag Array
  val tag_array = RegInit(VecInit(Seq.fill(VpnTableSetNum)(
    VecInit(Seq.fill(VpnTableWayNum)(0.U(vpnTabTagBits.W)))
  )))

  // Valid Array
  val valid_array = RegInit(VecInit(Seq.fill(VpnTableSetNum)(
    VecInit(Seq.fill(VpnTableWayNum)(false.B))
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

    rsp.valid := !reset.asBool

    val (main_idx, sub_idx) = (req.bits.main_idx, req.bits.sub_idx)

    for (j <- 0 until VpnTableWayNum) {
      rsp.bits.tag_vec(j)   := tag_array(main_idx)(j)
      rsp.bits.meta_vec(j)  := meta_array(main_idx)(j)(sub_idx)
      rsp.bits.valid_vec(j) := valid_array(main_idx)(j)
    }
  }

  // Train Logic
  when (train_req.valid && !is_refresh) {
    val (main_idx, sub_idx) = (train_req.bits.main_idx, train_req.bits.sub_idx)
    val target_way = train_req.bits.target_way
    val (alloc_main, alloc_sub) = (train_req.bits.alloc_main, train_req.bits.alloc_sub)
    val no_alloc = !alloc_main && !alloc_sub

    assert(!(alloc_main && alloc_sub), "TrainReq can't allocate both main entry and sub entry!")

    when (alloc_main) {
      // use target_way for replacement
      val replace_way = target_way

      // Update Tag
      tag_array(main_idx)(replace_way)  := train_req.bits.tag

      // Update Valid
      valid_array(main_idx)(replace_way) := true.B

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
    }

    when (alloc_sub) {
      // only update the meta of the target sub entry
      meta_array(main_idx)(target_way)(sub_idx).valid := true.B
      meta_array(main_idx)(target_way)(sub_idx).refCnt := 1.U
    }

    when (no_alloc) {
      // only update the refCnt of the target sub entry
      meta_array(main_idx)(target_way)(sub_idx).refCnt := meta_array(main_idx)(target_way)(sub_idx).refCnt + 1.U
    }
  }

  // Refresh Logic
  when (refreshCnt < VpnResetPeriod.U && train_req.valid){
    refreshCnt := refreshCnt + 1.U
  }

  is_refresh := refreshCnt >= VpnResetPeriod.U && !reset.asBool
  when (is_refresh) {
    refreshCnt := 0.U

    // go through every sub entry
    for (i <- 0 until VpnTableSetNum) {
      for (j <- 0 until VpnTableWayNum) {
        for (k <- 0 until VpnTableSubEntryNum) {
          val entry = meta_array(i)(j)(k)

          /**
           * Update Entry:
           *  prevRefCnt -> 0.8 * refCnt + 0.2 * prevRefCnt
           *  refCnt  -> 0
           *  hot -> prevRefCnt > hot_threshold ? 1 : 0
           * */
          // TODO: For better timing, maybe we should pipeline this.
          val nxt_prevRefCnt = ((entry.refCnt * 13.U) + (entry.prevRefCnt * 3.U)) >> 4.U

          entry.refCnt      := 0.U
          entry.prevRefCnt  := nxt_prevRefCnt
          entry.hot         := Mux(nxt_prevRefCnt > hot_threshold.U, true.B, false.B)
        }
      }
    }
  }

  // ------------------ Performance Counter ------------------
  XSPerfAccumulate("vt_refresh", is_refresh)
  XSPerfAccumulate("vt_alloc_main", train_req.valid && train_req.bits.alloc_main)
  XSPerfAccumulate("vt_alloc_sub", train_req.valid && train_req.bits.alloc_sub)
  XSPerfAccumulate("vt_no_alloc", train_req.valid && !train_req.bits.alloc_main && !train_req.bits.alloc_sub)

  // train trig data
  XSPerfAccumulate("in_train_trig_used", train_req.valid && !is_refresh)
  XSPerfAccumulate("in_train_trig_drop_by_refresh", train_req.valid && is_refresh)
}

class ftQueryReq(implicit p: Parameters) extends CDPBundle {
  val set_idx = UInt(FilterTableSetBits.W)
  val offset  = UInt(FilterTableOffsetBits.W)
}

class ftQueryRsp(implicit p: Parameters) extends CDPBundle {
  val valid_vec = Vec(FilterTableWayNum, Bool())
  val tag_vec   = Vec(FilterTableWayNum, UInt(FilterTableTagBits.W))
  val sat_vec   = Vec(FilterTableWayNum, UInt(2.W))
}

class ftTrainReq(implicit p: Parameters) extends CDPBundle {
  val alloc   = Bool()
  val set_idx = UInt(FilterTableSetBits.W)
  val target_way  = UInt(log2Ceil(FilterTableWayNum).W)
  val tag     = UInt(FilterTableTagBits.W)
  val offset  = UInt(FilterTableOffsetBits.W)

  val is_used   = Bool()
}

class FilterTable(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val query_req = Flipped(Vec(2, ValidIO(new ftQueryReq)))
    val query_rsp = Vec(2, Output(new ftQueryRsp))
    val train_req = Flipped(ValidIO(new ftTrainReq))
  })

  val (query_req, query_rsp) = (io.query_req, io.query_rsp)
  val train_req = io.train_req

  // Tag Array
  val tag_array = RegInit(VecInit(Seq.fill(FilterTableSetNum)(
    VecInit(Seq.fill(FilterTableWayNum)(0.U(FilterTableTagBits.W)))
  )))

  // Valid Array
  val valid_array = RegInit(VecInit(Seq.fill(FilterTableSetNum)(
    VecInit(Seq.fill(FilterTableWayNum)(false.B))
  )))

  // Sat Cnt Array
  val sat_array = RegInit(VecInit(Seq.fill(FilterTableSetNum)(
    VecInit(Seq.fill(FilterTableWayNum)(
      VecInit(Seq.fill(FilterEntryBlks)(2.U(2.W)))
    ))
  )))

  // query
  for (i <- 0 until 2) {
    val (req, rsp) = (query_req(i), query_rsp(i))

    rsp.valid_vec := valid_array(req.bits.set_idx)
    for (j <- 0 until FilterTableWayNum) {
      rsp.tag_vec(j) := tag_array(req.bits.set_idx)(j)
      rsp.sat_vec(j) := sat_array(req.bits.set_idx)(j)(req.bits.offset)
    }
  }

  // train
  when (train_req.valid) {
    val (set_idx, target_way) = (train_req.bits.set_idx, train_req.bits.target_way)

    when (train_req.bits.alloc) {
      // Update Tag
      tag_array(set_idx)(target_way) := train_req.bits.tag

      // Update Valid
      valid_array(set_idx)(target_way) := true.B

      // Update Sat Cnt
      for (i <- 0 until FilterEntryBlks) {
        when (i.U =/= train_req.bits.offset) {
          sat_array(set_idx)(target_way)(i) := 2.U
        }.otherwise {
          sat_array(set_idx)(target_way)(i) := Mux(train_req.bits.is_used, 1.U, 3.U)
        }
      }
    }.otherwise {
      // only update the sat counter of the target way & offset
      val offset = get_filter_offset(train_req.bits.tag)
      val sat_cnt = sat_array(set_idx)(target_way)(offset)

      when (train_req.bits.is_used) {
        // if this prefetch is used, it's a good sign, decrease the counter (saturate at 0)
        sat_array(set_idx)(target_way)(offset) := Mux(sat_cnt === 0.U, 0.U, sat_cnt - 1.U)
      }.otherwise {
        // if this prefetch is not used, it's a bad sign, increase the counter (saturate at 3)
        sat_array(set_idx)(target_way)(offset) := Mux(sat_cnt === 3.U, 3.U, sat_cnt + 1.U)
      }
    }
  }
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

class TrainPipeline(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val vt_train_trigger = Flipped(DecoupledIO(new PrefetchTrain))
    val ft_train_trigger = Flipped(DecoupledIO(new PrefetchTrain))

    // to VPN Table
    val vt_query_req = ValidIO(new vtQueryReq)
    val vt_query_rsp = Flipped(ValidIO(new vtQueryRsp))
    val vt_train_req = ValidIO(new vtTrainReq)

    // to Filter Table
    val ft_query_req = ValidIO(new ftQueryReq)
    val ft_query_rsp = Input(new ftQueryRsp)
    val ft_train_req = ValidIO(new ftTrainReq)
  })

  val (vt_train_trigger, ft_train_trigger) = (io.vt_train_trigger, io.ft_train_trigger)
  val (vt_query_req, vt_query_rsp, vt_train_req) = (io.vt_query_req, io.vt_query_rsp, io.vt_train_req)
  val (ft_query_req, ft_query_rsp, ft_train_req) = (io.ft_query_req, io.ft_query_rsp, io.ft_train_req)

  // Replacer
  val vt_replacer = ReplacementPolicy.fromString(replType, VpnTableWayNum, VpnTableSetNum)
  val ft_replacer = ReplacementPolicy.fromString(replType, FilterTableWayNum, FilterTableSetNum)

  // Pipeline Control Signals Def
  val vt_stage_valid  = Wire(Vec(5, Bool()))
  val ft_stage_valid  = Wire(Vec(5, Bool()))

  val vt_same_vec = Wire(Vec(4, Bool()))
  val ft_same_vec = Wire(Vec(4, Bool()))

  // ----------- s0 -----------
  val vt_same_addr  = vt_same_vec.reduce(_ || _)
  val ft_same_addr  = ft_same_vec.reduce(_ || _)
  
  // VpnTable
  vt_stage_valid(0) := vt_train_trigger.valid && !vt_same_addr
  vt_train_trigger.ready := reset.asBool || !vt_same_addr

  val vt_train_vaddr  = vt_train_trigger.bits.vaddr.getOrElse(0.U) << log2Ceil(blockBytes)
  val vt_s0_main_idx = get_main_idx(vt_train_vaddr)
  val vt_s0_sub_idx  = get_sub_idx(vt_train_vaddr)
  val vt_s0_tag      = get_vpntab_tag(vt_train_vaddr)

  // FilterTable
  ft_stage_valid(0) := ft_train_trigger.valid && !ft_same_addr
  ft_train_trigger.ready := reset.asBool || !ft_same_addr

  val ft_train_paddr  = ft_train_trigger.bits.addr
  val ft_s0_set_idx   = get_filter_set(ft_train_paddr)
  val ft_s0_offset    = get_filter_offset(ft_train_paddr)
  val ft_s0_tag       = get_filter_tag(ft_train_paddr)
  val ft_s0_is_used   = ft_train_trigger.bits.cdp_filter_train_hit

  // ----------- s1 -----------
  // VpnTable: query
  vt_stage_valid(1) := RegNext(vt_stage_valid(0))

  val vt_s1_main_idx  = RegNext(vt_s0_main_idx)
  val vt_s1_sub_idx   = RegNext(vt_s0_sub_idx)
  val vt_s1_tag       = RegNext(vt_s0_tag)
  val vt_s1_tab_rsp   = vt_query_rsp.bits

  vt_query_req.valid := vt_stage_valid(1)
  vt_query_req.bits.main_idx := vt_s1_main_idx
  vt_query_req.bits.sub_idx  := vt_s1_sub_idx

  vt_same_vec(0) := vt_stage_valid(1) && vt_s1_main_idx === vt_s0_main_idx && vt_s1_tag === vt_s0_tag

  // FilterTable: query
  ft_stage_valid(1) := RegNext(ft_stage_valid(0))

  val ft_s1_set_idx   = RegNext(ft_s0_set_idx)
  val ft_s1_offset    = RegNext(ft_s0_offset)
  val ft_s1_tag       = RegNext(ft_s0_tag)
  val ft_s1_is_used   = RegNext(ft_s0_is_used)
  val ft_s1_tab_rsp   = ft_query_rsp

  ft_query_req.valid := ft_stage_valid(1)
  ft_query_req.bits.set_idx := ft_s1_set_idx
  ft_query_req.bits.offset  := ft_s1_offset
  ft_same_vec(0) := ft_stage_valid(1) && ft_s1_set_idx === ft_s0_set_idx && ft_s1_tag === ft_s0_tag

  // ----------- s2 -----------
  // VpnTable: check miss
  vt_stage_valid(2) := RegNext(vt_stage_valid(1))

  val vt_s2_main_idx = RegNext(vt_s1_main_idx)
  val vt_s2_sub_idx  = RegNext(vt_s1_sub_idx)
  val vt_s2_tag      = RegNext(vt_s1_tag)
  val vt_s2_tab_rsp  = RegNext(vt_s1_tab_rsp)

  val vt_s2_tag_vec    = vt_s2_tab_rsp.tag_vec
  val vt_s2_meta_vec   = vt_s2_tab_rsp.meta_vec
  val vt_s2_valid_vec  = vt_s2_tab_rsp.valid_vec

  val vt_s2_hit_main_vec = vt_s2_tag_vec.zip(vt_s2_valid_vec).map {
    case (t, v) =>
      t === vt_s2_tag && v
  }
  val vt_s2_hit_main_idx = PriorityEncoder(vt_s2_hit_main_vec)
  val vt_s2_hit_main     = vt_s2_hit_main_vec.reduce(_ || _)
  assert(PopCount(vt_s2_hit_main_vec) < 2.U || !vt_stage_valid(2), "Main entry multiple hit!")

  val vt_s2_hit_sub_vec  = vt_s2_tag_vec.zip(vt_s2_meta_vec).zip(vt_s2_valid_vec).map {
    case ((t, m), v) =>
      t === vt_s2_tag && m.valid && v
  }
  val vt_s2_hit_sub      = vt_s2_hit_sub_vec.reduce(_ || _)

  vt_same_vec(1) := vt_stage_valid(2) && vt_s2_main_idx === vt_s0_main_idx && vt_s2_tag === vt_s0_tag

  // FilterTable: check miss
  ft_stage_valid(2) := RegNext(ft_stage_valid(1))
  val ft_s2_set_idx   = RegNext(ft_s1_set_idx)
  val ft_s2_offset    = RegNext(ft_s1_offset)
  val ft_s2_tag       = RegNext(ft_s1_tag)
  val ft_s2_is_used   = RegNext(ft_s1_is_used)

  val ft_s2_valid_vec  = ft_s1_tab_rsp.valid_vec
  val ft_s2_tag_vec    = ft_s1_tab_rsp.tag_vec

  val ft_s2_hit_vec    = ft_s2_tag_vec.zip(ft_s2_valid_vec).map {
    case (t, v) =>
      t === ft_s2_tag && v
  }

  val ft_s2_hit         = ft_s2_hit_vec.reduce(_ || _)
  val ft_s2_hit_idx     = PriorityEncoder(ft_s2_hit_vec)

  ft_same_vec(1) := ft_stage_valid(2) && ft_s2_set_idx === ft_s0_set_idx && ft_s2_tag === ft_s0_tag

  // ----------- s3 -----------
  // VpnTable: get replacement way & generate update info
  vt_stage_valid(3) := RegNext(vt_stage_valid(2))

  val vt_s3_main_idx = RegNext(vt_s2_main_idx)
  val vt_s3_sub_idx  = RegNext(vt_s2_sub_idx)
  val vt_s3_tag      = RegNext(vt_s2_tag)

  val vt_s3_hit_main     = RegNext(vt_s2_hit_main)
  val vt_s3_hit_sub      = RegNext(vt_s2_hit_sub)
  val vt_s3_hit_main_idx = RegNext(vt_s2_hit_main_idx)

  val vt_s3_update_info = WireInit(0.U.asTypeOf(new vtTrainReq))

  val plru_way = vt_replacer.way(vt_s3_main_idx)

  vt_s3_update_info.tag  := vt_s3_tag
  vt_s3_update_info.main_idx := vt_s3_main_idx
  vt_s3_update_info.sub_idx  := vt_s3_sub_idx

  vt_s3_update_info.alloc_main := !vt_s3_hit_main
  vt_s3_update_info.alloc_sub  := vt_s3_hit_main && !vt_s3_hit_sub
  vt_s3_update_info.target_way := Mux(vt_s3_hit_main, vt_s3_hit_main_idx, plru_way)

  vt_same_vec(2) := vt_stage_valid(3)  && vt_s3_main_idx === vt_s0_main_idx && vt_s3_tag === vt_s0_tag

  // FilterTable: get replacement way & generate update info
  ft_stage_valid(3) := RegNext(ft_stage_valid(2))
  val ft_s3_set_idx   = RegNext(ft_s2_set_idx)
  val ft_s3_offset    = RegNext(ft_s2_offset)
  val ft_s3_tag       = RegNext(ft_s2_tag)
  val ft_s3_is_used   = RegNext(ft_s2_is_used)

  val ft_s3_hit        = RegNext(ft_s2_hit)
  val ft_s3_hit_idx    = RegNext(ft_s2_hit_idx)

  val ft_s3_update_info = WireInit(0.U.asTypeOf(new ftTrainReq))
  val ft_plru_way = ft_replacer.way(ft_s3_set_idx)
  ft_s3_update_info.set_idx := ft_s3_set_idx
  ft_s3_update_info.offset  := ft_s3_offset
  ft_s3_update_info.tag     := ft_s3_tag
  ft_s3_update_info.alloc    := !ft_s3_hit
  ft_s3_update_info.is_used  := ft_s3_is_used
  ft_s3_update_info.target_way := Mux(ft_s3_hit, ft_s3_hit_idx, ft_plru_way)

  ft_same_vec(2) := ft_stage_valid(3) && ft_s3_hit && ft_s3_set_idx === ft_s0_set_idx && ft_s3_tag === ft_s0_tag

  // ----------- s4 -----------
  // VpnTable: update table & plru
  vt_stage_valid(4) := RegNext(vt_stage_valid(3))

  val vt_s4_update_info  = RegNext(vt_s3_update_info)

  vt_train_req.valid  := vt_stage_valid(4)
  vt_train_req.bits   := vt_s4_update_info

  vt_replacer.access(vt_s4_update_info.main_idx, vt_s4_update_info.target_way)

  vt_same_vec(3) := vt_stage_valid(4) && vt_s4_update_info.main_idx === vt_s0_main_idx && vt_s4_update_info.tag === vt_s0_tag

  // FilterTable: update table & plru
  ft_stage_valid(4) := RegNext(ft_stage_valid(3))

  val ft_s4_update_info = RegNext(ft_s3_update_info)

  ft_train_req.valid := ft_stage_valid(4)
  ft_train_req.bits  := ft_s4_update_info

  ft_replacer.access(ft_s4_update_info.set_idx, ft_s4_update_info.target_way)

  ft_same_vec(3) := ft_stage_valid(4) && ft_s4_update_info.set_idx === ft_s0_set_idx && ft_s4_update_info.tag === ft_s0_tag

  // ----------- ChiselDB -----------
  class trainTriggerEntry extends CDPBundle {
    val vaddr = UInt(fullAddressBits.W)
    val main_idx = UInt(mainEntryBits.W)
    val sub_idx  = UInt(subEntryBits.W)
    val way      = UInt(vpnWayBits.W)
    val alloc_main  = Bool()
    val alloc_sub   = Bool()
    val no_alloc    = Bool()
  }

  val cdpTrainTriggerDB = ChiselDB.createTable("cdpTrain", new trainTriggerEntry, basicDB = debug)

  val train_trigger_entry = Wire(new trainTriggerEntry)
  train_trigger_entry.vaddr := RegNext(RegNext(RegNext(RegNext(vt_train_vaddr))))     // vaddr in s4
  train_trigger_entry.main_idx  := vt_s4_update_info.main_idx
  train_trigger_entry.sub_idx   := vt_s4_update_info.sub_idx
  train_trigger_entry.way       := vt_s4_update_info.target_way
  train_trigger_entry.alloc_main := vt_s4_update_info.alloc_main
  train_trigger_entry.alloc_sub  := vt_s4_update_info.alloc_sub
  train_trigger_entry.no_alloc   := !vt_s4_update_info.alloc_main && !vt_s4_update_info.alloc_sub

  cdpTrainTriggerDB.log(train_trigger_entry, vt_stage_valid(4), "", clock, reset)
}

class DetectPipeline(name:String)(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val detect_req    = Flipped(ValidIO(new CDPDetectReq))

    // to Vpn Table
    val vt_query_req  = ValidIO(new vtQueryReq)
    val vt_query_rsp  = Flipped(ValidIO(new vtQueryRsp))

    // Prefetch Req
    val pft_req = ValidIO(new CDPPrefetchReq)
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
  assert(PopCount(s2_vt_hit_vec) < 2.U || !s2_req.valid, "VpnTable multiple hit in DetectPipeline!")
  
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
  // Valid VpnTable hit/miss and distribution
  XSPerfAccumulate("valid_vt_hit", s2_req.valid && s2_vt_hit && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero)
  XSPerfAccumulate("valid_vt_miss", s2_req.valid && !s2_vt_hit && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero)

  // ----------- ChiselDB -----------
  class detectTriggerEntry extends CDPBundle {
    val vaddr     = UInt(fullAddressBits.W)
    val pfDepth   = UInt(4.W)
    val pfSource  = UInt(PfSource.pfSourceBits.W)
    val main_idx    = UInt(mainEntryBits.W)
    val sub_idx     = UInt(subEntryBits.W)
    val vt_hit      = Bool()
    val vt_hit_hot  = Bool()
    val canPft      = Bool()
  }

  val cdpDetectTriggerDB = ChiselDB.createTable(name + "_cdpDetect", new detectTriggerEntry, basicDB = debug)

  val detect_trigger_entry = Wire(new detectTriggerEntry)
  detect_trigger_entry.vaddr := s2_addr
  detect_trigger_entry.pfDepth := s2_depth
  detect_trigger_entry.pfSource := s2_req.bits.pfSource
  detect_trigger_entry.main_idx := RegNext(s1_main_idx)
  detect_trigger_entry.sub_idx  := RegNext(s1_sub_idx)
  detect_trigger_entry.vt_hit := s2_vt_hit
  detect_trigger_entry.vt_hit_hot := s2_vt_hit_hot
  detect_trigger_entry.canPft := s2_can_pft

  val en = s2_req.valid && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero
  cdpDetectTriggerDB.log(detect_trigger_entry, en, "", clock, reset)
}

class PrefetchFilterEntry(implicit p: Parameters) extends CDPBundle {
  val paddr_valid = Bool()
  val pTag  = UInt(ReqFilterTagBits.W)
  val vTag  = UInt(ReqFilterTagBits.W)
  val pfDepth = UInt(4.W)

  // for TLB retry
  val retry_en    = Bool()
  val retry_timer = UInt(4.W)
  
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

class SentUnit(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val in  = Flipped(DecoupledIO(new CDPPrefetchReq))
    val out = DecoupledIO(new PrefetchReq)

    // tlb
    val tlb_req = new L2ToL1TlbIO

    // filter table
    val ft_query_req = ValidIO(new ftQueryReq)
    val ft_query_rsp = Input(new ftQueryRsp)
  })

  val (in, out) = (io.in, io.out)
  val (ft_query_req, ft_query_rsp) = (io.ft_query_req, io.ft_query_rsp)

  val tlb_req = io.tlb_req.req
  val tlb_rsp = io.tlb_req.resp
  val pmp_rsp = io.tlb_req.pmp_resp
  io.tlb_req.req_kill := false.B
  tlb_rsp.ready := true.B

  // check same cacheline
  def block_addr(addr: UInt) = {
    addr(fullAddressBits - 1, log2Ceil(blockBytes))
  }

  // buffer
  val valids    = RegInit(VecInit(Seq.fill(ReqFilterEntryNum)(false.B)))
  val entries   = RegInit(VecInit(Seq.fill(ReqFilterEntryNum)(0.U.asTypeOf(new PrefetchFilterEntry))))
  
  val tlb_inflight  = RegInit(VecInit(Seq.fill(ReqFilterEntryNum)(false.B)))
  val req_inflight  = RegInit(VecInit(Seq.fill(ReqFilterEntryNum)(false.B)))

  val tlb_arb = Module(new RRArbiterInit(new L2TlbReq, ReqFilterEntryNum))
  val pft_arb = Module(new RRArbiterInit(new PrefetchReq, ReqFilterEntryNum))
  tlb_arb.io.out <> tlb_req
  pft_arb.io.out <> out

  // enq buf logic
  in.ready := true.B  // TODO: backpressure when buffer full

  val entry_hit_vec = entries.zip(valids).map{
    case (e, v) =>
      v && e.vTag === block_addr(in.bits.pfAddr)
  }
  val entry_hit = entry_hit_vec.reduce(_ || _)

  val free_entry_vec = valids.map(!_)
  val has_free_entry = free_entry_vec.reduce(_ || _)
  val free_entry_idx = PriorityEncoder(free_entry_vec)

  val idx = free_entry_idx
  val entry = entries(idx)
  when (in.valid && !entry_hit && has_free_entry) {
    val alloc_entry = WireInit(0.U.asTypeOf(new PrefetchFilterEntry))
    alloc_entry.vTag := block_addr(in.bits.pfAddr)
    alloc_entry.pfDepth   := in.bits.pfDepth
    alloc_entry.pfSource  := in.bits.pfSource
    alloc_entry.is_hit    := in.bits.is_hit

    entry := alloc_entry
    valids(idx) := true.B
  }

  // timer
  for (i <- 0 until ReqFilterEntryNum) {
    when (entries(i).retry_en && entries(i).retry_timer < 10.U) {   // TODO: parameterize the interval value
      entries(i).retry_timer := entries(i).retry_timer + 1.U
    }
  }

  // --------------- tlb pipe ---------------
  val tlb_s0_valid = Wire(Bool())
  val tlb_s1_valid = Wire(Bool())
  val tlb_s2_valid = Wire(Bool())

  val tlb_s0_chosen_idx = Wire(UInt(log2Ceil(ReqFilterEntryNum).W))
  val tlb_s1_chosen_idx = RegNext(tlb_s0_chosen_idx)
  val tlb_s2_chosen_idx = RegNext(tlb_s1_chosen_idx)

  // -------- tlb s0: arb tlb req --------
  for (i <- 0 until ReqFilterEntryNum) {
    val entry = entries(i)
    val entry_tlb_req = tlb_arb.io.in(i)

    val entry_timer_ok = !entry.retry_en || entry.retry_timer >= 10.U

    entry_tlb_req.valid := valids(i) && !entry.paddr_valid && entry_timer_ok && !tlb_inflight(i)
    entry_tlb_req.bits  := DontCare
    entry_tlb_req.bits.vaddr  := Cat(entry.vTag, 0.U(log2Ceil(blockBytes).W))
    entry_tlb_req.bits.cmd    := TlbCmd.read
    entry_tlb_req.bits.isPrefetch := true.B
    entry_tlb_req.bits.size   := 3.U

    when (entry_tlb_req.fire) {
      tlb_inflight(i) := true.B
    }
  }

  tlb_s0_valid := tlb_arb.io.out.valid
  tlb_s0_chosen_idx := tlb_arb.io.chosen

  // -------- tlb s1: recv tlb rsp --------
  // check page/access fault, if fault, drop the req by clearing the entry
  // if miss, enable retry. If second miss, drop the req
  val tlb_s1_rsp = tlb_rsp

  tlb_s1_valid := RegNext(tlb_s0_valid)

  val s1_drop = 
    tlb_s1_rsp.valid && 
    tlb_s1_rsp.bits.miss && entries(tlb_s1_chosen_idx).retry_en

  // drop when second tlb miss
  when (tlb_s1_valid && tlb_s1_rsp.valid && s1_drop) {
    valids(tlb_s1_chosen_idx) := false.B
    tlb_inflight(tlb_s1_chosen_idx) := false.B
  }

  // update when first tlb miss
  when (tlb_s1_valid && tlb_s1_rsp.valid && tlb_s1_rsp.bits.miss && !entries(tlb_s1_chosen_idx).retry_en) {
    entries(tlb_s1_chosen_idx).retry_en := true.B
  }

  // -------- tlb s2: recv pmp rsp --------
  tlb_s2_valid := RegNext(tlb_s1_valid && !s1_drop)
  val tlb_s2_pmp        = pmp_rsp
  val tlb_s2_rsp_valid  = RegNext(tlb_s1_rsp.valid)
  val tlb_s2_rsp_bits   = RegNext(tlb_s1_rsp.bits)

  val s2_drop = 
    // page/access fault
    tlb_s2_rsp_bits.excp.head.pf.ld || tlb_s2_rsp_bits.excp.head.gpf.ld || tlb_s2_rsp_bits.excp.head.af.ld ||
    // uncache
    tlb_s2_pmp.mmio || Pbmt.isUncache(tlb_s2_rsp_bits.pbmt) ||
    // pmp access fault
    tlb_s2_pmp.ld

  when (tlb_s2_valid && tlb_s2_rsp_valid && !tlb_s2_rsp_bits.miss) {
    when (s2_drop) {
      valids(tlb_s2_chosen_idx) := false.B
    }.otherwise {
      entries(tlb_s2_chosen_idx).paddr_valid := true.B
      entries(tlb_s2_chosen_idx).pTag := block_addr(tlb_s2_rsp_bits.paddr.head)
    }
  }

  when (tlb_s2_valid) {
    tlb_inflight(tlb_s2_chosen_idx) := false.B
  }

  // --------------- prefetch req pipe ---------------
  val pft_s0_valid = Wire(Bool())
  val pft_s1_valid = RegNext(pft_s0_valid)

  val pft_s0_chosen_idx = Wire(UInt(log2Ceil(ReqFilterEntryNum).W))
  val pft_s1_chosen_idx = RegNext(pft_s0_chosen_idx)

  // --------- req s0: arb prefetch req ---------
  for (i <- 0 until ReqFilterEntryNum) {
    val entry = entries(i)
    val entry_pft_req = pft_arb.io.in(i)

    entry_pft_req.valid := valids(i) && entry.paddr_valid && !req_inflight(i)
    entry_pft_req.bits := entry.toPrefetchReq()

    when (entry_pft_req.fire) {
      req_inflight(i) := true.B
    }
  }

  pft_s0_valid := pft_arb.io.out.valid
  pft_arb.io.out.ready := true.B
  pft_s0_chosen_idx := pft_arb.io.chosen

  val pft_s0_req = pft_arb.io.out.bits

  // query the FilterTable
  ft_query_req.valid := pft_s0_valid
  ft_query_req.bits.set_idx := get_filter_set(pft_s0_req.addr)
  ft_query_req.bits.offset  := get_filter_offset(pft_s0_req.addr)

  // --------- req s1: recv filter table rsp ---------
  val ft_s1_rsp = RegNext(ft_query_rsp)

  val valid_vec = ft_s1_rsp.valid_vec
  val tag_vec   = ft_s1_rsp.tag_vec
  val hit_vec   = valid_vec.zip(tag_vec).map{
    case (v, t) =>
      v && t === get_filter_tag(pft_s0_req.addr)
  }
  val hit = hit_vec.reduce(_ || _)
  val hit_idx = PriorityEncoder(hit_vec)

  val sat_vec = ft_s1_rsp.sat_vec
  val can_pft = !hit || sat_vec(hit_idx) === 3.U

  out.valid := pft_s1_valid && can_pft
  out.bits := pft_s0_req

  when (out.fire) {
    valids(pft_s1_chosen_idx) := false.B
  }
  
  when (pft_s1_valid) {
    req_inflight(pft_s1_chosen_idx) := false.B
  }
}

class CDPPrefetcher(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    // TODO: require Slice Num == 4
    // detect
    val l2_detect_triggers = Flipped(Vec(4, ValidIO(new CDPDetectTrigger)))

    // train
    val vpn_train     = Flipped(ValidIO(new PrefetchTrain))
    val filter_train  = Flipped(ValidIO(new PrefetchTrain))

    // tlb?
    val tlb_req = new L2ToL1TlbIO

    // prefetch req?
    val pft_req = DecoupledIO(new PrefetchReq)
  })

  println(s"====== CDP Prefetcher Config (hart ${cacheParams.hartId}) ======")
  println(s"UseFilteredDetect:  $UseFilteredDetect")
  println(s"Degree:             $Degree")
  println(s"VpnTableTagBits:    $vpnTabTagBits")
  println(s"VpnSubEntryBits:    $EntryBits")
  println(s"VpnResetPeriod:     $VpnResetPeriod")
  println(s"HotThreshold:       $hot_threshold")
  println(s"debug mode:         $debug")
  println(s"============================================")


  private val cstEnable = Constantin.createRecord("cdp_enable"+cacheParams.hartId.toString, initValue = 1)

  val enable = io.enable & cstEnable.orR
  val (vpn_train, filter_train) = (io.vpn_train, io.filter_train)

  val l2_triggers = io.l2_detect_triggers

  val filter_table      = Module(new FilterTable)
  val vpn_table         = Module(new VpnTable)
  val train_pipe        = Module(new TrainPipeline)
  val detect_pipe_seq   = Seq.tabulate(DetectPipeNum)(i => Module(new DetectPipeline(s"dp$i")))

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
          b) Hit a SMS/BOP prefetched block

      * Refill Trigger:
          a) Refill a true demanded block
          b) Refill a CDP required block
          c) Refill other prefetcher's block.
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

    detect_trig_queue.io.enq(0).valid := detect_trig.valid && (hit_trigger || refill_trigger) && enable
    detect_trig_queue.io.enq(0).bits.half_cacheblock  := detect_trig.bits.cacheblock(blockBits / 2 - 1, 0)
    detect_trig_queue.io.enq(0).bits.pfDepth  := detect_trig.bits.pfDepth
    detect_trig_queue.io.enq(0).bits.pfSource := detect_trig.bits.pfSource
    detect_trig_queue.io.enq(0).bits.is_hit   := detect_trig.bits.is_hit

    detect_trig_queue.io.enq(1).valid := detect_trig.valid && (hit_trigger || refill_trigger) && enable
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

  // VpnTable & FilterTable Train Trigger
  val vpn_train_reqBuf  = Module(new Queue(new PrefetchTrain, 8))
  vpn_train_reqBuf.io.enq.valid := vpn_train.valid && enable
  vpn_train_reqBuf.io.enq.bits  := vpn_train.bits
  train_pipe.io.vt_train_trigger <> vpn_train_reqBuf.io.deq

  val filter_train_reqBuf = Module(new Queue(new PrefetchTrain, 8))
  filter_train_reqBuf.io.enq.valid := filter_train.valid && enable
  filter_train_reqBuf.io.enq.bits  := filter_train.bits
  train_pipe.io.ft_train_trigger <> filter_train_reqBuf.io.deq

  val vpn_tab_query_req_seq = detect_pipe_seq.map(_.io.vt_query_req) ++ Seq(train_pipe.io.vt_query_req)
  val vpn_tab_query_rsp_seq = detect_pipe_seq.map(_.io.vt_query_rsp) ++ Seq(train_pipe.io.vt_query_rsp)
  require(vpn_tab_query_req_seq.size == vpn_table.io.query_req.size)
  vpn_table.io.query_req.zip(vpn_tab_query_req_seq).foreach{
    case (tab_req, pipe_req) =>
      tab_req.valid := pipe_req.valid
      tab_req.bits := pipe_req.bits
  }
  vpn_table.io.query_rsp.zip(vpn_tab_query_rsp_seq).foreach{
    case (tab_rsp, pipe_rsp) =>
      pipe_rsp.valid := tab_rsp.valid
      pipe_rsp.bits := tab_rsp.bits
  }
  vpn_table.io.train_req  <> train_pipe.io.vt_train_req

  filter_table.io.query_req(0)  <> train_pipe.io.ft_query_req
  filter_table.io.query_rsp(0)  <> train_pipe.io.ft_query_rsp
  filter_table.io.train_req     <> train_pipe.io.ft_train_req

  val cdp_pft_req_buffer = Module(new MIMOQueue(new CDPPrefetchReq, 8, DetectPipeNum, 1))
  cdp_pft_req_buffer.io.flush := reset.asBool
  for (i <- 0 until DetectPipeNum) {
    cdp_pft_req_buffer.io.enq(i).valid      := detect_pipe_seq(i).io.pft_req.valid
    cdp_pft_req_buffer.io.enq(i).bits       := detect_pipe_seq(i).io.pft_req.bits
  }

  // SendUnit
  val send_unit = Module(new SentUnit)
  send_unit.io.tlb_req <> io.tlb_req
  send_unit.io.ft_query_req <> filter_table.io.query_req(1)
  send_unit.io.ft_query_rsp <> filter_table.io.query_rsp(1)
  send_unit.io.in   <> cdp_pft_req_buffer.io.deq(0)
  send_unit.io.out  <> io.pft_req
}
