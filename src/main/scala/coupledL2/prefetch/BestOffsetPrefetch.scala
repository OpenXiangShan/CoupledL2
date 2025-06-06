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
 *
 *
 * Acknowledgement
 *
 * This implementation is inspired by several key papers:
 * [1] Pierre Michaud. "[A Best-Offset Prefetcher.](https://inria.hal.science/hal-01165600/)" 2nd Data Prefetching
 * Championship (DPC). 2015.
 * [2] Pierre Michaud. "[Best-Offset Hardware Prefetching.](https://doi.org/10.1109/HPCA.2016.7446087)" IEEE
 * International Symposium on High Performance Computer Architecture (HPCA). 2016.
 * *************************************************************************************
 */

package coupledL2.prefetch

import utility.{ChiselDB, Constantin, MemReqSource, ParallelPriorityMux, RRArbiterInit, SRAMTemplate, XSPerfAccumulate}
import org.chipsalliance.cde.config.Parameters
import chisel3.DontCare.:=
import chisel3._
import chisel3.util._
import coupledL2.{HasCoupledL2Parameters, L2TlbReq, L2ToL1TlbIO, TlbCmd, Pbmt}
import coupledL2.utils.ReplacementPolicy
import scopt.Read
import freechips.rocketchip.util.SeqToAugmentedSeq

case class BOPParameters(
  virtualTrain: Boolean = true,
  rrTableEntries: Int = 256,
  rrTagBits:      Int = 12,
  scoreBits:      Int = 5,
  roundMax:       Int = 50,
  badScore:       Int = 2,
  tlbReplayCnt:   Int = 10,
  dQEntries: Int = 16,
  dQLatency: Int = 300,
  dQMaxLatency: Int = 512,
  offsetList: Seq[Int] = Seq(
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
  )
    extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val hasPrefetchSrc:  Boolean = true
  override val inflightEntries: Int = 16
}

trait HasBOPParams extends HasPrefetcherHelper {
  def bopParams = prefetchers.find {
      case p: BOPParameters => true
      case _ => false
    }.get.asInstanceOf[BOPParameters]

  // train address space: virtual or physical
  def virtualTrain = bopParams.virtualTrain
  def fullAddrBits = if(virtualTrain) fullVAddrBits else fullAddressBits
  def noOffsetAddrBits = fullAddrBits - offsetBits
  override val REQ_FILTER_SIZE = 16

  // Best offset
  def defaultMinAddrBits = offsetBits + log2Up(bopParams.rrTableEntries) + bopParams.rrTagBits
  def defaultConfig = fullAddrBits >= defaultMinAddrBits

  def rrTableEntries = if (defaultConfig) bopParams.rrTableEntries else 2
  def rrIdxBits = log2Up(rrTableEntries)
  def rrTagBits = if (defaultConfig) bopParams.rrTagBits else (fullAddrBits - offsetBits - rrIdxBits)
  def scoreBits = bopParams.scoreBits
  def roundMax = bopParams.roundMax
  def badScore = bopParams.badScore
  def offsetList = bopParams.offsetList
  def inflightEntries = bopParams.inflightEntries
  def dQEntries = bopParams.dQEntries
  def dQLatency = bopParams.dQLatency
  def dQMaxLatency = bopParams.dQMaxLatency

  def scores = offsetList.length
  def offsetWidth = log2Up(offsetList.max) + 2 // -32 <= offset <= 31
  def roundBits = log2Up(roundMax)
  def scoreMax = (1 << scoreBits) - 1
  def scoreTableIdxBits = log2Up(scores)
  // val prefetchIdWidth = log2Up(inflightEntries)

  def signedExtend(x: UInt, width: Int): UInt = {
    if (x.getWidth >= width) {
      x
    } else {
      Cat(Fill(width - x.getWidth, x.head(1)), x)
    }
  }
}

abstract class BOPBundle(implicit val p: Parameters) extends Bundle with HasBOPParams
abstract class BOPModule(implicit val p: Parameters) extends Module with HasBOPParams

class ScoreTableEntry(implicit p: Parameters) extends BOPBundle {
  // val offset = UInt(offsetWidth.W)
  val score = UInt(scoreBits.W)

  def apply(score: UInt) = {
    val entry = Wire(this)
    // entry.offset := offset
    entry.score := score
    entry
  }
}

class TestOffsetReq(implicit p: Parameters) extends BOPBundle {
  // find whether (X-d) is in recent request table
  val addr = UInt(fullAddrBits.W)
  val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
}

class TestOffsetResp(implicit p: Parameters) extends BOPBundle {
  // val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
  val hit = Bool()
}

class TestOffsetBundle(implicit p: Parameters) extends BOPBundle {
  val req = DecoupledIO(new TestOffsetReq)
  val resp = Flipped(ValidIO(new TestOffsetResp))
}

class RecentRequestTable(name: String)(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle {
    val w = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
    val r = Flipped(new TestOffsetBundle)
  })

  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(addr: UInt) = addr(fullAddrBits - 1, offsetBits)
  def hash1(addr:    UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
  def hash2(addr:    UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
  def idx(addr:      UInt) = hash1(addr) ^ hash2(addr)
  def tag(addr:      UInt) = lineAddr(addr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
  def rrTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(rrTagBits.W)
  }

  val rrTable = Module(
    new SRAMTemplate(
      rrTableEntry(),
      set = rrTableEntries,
      way = 1,
      shouldReset = true,
      singlePort = true,
      hasMbist = cacheParams.hasMbist,
      hasSramCtl = cacheParams.hasSramCtl
    )
  )

  val wAddr = io.w.bits
  rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data(0).valid := true.B
  rrTable.io.w.req.bits.data(0).tag := tag(wAddr)

  val rAddr = io.r.req.bits.addr - signedExtend((io.r.req.bits.testOffset << offsetBits), fullAddrBits)
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.r.req.fire
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  assert(!RegNext(io.w.fire && io.r.req.fire), "single port SRAM should not read and write at the same time")

  /** s0: req handshake */
  val s0_valid = rrTable.io.r.req.fire
  /** s1: rrTable read result */
  val s1_valid = RegNext(s0_valid, false.B)
  val s1_ptr = RegNext(io.r.req.bits.ptr)
  val s1_hit = rData.valid && rData.tag === RegNext(tag(rAddr))
  /** s2: return resp to ScoreTable */
  val s2_valid = RegNext(s1_valid, false.B)

  io.w.ready := rrTable.io.w.req.ready && !io.r.req.valid
  io.r.req.ready := true.B
  io.r.resp.valid := s2_valid
  io.r.resp.bits.ptr := RegEnable(s1_ptr, s1_valid)
  io.r.resp.bits.hit := RegEnable(s1_hit, s1_valid)

  class WRRTEntry extends Bundle{
    val addr = UInt(fullAddrBits.W)
  }
  val wrrt = ChiselDB.createTable(name+"WriteRecentRequestTable", new WRRTEntry, basicDB = false)
  val e = Wire(new WRRTEntry)
  e.addr := wAddr
  wrrt.log(e, io.w.valid && !io.r.req.valid, site = "RecentRequestTable", clock, reset)

}

class OffsetScoreTable(name: String = "")(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
    val prefetchOffset = Output(UInt(offsetWidth.W))
    val prefetchDisable = Output(Bool())
    val test = new TestOffsetBundle
  })

  private val badscoreConstant = Constantin.createRecord(name+"_badScore", bopParams.badScore)
  private val roundMaxConstant = Constantin.createRecord(name+"_roundMax", roundMax)

  val prefetchOffset = RegInit(2.U(offsetWidth.W))
  val prefetchDisable = RegInit(false.B)
  // score table
  // val st = RegInit(VecInit(offsetList.map(off => (new ScoreTableEntry).apply(off.U, 0.U))))
  val st = RegInit(VecInit(Seq.fill(scores)((new ScoreTableEntry).apply(0.U))))
  val offList = WireInit(VecInit(offsetList.map(off => off.S(offsetWidth.W).asUInt)))
  val ptr = RegInit(0.U(scoreTableIdxBits.W))
  val round = RegInit(0.U(roundBits.W))

  val bestOffset = RegInit(2.U(offsetWidth.W)) // the entry with the highest score while traversing
  val bestScore = RegInit(0.U)
  val testOffset = offList(ptr)
  // def winner(e1: ScoreTableEntry, e2: ScoreTableEntry): ScoreTableEntry = {
  //   val w = Wire(new ScoreTableEntry)
  //   w := Mux(e1.score > e2.score, e1, e2)
  //   w
  // }

  val s_idle :: s_learn :: Nil = Enum(2)
  val state = RegInit(s_idle)

  // 1. At the start of a learning phase
  // All the scores are reset to 0.
  // At the end of every learning phase, the prefetch offset is updated as the one with the highest score.
  val isBad = bestScore < badscoreConstant
  when(state === s_idle) {
    st.foreach(_.score := 0.U)
    ptr := 0.U
    round := 0.U
    bestScore := 0.U
    prefetchOffset := bestOffset
    prefetchDisable := isBad
    state := s_learn
  }

  // 2. During a learning phase
  // On every eligible L2 read access (miss or prefetched hit), we test an offset d_i from the list.
  // If X-d_i hits in the RR table, the score of offset d_i is incremented. During a round, each offset
  // in the list is test once. When all the offsets in the list have been tested, the current round is
  // finished, and a new round begins from offset d_1 again.
  // The current learning phase finishes at the end of a round when:
  // (1) one of the score equals SCOREMAX, or
  // (2) the number of rounds equals ROUNDMAX.
  when(state === s_learn) {
    when(io.test.req.fire) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)
    }

    // (2) the number of rounds equals ROUNDMAX.
    when(round >= roundMaxConstant) {
      state := s_idle
    }

    when(io.test.resp.valid && io.test.resp.bits.hit) {
      val oldScore = st(io.test.resp.bits.ptr).score
      val newScore = oldScore + 1.U
      val offset = offList(io.test.resp.bits.ptr)
      st(io.test.resp.bits.ptr).score := newScore
      // bestOffset := winner((new ScoreTableEntry).apply(offset, newScore), bestOffset)
      val renewOffset = newScore > bestScore
      bestOffset := Mux(renewOffset, offset, bestOffset)
      bestScore := Mux(renewOffset, newScore, bestScore)
      // (1) one of the score equals SCOREMAX
      when(newScore >= scoreMax.U) {
        state := s_idle
      }
    }
  }

  io.req.ready := state === s_learn
  io.prefetchOffset := prefetchOffset
  io.prefetchDisable := prefetchDisable
  io.test.req.valid := state === s_learn && io.req.valid
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr

  XSPerfAccumulate("total_learn_phase", state === s_idle)
  XSPerfAccumulate("total_bop_disable", state === s_idle && isBad)
  XSPerfAccumulate("total_bop_high_confidence", state === s_idle && bestScore === scoreMax.U)

  for (off <- offsetList) {
    if (off < 0) {
      XSPerfAccumulate("best_offset_neg_" + (-off).toString + "_learning_phases",
        Mux(state === s_idle, (bestOffset === off.S(offsetWidth.W).asUInt).asUInt, 0.U))
    } else {
      XSPerfAccumulate("best_offset_pos_" + off.toString + "_learning_phases",
        Mux(state === s_idle, (bestOffset === off.U).asUInt, 0.U))
    }
  }

  // FIXME lyq: set basicDB false
  // TODO lyq: How to record the offset to the variable name so that the offset is not recorded separately
  class BopTrainEntry extends Bundle {
    val bestOffset = UInt(offsetWidth.W)
    val bestScore = UInt(scoreBits.W)
    val offsets = Vec(offsetList.length, UInt(offsetWidth.W))
    val scores = Vec(offsetList.length, UInt(scoreBits.W))
  }
  val l2BopTrainTable = ChiselDB.createTable(name+"OffsetScoreTable", new BopTrainEntry, basicDB = false)
  val data = Wire(new BopTrainEntry)
  data.bestOffset := bestOffset
  data.bestScore := bestScore
  for (i <- 0 until offsetList.length){
    data.offsets(i) := offList(i)
    data.scores(i) := st(i).score
  }
  // l2BopTrainTable.log(data = data, en = (state === s_idle) && !isBad, site = name+"OffsetScoreTable", clock, reset)
  l2BopTrainTable.log(data = data, en = (state === s_idle) && !isBad, site = name+"OffsetScoreTable", clock, reset)

}

class BopReqBundle(implicit p: Parameters) extends BOPBundle{
  val full_vaddr = UInt(fullVAddrBits.W)
  val base_vaddr = UInt(vaddrBitsOpt.getOrElse(0).W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val isBOP = Bool()
}

class BopReqBufferEntry(implicit p: Parameters) extends BOPBundle {
  // for tlb req
  val paddrValid = Bool()
  val vaddrNoOffset = UInt((fullVAddrBits-offsetBits).W)
  val baseVaddr = UInt((fullVAddrBits-offsetBits).W)
  val paddrNoOffset = UInt(fullVAddrBits.W)
  val replayEn = Bool()
  val replayCnt = UInt(4.W)
  // for pf req
  val needT = Bool()
  val source = UInt(sourceIdBits.W)

  def fromBopReqBundle(req: BopReqBundle) = {
    paddrValid := false.B
    vaddrNoOffset := get_block_vaddr(req.full_vaddr)
    baseVaddr := req.base_vaddr
    replayEn := false.B
    replayCnt := 0.U
    paddrNoOffset := 0.U
    needT := req.needT
    source := req.source
  }

  def toPrefetchReq(): PrefetchReq = {
    val req = Wire(new PrefetchReq)
    req.tag := parseFullAddress(get_pf_paddr())._1
    req.set := parseFullAddress(get_pf_paddr())._2
    req.vaddr.foreach(_ := baseVaddr)
    req.needT := needT
    req.source := source
    req.pfSource := MemReqSource.Prefetch2L2BOP.id.U
    req
  }

  def get_pf_paddr(): UInt = {
    Cat(paddrNoOffset, 0.U(offsetBits.W))
  }

  def get_tlb_vaddr(): UInt = {
    Cat(vaddrNoOffset, 0.U(offsetBits.W))
  }

  def update_paddr(paddr: UInt) = {
    paddrValid := true.B
    paddrNoOffset := paddr(paddr.getWidth-1, offsetBits)
    replayEn := false.B
    replayCnt := 0.U
  }

}

class PrefetchReqBuffer(name: String = "vbop")(implicit p: Parameters) extends BOPModule{
  val io = IO(new Bundle() {
    val in_req = Flipped(ValidIO(new BopReqBundle))
    val tlb_req = new L2ToL1TlbIO(nRespDups = 1)
    val out_req = DecoupledIO(new PrefetchReq)
  })

  val firstTlbReplayCnt = Constantin.createRecord(name+"_firstTlbReplayCnt", bopParams.tlbReplayCnt)
  // if full then drop new req, so there is no need to use s1_evicted_oh & replacement
  val valids = Seq.fill(REQ_FILTER_SIZE)(RegInit(false.B))
  val entries = Seq.fill(REQ_FILTER_SIZE)(Reg(new BopReqBufferEntry))
  //val replacement = ReplacementPolicy.fromString("plru", REQ_FILTER_SIZE)
  val tlb_req_arb = Module(new RRArbiterInit(new L2TlbReq, REQ_FILTER_SIZE))
  val pf_req_arb = Module(new RRArbiterInit(new PrefetchReq, REQ_FILTER_SIZE))

  def wayMap[T <: Data](f: Int => T) = VecInit((0 until REQ_FILTER_SIZE).map(f))

  def get_flag(vaddr: UInt) = get_block_vaddr(vaddr)

  def alloc_entry(i: Int, e: BopReqBufferEntry): Unit = {
    valids(i) := true.B
    entries(i) := e
  }

  def invalid_entry(i: Int): Unit = {
    valids(i) := false.B
  }

  def can_send_pf(i: Int): Bool = {
    valids(i) && entries(i).paddrValid
  }

  def isEqualBopReq(i: Int, req: BopReqBundle): Bool = {
    // FIXME lyq: the comparision logic is very complicated, is there a way to simplify
    val v = valids(i)
    val e = entries(i)
    v &&
      e.vaddrNoOffset === get_block_vaddr(req.full_vaddr) &&
      e.baseVaddr === req.base_vaddr &&
      e.needT === req.needT &&
      e.source === req.source
  }

  /* timing description:
   * entry    : s0 look up, s1 update & replace
   * tlb      : s0 arbiter, s1 send tlb req, s2 receive tlb resp, s3 receive pmp resp
   * prefetch : s0 arbiter & send pf req
   */

  // add a cycle for timing
  tlb_req_arb.io.out.ready := true.B
  io.tlb_req.req.valid := RegNext(tlb_req_arb.io.out.valid)
  io.tlb_req.req.bits := RegEnable(tlb_req_arb.io.out.bits, tlb_req_arb.io.out.valid)
  io.tlb_req.req_kill := false.B
  io.tlb_req.resp.ready := true.B
  io.out_req <> pf_req_arb.io.out

  /* s0: entries look up */
  val prev_in_valid = RegNext(io.in_req.valid, false.B)
  val prev_in_req = RegEnable(io.in_req.bits, io.in_req.valid)
  val prev_in_flag = get_flag(prev_in_req.full_vaddr)
  // s1 entry update
  val alloc = Wire(Vec(REQ_FILTER_SIZE, Bool()))

  val s0_in_req = io.in_req.bits
  val s0_in_flag = get_flag(s0_in_req.full_vaddr)
  val s0_conflict_prev = prev_in_valid && s0_in_flag === prev_in_flag
  // FIXME lyq: the comparision logic is very complicated, is there a way to simplify
  val s0_match_oh = VecInit(entries.indices.map(i =>
    valids(i) && entries(i).vaddrNoOffset === s0_in_flag &&
    entries(i).needT === s0_in_req.needT && entries(i).source === s0_in_req.source &&
    entries(i).baseVaddr === s0_in_req.base_vaddr
  )).asUInt
  val s0_match = Cat(s0_match_oh).orR

  val s0_invalid_vec = wayMap(w => !valids(w) && !alloc(w))
  val s0_has_invalid_way = s0_invalid_vec.asUInt.orR
  val s0_invalid_oh = PriorityEncoderOH(s0_invalid_vec).asUInt

  val s0_req_valid = io.in_req.valid && !s0_conflict_prev && !s0_match && s0_has_invalid_way
  val s0_tlb_fire_oh = VecInit(tlb_req_arb.io.in.map(_.fire)).asUInt
  val s0_pf_fire_oh = VecInit(pf_req_arb.io.in.map(_.fire)).asUInt
  //val s0_access_way = Mux(s0_match, OHToUInt(s0_match_oh), OHToUInt(s0_replace_oh))
  //when(s0_req_valid){
  //  replacement.access(s0_access_way)
  //}
  XSPerfAccumulate("recv_req", io.in_req.valid)
  XSPerfAccumulate("recv_req_drop_conflict", io.in_req.valid && s0_conflict_prev)
  XSPerfAccumulate("recv_req_drop_match", io.in_req.valid && !s0_conflict_prev && s0_match)
  XSPerfAccumulate("recv_req_drop_full", io.in_req.valid && !s0_conflict_prev && !s0_match && !s0_has_invalid_way)


  /* s1 update and replace */
  val s1_valid = RegNext(s0_req_valid, false.B)
  val s1_in_req = RegEnable(s0_in_req, s0_req_valid)
  val s1_invalid_oh = RegEnable(s0_invalid_oh, 0.U, s0_req_valid)
  val s1_pf_fire_oh = RegNext(s0_pf_fire_oh, 0.U)
  val s1_tlb_fire_oh = RegNext(s0_tlb_fire_oh, 0.U)
  val s1_alloc_entry = Wire(new BopReqBufferEntry)
  s1_alloc_entry.fromBopReqBundle(s1_in_req)

  /* s2 tlb resp */
  val s2_tlb_fire_oh = RegNext(s1_tlb_fire_oh, 0.U)

  /* s3 pmp resp */
  val s3_tlb_fire_oh = RegNext(s2_tlb_fire_oh, 0.U)
  val s3_tlb_resp_valid = RegNext(io.tlb_req.resp.valid)
  val s3_tlb_resp = RegEnable(io.tlb_req.resp.bits, io.tlb_req.resp.valid)

  /* entry update */
  val exp_drop = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val miss_drop = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val miss_first_replay = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val pf_fired = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  val tlb_fired = Wire(Vec(REQ_FILTER_SIZE, Bool()))
  for ((e, i) <- entries.zipWithIndex){
    alloc(i) := s1_valid && s1_invalid_oh(i)
    pf_fired(i) := s0_pf_fire_oh(i)
    exp_drop(i) := s3_tlb_fire_oh(i) && s3_tlb_resp_valid && !s3_tlb_resp.miss && (
      s3_tlb_resp.excp.head.pf.ld || s3_tlb_resp.excp.head.gpf.ld || s3_tlb_resp.excp.head.af.ld ||
      io.tlb_req.pmp_resp.ld || io.tlb_req.pmp_resp.mmio || Pbmt.isUncache(s3_tlb_resp.pbmt)
    )
    val miss = s3_tlb_fire_oh(i) && s3_tlb_resp_valid && s3_tlb_resp.miss
    tlb_fired(i) := s3_tlb_fire_oh(i) && s3_tlb_resp_valid && !s3_tlb_resp.miss && !exp_drop(i)
    miss_drop(i) := miss && e.replayEn
    miss_first_replay(i) := miss && !e.replayEn

    // old data: update replayCnt
    when(valids(i) && e.replayCnt.orR) {
      e.replayCnt := e.replayCnt - 1.U
    }
    // recent data: update tlb resp
    when(tlb_fired(i)){
      e.update_paddr(s3_tlb_resp.paddr.head)
    }.elsewhen(miss_drop(i)) { // miss
      invalid_entry(i)
    }.elsewhen(miss_first_replay(i)){
      e.replayCnt := firstTlbReplayCnt
      e.replayEn := 1.U
    }.elsewhen(exp_drop(i)){
      invalid_entry(i)
    }
    // issue data: update pf
    when(pf_fired(i)){
      invalid_entry(i)
    }
    // new data: update data
    when(alloc(i)){
      alloc_entry(i, s1_alloc_entry)
    }
  }

  /* tlb & pf */
  for((e, i) <- entries.zipWithIndex){
    tlb_req_arb.io.in(i).valid := valids(i) && !e.paddrValid && !s1_tlb_fire_oh(i) && !s2_tlb_fire_oh(i) && !s3_tlb_fire_oh(i) && !e.replayCnt.orR
    tlb_req_arb.io.in(i).bits.vaddr := e.get_tlb_vaddr()
    tlb_req_arb.io.in(i).bits.cmd := TlbCmd.read
    tlb_req_arb.io.in(i).bits.size := 3.U
    tlb_req_arb.io.in(i).bits.kill := false.B
    tlb_req_arb.io.in(i).bits.no_translate := false.B
    tlb_req_arb.io.in(i).bits.isPrefetch := true.B

    pf_req_arb.io.in(i).valid := can_send_pf(i)
    pf_req_arb.io.in(i).bits := e.toPrefetchReq()
  }

  XSPerfAccumulate("tlb_req", io.tlb_req.req.valid)
  XSPerfAccumulate("tlb_miss", io.tlb_req.resp.valid && io.tlb_req.resp.bits.miss)
  XSPerfAccumulate("tlb_excp", s3_tlb_resp_valid && !s3_tlb_resp.miss && (
    s3_tlb_resp.excp.head.pf.ld || s3_tlb_resp.excp.head.gpf.ld || s3_tlb_resp.excp.head.af.ld ||
    io.tlb_req.pmp_resp.ld || io.tlb_req.pmp_resp.mmio || Pbmt.isUncache(s3_tlb_resp.pbmt)
  ))
  XSPerfAccumulate("tlb_excp_pmp_af", s3_tlb_resp_valid && !s3_tlb_resp.miss && io.tlb_req.pmp_resp.ld)
  XSPerfAccumulate("tlb_excp_uncache", s3_tlb_resp_valid && !s3_tlb_resp.miss && (io.tlb_req.pmp_resp.mmio || Pbmt.isUncache(s3_tlb_resp.pbmt)))
  XSPerfAccumulate("entry_alloc", PopCount(alloc))
  XSPerfAccumulate("entry_miss_first_replay", PopCount(miss_first_replay))
  XSPerfAccumulate("entry_miss_drop", PopCount(miss_drop))
  XSPerfAccumulate("entry_excp", PopCount(exp_drop))
  XSPerfAccumulate("entry_merge", io.in_req.valid && s0_match)
  XSPerfAccumulate("entry_pf_fire", PopCount(pf_fired))

  /*
  val enTalbe = WireInit(Constantin.createRecord(name+"_isWriteL2BopTable", 1.U))
  val l2BOPTable = ChiselDB. createTable("L2BOPTable", new BopReqBufferEntry, basicDB = false)
  for (i <- 0 until REQ_FILTER_SIZE){
    when(alloc(i)){
      l2BOPTable.log(
        data = entries(i),
        en = enTalbe.orR && pf_fired(i),
        site = "L2BOPTable",
        clock = clock,
        reset = reset
      )
    }
  }
  */
}

class DelayQueue(name: String = "")(implicit p: Parameters) extends  BOPModule{
  val io = IO(new Bundle(){
    val in = Flipped(DecoupledIO(UInt(noOffsetAddrBits.W)))
    val out = DecoupledIO(UInt(fullAddrBits.W))
    // only record `fullAddrBits - offsetBits` bits
    // out.bits = Cat(record, 0.U(offsetBits))
  })

  /* Setting */
  val IdxWidth = log2Up(dQEntries)
  val LatencyWidth = log2Up(dQMaxLatency)
  class Entry extends Bundle{
    val addrNoOffset = UInt(noOffsetAddrBits.W)
    val cnt = UInt(LatencyWidth.W)
  }
  val queue = RegInit(VecInit(Seq.fill(dQEntries)(0.U.asTypeOf(new Entry))))
  val valids = RegInit(VecInit(Seq.fill(dQEntries)(false.B)))
  val head = RegInit(0.U(IdxWidth.W))
  val tail = RegInit(0.U(IdxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last
  val outValid = !empty && !queue(head).cnt.orR && valids(head)

  /* In & Out */
  var setDqLatency = Constantin.createRecord(name+"_delayQueueLatency", dQLatency)
  when(io.in.valid && !full) {
    // if queue is full, we drop the new request
    queue(tail).addrNoOffset := io.in.bits
    queue(tail).cnt := setDqLatency // dQLatency.U
    valids(tail) := true.B
    tail := tail + 1.U

    /*
    // if full, drop the old request
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
    */
  }
  when(outValid && io.out.ready) {
    valids(head) := false.B
    head := head + 1.U
  }
  io.in.ready := true.B
  io.out.valid := outValid
  io.out.bits := Cat(queue(head).addrNoOffset, 0.U(offsetBits.W))

  /* Update */
  for(i <- 0 until dQEntries){
    when(queue(i).cnt.orR){
      queue(i).cnt := queue(i).cnt - 1.U
    }
  }

  /* Perf */
  XSPerfAccumulate("full", full)
  XSPerfAccumulate("empty", empty)
  XSPerfAccumulate("entryNumber", PopCount(valids.asUInt))
  XSPerfAccumulate("inNumber", io.in.valid)
  XSPerfAccumulate("outNumber", io.out.valid)

}

class VBestOffsetPrefetch(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle() {
    val enable = Input(Bool())
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val pbopCrossPage = Input(Bool())
    val tlb_req = new L2ToL1TlbIO(nRespDups= 1)
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
  })
  // 0 / 1: whether to enable
  private val cstEnable = Constantin.createRecord("vbop_enable"+cacheParams.hartId.toString, initValue = 1)
  val enable = io.enable && cstEnable.orR

  val delayQueue = Module(new DelayQueue("vbop"))
  val rrTable = Module(new RecentRequestTable("vbop"))
  val scoreTable = Module(new OffsetScoreTable("vbop"))

  val s0_fire = scoreTable.io.req.fire && io.pbopCrossPage
  val s1_fire = WireInit(false.B)
  val s0_ready, s1_ready = WireInit(false.B)

  /* s0 train */
  val prefetchOffset = scoreTable.io.prefetchOffset
  val prefetchDisable = scoreTable.io.prefetchDisable
  // NOTE: vaddr from l1 to l2 has no offset bits
  val s0_reqVaddr = io.train.bits.vaddr.getOrElse(0.U)
  val s0_oldFullAddr = if(virtualTrain) Cat(io.train.bits.vaddr.getOrElse(0.U), 0.U(offsetBits.W)) else io.train.bits.addr
  val s0_oldFullAddrNoOff = s0_oldFullAddr(s0_oldFullAddr.getWidth-1, offsetBits)
  val s0_newFullAddr = s0_oldFullAddr + signedExtend((prefetchOffset << offsetBits), fullAddrBits)
  val s0_crossPage = getPPN(s0_newFullAddr) =/= getPPN(s0_oldFullAddr) // unequal tags
  val respFullAddr = if(virtualTrain) Cat(io.resp.bits.vaddr.getOrElse(0.U), 0.U(offsetBits.W))
                 else io.resp.bits.addr - signedExtend((prefetchOffset << offsetBits), fullAddrBits)

  rrTable.io.r <> scoreTable.io.test
  rrTable.io.w <> delayQueue.io.out
  delayQueue.io.in.valid := io.train.valid
  delayQueue.io.in.bits := s0_oldFullAddrNoOff
  scoreTable.io.req.valid := io.train.valid
  scoreTable.io.req.bits := s0_oldFullAddr

  /* s1 get or send req */
  val s1_req_valid = RegInit(false.B)
  val s1_needT = RegEnable(io.train.bits.needT, s0_fire)
  val s1_source = RegEnable(io.train.bits.source, s0_fire)
  val s1_newFullAddr = RegEnable(s0_newFullAddr, s0_fire)
  val s1_reqVaddr = RegEnable(s0_reqVaddr, s0_fire)
  // val out_req = Wire(new PrefetchReq)
  // val out_req_valid = Wire(Bool())
  // val out_drop_req = WireInit(false.B)

  // pipeline control signal
  when(s0_fire) {
    if(virtualTrain) s1_req_valid := true.B
    else s1_req_valid := !s0_crossPage // stop prefetch when prefetch req crosses pages
  }.elsewhen(s1_fire){
    s1_req_valid := false.B
  }

  if (virtualTrain) {
    // FIXME lyq: it it not correct
    s0_ready := io.tlb_req.req.ready && s1_ready || !s1_req_valid
    s1_ready := io.req.ready || !io.req.valid
    s1_fire := s1_ready && s1_req_valid
  } else {
    s0_ready := io.req.ready || !io.req.valid
    s1_ready := io.req.ready
    s1_fire := io.req.fire
  }

  // out value
  io.train.ready := delayQueue.io.in.ready && scoreTable.io.req.ready && s0_ready
  io.resp.ready := rrTable.io.w.ready
  io.tlb_req.resp.ready := true.B

  // different situation
  val reqFilter = Module(new PrefetchReqBuffer)
  when(prefetchDisable || !virtualTrain.B){
    reqFilter.io.in_req.valid := false.B
    reqFilter.io.in_req.bits := DontCare
  }.otherwise{
    reqFilter.io.in_req.valid := s1_req_valid
    reqFilter.io.in_req.bits.full_vaddr := s1_newFullAddr
    reqFilter.io.in_req.bits.base_vaddr := s1_reqVaddr
    reqFilter.io.in_req.bits.needT := s1_needT
    reqFilter.io.in_req.bits.source := s1_source
    reqFilter.io.in_req.bits.isBOP := true.B
  }

  if(virtualTrain){
    io.tlb_req <> reqFilter.io.tlb_req
    io.req <> reqFilter.io.out_req
    io.req.valid := enable && reqFilter.io.out_req.valid
  } else {
    io.tlb_req.req.valid := false.B
    io.tlb_req.req.bits := DontCare
    io.tlb_req.req_kill := false.B

    /* s1 send prefetch req */
    io.req.valid := enable && s1_req_valid
    io.req.bits.tag := parseFullAddress(s1_newFullAddr)._1
    io.req.bits.set := parseFullAddress(s1_newFullAddr)._2
    io.req.bits.vaddr.foreach(_ := 0.U)
    io.req.bits.needT := s1_needT
    io.req.bits.source := s1_source
    io.req.bits.pfSource := MemReqSource.Prefetch2L2BOP.id.U
    io.req.bits.isBOP := true.B
  }

  for (off <- offsetList) {
    if (off < 0) {
      XSPerfAccumulate("best_offset_neg_" + (-off).toString, prefetchOffset === off.S(offsetWidth.W).asUInt)
    } else {
      XSPerfAccumulate("best_offset_pos_" + off.toString, prefetchOffset === off.U)
    }
  }
  XSPerfAccumulate("bop_req", io.req.fire)
  XSPerfAccumulate("bop_train", io.train.fire)
  XSPerfAccumulate("bop_resp", io.resp.fire)
  XSPerfAccumulate("bop_train_stall_for_st_not_ready", io.train.valid && !scoreTable.io.req.ready)
  if(virtualTrain){
    XSPerfAccumulate("bop_train_stall_for_tlb_not_ready", io.train.valid && !io.tlb_req.req.ready)
    // XSPerfAccumulate("bop_req_drop", out_drop_req)
  }else{
    XSPerfAccumulate("bop_cross_page", scoreTable.io.req.fire && s0_crossPage)
  }
  XSPerfAccumulate("bop_drop_for_disable", scoreTable.io.req.fire && prefetchDisable)
}

class PBestOffsetPrefetch(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle() {
    val enable = Input(Bool())
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val pbopCrossPage = Output(Bool())
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
  })

  // 0 / 1: whether to enable
  private val cstEnable = Constantin.createRecord("pbop_enable"+cacheParams.hartId.toString, initValue = 1)
  val enable = io.enable && cstEnable.orR

  val delayQueue = Module(new DelayQueue("pbop"))
  val rrTable = Module(new RecentRequestTable("pbop"))
  val scoreTable = Module(new OffsetScoreTable("pbop"))

  val prefetchOffset = scoreTable.io.prefetchOffset
  val prefetchDisable = scoreTable.io.prefetchDisable
  val oldAddr = io.train.bits.addr
  val oldAddrNoOff = oldAddr(oldAddr.getWidth-1, offsetBits)
  val newAddr = oldAddr + signedExtend((prefetchOffset << offsetBits), fullAddressBits)

  rrTable.io.r <> scoreTable.io.test
  rrTable.io.w <> delayQueue.io.out
  delayQueue.io.in.valid := io.train.valid
  delayQueue.io.in.bits := oldAddrNoOff
  scoreTable.io.req.valid := io.train.valid
  scoreTable.io.req.bits := oldAddr

  val req = Reg(new PrefetchReq)
  val req_valid = RegInit(false.B)
  val crossPage = getPPN(newAddr) =/= getPPN(oldAddr) // unequal tags
  when(io.req.fire) {
    req_valid := false.B
  }
  when(scoreTable.io.req.fire) {
    req.tag := parseFullAddress(newAddr)._1
    req.set := parseFullAddress(newAddr)._2
    req.needT := io.train.bits.needT
    req.source := io.train.bits.source
    req_valid := !crossPage && !prefetchDisable // stop prefetch when prefetch req crosses pages
  }

  io.pbopCrossPage := crossPage
  io.req.valid := enable && req_valid
  io.req.bits := req
  io.req.bits.pfSource := MemReqSource.Prefetch2L2PBOP.id.U
  io.train.ready := delayQueue.io.in.ready && scoreTable.io.req.ready && (!req_valid || io.req.ready)
  io.resp.ready := rrTable.io.w.ready

  for (off <- offsetList) {
    if (off < 0) {
      XSPerfAccumulate("best_offset_neg_" + (-off).toString, prefetchOffset === off.S(offsetWidth.W).asUInt)
    } else {
      XSPerfAccumulate("best_offset_pos_" + off.toString, prefetchOffset === off.U)
    }
  }
  XSPerfAccumulate("bop_req", io.req.fire)
  XSPerfAccumulate("bop_train", io.train.fire)
  XSPerfAccumulate("bop_resp", io.resp.fire)
  XSPerfAccumulate("bop_train_stall_for_st_not_ready", io.train.valid && !scoreTable.io.req.ready)
  XSPerfAccumulate("bop_drop_for_cross_page", scoreTable.io.req.fire && crossPage)
  XSPerfAccumulate("bop_drop_for_disable", scoreTable.io.req.fire && prefetchDisable)
}
