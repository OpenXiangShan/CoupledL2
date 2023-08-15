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

import utility.SRAMTemplate
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.{HasCoupledL2Parameters, L2TlbReq, L2ToL1TlbIO, TlbCmd}
import coupledL2.utils.XSPerfAccumulate

case class BOPParameters(
  rrTableEntries: Int = 256,
  rrTagBits:      Int = 12,
  scoreBits:      Int = 5,
  roundMax:       Int = 50,
  badScore:       Int = 1,
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
    225, 240, 243, 250, 256
  ))
    extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 16
}

trait HasBOPParams extends HasCoupledL2Parameters {
  val bopParams = prefetchOpt.get.asInstanceOf[BOPParameters]

  // train address space: virtual or physical
  val virtualTrain = true
  val addrBits = if(virtualTrain) vaddrBits else fullAddressBits

  // Best offset
  val defaultMinAddrBits = offsetBits + log2Up(bopParams.rrTableEntries) + bopParams.rrTagBits
  val defaultConfig = addrBits >= defaultMinAddrBits

  val rrTableEntries = if (defaultConfig) bopParams.rrTableEntries else 2
  val rrIdxBits = log2Up(rrTableEntries)
  val rrTagBits = if (defaultConfig) bopParams.rrTagBits else (addrBits - offsetBits - rrIdxBits)
  val scoreBits = bopParams.scoreBits
  val roundMax = bopParams.roundMax
  val badScore = bopParams.badScore
  val offsetList = bopParams.offsetList
  val inflightEntries = bopParams.inflightEntries

  val scores = offsetList.length
  val offsetWidth = log2Up(offsetList.max) + 2 // -32 <= offset <= 31
  val roundBits = log2Up(roundMax)
  val scoreMax = (1 << scoreBits) - 1
  val scoreTableIdxBits = log2Up(scores)
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
  val addr = UInt(addrBits.W)
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
  val resp = Flipped(DecoupledIO(new TestOffsetResp))
}

class RecentRequestTable(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle {
    val w = Flipped(DecoupledIO(UInt(addrBits.W)))
    val r = Flipped(new TestOffsetBundle)
  })

  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(addr: UInt) = addr(addrBits - 1, offsetBits)
  def hash1(addr:    UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
  def hash2(addr:    UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
  def idx(addr:      UInt) = hash1(addr) ^ hash2(addr)
  def tag(addr:      UInt) = lineAddr(addr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
  def rrTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(rrTagBits.W)
  }

  val rrTable = Module(
    new SRAMTemplate(rrTableEntry(), set = rrTableEntries, way = 1, shouldReset = true, singlePort = true)
  )

  val wAddr = io.w.bits
  rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data(0).valid := true.B
  rrTable.io.w.req.bits.data(0).tag := tag(wAddr)

  val rAddr = io.r.req.bits.addr - signedExtend((io.r.req.bits.testOffset << offsetBits), addrBits)
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.r.req.fire()
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  assert(!RegNext(io.w.fire() && io.r.req.fire()), "single port SRAM should not read and write at the same time")

  io.w.ready := rrTable.io.w.req.ready && !io.r.req.valid
  io.r.req.ready := true.B
  io.r.resp.valid := RegNext(rrTable.io.r.req.fire(), false.B)
  io.r.resp.bits.ptr := RegNext(io.r.req.bits.ptr)
  io.r.resp.bits.hit := rData.valid && rData.tag === RegNext(tag(rAddr))

}

class OffsetScoreTable(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(UInt(addrBits.W)))
    val prefetchOffset = Output(UInt(offsetWidth.W))
    val test = new TestOffsetBundle
  })

  val prefetchOffset = RegInit(2.U(offsetWidth.W))
  // score table
  // val st = RegInit(VecInit(offsetList.map(off => (new ScoreTableEntry).apply(off.U, 0.U))))
  val st = RegInit(VecInit(Seq.fill(scores)((new ScoreTableEntry).apply(0.U))))
  val offList = WireInit(VecInit(offsetList.map(off => off.S(offsetWidth.W).asUInt())))
  val ptr = RegInit(0.U(scoreTableIdxBits.W))
  val round = RegInit(0.U(roundBits.W))

  val bestOffset = RegInit(2.U(offsetWidth.W)) // the entry with the highest score while traversing
  val bestScore = RegInit(badScore.U(scoreBits.W))
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
  when(state === s_idle) {
    st.foreach(_.score := 0.U)
    ptr := 0.U
    round := 0.U
    bestScore := badScore.U
    prefetchOffset := bestOffset
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
    when(io.test.req.fire()) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)
    }

    // (2) the number of rounds equals ROUNDMAX.
    when(round >= roundMax.U) {
      state := s_idle
    }

    when(io.test.resp.fire() && io.test.resp.bits.hit) {
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
  io.test.req.valid := state === s_learn && io.req.valid
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr
  io.test.resp.ready := true.B

  for (off <- offsetList) {
    if (off < 0) {
      XSPerfAccumulate(cacheParams, "best_offset_neg_" + (-off).toString + "_learning_phases",
        Mux(state === s_idle, (bestOffset === off.S(offsetWidth.W).asUInt).asUInt, 0.U))
    } else {
      XSPerfAccumulate(cacheParams, "best_offset_pos_" + off.toString + "_learning_phases",
        Mux(state === s_idle, (bestOffset === off.U).asUInt, 0.U))
    }
  }

}

class BestOffsetPrefetch(implicit p: Parameters) extends BOPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val tlb_req = new L2ToL1TlbIO(nRespDups= 1)
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
  })

  val rrTable = Module(new RecentRequestTable)
  val scoreTable = Module(new OffsetScoreTable)

  val s0_fire = scoreTable.io.req.fire
  val s1_fire = WireInit(false.B)
  val s0_ready, s1_ready = WireInit(false.B)

  /* s0 train */
  val prefetchOffset = scoreTable.io.prefetchOffset
  // NOTE: vaddr from l1 to l2 has no offset bits
  val s0_oldAddr = if(virtualTrain) Cat(io.train.bits.vaddr.getOrElse(0.U), 0.U(offsetBits.W)) else io.train.bits.addr
  val s0_newAddr = s0_oldAddr + signedExtend((prefetchOffset << offsetBits), addrBits)
  val s0_crossPage = getPPN(s0_newAddr) =/= getPPN(s0_oldAddr) // unequal tags
  val respAddr = if(virtualTrain) Cat(io.resp.bits.vaddr.getOrElse(0.U), 0.U(offsetBits.W)) else io.resp.bits.addr

  rrTable.io.r <> scoreTable.io.test
  rrTable.io.w.valid := io.resp.valid
  rrTable.io.w.bits := respAddr + signedExtend((prefetchOffset << offsetBits), addrBits)
  scoreTable.io.req.valid := io.train.valid
  scoreTable.io.req.bits := s0_oldAddr

  /* s1 get or send req */
  val s1_req_valid = RegInit(false.B)
  val s1_needT = RegEnable(io.train.bits.needT, s0_fire)
  val s1_source = RegEnable(io.train.bits.source, s0_fire)
  val s1_newAddr = RegEnable(s0_newAddr, s0_fire)
  val out_drop_req = io.tlb_req.resp.valid && (io.tlb_req.resp.bits.miss || io.tlb_req.resp.bits.excp.head.pf.ld || io.tlb_req.resp.bits.excp.head.af.ld)
  val out_req = Wire(new PrefetchReq)
  val out_req_valid = Wire(Bool())

  // pipeline control signal
  when(s0_fire) {
    if(virtualTrain) s1_req_valid := true.B
    else s1_req_valid := !s0_crossPage // stop prefetch when prefetch req crosses pages
  }.elsewhen(s1_fire){
    s1_req_valid := false.B
  }

  if (virtualTrain) {
    s0_ready := io.tlb_req.req.ready && s1_ready || !s1_req_valid
    s1_ready := io.req.ready || !io.req.valid
    s1_fire := s1_ready && s1_req_valid
  } else {
    s0_ready := io.req.ready || !io.req.valid
    s1_ready := io.req.ready
    s1_fire := io.req.fire
  }

  // out value
  io.req.valid := out_req_valid
  io.req.bits := out_req
  io.train.ready := scoreTable.io.req.ready && s0_ready
  io.resp.ready := rrTable.io.w.ready
  io.tlb_req.resp.ready := true.B

  // different situation
  if(virtualTrain){
    /* s1 send tlb req */
    io.tlb_req.req.valid := s1_req_valid
    io.tlb_req.req.bits.vaddr := s1_newAddr
    when(s1_needT){
      io.tlb_req.req.bits.cmd := TlbCmd.exec
    }.otherwise{
      io.tlb_req.req.bits.cmd := TlbCmd.read
    }
    io.tlb_req.req.bits.size := 3.U
    io.tlb_req.req.bits.kill := false.B
    io.tlb_req.req.bits.no_translate := false.B
    io.tlb_req.req_kill := false.B

    /* s2 get tlb resp and send prefetch req */
    out_req_valid := RegNext(s1_req_valid) && io.tlb_req.resp.valid && !out_drop_req
    out_req.tag := parseFullAddress(io.tlb_req.resp.bits.paddr.head)._1
    out_req.set := parseFullAddress(io.tlb_req.resp.bits.paddr.head)._2
    out_req.needT := RegNext(s1_needT)
    out_req.source := RegNext(s1_source)
    out_req.isBOP := true.B

  } else {
    io.tlb_req.req.valid := false.B
    io.tlb_req.req.bits := DontCare
    io.tlb_req.req_kill := false.B

    /* s1 send prefetch req */
    out_req_valid := s1_req_valid
    out_req.tag := parseFullAddress(s1_newAddr)._1
    out_req.set := parseFullAddress(s1_newAddr)._2
    out_req.needT := s1_needT
    out_req.source := s1_source
    out_req.isBOP := true.B
  }

  for (off <- offsetList) {
    if (off < 0) {
      XSPerfAccumulate(cacheParams, "best_offset_neg_" + (-off).toString, prefetchOffset === off.S(offsetWidth.W).asUInt)
    } else {
      XSPerfAccumulate(cacheParams, "best_offset_pos_" + off.toString, prefetchOffset === off.U)
    }
  }
  XSPerfAccumulate(cacheParams, "bop_req", io.req.fire())
  XSPerfAccumulate(cacheParams, "bop_req_drop", out_drop_req)
  XSPerfAccumulate(cacheParams, "bop_train", io.train.fire())
  XSPerfAccumulate(cacheParams, "bop_train_stall_for_st_not_ready", io.train.valid && !scoreTable.io.req.ready)
  if(!virtualTrain){
    XSPerfAccumulate(cacheParams, "bop_cross_page", scoreTable.io.req.fire() && s0_crossPage)
  }
}
