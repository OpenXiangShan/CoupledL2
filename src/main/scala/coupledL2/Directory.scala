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

package coupledL2

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SetAssocLRU
import coupledL2.utils._
import utility.{ParallelPriorityMux, RegNextN}
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._

class MetaEntry(implicit p: Parameters) extends L2Bundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clients = UInt(clientBits.W)  // valid-bit of clients
  // TODO: record specific state of clients instead of just 1-bit
  val alias = aliasBitsOpt.map(width => UInt(width.W)) // alias bits of client
  val prefetch = if (hasPrefetchBit) Some(Bool()) else None // whether block is prefetched
  val accessed = Bool()

  def =/=(entry: MetaEntry): Bool = {
    this.asUInt =/= entry.asUInt
  }
}

object MetaEntry {
  def apply()(implicit p: Parameters) = {
    val init = WireInit(0.U.asTypeOf(new MetaEntry))
    init
  }
  def apply(dirty: Bool, state: UInt, clients: UInt, alias: Option[UInt],
            prefetch: Bool = false.B, accessed: Bool = false.B)(implicit p: Parameters) = {
    val entry = Wire(new MetaEntry)
    entry.dirty := dirty
    entry.state := state
    entry.clients := clients
    entry.alias.foreach(_ := alias.getOrElse(0.U))
    entry.prefetch.foreach(_ := prefetch)
    entry.accessed := accessed
    entry
  }
}

class DirRead(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val wayMask = UInt(cacheParams.ways.W)
  val replacerInfo = new ReplacerInfo()
  // only need to choose a replaced way, for MSHR refill
  val replMode = Bool()
  val mshrId = UInt(mshrBits.W)
}

class DirResult(implicit p: Parameters) extends L2Bundle {
  val hit = Bool()
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)  // hit way or victim way
  val meta = new MetaEntry()
  val error = Bool()
  val replacerInfo = new ReplacerInfo() // for TopDown usage
}

class ReplacerResult(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val meta = new MetaEntry()
  val mshrId = UInt(mshrBits.W)
}

class MetaWrite(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val wayOH = UInt(cacheParams.ways.W)
  val wmeta = new MetaEntry
}

class TagWrite(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val wtag = UInt(tagBits.W)
}

class Directory(implicit p: Parameters) extends L2Module with DontCareInnerLogic {

  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new DirRead))
    val resp = Output(new DirResult)
    val metaWReq = Flipped(ValidIO(new MetaWrite))
    val tagWReq = Flipped(ValidIO(new TagWrite))
    val replResp = ValidIO(new ReplacerResult)
  })

  def invalid_way_sel(metaVec: Seq[MetaEntry], repl: UInt) = {
    val invalid_vec = metaVec.map(_.state === MetaData.INVALID)
    val has_invalid_way = Cat(invalid_vec).orR
    val way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(wayBits.W)))
    (has_invalid_way, way)
  }

  val sets = cacheParams.sets
  val ways = cacheParams.ways
  val banks = cacheParams.dirNBanks

  val tagWen  = io.tagWReq.valid
  val metaWen = io.metaWReq.valid
  val replacerWen = WireInit(false.B)

  val tagArray  = Module(new BankedSRAM(UInt(tagBits.W), sets, ways, banks, singlePort = true))
  val metaArray = Module(new BankedSRAM(new MetaEntry, sets, ways, banks, singlePort = true))
  val tagRead = Wire(Vec(ways, UInt(tagBits.W)))
  val metaRead = Wire(Vec(ways, new MetaEntry()))

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((sets - 1).U)

  // Replacer
  val repl = ReplacementPolicy.fromString(cacheParams.replacement, ways)
  val random_repl = cacheParams.replacement == "random"
  val replacer_sram_opt = if(random_repl) None else
    Some(Module(new BankedSRAM(UInt(repl.nBits.W), sets, 1, banks, singlePort = true, shouldReset = true)))

  /* ====== Generate response signals ====== */
  // hit/way calculation in stage 3, Cuz SRAM latency is high under high frequency
  /* stage 1: io.read.fire, access Tag/Meta
     stage 2: get Tag/Meta, latch
     stage 3: calculate hit/way and chosen meta/tag by way
  */
  val reqValid_s2 = RegNext(io.read.fire, false.B)
  val reqValid_s3 = RegNext(reqValid_s2, false.B)
  val req_s2 = RegEnable(io.read.bits, 0.U.asTypeOf(io.read.bits), io.read.fire)
  val req_s3 = RegEnable(req_s2, 0.U.asTypeOf(req_s2), reqValid_s2)

  val replReqValid_s2 = RegNext(io.read.fire && io.read.bits.replMode, false.B)
  val replReqValid_s3 = RegNext(replReqValid_s2, false.B)

  // Tag R/W
  tagRead := tagArray.io.r(io.read.fire, io.read.bits.set).resp.data
  tagArray.io.w(
    tagWen,
    io.tagWReq.bits.wtag,
    io.tagWReq.bits.set,
    UIntToOH(io.tagWReq.bits.way)
  )

  // Meta R/W
  metaRead := metaArray.io.r(io.read.fire, io.read.bits.set).resp.data
  metaArray.io.w(
    metaWen,
    io.metaWReq.bits.wmeta,
    io.metaWReq.bits.set,
    io.metaWReq.bits.wayOH
  )

  val metaAll_s3 = RegEnable(metaRead, 0.U.asTypeOf(metaRead), reqValid_s2)
  val tagAll_s3 = RegEnable(tagRead, 0.U.asTypeOf(tagRead), reqValid_s2)

  val tagMatchVec = tagAll_s3.map(_ (tagBits - 1, 0) === req_s3.tag)
  val metaValidVec = metaAll_s3.map(_.state =/= MetaData.INVALID)
  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)

  val hitWay = OHToUInt(hitVec)
  val replaceWay = WireInit(UInt(wayBits.W), 0.U)
  val (inv, invalidWay) = invalid_way_sel(metaAll_s3, replaceWay)
  val chosenWay = Mux(inv, invalidWay, replaceWay)
  // if chosenWay not in wayMask, then choose a way in wayMask
  val finalWay = Mux(
    req_s3.wayMask(chosenWay),
    chosenWay,
    PriorityEncoder(req_s3.wayMask)
  )

  val hit_s3 = Cat(hitVec).orR
  val way_s3 = Mux(hit_s3, hitWay, finalWay)
  val meta_s3 = metaAll_s3(way_s3)
  val tag_s3 = tagAll_s3(way_s3)
  val set_s3 = req_s3.set
  val replacerInfo_s3 = req_s3.replacerInfo

  io.resp.hit   := hit_s3
  io.resp.way   := way_s3
  io.resp.meta  := meta_s3
  io.resp.tag   := tag_s3
  io.resp.set   := set_s3
  io.resp.error := false.B  // depends on ECC
  io.resp.replacerInfo := replacerInfo_s3

  dontTouch(io)
  dontTouch(metaArray.io)
  dontTouch(tagArray.io)

  //[deprecated] io.read.ready := !io.metaWReq.valid && !io.tagWReq.valid && !replacerWen
  val replacerRready = if(cacheParams.replacement == "random") true.B else replacer_sram_opt.get.io.r.req.ready
  io.read.ready := tagArray.io.r.req.ready && metaArray.io.r.req.ready && replacerRready

  /* ======!! Replacement logic !!====== */
  /* ====== Read, choose replaceWay ====== */
  val repl_state_s3 = if(random_repl) {
    when(io.tagWReq.fire){
      repl.miss
    }
    0.U
  } else {
    val repl_sram_r = replacer_sram_opt.get.io.r(io.read.fire, io.read.bits.set).resp.data(0)
    val repl_state = RegEnable(repl_sram_r, 0.U(repl.nBits.W), reqValid_s2)
    repl_state
  }

  replaceWay := repl.get_replace_way(repl_state_s3)

  io.replResp.valid := replReqValid_s3
  io.replResp.bits.tag := tagAll_s3(replaceWay)
  io.replResp.bits.set := req_s3.set
  io.replResp.bits.way := replaceWay
  io.replResp.bits.meta := metaAll_s3(replaceWay)
  io.replResp.bits.mshrId := req_s3.mshrId

  /* ====== Update ====== */
  // update replacer only when A hit or refill, at stage 3
  val updateHit = reqValid_s3 && hit_s3 && req_s3.replacerInfo.channel(0) &&
    (req_s3.replacerInfo.opcode === AcquirePerm || req_s3.replacerInfo.opcode === AcquireBlock)
  val updateRefill = replReqValid_s3
  replacerWen := updateHit || updateRefill

  // !!![TODO]!!! check this @CLS
  // hit-Promotion, miss-Insertion for RRIP, so replMode should hit = false.B
  val touch_way_s3 = Mux(replReqValid_s3, replaceWay, way_s3)
  val rrip_hit_s3 = Mux(replReqValid_s3, false.B, hit_s3)

  if(cacheParams.replacement == "srrip"){
    val next_state_s3 = repl.get_next_state(repl_state_s3, touch_way_s3, rrip_hit_s3)
    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s3, repl_init.asUInt),
      Mux(resetFinish, set_s3, resetIdx),
      1.U
    )
  } else if(cacheParams.replacement == "drrip"){
    //Set Dueling
    val PSEL = RegInit(512.U(10.W)) //32-monitor sets, 10-bits psel
    // track monitor sets' hit rate for each policy: srrip-0,128...3968;brrip-64,192...4032
    when(replReqValid_s3 && (set_s3(6,0)===0.U) && !rrip_hit_s3){  //SDMs_srrip miss
      PSEL := PSEL + 1.U
    } .elsewhen(replReqValid_s3 && (set_s3(6,0)===64.U) && !rrip_hit_s3){ //SDMs_brrip miss
      PSEL := PSEL - 1.U
    }
    // decide use which policy by policy selection counter, for insertion
    /* if set -> SDMs: use fix policy
       else if PSEL(MSB)==0: use srrip
       else if PSEL(MSB)==1: use brrip */
    val repl_type = WireInit(false.B)
    repl_type := Mux(set_s3(6,0)===0.U, false.B,
      Mux(set_s3(6,0)===64.U, true.B,
        Mux(PSEL(9)===0.U, false.B, true.B)))    // false.B - srrip, true.B - brrip
    val next_state_s3 = repl.get_next_state(repl_state_s3, touch_way_s3, rrip_hit_s3, repl_type)

    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s3, repl_init.asUInt),
      Mux(resetFinish, set_s3, resetIdx),
      1.U
    )
  } else {
    val next_state_s3 = repl.get_next_state(repl_state_s3, touch_way_s3)
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s3, 0.U),
      Mux(resetFinish, set_s3, resetIdx),
      1.U
    )
  }

  /* ====== Reset ====== */
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  XSPerfAccumulate(cacheParams, "dirRead_cnt", io.read.fire)
  XSPerfAccumulate(cacheParams, "choose_busy_way", reqValid_s3 && !req_s3.wayMask(chosenWay))
}
