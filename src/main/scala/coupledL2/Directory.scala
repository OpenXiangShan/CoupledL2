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
import utility.{ParallelPriorityMux, RegNextN, XSPerfAccumulate, Code, SRAMTemplate}
import org.chipsalliance.cde.config.Parameters
import coupledL2.prefetch.PfSource
import freechips.rocketchip.tilelink.TLMessages._

class MetaEntry(implicit p: Parameters) extends L2Bundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clients = UInt(clientBits.W)  // valid-bit of clients
  // TODO: record specific state of clients instead of just 1-bit
  val alias = aliasBitsOpt.map(width => UInt(width.W)) // alias bits of client
  val prefetch = if (hasPrefetchBit) Some(Bool()) else None // whether block is prefetched
  val prefetchSrc = if (hasPrefetchSrc) Some(UInt(PfSource.pfSourceBits.W)) else None // prefetch source
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
  def apply(dirty: Bool, state: UInt, clients: UInt, alias: Option[UInt], prefetch: Bool = false.B,
            pfsrc: UInt = PfSource.NoWhere.id.U, accessed: Bool = false.B
  )(implicit p: Parameters) = {
    val entry = Wire(new MetaEntry)
    entry.dirty := dirty
    entry.state := state
    entry.clients := clients
    entry.alias.foreach(_ := alias.getOrElse(0.U))
    entry.prefetch.foreach(_ := prefetch)
    entry.prefetchSrc.foreach(_ := pfsrc)
    entry.accessed := accessed
    entry
  }
}

class DirRead(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  // dirResult.way must only be in the wayMask
  val wayMask = UInt(cacheParams.ways.W)
  val replacerInfo = new ReplacerInfo()
  // dirRead when refill
  val refill = Bool()
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
  val retry = Bool()
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

case object HasTagECCParam

class Directory(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new DirRead))
    val resp = ValidIO(new DirResult)
    val metaWReq = Flipped(ValidIO(new MetaWrite))
    val tagWReq = Flipped(ValidIO(new TagWrite))
    val replResp = ValidIO(new ReplacerResult)
    // used to count occWays for Grant to retry
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
  })

  def invalid_way_sel(metaVec: Seq[MetaEntry], repl: UInt) = {
    val invalid_vec = metaVec.map(_.state === MetaData.INVALID)
    val has_invalid_way = Cat(invalid_vec).orR
    val way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(wayBits.W)))
    (has_invalid_way, way)
  }

  def get_ecc_from_encTag(encTag: UInt) = {
    require((encTag.getWidth == encTagBits))
    encTag(encTagBits - 1, tagBits)
  }

  val tagECCParam = if(enableTagECC) Some(HasTagECCParam) else None

  val sets = cacheParams.sets
  val ways = cacheParams.ways

  val tagWen  = io.tagWReq.valid
  val metaWen = io.metaWReq.valid
  val replacerWen = WireInit(false.B)

  // val tagArray  = Module(new SRAMTemplate(UInt(tagBits.W), sets, ways, singlePort = true))
  val tagArray = Module(new SplittedSRAM(
    gen = UInt(tagBits.W),
    set = sets,
    way = ways,
    waySplit = 2,
    singlePort = true,
    readMCP2 = false
  ))
  val metaArray = Module(new SRAMTemplate(new MetaEntry, sets, ways, singlePort = true))
  val eccArray = tagECCParam.map {
    case _ =>
      val array = Module(new SRAMTemplate(
        gen = UInt(eccTagBits.W),
        set = sets,
        way = ways,
        singlePort = true,
      ))
      array
  }
  val tagRead = Wire(Vec(ways, UInt(tagBits.W)))
  val metaRead = Wire(Vec(ways, new MetaEntry()))
  val eccRead = Wire(Vec(ways, UInt(eccTagBits.W)))

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((sets - 1).U)

  // Replacer
  val repl = ReplacementPolicy.fromString(cacheParams.replacement, ways)
  val random_repl = cacheParams.replacement == "random"
  val replacer_sram_opt = if(random_repl) None else
    Some(Module(new SRAMTemplate(UInt(repl.nBits.W), sets, 1, singlePort = true, shouldReset = true)))

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

  val refillReqValid_s2 = RegNext(io.read.fire && io.read.bits.refill, false.B)
  val refillReqValid_s3 = RegNext(refillReqValid_s2, false.B)

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

  // ECC R/W
  eccArray match {
    case Some(ecc) =>
      eccRead := ecc.io.r(io.read.fire, io.read.bits.set).resp.data
    case None =>
      eccRead := 0.U.asTypeOf(eccRead)
  }
  eccArray match {
    case Some(ecc) =>
      ecc.io.w(
        tagWen,
        get_ecc_from_encTag(cacheParams.tagCode.encode(io.tagWReq.bits.wtag)),
        io.tagWReq.bits.set,
        UIntToOH(io.tagWReq.bits.way)
      )
    case None =>
  }

  val metaAll_s3 = RegEnable(metaRead, 0.U.asTypeOf(metaRead), reqValid_s2)
  val tagAll_s3 = RegEnable(tagRead, 0.U.asTypeOf(tagRead), reqValid_s2)
  val eccAll_s3 = RegEnable(eccRead, 0.U.asTypeOf(eccRead), reqValid_s2)

  val tagMatchVec = tagAll_s3.map(_ (tagBits - 1, 0) === req_s3.tag)
  val metaValidVec = metaAll_s3.map(_.state =/= MetaData.INVALID)
  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)

  /* ====== refill retry ====== */
  // when refill, ways that have not finished writing its refillData back to DS (in MSHR Release),
  // or using by Alias-Acquire (hit), can not be used for replace.
  // choose free way to refill, if all ways are occupied, we cancel the Grant and LET IT RETRY
  // compare is done at Stage2 for better timing
  val occWayMask_s2 = VecInit(io.msInfo.map(s =>
    Mux(
      s.valid && (s.bits.set === req_s2.set) && (s.bits.blockRefill || s.bits.dirHit),
      UIntToOH(s.bits.way, ways),
      0.U(ways.W)
    )
  )).reduceTree(_ | _)
  
  val freeWayMask_s3 = RegEnable(~occWayMask_s2, refillReqValid_s2)
  val refillRetry = !(freeWayMask_s3.orR)

  val hitWay = OHToUInt(hitVec)
  val replaceWay = WireInit(UInt(wayBits.W), 0.U)
  val (inv, invalidWay) = invalid_way_sel(metaAll_s3, replaceWay)
  val chosenWay = Mux(inv, invalidWay, replaceWay)
  // if chosenWay not in wayMask, then choose a way in wayMask
  // for retry bug fixing: if the chosenway cause retry last time, choose another way
  /*val finalWay = Mux(
    req_s3.wayMask(chosenWay),
    chosenWay,
    PriorityEncoder(req_s3.wayMask)
  )*/
  // for retry bug fixing: if the chosenway not in freewaymask, choose another way
  // TODO: req_s3.wayMask not take into consideration
  val finalWay = Mux(
    freeWayMask_s3(chosenWay),
    chosenWay,
    PriorityEncoder(freeWayMask_s3)
  )

  val hit_s3 = Cat(hitVec).orR
  val way_s3 = Mux(hit_s3, hitWay, finalWay)
  val meta_s3 = metaAll_s3(way_s3)
  val tag_s3 = tagAll_s3(way_s3)
  val set_s3 = req_s3.set
  val replacerInfo_s3 = req_s3.replacerInfo
  val eccTag_s3 = Cat(eccAll_s3(way_s3), tag_s3)
  val error_s3 = cacheParams.tagCode.decode(eccTag_s3).error //TODO: distinguish correctable or

  io.resp.valid      := reqValid_s3
  io.resp.bits.hit   := hit_s3
  io.resp.bits.way   := way_s3
  io.resp.bits.meta  := meta_s3
  io.resp.bits.tag   := tag_s3
  io.resp.bits.set   := set_s3
  io.resp.bits.error := error_s3  // depends on ECC
  io.resp.bits.replacerInfo := replacerInfo_s3

  dontTouch(io)
  dontTouch(metaArray.io)
  dontTouch(tagArray.io)

  io.read.ready := !io.metaWReq.valid && !io.tagWReq.valid && !replacerWen

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

  io.replResp.valid := refillReqValid_s3
  io.replResp.bits.tag := tagAll_s3(finalWay)
  io.replResp.bits.set := req_s3.set
  io.replResp.bits.way := finalWay
  io.replResp.bits.meta := metaAll_s3(finalWay)
  io.replResp.bits.mshrId := req_s3.mshrId
  io.replResp.bits.retry := refillRetry

  /* ====== Update ====== */
  // PLRU: update replacer only when A hit or refill, at stage 3
  // RRIP: update replacer when A/C hit or refill
  val updateHit = if(cacheParams.replacement == "drrip" || cacheParams.replacement == "srrip"){
    reqValid_s3 && hit_s3 &&
    ((req_s3.replacerInfo.channel(0) && (req_s3.replacerInfo.opcode === AcquirePerm || req_s3.replacerInfo.opcode === AcquireBlock || req_s3.replacerInfo.opcode === Hint)) ||
     (req_s3.replacerInfo.channel(2) && (req_s3.replacerInfo.opcode === Release || req_s3.replacerInfo.opcode === ReleaseData)))
  } else {
    reqValid_s3 && hit_s3 && req_s3.replacerInfo.channel(0) &&
    (req_s3.replacerInfo.opcode === AcquirePerm || req_s3.replacerInfo.opcode === AcquireBlock)
  }
  val updateRefill = refillReqValid_s3 && !refillRetry
  // update replacer when A/C hit or refill
  replacerWen := updateHit || updateRefill

  // hit-Promotion, miss-Insertion for RRIP
  // origin-bit marks whether the data_block is reused
  val origin_bit_opt = if(random_repl) None else
    Some(Module(new SRAMTemplate(Bool(), sets, ways, singlePort = true, shouldReset = true)))
  val origin_bits_r = origin_bit_opt.get.io.r(io.read.fire, io.read.bits.set).resp.data
  val origin_bits_hold = Wire(Vec(ways, Bool()))
  origin_bits_hold := HoldUnless(origin_bits_r, RegNext(io.read.fire, false.B))
  origin_bit_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, hit_s3, false.B),
      Mux(resetFinish, req_s3.set, resetIdx),
      UIntToOH(way_s3)
  )
  val rrip_req_type = WireInit(0.U(4.W))
  // [3]: 0-firstuse, 1-reuse;
  // [2]: 0-acquire, 1-release;
  // [1]: 0-non-prefetch, 1-prefetch;
  // [0]: 0-not-refill, 1-refill
  rrip_req_type := Cat(origin_bits_hold(way_s3),
    req_s3.replacerInfo.channel(2),
    (!refillReqValid_s3 && req_s3.replacerInfo.channel(0) && req_s3.replacerInfo.opcode === Hint) || (req_s3.replacerInfo.channel(2) && metaAll_s3(way_s3).prefetch.getOrElse(false.B)) || (refillReqValid_s3 && req_s3.replacerInfo.refill_prefetch),
    req_s3.refill
  )

  if(cacheParams.replacement == "srrip"){
    val next_state_s3 = repl.get_next_state(repl_state_s3, way_s3, hit_s3, inv, rrip_req_type)
    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s3, repl_init.asUInt),
      Mux(resetFinish, set_s3, resetIdx),
      1.U
    )
    
  } else if(cacheParams.replacement == "drrip"){
    // Set Dueling
    val PSEL = RegInit(512.U(10.W)) //32-monitor sets, 10-bits psel
    // track monitor sets' hit rate for each policy
    // basic SDMs complement-selection policy: srrip--set_idx[group-:]==set_idx[group_offset-:]; brrip--set_idx[group-:]==!set_idx[group_offset-:]
    val setBits = log2Ceil(sets)
    val half_setBits = setBits >> 1
    val match_a = set_s3(setBits-1,setBits-half_setBits-1)===set_s3(setBits-half_setBits-1,0)  // 512 sets [8:4][4:0]
    val match_b = set_s3(setBits-1,setBits-half_setBits-1)===(~set_s3(setBits-half_setBits-1,0))
    when(refillReqValid_s3 && match_a && !hit_s3 && (PSEL=/=1023.U)){  //SDMs_srrip miss
      PSEL := PSEL + 1.U
    } .elsewhen(refillReqValid_s3 && match_b && !hit_s3 && (PSEL=/=0.U)){ //SDMs_brrip miss
      PSEL := PSEL - 1.U
    }
    // decide use which policy by policy selection counter, for insertion
    /* if set -> SDMs: use fix policy
       else if PSEL(MSB)==0: use srrip
       else if PSEL(MSB)==1: use brrip */
    val repl_type = WireInit(false.B)
    repl_type := Mux(match_a, false.B, 
                    Mux(match_b, true.B,
                      Mux(PSEL(9)===0.U, false.B, true.B)))    // false.B - srrip, true.B - brrip

    val next_state_s3 = repl.get_next_state(repl_state_s3, way_s3, hit_s3, inv, repl_type, rrip_req_type)

    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s3, repl_init.asUInt),
      Mux(resetFinish, set_s3, resetIdx),
      1.U
    )
  } else {
    val next_state_s3 = repl.get_next_state(repl_state_s3, way_s3)
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

  XSPerfAccumulate("dirRead_cnt", io.read.fire)
  XSPerfAccumulate("choose_busy_way", reqValid_s3 && !req_s3.wayMask(chosenWay))
}
