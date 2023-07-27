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
import utility.ParallelPriorityMux
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink.TLMessages

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

class Directory(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new DirRead))
    val resp = Output(new DirResult)
    val metaWReq = Flipped(ValidIO(new MetaWrite))
    val tagWReq = Flipped(ValidIO(new TagWrite))
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
  val replacerWen = RegInit(false.B)

  val tagArray  = Module(new BankedSRAM(UInt(tagBits.W), sets, ways, banks, singlePort = true))
  val metaArray = Module(new BankedSRAM(new MetaEntry, sets, ways, banks, singlePort = true))
  val tagRead = Wire(Vec(ways, UInt(tagBits.W)))
  val metaRead = Wire(Vec(ways, new MetaEntry()))

  val reqValidReg = RegNext(io.read.fire, false.B)
  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((sets - 1).U)

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

  // Generate response signals
  /* stage 1: io.read.fire, access Tag/Meta
     stage 2: get Tag/Meta, calculate hit/way
     stage 3: output latched hit/way and chosen meta/tag by way
  */
  // TODO: how about moving hit/way calculation to stage 2? Cuz SRAM latency can be high under high frequency
  val reqReg = RegEnable(io.read.bits, 0.U.asTypeOf(io.read.bits), enable = io.read.fire)
  val hit_s2 = Wire(Bool())
  val way_s2 = Wire(UInt(wayBits.W))

  // Replacer
  val repl = ReplacementPolicy.fromString(cacheParams.replacement, ways)
  val random_repl = cacheParams.replacement == "random"
  val replacer_sram_opt = if(random_repl) None else
    Some(Module(new BankedSRAM(UInt(repl.nBits.W), sets, 1, banks, singlePort = true, shouldReset = true)))

  val repl_state = if(random_repl){
    when(io.tagWReq.fire){
      repl.miss
    }
    0.U
  } else if(cacheParams.replacement == "srrip"){
    val repl_sram_r = replacer_sram_opt.get.io.r(io.read.fire(), io.read.bits.set).resp.data(0)
    val repl_state_hold = WireInit(0.U(repl.nBits.W))
    repl_state_hold := HoldUnless(repl_sram_r, RegNext(io.read.fire(), false.B))
    val next_state = repl.get_next_state(repl_state_hold, way_s2, hit_s2)
    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, RegNext(next_state, 0.U.asTypeOf(next_state)), repl_init.asUInt),
      Mux(resetFinish, RegNext(reqReg.set, 0.U.asTypeOf(reqReg.set)), resetIdx),
      1.U
    )

    repl_state_hold
  } else if(cacheParams.replacement == "drrip"){
    //Set Dueling
    val PSEL = RegInit(512.U(10.W)) //32-monitor sets, 10-bits psel
    // track monitor sets' hit rate for each policy: srrip-0,128...3968;brrip-64,192...4032
    when(reqValidReg && (reqReg.set(6,0)===0.U) && !hit_s2){  //SDMs_srrip miss
      PSEL := PSEL + 1.U
    } .elsewhen(reqValidReg && (reqReg.set(6,0)===64.U) && !hit_s2){ //SDMs_brrip miss
      PSEL := PSEL - 1.U
    }

    val repl_sram_r = replacer_sram_opt.get.io.r(io.read.fire(), io.read.bits.set).resp.data(0)
    val repl_state_hold = WireInit(0.U(repl.nBits.W))
    repl_state_hold := HoldUnless(repl_sram_r, RegNext(io.read.fire(), false.B))
    // decide use which policy by policy selection counter, for insertion
    /*if set -> SDMs: use fix policy
      else if PSEL(MSB)==0: use srrip
      else if PSEL(MSB)==1: use brrip*/
    val repl_type = WireInit(false.B)
    repl_type := Mux(reqReg.set(6,0)===0.U, false.B, 
                    Mux(reqReg.set(6,0)===64.U, true.B,
                      Mux(PSEL(9)===0.U, false.B, true.B)))    // false.B - srrip, true.B - brrip
    val next_state = repl.get_next_state(repl_state_hold, way_s2, hit_s2, repl_type)

    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, RegNext(next_state, 0.U.asTypeOf(next_state)), repl_init.asUInt),
      Mux(resetFinish, RegNext(reqReg.set, 0.U.asTypeOf(reqReg.set)), resetIdx),
      1.U
    )

    repl_state_hold
  } else {
    val repl_sram_r = replacer_sram_opt.get.io.r(io.read.fire, io.read.bits.set).resp.data(0)
    val repl_state_hold = WireInit(0.U(repl.nBits.W))
    repl_state_hold := HoldUnless(repl_sram_r, RegNext(io.read.fire, false.B))
    val next_state = repl.get_next_state(repl_state_hold, way_s2)
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, RegNext(next_state, 0.U.asTypeOf(next_state)), 0.U),
      Mux(resetFinish, RegNext(reqReg.set, 0.U.asTypeOf(reqReg.set)), resetIdx),
      1.U
    )
    repl_state_hold
  }

  val tagMatchVec = tagRead.map(_ (tagBits - 1, 0) === reqReg.tag)
  val metaValidVec = metaRead.map(_.state =/= MetaData.INVALID)
  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)
  val replaceWay = repl.get_replace_way(repl_state)
  val (inv, invalidWay) = invalid_way_sel(metaRead, replaceWay)
  val chosenWay = Mux(inv, invalidWay, replaceWay)
  // if chosenWay not in wayMask, then choose a way in wayMask
  val finalWay = Mux(
    reqReg.wayMask(chosenWay),
    chosenWay,
    PriorityEncoder(reqReg.wayMask)
  )

  hit_s2 := Cat(hitVec).orR
  way_s2 := Mux(hit_s2, hitWay, finalWay)

  val hit_s3 = RegEnable(hit_s2, false.B, reqValidReg)
  val way_s3 = RegEnable(way_s2, 0.U, reqValidReg)
  val metaAll_s3 = RegEnable(metaRead, 0.U.asTypeOf(metaRead), reqValidReg)
  val tagAll_s3 = RegEnable(tagRead, 0.U.asTypeOf(tagRead), reqValidReg)
  val meta_s3 = metaAll_s3(way_s3)
  val tag_s3 = tagAll_s3(way_s3)
  val set_s3 = RegEnable(reqReg.set, reqValidReg)
  val replacerInfo_s3 = RegEnable(reqReg.replacerInfo, reqValidReg)

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

  // io.read.ready := !io.metaWReq.valid && !io.tagWReq.valid && !replacerWen
  val replacerRready = if(cacheParams.replacement == "random") true.B else replacer_sram_opt.get.io.r.req.ready
  io.read.ready := tagArray.io.r.req.ready && metaArray.io.r.req.ready && replacerRready

  val update = reqReg.replacerInfo.channel(0) && (reqReg.replacerInfo.opcode === TLMessages.AcquirePerm || reqReg.replacerInfo.opcode === TLMessages.AcquireBlock)
  when(reqValidReg && update) {
    replacerWen := true.B
  }.otherwise {
    replacerWen := false.B
  }

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  XSPerfAccumulate(cacheParams, "dirRead_cnt", reqValidReg)
  XSPerfAccumulate(cacheParams, "choose_busy_way", reqValidReg && !reqReg.wayMask(chosenWay))
}
