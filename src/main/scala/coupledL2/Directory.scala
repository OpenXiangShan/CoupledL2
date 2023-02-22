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
  val prefetch = if (hasPrefetchBit) Some(Bool()) else None

  def =/=(entry: MetaEntry): Bool = {
    this.asUInt =/= entry.asUInt
  }
}

object MetaEntry {
  def apply()(implicit p: Parameters) = {
    val init = WireInit(0.U.asTypeOf(new MetaEntry))
    init
  }
  def apply(dirty: Bool, state: UInt, clients: UInt, alias: Option[UInt])(implicit p: Parameters) = {
    val entry = Wire(new MetaEntry)
    entry.dirty := dirty
    entry.state := state
    entry.clients := clients
    entry.alias.foreach(_ := alias.getOrElse(0.U))
    entry.prefetch.foreach(_ := false.B)
    entry
  }
  def apply(dirty: Bool, state: UInt, clients: UInt, alias: Option[UInt], prefetch: Bool)(implicit p: Parameters) = {
    val entry = Wire(new MetaEntry)
    entry.dirty := dirty
    entry.state := state
    entry.clients := clients
    entry.alias.foreach(_ := alias.getOrElse(0.U))
    entry.prefetch.foreach(_ := prefetch)
    entry
  }
}

class DirRead(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val source = UInt(sourceIdBits.W)
  val replacerInfo = new ReplacerInfo()
}

class DirResult(implicit p: Parameters) extends L2Bundle {
  val hit = Bool()
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)  // hit way or victim way
  val meta = new MetaEntry()
  val error = Bool()
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
    val resp = ValidIO(new DirResult)
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

  tagArray.io.r <> DontCare
  tagArray.io.w <> DontCare
  metaArray.io.r <> DontCare
  metaArray.io.w <> DontCare

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
  val reqReg = RegEnable(io.read.bits, enable = io.read.fire)
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
  } else {
    val repl_sram_r = replacer_sram_opt.get.io.r(io.read.fire, io.read.bits.set).resp.data(0)
    val repl_state_hold = WireInit(0.U(repl.nBits.W))
    repl_state_hold := HoldUnless(repl_sram_r, RegNext(io.read.fire, false.B))
    val next_state = repl.get_next_state(repl_state_hold, way_s2)
    replacer_sram_opt.get.io.w(replacerWen, RegNext(next_state), RegNext(reqReg.set), 1.U)
    repl_state_hold
  }

  val tagMatchVec = tagRead.map(_ (tagBits - 1, 0) === reqReg.tag)
  val metaValidVec = metaRead.map(_.state =/= MetaData.INVALID)
  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)
  val replaceWay = repl.get_replace_way(repl_state)
  val (inv, invalidWay) = invalid_way_sel(metaRead, replaceWay)
  val chosenWay = Mux(inv, invalidWay, replaceWay)

  hit_s2 := Cat(hitVec).orR
  way_s2 := Mux(hit_s2, hitWay, chosenWay)

  val reqValid_s3 = reqValidReg
  val hit_s3 = RegEnable(hit_s2, false.B, reqValidReg)
  val way_s3 = RegEnable(way_s2, 0.U, reqValidReg)
  val metaAll_s3 = RegEnable(metaRead, reqValidReg)
  val tagAll_s3 = RegEnable(tagRead, reqValidReg)
  val meta_s3 = metaAll_s3(way_s3)
  val tag_s3 = tagAll_s3(way_s3)
  val set_s3 = RegEnable(reqReg.set, reqValidReg)

  io.resp.valid      := reqValid_s3
  io.resp.bits.hit   := hit_s3
  io.resp.bits.way   := way_s3
  io.resp.bits.meta  := meta_s3
  io.resp.bits.tag   := tag_s3
  io.resp.bits.set   := set_s3
  io.resp.bits.error := false.B  // depends on ECC

  dontTouch(io)
  dontTouch(metaArray.io)
  dontTouch(tagArray.io)

  io.read.ready := !io.metaWReq.valid && !io.tagWReq.valid && !replacerWen
  val replacerRready = if(cacheParams.replacement == "random") true.B else replacer_sram_opt.get.io.r.req.ready
  io.read.ready := tagArray.io.r.req.ready && metaArray.io.r.req.ready && replacerRready

  val update = reqReg.replacerInfo.channel(0) && (reqReg.replacerInfo.opcode === TLMessages.AcquirePerm || reqReg.replacerInfo.opcode === TLMessages.AcquireBlock)
  when(reqValidReg && update) {
    replacerWen := true.B
  }.otherwise {
    replacerWen := false.B
  }

}
