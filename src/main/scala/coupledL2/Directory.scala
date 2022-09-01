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
import chipsalliance.rocketchip.config.Parameters

class MetaEntry(implicit p: Parameters) extends L2Bundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clients = UInt(clientBits.W)  // valid-bit of clients
  // TODO: record prefetcher info
}

class DirRead(implicit p: Parameters) extends L2Bundle {
  val idOH = UInt(mshrsAll.W)
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val source = UInt(sourceIdBits.W)
  val replacerInfo = new ReplacerInfo()
}

class DirResult(implicit p: Parameters) extends L2Bundle {
  val idOH = UInt(mshrsAll.W)
  val hit = Bool()
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)  // hit way or victim way
  val meta = new MetaEntry()
  val error = Bool()
}

class MetaWrite(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
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
    val resp = DecoupledIO(new DirResult)
    val metaWReq = Flipped(DecoupledIO(new MetaWrite))
    val tagWReq = Flipped(DecoupledIO(new TagWrite))
  })

  def invalid_way_sel(metaVec: Seq[MetaEntry], repl: UInt) = {
    val invalid_vec = metaVec.map(_.state === MetaData.INVALID)
    val has_invalid_way = Cat(invalid_vec).orR
    val way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(wayBits.W)))
    (has_invalid_way, way)
  }

  val sets = cacheParams.sets
  val ways = cacheParams.ways
  val tag_wen = io.metaWReq.valid
  val dir_wen = io.tagWReq.valid

  val metaInit = Wire(new MetaEntry())
  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((sets - 1).U)
  val cycleCnt = Counter(true.B, 2)

  val tagArray = Module(new SRAMTemplate(UInt(tagBits.W), sets, ways, singlePort = true))
  val metaArray = Module(new SRAMTemplate(new MetaEntry, sets, ways, singlePort = true))
  val tagRead = Wire(Vec(ways, UInt(tagBits.W)))
  val metaRead = Wire(Vec(ways, new MetaEntry()))

  val reqValidReg = RegInit(false.B)

  tagArray.io.r <> DontCare
  tagArray.io.w <> DontCare
  metaArray.io.r <> DontCare
  metaArray.io.w <> DontCare

  // Tag R/W
  tagRead := tagArray.io.r(io.read.fire, io.read.bits.set).resp.data
  tagArray.io.w(
    io.tagWReq.fire,
    io.tagWReq.bits.wtag,
    io.tagWReq.bits.set,
    UIntToOH(io.tagWReq.bits.way)
  )

  // Meta R/W
  metaRead := metaArray.io.r(io.read.fire, io.read.bits.set).resp.data
  metaArray.io.w(
    !resetFinish || dir_wen,
    Mux(resetFinish, io.metaWReq.bits.wmeta, metaInit),
    Mux(resetFinish, io.metaWReq.bits.set, resetIdx),
    Mux(resetFinish, UIntToOH(io.metaWReq.bits.way), Fill(ways, true.B))
  )

  // Generate response signals
  /* stage 0: io.read.fire, access Tag/Meta
     stage 1: get Tag/Meta, calculate hit/way
     stage 2: output latched hit/way and chosen meta/tag by way
  */
  // TODO: how about moving hit/way calculation to stage 2? Cuz SRAM latency can be high under high frequency
  reqValidReg := RegNext(io.read.fire, false.B)
  val reqReg = RegEnable(io.read.bits, enable = io.read.fire)
  val tagMatchVec = tagRead.map(_ (tagBits - 1, 0) === reqReg.tag)
  val metaValidVec = metaRead.map(_.state =/= MetaData.INVALID)
  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)
  val replaceWay = 0.U(wayBits.W)  // TODO: add replacer logic
  val (inv, invalidWay) = invalid_way_sel(metaRead, replaceWay)
  val chosenWay = Mux(inv, invalidWay, replaceWay)

  val hit_s1 = Cat(hitVec).orR
  val way_s1 = Mux(hit_s1, hitWay, chosenWay)

  val reqValid_s2 = reqValidReg
  val hit_s2 = RegEnable(hit_s1, false.B, reqValidReg)
  val way_s2 = RegEnable(way_s1, 0.U, reqValidReg)
  val metaAll_s2 = RegEnable(metaRead, reqValidReg)
  val tagAll_s2 = RegEnable(tagRead, reqValidReg)
  val meta_s2 = metaAll_s2(way_s2)
  val tag_s2 = tagAll_s2(way_s2)

  io.resp.valid      := reqValid_s2
  io.resp.bits.hit   := hit_s2
  io.resp.bits.way   := way_s2
  io.resp.bits.meta  := meta_s2
  io.resp.bits.tag   := tag_s2
  io.resp.bits.error := false.B  // depends on ECC

  // Manual initialize meta before reading
  // TODO: move initialization logic to requestArb
  metaInit := DontCare
  metaInit.state := MetaData.INVALID
  when(resetIdx === 0.U && !cycleCnt._1(0)) {
    resetFinish := true.B
  }
  when(!resetFinish && !cycleCnt._1(0)) {
    resetIdx := resetIdx - 1.U
  }
  io.read.ready := !tag_wen && !dir_wen && resetFinish

}