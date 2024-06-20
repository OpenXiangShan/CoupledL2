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

package openLLC

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.utils.{SRAMTemplate, ReplacementPolicy}
import utility.{ParallelPriorityMux}
import coupledL2.tl2chi.CHIOpcode.REQOpcodes._

trait HasClientInfo { this: HasOpenLLCParameters =>
  def clientCacheParams = cacheParams.clientCaches

  // TODO: improper parameter settings?
  def clientSets = clientCacheParams.map(_.sets).max
  def clientWays = clientCacheParams.map(_.ways).max
  def clientSetBits = log2Ceil(clientSets)
  def clientWayBits = log2Ceil(clientWays)
  def fullClientTagBits = fullAddressBits - clientSetBits - offsetBits
}

class SelfMetaEntry(implicit p: Parameters) extends Bundle {
  val valid = Bool()
  val dirty = Bool()
}

object SelfMetaEntry {
  def apply()(implicit p: Parameters) = {
    val init = WireInit(0.U.asTypeOf(new SelfMetaEntry))
    init
  }
  def apply(valid: Bool, dirty: Bool)(implicit p: Parameters) = {
    val entry = Wire(new SelfMetaEntry)
    entry.valid := valid
    entry.dirty := dirty
    entry
  }
}

class ClientMetaEntry(implicit p: Parameters) extends Bundle {
  val valid = Bool()
}

object ClientMetaEntry {
  def apply()(implicit p: Parameters) = {
    val init = WireInit(0.U.asTypeOf(new ClientMetaEntry))
    init
  }
  def apply(valid: Bool)(implicit p: Parameters) = {
    val entry = Wire(new ClientMetaEntry)
    entry.valid := valid
    entry
  }
}

class SubDirRead(tagBits: Int, setBits: Int, ways: Int)(implicit p: Parameters) extends LLCBundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val wayMask = UInt(ways.W)
  val replacerInfo = new ReplacerInfo()
  val mshrId = UInt(mshrBits.W)
}

class DirRead(implicit p: Parameters) extends LLCBundle with HasClientInfo {
  val self = new SubDirRead(tagBits, setBits, cacheParams.ways)
  val clients = new SubDirRead(fullClientTagBits, clientSetBits, clientWays)
}

class SubDirResult[T <: Data](tagBits: Int, wayBits: Int, gen: T)(implicit p: Parameters) extends LLCBundle {
  val hit = Bool()
  val tag = UInt(tagBits.W)
  val way = UInt(wayBits.W)
  val meta = gen.cloneType
  val error = Bool()
}

class DirResult(implicit p: Parameters) extends LLCBundle with HasClientInfo {
  val self = new SubDirResult[SelfMetaEntry](tagBits, wayBits, SelfMetaEntry())
  val clients = new SubDirResult[Vec[ClientMetaEntry]](
    fullClientTagBits,
    clientWayBits,
    VecInit(Seq.fill(clientBits)(ClientMetaEntry()))
  )
}

class MetaWrite[T <: Data](setBits: Int, ways: Int, gen: T)(implicit p: Parameters) extends LLCBundle {
  val set = UInt(setBits.W)
  val wayOH = UInt(ways.W)
  val wmeta = gen.cloneType
}

class TagWrite(setBits: Int, wayBits: Int, tagBits: Int)(implicit p: Parameters) extends LLCBundle {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val wtag = UInt(tagBits.W)
}

class SubDirectory[T <: Data](
  sets:         Int,
  ways:         Int,
  tagBits:      Int,
  meta_init_fn: () => T,
  meta_hit_fn:  T => Bool,
  invalid_way_sel: (Seq[T], UInt) => (Bool, UInt), // try to find a invalid way
  replacement: String)(implicit p: Parameters) extends Module {

  private val setBits = log2Ceil(sets)
  private val wayBits = log2Ceil(ways)
  
  val meta_init = meta_init_fn()

  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new SubDirRead(tagBits, setBits, ways)))
    val resp = ValidIO(new SubDirResult[T](tagBits, wayBits, meta_init))
    val tagWReq = Flipped(ValidIO(new TagWrite(setBits, wayBits, tagBits)))
    val metaWReq = Flipped(ValidIO(new MetaWrite[T](setBits, ways, meta_init)))
  })

  val tagWen  = io.tagWReq.valid
  val metaWen = io.metaWReq.valid

  val tagArray = Module(new SRAMTemplate(UInt(tagBits.W), sets, ways, singlePort = true))
  val metaArray = Module(new SRAMTemplate(chiselTypeOf(meta_init), sets, ways, singlePort = true))

  // Replacer
  require(replacement == "random" || replacement == "plru")
  val repl = ReplacementPolicy.fromString(replacement, ways)
  val random_repl = replacement == "random"
  val replacer_sram_opt = if(random_repl) None else
    Some(Module(new SRAMTemplate(UInt(repl.nBits.W), sets, 1, singlePort = true, shouldReset = true)))

  val tagRead = Wire(Vec(ways, UInt(tagBits.W)))
  val metaRead = Wire(Vec(ways, chiselTypeOf(meta_init)))
  val replacerWen = WireInit(false.B)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((sets - 1).U)

  /*
   *  stage 1: io.read.fire, access Tag/Meta
   *  stage 2: get Tag/Meta, latch
   *  stage 3: calculate hit/way and chosen meta/tag by way
   */
  val reqValid_s2 = RegNext(io.read.fire, false.B)
  val req_s2 = RegEnable(io.read.bits, 0.U.asTypeOf(io.read.bits), io.read.fire)

  val reqValid_s3 = RegNext(reqValid_s2, false.B)
  val req_s3 = RegEnable(req_s2, 0.U.asTypeOf(req_s2), reqValid_s2)

  /* Tag Read/Write */
  tagRead := tagArray.io.r(io.read.fire, io.read.bits.set).resp.data
  tagArray.io.w(
    io.tagWReq.valid,
    io.tagWReq.bits.wtag,
    io.tagWReq.bits.set,
    UIntToOH(io.tagWReq.bits.way)
  )

  /* Meta Read/Write */
  metaRead := metaArray.io.r(io.read.fire, io.read.bits.set).resp.data
  metaArray.io.w(
    metaWen || !resetFinish,
    Mux(resetFinish, io.metaWReq.bits.wmeta, meta_init),
    Mux(resetFinish, io.metaWReq.bits.set, resetIdx),
    Mux(resetFinish, io.metaWReq.bits.wayOH, Fill(ways, true.B))
  )

  val metaAll_s3 = RegEnable(metaRead, 0.U.asTypeOf(metaRead), reqValid_s2)
  val tagAll_s3 = RegEnable(tagRead, 0.U.asTypeOf(tagRead), reqValid_s2)

  /* Way selection logic */
  val tagMatchVec = tagAll_s3.map(_ (tagBits - 1, 0) === req_s3.tag)
  val metaValidVec = metaAll_s3.map(meta_hit_fn)
  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)

  val hitWay = OHToUInt(hitVec)
  val replaceWay = Wire(UInt(wayBits.W))
  val (inv, invalidWay) = invalid_way_sel(metaAll_s3, replaceWay)
  val chosenWay = Mux(inv, invalidWay, replaceWay)

  val hit_s3 = Cat(hitVec).orR
  val way_s3 = Mux(hit_s3, hitWay, chosenWay)
  val meta_s3 = metaAll_s3(way_s3)
  val tag_s3 = tagAll_s3(way_s3)
  val set_s3 = req_s3.set
  val replacerInfo_s3 = req_s3.replacerInfo

  /* Replacement logic */
  /** Read, choose replaceWay **/
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

  /** Update replacer meta **/
  // PLRU: update replacer only when Snoopable-Read hit or refill, at stage 3
  if (replacement == "plru") {
    val updateHit = reqValid_s3 && hit_s3 &&
      (replacerInfo_s3.opcode === ReadUnique || replacerInfo_s3.opcode === ReadNotSharedDirty)
    val updateRefill = reqValid_s3 && req_s3.replacerInfo.refill
    replacerWen := updateHit || updateRefill
    val next_state_s3 = repl.get_next_state(repl_state_s3, way_s3)
    replacer_sram_opt.get.io.w(
      !resetFinish || replacerWen,
      Mux(resetFinish, next_state_s3, 0.U),
      Mux(resetFinish, set_s3, resetIdx),
      1.U
    )
  }

  io.read.ready := !metaWen && !tagWen && !replacerWen && resetFinish

  io.resp.valid      := reqValid_s3
  io.resp.bits.hit   := hit_s3
  io.resp.bits.way   := way_s3
  io.resp.bits.meta  := meta_s3
  io.resp.bits.tag   := tag_s3
  io.resp.bits.error := false.B

  dontTouch(io)
  dontTouch(metaArray.io)
  dontTouch(tagArray.io)

  /* Reset logic */
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

}

class Directory(implicit p: Parameters) extends LLCModule with HasClientInfo {
  private val selfSets = cacheParams.sets
  private val selfWays = cacheParams.ways
  private val selfWayBits = wayBits
  private val selfSetBits = setBits
  private val selfTagBits = tagBits

  def client_meta_init_fn() = VecInit(Seq.fill(clientBits)(ClientMetaEntry()))
  def self_meta_init_fn() = SelfMetaEntry()

  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new DirRead()))
    val resp = ValidIO(new DirResult())
    val selfMetaWReq = Flipped(ValidIO(
      new MetaWrite[SelfMetaEntry](selfSetBits, selfWays, self_meta_init_fn())
    ))
    val selfTagWReq = Flipped(ValidIO(
      new TagWrite(selfSetBits, selfWayBits, selfTagBits)
    ))
    val clientMetaWReq = Flipped(ValidIO(
      new MetaWrite[Vec[ClientMetaEntry]](clientSetBits, clientWays, client_meta_init_fn())
    ))
    val clientTagWReq = Flipped(ValidIO(
      new TagWrite(clientSetBits, clientWayBits, fullClientTagBits)
    ))
  })

  def client_invalid_way_sel(metaVec: Seq[Vec[ClientMetaEntry]], repl: UInt): (Bool, UInt) = {
    val invalid_vec = metaVec.map(metas => Cat(metas.map(!_.valid)).andR)
    val has_invalid_way = Cat(invalid_vec).orR
    val way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(clientWayBits.W)))
    (has_invalid_way, way)
  }

  // Snoop filter
  val clientDir = Module(
    new SubDirectory[Vec[ClientMetaEntry]](
      sets = clientSets,
      ways = clientWays,
      tagBits = fullClientTagBits,
      meta_init_fn = client_meta_init_fn,
      meta_hit_fn = metas => Cat(metas.map(_.valid)).orR,
      invalid_way_sel = client_invalid_way_sel,
      replacement = "random"
    )
  )

  def self_invalid_way_sel(metaVec: Seq[SelfMetaEntry], repl: UInt): (Bool, UInt) = {
    val invalid_vec = metaVec.map(!_.valid)
    val has_invalid_way = Cat(invalid_vec).orR
    val way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(selfWayBits.W)))
    (has_invalid_way, way)
  }

  val selfDir = Module(
    new SubDirectory[SelfMetaEntry](
      sets = selfSets,
      ways = selfWays,
      tagBits = tagBits,
      meta_init_fn = self_meta_init_fn,
      meta_hit_fn = meta => meta.valid,
      invalid_way_sel = self_invalid_way_sel,
      replacement = cacheParams.replacement
    )
  )

  selfDir.io.read.valid := io.read.valid
  selfDir.io.read.bits <> io.read.bits.self
  selfDir.io.metaWReq <> io.selfMetaWReq
  selfDir.io.tagWReq <> io.selfTagWReq

  clientDir.io.read.valid := io.read.valid
  clientDir.io.read.bits <> io.read.bits.clients
  clientDir.io.metaWReq <> io.clientMetaWReq
  clientDir.io.tagWReq <> io.clientTagWReq
  
  io.read.ready := selfDir.io.read.ready && clientDir.io.read.ready
  io.resp.bits.self <> selfDir.io.resp.bits
  io.resp.bits.clients <> clientDir.io.resp.bits
  io.resp.valid := selfDir.io.resp.valid
  val respValids = Cat(selfDir.io.resp.valid, clientDir.io.resp.valid)
  assert(!respValids.xorR, "Resp valids must be all 1s or 0s")

}
