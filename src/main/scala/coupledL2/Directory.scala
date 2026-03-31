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
import utility.mbist.MbistPipeline
import coupledL2.utils._
import utility.{ChiselDB,  Code, MemReqSource, ParallelPriorityMux, RegNextN, XSPerfAccumulate}
import utility.sram.SRAMTemplate
import org.chipsalliance.cde.config.Parameters
import coupledL2.prefetch.PfSource
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util.SeqToAugmentedSeq

class MetaEntry(implicit p: Parameters) extends L2Bundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clients = UInt(clientBits.W)  // valid-bit of clients
  // TODO: record specific state of clients instead of just 1-bit
  val alias = aliasBitsOpt.map(width => UInt(width.W)) // alias bits of client
  val prefetch = if (hasPrefetchBit) Some(Bool()) else None // whether block is prefetched
  val prefetchSrc = if (hasPrefetchSrc) Some(UInt(PfSource.pfSourceBits.W)) else None // prefetch source
  val accessed = Bool()
  val tagErr = Bool() // ECC error from L1/L3; DataCheck for CHI
  val dataErr = Bool()

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
            pfsrc: UInt = PfSource.NoWhere.id.U, accessed: Bool = false.B,
            tagErr: Bool = false.B, dataErr: Bool = false.B
  )(implicit p: Parameters) = {
    val entry = Wire(new MetaEntry)
    entry.dirty := dirty
    entry.state := state
    entry.clients := clients
    entry.alias.foreach(_ := alias.getOrElse(0.U))
    entry.prefetch.foreach(_ := prefetch)
    entry.prefetchSrc.foreach(_ := pfsrc)
    entry.accessed := accessed
    entry.tagErr := tagErr
    entry.dataErr := dataErr
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
  // when flush l2
  val cmoAll = Bool()
  val cmoWay = UInt(wayBits.W)
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
  val validHold = Bool()
}

class MetaWrite(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val wayOH = UInt(cacheParams.ways.W)
  val wmeta = new MetaEntry
}

class TagWrite(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val wayOH = UInt(cacheParams.ways.W)
  val wtag = UInt(tagBits.W)
}

// DB entry for prefetch lifecycle tracking (arrival, access, eviction)
class PrefetchDbEntry(implicit p: Parameters) extends L2Bundle {
  val setIdx = UInt(setBits.W)
  val isPrefetch = Bool()
  val isHit = Bool()
  val tag = UInt(tagBits.W)
  val way = UInt(wayBits.W)
  val metaSource = UInt(PfSource.pfSourceBits.W)
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
}


class Directory(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new DirRead))
    val resp = ValidIO(new DirResult)
    val metaWReq = Flipped(ValidIO(new MetaWrite))
    val tagWReq = Flipped(ValidIO(new TagWrite))
    val replResp = ValidIO(new ReplacerResult)
    // used to count occWays for Grant to retry
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
    val metaOnHit = new MetaEntry()
    val errOnSnp = Bool()
    val wayOH = Output(UInt(cacheParams.ways.W))
    val replWayOH = Output(UInt(cacheParams.ways.W))
  })

  def invalid_way_sel(metaVec: Seq[MetaEntry]) = {
    val invalid_vec = metaVec.map(_.state === MetaData.INVALID)
    val has_invalid_way = Cat(invalid_vec).orR
    val invalid_oh = MaskToOH(invalid_vec.asUInt)
    val invalid_way = OHToUInt(invalid_oh)
    (has_invalid_way, invalid_way, invalid_oh) // one-hot of invalid ways
  }

  val sets = cacheParams.sets
  val ways = cacheParams.ways

  val tagWen  = io.tagWReq.valid
  val metaWen = io.metaWReq.valid
  val replacerWen = WireInit(false.B)

  // val tagArray  = Module(new SRAMTemplate(UInt(tagBits.W), sets, ways, singlePort = true))
  private val mbist = p(L2ParamKey).hasMbist
  private val hasSramCtl = p(L2ParamKey).hasSramCtl
  val tagArray = if (enableTagECC) {
    Module(new SplittedSRAM(
      gen = UInt((tagBankSplit * encTagBankBits).W),
      set = sets,
      way = ways,
      waySplit = 2,
      dataSplit = if (enableTagSRAMSplit) {
        tagSRAMSplit
      } else {
        1
      },
      singlePort = true,
      readMCP2 = false,
      hasMbist = mbist,
      hasSramCtl = hasSramCtl
    ))
  } else {
    Module(new SplittedSRAM(
      gen = UInt(tagBits.W),
      set = sets,
      way = ways,
      waySplit = 2,
      singlePort = true,
      readMCP2 = false,
      hasMbist = mbist,
      hasSramCtl = hasSramCtl
    ))
  }

  val metaArray = Module(new SRAMTemplate(new MetaEntry, sets, ways, singlePort = true, hasMbist = mbist, hasSramCtl = hasSramCtl))

  val metaRead = Wire(Vec(ways, new MetaEntry()))

  // Replacer
  val repl = ReplacementPolicy.fromString(cacheParams.replacement, ways)
  val random_repl = cacheParams.replacement == "random"
  val replacer_sram_opt = if(random_repl) None else
    Some(Module(new SRAMTemplate(UInt(repl.nBits.W), sets, 1, singlePort = true, shouldReset = true, hasMbist = mbist, hasSramCtl = hasSramCtl)))

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
  val refillReqValid_hold_s3 = RegEnable(refillReqValid_s2, false.B, !RegNext(refillReqValid_s2))

  // Tag(ECC) R/W
  val tagWrite = if (enableTagECC) {
    Cat(VecInit(Seq.tabulate(tagBankSplit)(i =>
      io.tagWReq.bits.wtag(tagBankBits * (i + 1) - 1, tagBankBits * i))).map(tag => cacheParams.dataCode.encode(tag)))
  } else {
    io.tagWReq.bits.wtag
  }
  val tagRead = tagArray.io.r(io.read.fire, io.read.bits.set).resp.data
  assert(PopCount(io.tagWReq.bits.wayOH) <= 1.U, "Tag write should be one-hot")
  tagArray.io.w(
    tagWen,
    tagWrite,
    io.tagWReq.bits.set,
    io.tagWReq.bits.wayOH
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
  val tagRead_s3 = RegEnable(tagRead, reqValid_s2)
  val tagAll_s3 = if (enableTagECC) {
    tagRead_s3.map(x =>
      Cat(VecInit(Seq.tabulate(tagBankSplit)(i => x(encTagBankBits * (i + 1) - 1, encTagBankBits * i)(tagBankBits - 1, 0))))
    )
  } else {
    tagRead_s3
  }
  val errorAll_s3 = if (enableTagECC) {
    tagRead_s3.map(x =>
      VecInit(Seq.tabulate(tagBankSplit)(i => x(encTagBankBits * (i + 1) - 1, encTagBankBits * i))).
        map(tag => cacheParams.dataCode.decode(tag).error).reduce(_ | _)
    )
  } else {
    VecInit(Seq.fill(ways)(false.B))
  }

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
  // val refillRetry = !(freeWayMask_s3.orR)
  val refillRetry = RegEnable(occWayMask_s2.andR, refillReqValid_s2)

  // val hitWay = OHToUInt(hitVec)
  val hitOH = hitVec.asUInt
  assert(PopCount(hitVec) <= 1.U, "Set should not have more than one hit")
  val replaceWay = WireInit(UInt(wayBits.W), 0.U)
  val replaceOH = WireInit(UInt(ways.W), 0.U)
  val (inv, invalidWay, invOH) = invalid_way_sel(metaAll_s3)
  // if chosenWay not in wayMask, then choose a way in wayMask
  // for retry bug fixing: if the chosenway cause retry last time, choose another way
  /*val finalWay = Mux(
    req_s3.wayMask(chosenWay),
    chosenWay,
    PriorityEncoder(req_s3.wayMask)
  )*/
  // for retry bug fixing: if the chosenway not in freewaymask, choose another way
  // TODO: req_s3.wayMask not take into consideration
  val finalReplOH = MuxCase(MaskToOH(freeWayMask_s3), Seq (
    inv -> invOH,
    Mux1H(replaceOH, freeWayMask_s3) -> replaceOH
  ))
  val hit_s3 = Cat(hitVec).orR || req_s3.cmoAll
  val wayOH_s3 = Mux(req_s3.cmoAll, UIntToOH(req_s3.cmoWay), Mux(hit_s3, hitOH, finalReplOH))
  val way_s3 = OHToUInt(wayOH_s3)
  val meta_s3 = Mux1H(wayOH_s3, metaAll_s3)
  val metaOnHit_s3 = Mux1H(hitOH, metaAll_s3) // only valid when hit
  val tag_s3 = Mux1H(wayOH_s3, tagAll_s3)
  val set_s3 = req_s3.set
  val replacerInfo_s3 = req_s3.replacerInfo
  val errorOnSNP_s3 = if (enableTagECC) {
    Mux1H(hitOH, errorAll_s3)
  } else {
    false.B
  }

  val error_s3 = if (enableTagECC) {
    Mux1H(wayOH_s3, errorAll_s3) && reqValid_s3 && !req_s3.cmoAll && meta_s3.state =/= MetaData.INVALID
  } else {
    false.B
  }

  io.resp.valid      := reqValid_s3
  io.resp.bits.hit   := hit_s3
  io.resp.bits.way   := way_s3
  io.resp.bits.meta  := meta_s3
  io.metaOnHit := metaOnHit_s3
  io.resp.bits.tag   := tag_s3
  io.resp.bits.set   := set_s3
  io.resp.bits.error := error_s3  // depends on ECC
  io.errOnSnp := errorOnSNP_s3
  io.resp.bits.replacerInfo := replacerInfo_s3
  io.wayOH := wayOH_s3

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

  replaceOH := (new DRRIP(ways)).get_replace_way(repl_state_s3)
  assert(PopCount(replaceOH) === 1.U, "Replacement way should be one-hot")
  replaceWay := OHToUInt(replaceOH)

  io.replResp.valid := refillReqValid_s3
  io.replResp.bits.tag := Mux1H(finalReplOH, tagAll_s3)
  io.replResp.bits.set := req_s3.set
  io.replResp.bits.way := OHToUInt(finalReplOH)
  io.replResp.bits.meta := Mux1H(finalReplOH, metaAll_s3)
  io.replResp.bits.mshrId := req_s3.mshrId
  io.replResp.bits.retry := refillRetry
  io.replResp.bits.validHold := refillReqValid_hold_s3
  io.replWayOH := finalReplOH

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
    Some(Module(new SRAMTemplate(Bool(), sets, ways, singlePort = true, shouldReset = true, hasMbist = mbist, hasSramCtl = hasSramCtl)))
  val origin_bits_r = origin_bit_opt.get.io.r(io.read.fire, io.read.bits.set).resp.data
  val origin_bits_hold = Wire(Vec(ways, Bool()))
  origin_bits_hold := HoldUnless(origin_bits_r, RegNext(io.read.fire, false.B))
  origin_bit_opt.get.io.w(replacerWen, hit_s3, req_s3.set, wayOH_s3)
  val rrip_req_type = WireInit(0.U(4.W))
  // [3]: 0-firstuse, 1-reuse;
  // [2]: 0-acquire, 1-release;
  // [1]: 0-non-prefetch, 1-prefetch;
  // [0]: 0-not-refill, 1-refill
  rrip_req_type := Cat(Mux1H(hitOH, origin_bits_hold),
    req_s3.replacerInfo.channel(2),
    (!refillReqValid_s3 && req_s3.replacerInfo.channel(0) && req_s3.replacerInfo.opcode === Hint) || (req_s3.replacerInfo.channel(2) && Mux1H(wayOH_s3, metaAll_s3).prefetch.getOrElse(false.B)) || (refillReqValid_s3 && req_s3.replacerInfo.refill_prefetch),
    req_s3.refill
  )
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, "L2Directory", mbist)
  if(cacheParams.replacement == "srrip"){
    val next_state_s3 = repl.get_next_state(repl_state_s3, wayOH_s3, hit_s3, inv, rrip_req_type)
    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(replacerWen, next_state_s3, set_s3, 1.U)

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

    val next_state_s3 = repl.get_next_state(repl_state_s3, wayOH_s3, hit_s3, inv, repl_type, rrip_req_type)

    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(replacerWen, next_state_s3, set_s3, 1.U)
  } else {
    val next_state_s3 = repl.get_next_state(repl_state_s3, way_s3)
    replacer_sram_opt.get.io.w(replacerWen, next_state_s3, set_s3, 1.U)
  }

  /* ====== Reset ====== */

  XSPerfAccumulate("dirRead_cnt", io.read.fire)
  XSPerfAccumulate("choose_busy_way", reqValid_s3 && !req_s3.wayMask(replaceWay))

  /* ====== ChiselDB logging for  prefetcher lifecycle ====== */
  if (cacheParams.enableMonitor && !cacheParams.FPGAPlatform) {
    val defaultPfSrc = PfSource.NoWhere.id.U
    val hartId = cacheParams.hartId
    val pfReqWriteTable = ChiselDB.createTable(s"L2_Slice${p(SliceIdKey)}_Write_Prefetch_hart$hartId", new PrefetchDbEntry, basicDB = false)
    val pfReqReadTable = ChiselDB.createTable(s"L2_Slice${p(SliceIdKey)}_Read_Prefetch_hart$hartId", new PrefetchDbEntry, basicDB = false)
    val pfReqEvictTable = ChiselDB.createTable(s"L2_Slice${p(SliceIdKey)}_Evict_Prefetch_hart$hartId", new PrefetchDbEntry, basicDB = false)
    
    // Write: meta write that marks a block as prefetched
    val wmeta = io.metaWReq.bits.wmeta
    val pfReqWriteEn = io.metaWReq.valid && wmeta.prefetch.getOrElse(false.B) 
    val pfReqWrite = Wire(new PrefetchDbEntry)
    val writeHasTag = io.tagWReq.valid && (io.tagWReq.bits.set === io.metaWReq.bits.set) &&
      (io.metaWReq.bits.wayOH === io.tagWReq.bits.wayOH) // try to attach tag when tagWReq coincides with metaWReq

    pfReqWrite.isHit := false.B //useless for write req, just set it to false.B
    pfReqWrite.setIdx := io.metaWReq.bits.set // when meta write, the set idx to be written
    pfReqWrite.way := OHToUInt(io.metaWReq.bits.wayOH) // when meta write, way to be written
    pfReqWrite.tag := Mux(writeHasTag, io.tagWReq.bits.wtag, 0.U)
    pfReqWrite.isPrefetch := wmeta.prefetch.getOrElse(false.B) // write data is for a prefetched block or not
    pfReqWrite.metaSource := wmeta.prefetchSrc.getOrElse(defaultPfSrc) // source of the block that writed
    pfReqWrite.reqSource := wmeta.prefetchSrc.getOrElse(defaultPfSrc) // The source of the request that causes this meta write
    pfReqWriteTable.log(pfReqWrite, pfReqWriteEn, s"L2${hartId}_${p(SliceIdKey)}", clock, reset)

    // Read: when a read hits a prefetched block
    val pfReqReadEn = io.resp.valid && io.resp.bits.hit && io.resp.bits.meta.prefetch.getOrElse(false.B) 
    val pfReqRead = Wire(new PrefetchDbEntry)
    pfReqRead.isHit := io.resp.bits.hit // read req hit or not
    pfReqRead.setIdx := io.resp.bits.set
    pfReqRead.tag := io.resp.bits.tag // read req accsess a prefetched block with which tag
    pfReqRead.isPrefetch := io.resp.bits.meta.prefetch.getOrElse(false.B) // read req accsess a prefetched block
    pfReqRead.way := io.resp.bits.way // read req accsess a prefetched block in which way
    pfReqRead.metaSource := io.resp.bits.meta.prefetchSrc.getOrElse(defaultPfSrc) // source of the block that read req accessed
    pfReqRead.reqSource := io.resp.bits.replacerInfo.reqSource // The source of the read request (eg:bop,tp,prefetcher, etc.)
    pfReqReadTable.log(pfReqRead, pfReqReadEn, s"L2${hartId}_${p(SliceIdKey)}", clock, reset)

    // Eviction: when Directory issues a replacement for a prefetched block
    val evictBlockEn = io.replResp.valid && !io.replResp.bits.retry
    val evictBlockMeta = Mux1H(finalReplOH, metaAll_s3) // meta of the block to be evicted 
    val pfReqEvictEn = evictBlockEn && evictBlockMeta.prefetch.getOrElse(false.B)
    val pfReqEvict = Wire(new PrefetchDbEntry)

    // read request info that causes eviction
    pfReqEvict.isHit := io.resp.bits.hit // read request :read hit 
    pfReqEvict.reqSource := req_s3.replacerInfo.reqSource // The source of the read request that caused this Eviction
    
    // evict block info
    pfReqEvict.setIdx := io.replResp.bits.set //set idx of evict block 
    pfReqEvict.tag := io.replResp.bits.tag //tag of evict block 
    pfReqEvict.isPrefetch := io.replResp.bits.meta.prefetch.getOrElse(false.B)
    pfReqEvict.way := io.replResp.bits.way // way of evict block
    pfReqEvict.metaSource := io.replResp.bits.meta.prefetchSrc.getOrElse(defaultPfSrc) // source of the block that evicted
    pfReqEvictTable.log(pfReqEvict, pfReqEvictEn, s"L2${hartId}_${p(SliceIdKey)}", clock, reset)
  }
}
