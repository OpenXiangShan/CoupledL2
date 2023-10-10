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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, MemReqSource, SRAMTemplate}
import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.{ReplacementPolicy, XSPerfAccumulate}

case class TPParameters(
    tpTableEntries: Int = 16384,
    tpTableAssoc: Int = 16,
    vaddrBits: Int = 39,
    blockOffBits: Int = 6,
    tpQueueDepth: Int = 4,
    throttleCycles: Int = 4,
    replacementPolicy: String = "random",
    verbose: Boolean = true
) extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val hasPrefetchSrc:  Boolean = true
  override val inflightEntries: Int = 34
}

trait HasTPParams extends HasCoupledL2Parameters {
  val tpParams = prefetchOpt.get.asInstanceOf[TPParameters]
  val tpTableAssoc = tpParams.tpTableAssoc
  val tpTableNrSet = tpParams.tpTableEntries / tpTableAssoc
  val tpTableSetBits = log2Ceil(tpTableNrSet)
  val tpEntryMaxLen = tpParams.inflightEntries
  val tpTableReplacementPolicy = tpParams.replacementPolicy
  val verbose = tpParams.verbose
  val vaddrBits = tpParams.vaddrBits
  val blockOffBits = tpParams.blockOffBits
  val tpQueueDepth = tpParams.tpQueueDepth
  val tpThrottleCycles = tpParams.throttleCycles
  require(tpThrottleCycles > 0, "tpThrottleCycles must be greater than 0")
}

abstract class TPBundle(implicit val p: Parameters) extends Bundle with HasTPParams
abstract class TPModule(implicit val p: Parameters) extends Module with HasTPParams

class tpMetaEntry(implicit p:Parameters) extends TPBundle {
  val valid = Bool()
  val triggerTag = UInt((vaddrBits-blockOffBits-tpTableSetBits).W)
}

class tpDataEntry(implicit p:Parameters) extends TPBundle {
  val rawData = Vec(tpEntryMaxLen, UInt(fullAddressBits.W))
  val rawData_debug = Vec(tpEntryMaxLen, UInt(vaddrBits.W))
  // TODO: val compressedData = UInt(512.W)
  val mode = UInt(3.W)
}

class triggerBundle(implicit p: Parameters) extends TPBundle {
  val vaddr = UInt(vaddrBits.W)
  val paddr = UInt(fullAddressBits.W)
}

class trainBundle(implicit p: Parameters) extends TPBundle {
  val vaddr = UInt(vaddrBits.W)
  val paddr = UInt(fullAddressBits.W)
  val hit = Bool()
  val prefetched = Bool()
  val pfsource = UInt(PfSource.pfSourceBits.W)
  val metahit = Bool()
}

class sendBundle(implicit p: Parameters) extends TPBundle {
  val paddr = UInt(fullAddressBits.W)
  val vaddr = UInt(vaddrBits.W)
}

/* VIVT, Physical Data */
class TemporalPrefetch(implicit p: Parameters) extends TPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
  })

  def parseVaddr(x: UInt): (UInt, UInt) = {
    (x(x.getWidth-1, tpTableSetBits), x(tpTableSetBits-1, 0))
  }

  def recoverVaddr(x: UInt): UInt = {
    (x << 6.U).asUInt
  }

  val tpMetaTable = Module(
    new SRAMTemplate(new tpMetaEntry(), set = tpTableNrSet, way = tpTableAssoc, shouldReset = false, singlePort = true)
  )
  val tpDataTable = Module(
    new SRAMTemplate(new tpDataEntry(), set = tpTableNrSet, way = tpTableAssoc, shouldReset = false, singlePort = true)
  )
  val tpDataQueue = Module(new Queue(new tpDataEntry(), tpQueueDepth+1, pipe = false, flow = false))
  val repl = ReplacementPolicy.fromString(tpTableReplacementPolicy, tpTableAssoc)
  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((tpTableNrSet - 1).U)

  /* Stage 0: query tpMetaTable */

  val s0_valid = io.train.fire
  val trainVaddr = io.train.bits.vaddr.getOrElse(0.U)
  val (vtag, vset) = parseVaddr(trainVaddr)
  val metas = tpMetaTable.io.r(io.train.fire, vset).resp.data

  /* Stage 1: parse tpMeta to judge hit or miss, choose the victim */

  val s1_valid = RegNext(s0_valid, false.B)
  val train_s1 = RegEnable(io.train.bits, s0_valid)
  val trainVaddr_s1 = train_s1.vaddr.getOrElse(0.U)
  val (vtag_s1, vset_s1) = parseVaddr(trainVaddr_s1)

  val tagMatchVec = metas.map(_.triggerTag === vtag_s1)
  val metaValidVec = metas.map(_.valid === true.B)

  val debug_metaValidVec = VecInit(metaValidVec.map(_ && s1_valid))
  val debug_tagVec = VecInit(metas.map(x => Mux(s1_valid, x.triggerTag, 0.U)))
  dontTouch(debug_metaValidVec)
  dontTouch(debug_tagVec)

  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)
  val victimWay = repl.get_replace_way(0.U /*never mind for random*/)

  val hit_s1 = Cat(hitVec).orR
  val way_s1 = Mux(hit_s1, hitWay, victimWay)

  /* Stage 2: access tpData on meta hit, record it on meta miss */

  val s2_valid = RegNext(s1_valid, false.B)
  val hit_s2 = RegEnable(hit_s1, false.B, s1_valid)
  val way_s2 = RegEnable(way_s1, s1_valid)
  val vset_s2 = RegEnable(vset_s1, s1_valid)
  val vtag_s2 = RegEnable(vtag_s1, s1_valid)
  val train_s2 = RegEnable(train_s1, s1_valid)

  val dorecord_s2 = !hit_s2 && s2_valid

  tpDataTable.io.r.req.valid := hit_s2
  tpDataTable.io.r.req.bits.setIdx := vset_s2

  /* Stage 3: get tpData and insert it into tpDataQueue */

  val s3_valid = RegNext(s2_valid && hit_s2, false.B)
  val train_s3 = RegEnable(train_s2, s2_valid)
  val way_s3 = RegEnable(way_s2, s2_valid)
  tpDataQueue.io.enq.valid := s3_valid
  tpDataQueue.io.enq.bits := tpDataTable.io.r.resp.data(way_s3)
  val tpDataQFull = tpDataQueue.io.count === tpQueueDepth.U

  /* Recorder logic */
  // TODO: compress data based on max delta
  val recorder_idx = RegInit(0.U(6.W))
  val recorder_data = Reg(Vec(tpEntryMaxLen, UInt(fullAddressBits.W)))
  val recorder_data_debug = Reg(Vec(tpEntryMaxLen, UInt(vaddrBits.W)))
  val record_data_in = train_s2.addr
  val record_data_in_debug = train_s2.vaddr.getOrElse(0.U)
  val write_record = RegInit(false.B)
  val write_record_way = RegInit(0.U.asTypeOf(way_s2))
  val write_record_trigger = RegInit(0.U.asTypeOf(new triggerBundle()))

  when(dorecord_s2) {
    recorder_idx := recorder_idx + 1.U
    recorder_data(recorder_idx) := record_data_in
    recorder_data_debug(recorder_idx) := record_data_in_debug
    when(recorder_idx === (tpEntryMaxLen-1).U) {
      write_record := true.B
      write_record_way := way_s2  // choose way lazily
      recorder_idx := 0.U
    }
    when(recorder_idx === 0.U) {
      // set trigger as the first addr
      // TODO: separate trigger and data
      write_record_trigger.vaddr := train_s2.vaddr.getOrElse(0.U)
      write_record_trigger.paddr := train_s2.addr
    }
  }
  when(write_record) {
    write_record := false.B
  }

  val tpTable_w_valid = write_record && !tpDataTable.io.r.req.valid
  assert(RegNext(s2_valid, false.B) || !tpTable_w_valid, "tpTable_w_valid can only be true in s3")

  val (write_record_vtag, write_record_vset) = parseVaddr(write_record_trigger.vaddr)

  val tpData_w_bits = Wire(new tpDataEntry())
  tpData_w_bits.rawData.zip(recorder_data).foreach(x => x._1 := x._2)
  tpData_w_bits.rawData_debug.zip(recorder_data_debug).foreach(x => x._1 := x._2)
  tpData_w_bits.mode := 0.U

  val tpMeta_w_bits = Wire(new tpMetaEntry())
  tpMeta_w_bits.valid := true.B
  tpMeta_w_bits.triggerTag := write_record_vtag

  val tpTable_w_vset = Mux(resetFinish, write_record_vset, resetIdx)
  val tpTable_w_wayOH = Mux(resetFinish, UIntToOH(write_record_way), Fill(tpTableAssoc, true.B))

  when(!resetFinish) {
    tpMeta_w_bits.valid := false.B
    tpMeta_w_bits.triggerTag := 0.U
  }

  tpDataTable.io.w.apply(tpTable_w_valid, tpData_w_bits, tpTable_w_vset, tpTable_w_wayOH)
  tpMetaTable.io.w.apply(tpTable_w_valid || !resetFinish, tpMeta_w_bits, tpTable_w_vset, tpTable_w_wayOH)

  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }

  val tpData_rw_conflict = write_record && tpDataTable.io.r.req.valid
  // TODO: resolve r/w conflict

  /* Send prefetch request */

  val do_sending = RegInit(false.B)
  val sending_idx = RegInit(0.U(6.W))
  val sending_data = Reg(Vec(tpEntryMaxLen, UInt(fullAddressBits.W)))
  val sending_data_debug = Reg(Vec(tpEntryMaxLen, UInt(vaddrBits.W)))
  val sending_throttle = RegInit(0.U(4.W))
  val sending_valid = do_sending && !tpDataQFull && sending_throttle === tpThrottleCycles.U
  val (sendingTag, sendingSet, _) = parseFullAddress(sending_data(sending_idx))

  tpDataQueue.io.deq.ready := tpDataQFull || !do_sending
  when(tpDataQueue.io.deq.fire) {
    sending_data := tpDataQueue.io.deq.bits.rawData
    sending_data_debug := tpDataQueue.io.deq.bits.rawData_debug
    sending_idx := 1.U  // TODO: dismiss the first addr because it is the trigger
    do_sending := true.B
  }
  when(((do_sending && !tpDataQFull) || sending_throttle =/= 0.U) && (sending_throttle =/= tpThrottleCycles.U)) {
    sending_throttle := sending_throttle + 1.U
  }
  when(io.req.fire) {
    sending_idx := sending_idx + 1.U
    sending_throttle := 0.U
    if (verbose) {
      printf("[TP] sending data: %x vaddr: %x\n",
        sending_data(sending_idx),
        recoverVaddr(sending_data_debug(sending_idx))
      )
    }
    when(sending_idx === (tpEntryMaxLen-1).U) {
      do_sending := false.B
    }
  }

  io.req.valid := false.B // sending_valid
  io.req.bits.tag := sendingTag
  io.req.bits.set := sendingSet
  io.req.bits.needT := true.B
  io.req.bits.source := 0.U // TODO: ensure source 0 is dcache
  io.req.bits.pfSource := MemReqSource.Prefetch2L2TP.id.U

  io.resp.ready := true.B
  io.train.ready := resetFinish

  /* Performance collection */
  val triggerDB = ChiselDB.createTable("tptrigger", new triggerBundle(), basicDB=true)
  val triggerPt = Wire(new triggerBundle())
  triggerPt.paddr := write_record_trigger.paddr
  triggerPt.vaddr := recoverVaddr(write_record_trigger.vaddr)

  val trainDB = ChiselDB.createTable("tptrain", new trainBundle(), basicDB=true)
  val trainPt = Wire(new trainBundle())
  trainPt.vaddr := recoverVaddr(train_s2.vaddr.getOrElse(0.U))
  trainPt.paddr := train_s2.addr
  trainPt.hit := train_s2.hit
  trainPt.prefetched := train_s2.prefetched
  trainPt.pfsource := train_s2.pfsource
  trainPt.metahit := hit_s2

  val sendDB = ChiselDB.createTable("tpsend", new sendBundle(), basicDB=true)
  val sendPt = Wire(new sendBundle())
  sendPt.paddr := sending_data(sending_idx)
  sendPt.vaddr := recoverVaddr(sending_data_debug(sending_idx))

  triggerDB.log(triggerPt, tpTable_w_valid, "", clock, reset)
  trainDB.log(trainPt, s2_valid, "", clock, reset)
  sendDB.log(sendPt, io.req.fire, "", clock, reset)
}
