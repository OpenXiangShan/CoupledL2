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
import utility._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._

// Send out Grant/GrantData/ReleaseAck through d and
// receive GrantAck through e
class GrantBuffer(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val d_task = Flipped(DecoupledIO(new Bundle() {
      val task = new TaskBundle()
      val data = new DSBlock()
    }))
    val d = DecoupledIO(new TLBundleD(edgeIn.bundle))
    val e = Flipped(DecoupledIO(new TLBundleE(edgeIn.bundle)))
    val e_resp = Output(new RespBundle)

    val fromReqArb = Input(new Bundle() {
      val status_s1 = new PipeEntranceStatus
    })
    val toReqArb = Output(new BlockInfo())
  })

  val beat_valids = RegInit(VecInit(Seq.fill(mshrsAll) {
    VecInit(Seq.fill(beatSize)(false.B))
  }))
  val block_valids = VecInit(beat_valids.map(_.asUInt.orR)).asUInt
  val tasks = Reg(Vec(mshrsAll, new TaskBundle))
  val datas = Reg(Vec(mshrsAll, new DSBlock))
  val full = block_valids.andR
  val selectOH = ParallelPriorityMux(~block_valids, (0 until mshrsAll).map(i => (1 << i).U))

  // used to block Probe upwards
  val inflight_grant_set = Reg(Vec(sourceIdAll, UInt(setBits.W)))
  val inflight_grant_tag = Reg(Vec(sourceIdAll, UInt(tagBits.W)))
  val inflight_grant_valid = RegInit(VecInit(Seq.fill(sourceIdAll)(false.B)))
  val inflight_grant = inflight_grant_valid zip (inflight_grant_set zip inflight_grant_tag)

  when (io.d_task.fire() && io.d_task.bits.task.opcode(2, 1) === Grant(2, 1)) {
    val id = io.d_task.bits.task.mshrId(sourceIdBits-1, 0)
    inflight_grant_set(id) := io.d_task.bits.task.set
    inflight_grant_tag(id) := io.d_task.bits.task.tag
    inflight_grant_valid(id) := true.B
  }
  when (io.e.fire) {
    val id = io.e.bits.sink(sourceIdBits-1, 0)
    inflight_grant_valid(id) := false.B
  }

  io.toReqArb.blockA_s1 := Cat(inflight_grant.map { case (v, (set, _)) =>
    v && set === io.fromReqArb.status_s1.a_set
  }).orR
  io.toReqArb.blockB_s1 := Cat(inflight_grant.map { case (v, (set, tag)) =>
    v && set === io.fromReqArb.status_s1.b_set && tag === io.fromReqArb.status_s1.b_tag
  }).orR
  io.toReqArb.blockC_s1 := false.B

  selectOH.asBools.zipWithIndex.foreach {
    case (sel, i) =>
      when (sel && io.d_task.fire()) {
        beat_valids(i).foreach(_ := true.B)
        tasks(i) := io.d_task.bits.task
        datas(i) := io.d_task.bits.data
      }
  }

  def toTLBundleD(task: TaskBundle, data: UInt = 0.U) = {
    val d = Wire(new TLBundleD(edgeIn.bundle))
    d := DontCare
    d.opcode := task.opcode
    d.param := task.param
    d.size := offsetBits.U
    d.source := task.sourceId
    d.sink := task.mshrId
    d.denied := false.B
    d.data := data
    d.corrupt := false.B
    d
  }

  def getBeat(data: UInt, beatsOH: UInt): (UInt, UInt) = {
    // get one beat from data according to beatsOH
    require(data.getWidth == (blockBytes * 8))
    require(beatsOH.getWidth == beatSize)
    // next beat
    val next_beat = ParallelPriorityMux(beatsOH, data.asTypeOf(Vec(beatSize, UInt((beatBytes * 8).W))))
    val selOH = PriorityEncoderOH(beatsOH)
    // remaining beats that haven't been sent out
    val next_beatsOH = beatsOH & ~selOH
    (next_beat, next_beatsOH)
  }

  val out_bundles = Wire(Vec(mshrsAll, io.d.cloneType))
  out_bundles.zipWithIndex.foreach {
    case (out, i) =>
      out.valid := block_valids(i)
      val data = datas(i).data
      val beatsOH = beat_valids(i).asUInt
      val (beat, next_beatsOH) = getBeat(data, beatsOH)
      out.bits := toTLBundleD(tasks(i), beat)
      val hasData = out.bits.opcode(0)

      when (out.fire()) {
        when (hasData) {
          beat_valids(i) := VecInit(next_beatsOH.asBools)
        }.otherwise {
          beat_valids(i).foreach(_ := false.B)
        }
      }
  }

  TLArbiter.lowest(edgeIn, io.d, out_bundles:_*)

  io.d_task.ready := !full

  io.e.ready := true.B
  io.e_resp := DontCare
  io.e_resp.valid := io.e.valid
  io.e_resp.mshrId := io.e.bits.sink
  io.e_resp.respInfo.opcode := GrantAck
  io.e_resp.respInfo.last := true.B
}