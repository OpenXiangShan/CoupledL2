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
import coupledL2.utils._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._

class SourceD(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new Bundle() {
      val task = new TaskBundle()
      val data = new DSBlock()
    }))
    val out = DecoupledIO(new TLBundleD(edgeIn.bundle))
  })

  val beat_valids = RegInit(VecInit(Seq.fill(mshrsAll) {
    VecInit(Seq.fill(beatSize)(false.B))
  }))
  val block_valids = VecInit(beat_valids.map(_.asUInt.orR)).asUInt
  val tasks = Reg(Vec(mshrsAll, new TaskBundle))
  val datas = Reg(Vec(mshrsAll, new DSBlock))
  val full = block_valids.andR
  val selectOH = ParallelPriorityMux(~block_valids, (0 until mshrsAll).map(i => (1 << i).U))

  selectOH.asBools.zipWithIndex.foreach {
    case (sel, i) =>
      when (sel && io.in.fire()) {
        beat_valids(i).foreach(_ := true.B)
        tasks(i) := io.in.bits.task
        datas(i) := io.in.bits.data
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

  val out_bundles = Wire(Vec(mshrsAll, io.out.cloneType))
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

  TLArbiter.lowest(edgeIn, io.out, out_bundles:_*)

  io.in.ready := !full
}