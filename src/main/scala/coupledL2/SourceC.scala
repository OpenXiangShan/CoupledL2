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
import coupledL2.utils.XSPerfAccumulate

// // wbq receive reqs from MainPipe unconditionally, and send them out through channel C
// // NOTICE: channel C may be unable to receive
// // NOTICE: two beats of the same address should not be interrupted by another address
// class WritebackQueue(implicit p: Parameters) extends L2Module {
//   val io = IO(new Bundle() {
//     val req = Flipped(Decoupled(new TLBundleC(edgeOut.bundle)))
//     val c = Decoupled(new TLBundleC(edgeOut.bundle))
//   })

//   val wbqSize = 4

//   val wb_bits_init    = 0.U.asTypeOf(new TLBundleC(edgeOut.bundle))
//   val wbq_valid_reg   = RegInit(VecInit(Seq.fill(wbqSize)(false.B)))
//   val wbq_bits_reg    = RegInit(VecInit(Seq.fill(wbqSize)(wb_bits_init)))
//   val wb_valid_s0_reg = RegInit(false.B)
//   val wb_bits_s0_reg  = RegInit(wb_bits_init)

//   val wbq_older_reg   = RegInit(VecInit(Seq.fill(wbqSize)(0.U(wbqSize.W))))

//   val wbq_in_sel  = Wire(Vec(wbqSize, Bool()))
//   val wbq_out_sel = Wire(Vec(wbqSize, Bool()))
//   val wbq_alloc   = Wire(Vec(wbqSize, Bool()))
//   val wbq_free    = Wire(Vec(wbqSize, Bool()))

//   for (i <- 0 until wbqSize) {
//     // alloc a entry of write back queue
//     if (i == 0) {
//       wbq_in_sel(i) := ~wbq_valid_reg(i)
//     } else {
//       wbq_in_sel(i) := ~wbq_valid_reg(i) & wbq_valid_reg.asUInt(i - 1, 0).andR
//     }
//     wbq_alloc(i) := io.req.valid & wbq_in_sel(i)

//     // free a entry of write back queue
//     wbq_out_sel(i) := ~(wbq_older_reg(i) & wbq_valid_reg.asUInt).orR
//     wbq_free(i)    := wbq_valid_reg(i) & wbq_out_sel(i) & (~wb_valid_s0_reg | io.c.ready)

//     when(wbq_alloc(i)) {
//       wbq_valid_reg(i) := io.req.valid
//       wbq_bits_reg(i)  := io.req.bits
//     }
//     .elsewhen(wbq_free(i)) {
//       wbq_valid_reg(i) := false.B
//     }

//     // update write back queue older array
//     when(wbq_alloc(i)) {
//       wbq_older_reg(i) := ~wbq_alloc.asUInt
//     }
//     .elsewhen(wbq_alloc.asUInt.orR) {
//       wbq_older_reg(i) := wbq_older_reg(i) & ~wbq_alloc.asUInt
//     }
//   }

//   val wb_valid_en_s0 = (wbq_valid_reg.asUInt.orR & ~wb_valid_s0_reg) |
//                        (~wbq_valid_reg.asUInt.orR & wb_valid_s0_reg & io.c.ready)

//   val wb_valid_s0_in = wbq_valid_reg.asUInt.orR & ~wb_valid_s0_reg

//   when(wb_valid_en_s0) {
//     wb_valid_s0_reg := wb_valid_s0_in
//   }

//   val stage_en_s0 = (wbq_valid_reg.asUInt.orR & ~wb_valid_s0_reg) |
//                     (wbq_valid_reg.asUInt.orR & io.c.ready)

//   val wbqBitsWidth  = wb_bits_s0_reg.asUInt.getWidth
//   val wb_bits_s0_in = wbq_bits_reg.zipWithIndex.map{ case (x, i) => x.asUInt & Fill(wbqBitsWidth, wbq_free(i)) }.reduce((x, y) => x | y)
//   when(stage_en_s0) {
//     wb_bits_s0_reg := wb_bits_s0_in.asTypeOf(wb_bits_s0_reg)
//   }

//   io.req.ready := ~wbq_valid_reg.asUInt.andR

//   io.c.valid := wb_valid_s0_reg
//   io.c.bits  := wb_bits_s0_reg
// }

class SourceC(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new Bundle() {
      val task = new TaskBundle()
      val data = new DSBlock()
    }))
    val out = DecoupledIO(new TLBundleC(edgeOut.bundle))
    val resp = Output(new RespBundle)
  })

  val beat_valids = RegInit(VecInit(Seq.fill(mshrsAll) { // TODO: make sure there are enough entries
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

  def toTLBundleC(task: TaskBundle, data: UInt = 0.U) = {
    val c = Wire(new TLBundleC(edgeOut.bundle))
    c := DontCare
    c.opcode := task.opcode
    c.param := task.param
    c.size := offsetBits.U
    c.source := task.mshrId
    c.address := Cat(task.tag, task.set, task.off)
    c.data := data
    c.corrupt := false.B
    c.echo.lift(DirtyKey).foreach(_ := task.opcode(0))
    c
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
      out.bits := toTLBundleC(tasks(i), beat)
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

  val (first, last, done, count) = edgeOut.count(io.out)
  val isRelease = io.out.bits.opcode === TLMessages.Release
  val isReleaseData = io.out.bits.opcode === TLMessages.ReleaseData
  io.resp.valid := io.out.fire() && first && (isRelease || isReleaseData)
  io.resp.mshrId := io.out.bits.source
  io.resp.set := parseFullAddress(io.out.bits.address)._2
  io.resp.tag := parseFullAddress(io.out.bits.address)._1
  io.resp.respInfo := DontCare

  XSPerfAccumulate(cacheParams, "sourceC_full", full)
}