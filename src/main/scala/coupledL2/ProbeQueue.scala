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
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config.Parameters

class ProbeQueue(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val sinkB = Flipped(DecoupledIO(new TLBundleB(edgeIn.bundle)))
    val arb_busy_s0 = Input(Bool())
    val prb_bits_s0 = Valid(new TLBundleB(edgeIn.bundle))
  })

  val prbqSize = 5

  val prb_bits_init    = 0.U.asTypeOf(new TLBundleB(edgeIn.bundle))
  val prbq_valid_reg   = RegInit(VecInit(Seq.fill(prbqSize)(false.B)))
  val prbq_bits_reg    = RegInit(VecInit(Seq.fill(prbqSize)(prb_bits_init)))
  val prb_valid_s0_reg = RegInit(false.B)
  val prb_bits_s0_reg  = RegInit(prb_bits_init)

  val prbq_older_arr   = RegInit(VecInit(Seq.fill(prbqSize)(0.U(prbqSize.W))))

  val prbq_in_sel   = Wire(Vec(prbqSize, Bool()))
  val prbq_out_sel  = Wire(Vec(prbqSize, Bool()))
  val prbq_alloc    = Wire(Vec(prbqSize, Bool()))
  val prbq_free     = Wire(Vec(prbqSize, Bool()))

  for (i <- 0 until prbqSize) {
    //**********************************
    // alloc a entry of probe queue
    if (i == 0) {
      prbq_in_sel(i) := ~prbq_valid_reg(i)
    } else {
      prbq_in_sel(i) := ~prbq_valid_reg(i) & prbq_valid_reg.asUInt()(i - 1, 0).andR
    }
    prbq_alloc(i) := io.sinkB.valid & prbq_in_sel(i)

    // free a entry of probe queue
    prbq_out_sel(i) := ~(prbq_valid_reg.asUInt() & prbq_older_arr(i)).orR
    prbq_free(i)    := prbq_valid_reg(i) & prbq_out_sel(i) & (~io.arb_busy_s0 | ~prb_valid_s0_reg)

    when(prbq_alloc(i)) {
      prbq_valid_reg(i) := io.sinkB.valid
      prbq_bits_reg(i)  := io.sinkB.bits
    }
    .elsewhen(prbq_free(i)) {
      prbq_valid_reg(i) := false.B
    }

    // update the older array by LRU Algorithm
    when(prbq_alloc(i)) {
      prbq_older_arr(i) := ~prbq_alloc.asUInt()
    }
    .elsewhen(prbq_alloc.asUInt.orR) {
      prbq_older_arr(i) := prbq_older_arr(i) & ~prbq_alloc.asUInt()
    }
  }

  val prb_valid_s0_en = (~prb_valid_s0_reg & prbq_valid_reg.asUInt.orR) |
                        (prb_valid_s0_reg & ~prbq_valid_reg.asUInt.orR & ~io.arb_busy_s0)
  val prb_valid_s0_in = ~prb_valid_s0_reg & prbq_valid_reg.asUInt.orR
  when(prb_valid_s0_en) {
    prb_valid_s0_reg := prb_valid_s0_in
  }

  val prb_bits_s0_en = (prbq_valid_reg.asUInt.orR & ~prb_valid_s0_reg) |
                       (prbq_valid_reg.asUInt.orR & ~io.arb_busy_s0)
  val prbBitsWidth   = prb_bits_s0_reg.asUInt.getWidth
  val prb_bits_s0_in = prbq_bits_reg.zipWithIndex.map { case (x, i) => x.asUInt & Fill(prbBitsWidth, prbq_free(i)) }.reduce((x, y) => x | y)
  when(prb_bits_s0_en) {
    prb_bits_s0_reg := prb_bits_s0_in.asTypeOf(prb_bits_s0_reg)
  }

  io.sinkB.ready := ~prbq_valid_reg.asUInt.andR

  io.prb_bits_s0.valid := prb_valid_s0_reg
  io.prb_bits_s0.bits  := prb_bits_s0_reg
}