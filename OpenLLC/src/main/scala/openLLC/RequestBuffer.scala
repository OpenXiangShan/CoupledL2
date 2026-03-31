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
import coupledL2.tl2chi.HasCHIOpcodes
import utility.{FastArbiter}

class RequestBuffer(entries: Int = 8)(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new Task()))
    val out = DecoupledIO(new Task())
  })

  /* Data Structure */
  val buffer = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(Valid(new Task())))))
  val issueArb = Module(new FastArbiter(new Task(), entries))

  val in   = io.in
  val out  = io.out
  val full = Cat(buffer.map(_.valid)).andR

  /* Alloc */
  in.ready := !full

  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val alloc = in.fire
  when(alloc) {
    val entry = buffer(insertIdx)
    entry.valid := true.B
    entry.bits := in.bits
  }

  /* Issue */
  issueArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid
    in.bits  := e.bits
  }
  issueArb.io.out.ready := true.B

  out.valid := issueArb.io.out.valid
  out.bits := issueArb.io.out.bits

  /* Dealloc */
  when(out.fire) {
    val entry = buffer(issueArb.io.chosen)
    entry.valid := false.B
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(entries)(0.U(16.W))))
    buffer.zip(bufferTimer).zipWithIndex.map { case ((e, t), i) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < timeoutThreshold.U, "ReqBuf Leak(id: %d)", i.U)
    }
  }

}
