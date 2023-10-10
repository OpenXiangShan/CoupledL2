
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
import org.chipsalliance.cde.config.Parameters
import utility._

class GrantStatus(implicit p: Parameters) extends L2Bundle {
  val valid  = Bool()
  val set     = UInt(setBits.W)
  val tag     = UInt(tagBits.W)
}

class ProbeEntry(implicit p: Parameters) extends L2Bundle {
  val valid = Bool()
  val rdy   = Bool()
  val waitG = UInt(sourceIdBits.W) // grantEntry probe is waiting for, sourceId as Idx
  val task  = new SourceBReq()
}

// send B reqs to upper level cache
// Attention! We stall Probes if there is same-addr Grant not received GrantAck
class SourceB(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val sourceB = DecoupledIO(new TLBundleB(edgeIn.bundle))
    val task = Flipped(DecoupledIO(new SourceBReq))
    val grantStatus = Input(Vec(grantBufInflightSize, new GrantStatus))
  })

  val dcacheSourceIdStart = edgeIn.client.clients
    .filter(_.supports.probe)
    .map(c => c.sourceId.start.U).head

  def toTLBundleB(task: SourceBReq) = {
    val b = Wire(new TLBundleB(edgeIn.bundle))
    b.opcode  := task.opcode
    b.param   := task.param
    b.size    := offsetBits.U
    b.source  := dcacheSourceIdStart
    b.address := Cat(task.tag, task.set, 0.U(offsetBits.W))
    b.mask    := Fill(beatBytes, 1.U(1.W))
    b.data    := Cat(task.alias.getOrElse(0.U), 0.U(1.W)) // this is the same as HuanCun
    b.corrupt := false.B
    b
  }

  /* ======== Data Structure ======== */
  // TODO: check XSPerf whether 4 entries is enough
  val entries = 4
  val probes  = RegInit(VecInit(
    Seq.fill(entries)(0.U.asTypeOf(new ProbeEntry))
  ))

  /* ======== Enchantment ======== */
  val full  = Cat(probes.map(_.valid)).andR

  // comparing with #sourceIdAll entries might have timing issues
  // but worry not, we can delay cycles cuz not critical
  val conflictMask = io.grantStatus.map(s =>
    s.valid && s.set === io.task.bits.set && s.tag === io.task.bits.tag
  )
  val conflict     = Cat(conflictMask).orR

  val noReadyEntry = Wire(Bool())

  /* ======== Alloc ======== */
  io.task.ready   := !full

  val insertIdx = PriorityEncoder(probes.map(!_.valid))
  val alloc     = !full && io.task.valid
  when(alloc) {
    val p = probes(insertIdx)
    p.valid := true.B
    p.rdy   := !conflict
    p.waitG := OHToUInt(conflictMask)
    p.task  := io.task.bits
    assert(PopCount(conflictMask) <= 1.U)
  }

  /* ======== Issue ======== */
  val issueArb = Module(new FastArbiter(new SourceBReq, entries))
  issueArb.io.in zip probes foreach{
    case (i, p) =>
      i.valid := p.valid && p.rdy
      i.bits  := p.task
      when(i.fire) {
        p.valid := false.B
      }
  }
  issueArb.io.out.ready := io.sourceB.ready
  noReadyEntry := !issueArb.io.out.valid

  /* ======== Update rdy ======== */
  probes foreach { p =>
    when(p.valid && !io.grantStatus(p.waitG).valid) {
      p.rdy := RegNext(true.B) // cuz GrantData has 2 beats, can move RegNext elsewhere
    }
  }

  /* ======== Output ======== */
  io.sourceB.valid := issueArb.io.out.valid
  io.sourceB.bits  := toTLBundleB(issueArb.io.out.bits)

  /* ======== Perf ======== */
  for(i <- 0 until entries){
    val update = PopCount(probes.map(_.valid)) === i.U
    XSPerfAccumulate(cacheParams, s"probe_buffer_util_$i", update)
  }
}
