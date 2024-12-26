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

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters

class TXBlockBundle(implicit p: Parameters) extends TL2CHIL2Bundle {
  // val blockSinkBReqEntrance = Bool()
  val blockMSHRReqEntrance = Bool()

  def apply() = 0.U.asTypeOf(this)
}

class TXREQ(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val pipeReq = Flipped(DecoupledIO(new CHIREQ()))
    val mshrReq = Flipped(DecoupledIO(new CHIREQ()))
    val out = DecoupledIO(new CHIREQ())

    val pipeStatusVec = Flipped(Vec(5, ValidIO(new PipeStatusWithCHI)))
    val toReqArb = Output(new TXBlockBundle)

    val sliceId = Input(UInt(bankBits.W))
  })

  assert(!io.pipeReq.valid || io.pipeReq.ready, "TXREQ should always be ready for pipeline req")
  require(chiOpt.isDefined)

  // TODO: an mshrsAll-entry queue is too much, evaluate for a proper size later
  val queue = Module(new Queue(new CHIREQ, entries = mshrsAll, flow = false))

  // Back pressure logic from TXREQ
  val queueCnt = queue.io.count
  // TODO: this may be imprecise, review this later
  val pipeStatus_s1_s5 = io.pipeStatusVec
  val pipeStatus_s2_s5 = pipeStatus_s1_s5.tail
  val pipeStatus_s1 = pipeStatus_s1_s5.head
  val pipeStatus_s2 = pipeStatus_s1_s5(1)
  val s2ReturnCredit = pipeStatus_s2.valid && !(pipeStatus_s2.bits.mshrTask && pipeStatus_s2.bits.toTXREQ)
  // inflightCnt equals the number of reqs on s2~s5 that may flow into TXREQ soon, plus queueCnt.
  // The calculation of inflightCnt might be imprecise and leads to false positive back pressue.
  val inflightCnt = PopCount(Cat(pipeStatus_s2_s5.map(s => s.valid && s.bits.mshrTask && s.bits.toTXREQ))) +
//    pipeStatus_s1.valid.asUInt +
    1.U - s2ReturnCredit.asUInt + // Fix Timing: always take credit and s2 return if not take
    queueCnt

  assert(inflightCnt <= mshrsAll.U, "in-flight overflow at TXREQ")

  val noSpace = inflightCnt >= mshrsAll.U

  io.toReqArb.blockMSHRReqEntrance := noSpace

  queue.io.enq.valid := io.pipeReq.valid || io.mshrReq.valid && !noSpace
  queue.io.enq.bits := Mux(io.pipeReq.valid, io.pipeReq.bits, io.mshrReq.bits)

  io.pipeReq.ready := true.B
  io.mshrReq.ready := !io.pipeReq.valid && !noSpace

  // Decoupled2LCredit(queue.io.deq, io.out)
  io.out <> queue.io.deq
  io.out.bits.tgtID := SAM(sam).lookup(io.out.bits.addr)
  io.out.bits.size := log2Ceil(blockBytes).U(SIZE_WIDTH.W) // TODO
  io.out.bits.addr := restoreAddressUInt(queue.io.deq.bits.addr, io.sliceId)
}
