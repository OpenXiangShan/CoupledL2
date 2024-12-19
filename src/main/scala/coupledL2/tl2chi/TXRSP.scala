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
import coupledL2.TaskBundle
import org.chipsalliance.cde.config.Parameters
import utility._

class TXRSPBlockBundle(implicit p: Parameters) extends TXBlockBundle {
  val blockSinkBReqEntrance = Bool()

  override def apply() = 0.U.asTypeOf(this)
}

class TXRSP(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    // val in = Flipped(DecoupledIO(new TaskBundle()))
    val pipeRsp = Flipped(DecoupledIO(new TaskBundle))
    val mshrRsp = Flipped(DecoupledIO(new CHIRSP()))
    val out = DecoupledIO(new CHIRSP())

    val pipeStatusVec = Flipped(Vec(5, ValidIO(new PipeStatusWithCHI)))
    val toReqArb = Output(new TXRSPBlockBundle)
  })

  assert(!io.pipeRsp.valid || io.pipeRsp.bits.toTXRSP, "txChannel is wrong for TXRSP")
  assert(io.pipeRsp.ready, "TXRSP should never be full")
  require(chiOpt.isDefined)

  // TODO: an mshrsAll-entry queue is too much, evaluate for a proper size later
  val queue = Module(new Queue(new CHIRSP, entries = mshrsAll, flow = false))

  // Back pressure logic from TXRSP
  val queueCnt = queue.io.count
  // TODO: this may be imprecise, review this later
  val pipeStatus_s1_s5 = io.pipeStatusVec
  val pipeStatus_s1_s2 = pipeStatus_s1_s5.take(2)
  val pipeStatus_s2 = pipeStatus_s1_s2.tail
  val pipeStatus_s3_s5 = pipeStatus_s1_s5.drop(2)
  // inflightCnt equals the number of reqs on s2~s5 that may flow into TXRSP soon, plus queueCnt.
  // The calculation of inflightCnt might be imprecise and leads to false positive back pressue.
  val inflightCnt =
    PopCount(Cat(pipeStatus_s3_s5.map(s => s.valid && s.bits.toTXRSP && (s.bits.fromB || s.bits.mshrTask)))) +
      PopCount(Cat(pipeStatus_s2.map(s => s.valid && Mux(s.bits.mshrTask, s.bits.toTXRSP, s.bits.fromB)))) +
      queueCnt

  assert(inflightCnt <= mshrsAll.U, "in-flight overflow at TXRSP")

  val noSpaceForSinkBReq = inflightCnt >= mshrsAll.U
  val noSpaceForMSHRReq = inflightCnt >= (mshrsAll - 2).U

  io.toReqArb.blockSinkBReqEntrance := noSpaceForSinkBReq
  io.toReqArb.blockMSHRReqEntrance := noSpaceForMSHRReq

  io.out.valid := queue.io.deq.valid
  io.out.bits := queue.io.deq.bits
  queue.io.deq.ready := io.out.ready

  queue.io.enq.valid := io.pipeRsp.valid || io.mshrRsp.valid && !noSpaceForSinkBReq && !noSpaceForMSHRReq
  queue.io.enq.bits := Mux(io.pipeRsp.valid, toCHIRSPBundle(io.pipeRsp.bits), io.mshrRsp.bits)

  io.pipeRsp.ready := true.B
  io.mshrRsp.ready := !io.pipeRsp.valid && !noSpaceForSinkBReq && !noSpaceForMSHRReq

  def toCHIRSPBundle(task: TaskBundle): CHIRSP = {
    val rsp = WireInit(0.U.asTypeOf(new CHIRSP()))
    rsp.tgtID := task.tgtID.get
    rsp.srcID := task.srcID.get
    rsp.txnID := task.txnID.get
    rsp.dbID := task.dbID.get
    rsp.pCrdType := task.pCrdType.get
    rsp.opcode := task.chiOpcode.get
    rsp.resp := task.resp.get
    rsp.fwdState := task.fwdState.get
    rsp.traceTag := task.traceTag.get
    // TODO: Finish this
    rsp
  }
}
