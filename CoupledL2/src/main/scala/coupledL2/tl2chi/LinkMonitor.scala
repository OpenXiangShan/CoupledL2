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
import org.chipsalliance.cde.config.Parameters
import utility._
import utility.chi._
import coupledL2.L2Module
import coupledL2.HasCoupledL2Parameters

class LinkMonitor(implicit p: Parameters) extends L2Module with HasCHIOpcodes {
  val io = IO(new Bundle() {
    val in = Flipped(new DecoupledPortIO())
    val out = new PortIO
    val nodeID = Input(UInt(NODEID_WIDTH.W))
    val exitco = Option.when(cacheParams.enableL2Flush) (Input(Bool()))
  })
  // val s_stop :: s_activate :: s_run :: s_deactivate :: Nil = Enum(4)

  val txState = RegInit(LinkStates.STOP)
  val rxState = RegInit(LinkStates.STOP)

  Seq(txState, rxState).zip(MixedVecInit(Seq(io.out.tx, io.out.rx))).foreach { case (state, link) =>
    state := MuxLookup(Cat(link.linkactivereq, link.linkactiveack), LinkStates.STOP)(Seq(
      Cat(true.B, false.B) -> LinkStates.ACTIVATE,
      Cat(true.B, true.B) -> LinkStates.RUN,
      Cat(false.B, true.B) -> LinkStates.DEACTIVATE,
      Cat(false.B, false.B) -> LinkStates.STOP
    ))
  }

  /* IO assignment */
  val rxsnpDeact, rxrspDeact, rxdatDeact = Wire(Bool())
  val rxDeact = rxsnpDeact && rxrspDeact && rxdatDeact
  val linkLayerParams = p.alterPartial {
    case utility.chi.CHIEnableAsyncBridgeKey => cacheParams.enableCHIAsyncBridge.getOrElse(false)
  }
  Decoupled2LCredit(setSrcID(io.in.tx.req, io.nodeID), io.out.tx.req, LinkState(txState), Some("txreq"), Some(8))(linkLayerParams)
  Decoupled2LCredit(setSrcID(io.in.tx.rsp, io.nodeID), io.out.tx.rsp, LinkState(txState), Some("txrsp"), Some(8))(linkLayerParams)
  Decoupled2LCredit(setSrcID(io.in.tx.dat, io.nodeID), io.out.tx.dat, LinkState(txState), Some("txdat"), Some(8))(linkLayerParams)
  LCredit2Decoupled(io.out.rx.snp, io.in.rx.snp, LinkState(rxState), rxsnpDeact, Some("rxsnp"))
  LCredit2Decoupled(io.out.rx.rsp, io.in.rx.rsp, LinkState(rxState), rxrspDeact, Some("rxrsp"), 15, false)
  LCredit2Decoupled(io.out.rx.dat, io.in.rx.dat, LinkState(rxState), rxdatDeact, Some("rxdat"), 15, false)

  //exit coherecy + deactive tx/rx when l2 flush done
  val exitco = io.exitco.getOrElse(false.B)
  val exitcoDone = !io.out.syscoreq && !io.out.syscoack && RegNext(true.B, init = false.B)

  io.out.tx.linkactivereq := RegNext(!exitcoDone, init = false.B)
  io.out.rx.linkactiveack := RegNext(
    next = RegNext(io.out.rx.linkactivereq) || !rxDeact,
    init = false.B
  )

  io.out.syscoreq := RegNext(!exitco, init = false.B)
  io.out.txsactive := RegNext(!exitcoDone, init = false.B)

  val retryAckCnt = RegInit(0.U(64.W))
  val pCrdGrantCnt = RegInit(0.U(64.W))
  val noAllowRetryCnt = RegInit(0.U(64.W))

  when (io.in.rx.rsp.fire && io.in.rx.rsp.bits.opcode === RetryAck) {
    retryAckCnt := retryAckCnt + 1.U
  }
  when (io.in.rx.rsp.fire && io.in.rx.rsp.bits.opcode === PCrdGrant) {
    pCrdGrantCnt := pCrdGrantCnt + 1.U
  }
  when (io.in.tx.req.fire && !io.in.tx.req.bits.allowRetry) {
    noAllowRetryCnt := noAllowRetryCnt + 1.U
  }

  dontTouch(io.out)
  dontTouch(retryAckCnt)
  dontTouch(pCrdGrantCnt)
  dontTouch(noAllowRetryCnt)

  def setSrcID[T <: Bundle](in: DecoupledIO[T], srcID: UInt = 0.U): DecoupledIO[T] = {
    val out = Wire(in.cloneType)
    out <> in
    out.bits.elements.filter(_._1 == "srcID").head._2 := srcID
    out
  }
}
