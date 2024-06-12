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
import coupledL2.tl2chi._

class DownwardsNoSnpLinkIO extends Bundle with HasLinkSwitch {
  val req = ChannelIO(new CHIREQ)
  val dat = ChannelIO(new CHIDAT)
}

class UpwardsNoSnpLinkIO extends Bundle with HasLinkSwitch {
  val rsp = ChannelIO(new CHIRSP)
  val dat = ChannelIO(new CHIDAT)
}

class NoSnpPortIO extends Bundle with HasPortSwitch {
  val tx = new DownwardsNoSnpLinkIO
  val rx = Flipped(new UpwardsNoSnpLinkIO)
}

class UpwardsLinkMonitor(implicit p: Parameters) extends LLCModule {
  private val maxLCreditNum = 15

  val io = IO(new Bundle() {
    val in = new DecoupledPortIO()
    val out = Flipped(new PortIO)
    val nodeID = Input(UInt(NODEID_WIDTH.W))
  })

  val txOut = io.out.rx
  val rxOut = io.out.tx
  val txIn = io.in.rx
  val rxIn = io.in.tx
  val txsactive = io.out.rxsactive

  val txState = RegInit(LinkStates.STOP)
  val rxState = RegInit(LinkStates.STOP)

  Seq(txState, rxState).zip(MixedVecInit(Seq(txOut, rxOut))).foreach { case (state, link) =>
    state := MuxLookup(Cat(link.linkactivereq, link.linkactiveack), LinkStates.STOP)(Seq(
      Cat(true.B, false.B) -> LinkStates.ACTIVATE,
      Cat(true.B, true.B) -> LinkStates.RUN,
      Cat(false.B, true.B) -> LinkStates.DEACTIVATE,
      Cat(false.B, false.B) -> LinkStates.STOP
    ))
  }

  /* IO assignment */
  val rxreqDeact, rxrspDeact, rxdatDeact = Wire(Bool())
  val rxDeact = rxreqDeact && rxrspDeact && rxdatDeact
  Decoupled2LCredit(setSrcID(txIn.snp, io.nodeID), txOut.snp, LinkState(txState), Some("txsnp"))
  Decoupled2LCredit(setSrcID(txIn.rsp, io.nodeID), txOut.rsp, LinkState(txState), Some("txrsp"))
  Decoupled2LCredit(setSrcID(txIn.dat, io.nodeID), txOut.dat, LinkState(txState), Some("txdat"))
  LCredit2Decoupled(rxOut.req, rxIn.req, LinkState(rxState), rxreqDeact, Some("rxreq"), maxLCreditNum)
  LCredit2Decoupled(rxOut.rsp, rxIn.rsp, LinkState(rxState), rxrspDeact, Some("rxrsp"), maxLCreditNum)
  LCredit2Decoupled(rxOut.dat, rxIn.dat, LinkState(rxState), rxdatDeact, Some("rxdat"), maxLCreditNum)

  txsactive := true.B
  txOut.linkactivereq := !reset.asBool
  rxOut.linkactiveack := RegNext(rxOut.linkactivereq) || !rxDeact

  dontTouch(io.out)

  def setSrcID[T <: Bundle](in: DecoupledIO[T], srcID: UInt = 0.U): DecoupledIO[T] = {
    val out = Wire(in.cloneType)
    out <> in
    out.bits.elements.filter(_._1 == "srcID").head._2 := srcID
    out
  }
}

class DownwardsLinkMonitor(implicit p: Parameters) extends LLCModule {
  private val maxLCreditNum = 15

  val io = IO(new Bundle() {
    val in = Flipped(new DecoupledNoSnpPortIO())
    val out = new NoSnpPortIO()
    val nodeID = Input(UInt(NODEID_WIDTH.W))
  })

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
  val rxrspDeact, rxdatDeact = Wire(Bool())
  val rxDeact = rxrspDeact && rxdatDeact
  Decoupled2LCredit(setSrcID(io.in.tx.req, io.nodeID), io.out.tx.req, LinkState(txState), Some("txreq"))
  Decoupled2LCredit(setSrcID(io.in.tx.dat, io.nodeID), io.out.tx.dat, LinkState(txState), Some("txdat"))
  LCredit2Decoupled(io.out.rx.rsp, io.in.rx.rsp, LinkState(rxState), rxrspDeact, Some("rxrsp"), maxLCreditNum)
  LCredit2Decoupled(io.out.rx.dat, io.in.rx.dat, LinkState(rxState), rxdatDeact, Some("rxdat"), maxLCreditNum)

  io.out.txsactive := true.B
  io.out.tx.linkactivereq := !reset.asBool
  io.out.rx.linkactiveack := RegNext(io.out.rx.linkactivereq) || !rxDeact

  dontTouch(io.out)

  def setSrcID[T <: Bundle](in: DecoupledIO[T], srcID: UInt = 0.U): DecoupledIO[T] = {
    val out = Wire(in.cloneType)
    out <> in
    out.bits.elements.filter(_._1 == "srcID").head._2 := srcID
    out
  }
}
