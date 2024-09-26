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

class DownwardsNoSnpLinkIO(implicit p: Parameters) extends Bundle with HasLinkSwitch {
  val req = ChannelIO(new CHIREQ)
  val dat = ChannelIO(new CHIDAT)
}

class UpwardsNoSnpLinkIO(implicit p: Parameters) extends Bundle with HasLinkSwitch {
  val rsp = ChannelIO(new CHIRSP)
  val dat = ChannelIO(new CHIDAT)
}

class NoSnpPortIO(implicit p: Parameters) extends Bundle with HasPortSwitch {
  val tx = new DownwardsNoSnpLinkIO
  val rx = Flipped(new UpwardsNoSnpLinkIO)

  def connect(e: PortIO): Unit = {
    e.tx.req <> this.tx.req
    e.tx.dat <> this.tx.dat
    e.tx.rsp <> DontCare
    e.tx.linkactivereq <> this.tx.linkactivereq
    e.rx.snp <> DontCare
    e.rx.linkactiveack <> this.rx.linkactiveack
    e.txsactive <> this.txsactive
    e.syscoreq <> DontCare
    e.syscoack <> DontCare

    this.tx.linkactiveack <> e.tx.linkactiveack
    this.rx.rsp <> e.rx.rsp
    this.rx.dat <> e.rx.dat
    this.rx.linkactivereq <> e.rx.linkactivereq
    this.rxsactive <> e.rxsactive
  }
}

class UpwardsLinkMonitor(implicit p: Parameters) extends LLCModule {
  private val maxLCreditNum = 15

  val io = IO(new Bundle() {
    val in = new DecoupledPortIO()
    val out = Flipped(new PortIO)
    val entranceID = Input(UInt(NODEID_WIDTH.W))
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
  LCredit2Decoupled(
    setSrcID(
      setTgtID(rxOut.req, new CHIREQ(), rxOut.req.flit.asTypeOf(new CHIREQ()).srcID),
      new CHIREQ(),
      io.entranceID
    ),
    rxIn.req, LinkState(rxState), rxreqDeact, Some("rxreq"), maxLCreditNum
  )
  LCredit2Decoupled(setSrcID(rxOut.rsp, new CHIRSP(), io.entranceID), rxIn.rsp,
    LinkState(rxState), rxrspDeact, Some("rxrsp"), maxLCreditNum)
  LCredit2Decoupled(setSrcID(rxOut.dat, new CHIRSP(), io.entranceID), rxIn.dat,
    LinkState(rxState), rxdatDeact, Some("rxdat"), maxLCreditNum)

  txsactive := true.B
  txOut.linkactivereq := !reset.asBool
  rxOut.linkactiveack := RegNext(rxOut.linkactivereq) || !rxDeact

  io.out.syscoack := RegNext(io.out.syscoreq)

  dontTouch(io.out)

  def setSrcID[T <: Bundle](in: DecoupledIO[T], srcID: UInt): DecoupledIO[T] = {
    val out = Wire(in.cloneType)
    out <> in
    out.bits.elements.filter(_._1 == "srcID").head._2 := srcID
    out
  }

  def setSrcID[T <: Bundle](in: ChannelIO[T], gen: T, srcID: UInt): ChannelIO[T] = {
    val out = Wire(in.cloneType)
    out <> in
    out.flit.asTypeOf(gen).elements.filter(_._1 == "srcID").head._2 := srcID
    out
  }

  def setTgtID[T <: Bundle](in: ChannelIO[T], gen: T, tgtID: UInt): ChannelIO[T] = {
    val out = Wire(in.cloneType)
    out <> in
    out.flit.asTypeOf(gen).elements.filter(_._1 == "tgtID").head._2 := tgtID
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

class TransmitterLinkMonitor(implicit p: Parameters) extends LLCModule with HasCHIMsgParameters {
  private val maxLCreditNum = 15
  val io = IO(new Bundle() {
    val in = Flipped(new DecoupledPortIO())
    val out = new PortIO
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
  val rxsnpDeact, rxrspDeact, rxdatDeact = Wire(Bool())
  val rxDeact = rxsnpDeact && rxrspDeact && rxdatDeact
  Decoupled2LCredit(io.in.tx.req, io.out.tx.req, LinkState(txState), Some("txreq"))
  Decoupled2LCredit(io.in.tx.rsp, io.out.tx.rsp, LinkState(txState), Some("txrsp"))
  Decoupled2LCredit(io.in.tx.dat, io.out.tx.dat, LinkState(txState), Some("txdat"))
  LCredit2Decoupled(io.out.rx.snp, io.in.rx.snp, LinkState(rxState), rxsnpDeact, Some("rxsnp"), maxLCreditNum)
  LCredit2Decoupled(io.out.rx.rsp, io.in.rx.rsp, LinkState(rxState), rxrspDeact, Some("rxrsp"), maxLCreditNum)
  LCredit2Decoupled(io.out.rx.dat, io.in.rx.dat, LinkState(rxState), rxdatDeact, Some("rxdat"), maxLCreditNum)

  io.out.txsactive := true.B
  io.out.tx.linkactivereq := !reset.asBool
  io.out.rx.linkactiveack := RegNext(io.out.rx.linkactivereq) || !rxDeact

  io.out.syscoreq := true.B
  dontTouch(io.out)
}
