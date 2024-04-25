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
import coupledL2.L2Module

class ChannelIO[+T <: Data](gen: T) extends Bundle {
  // Flit Pending. Early indication that a flit might be transmitted in the following cycle
  val flitpend = Output(Bool()) // To be confirmed: can this be ignored?
  // Flit Valid. The transmitter sets the signal HIGH to indicate when FLIT[(W-1):0] is valid.
  val flitv = Output(Bool())
  // Flit.
  val flit = Output(UInt(gen.getWidth.W))
  // L-Credit Valid. The receiver sets this signal HIGH to return a channel L-Credit to a transmitter.
  val lcrdv = Input(Bool())
}

object ChannelIO {
  def apply[T <: Data](gen: T): ChannelIO[T] = new ChannelIO(gen)

  private final class EmptyBundle extends Bundle
  def apply(): ChannelIO[Data] = apply(new EmptyBundle)
}

trait HasLinkSwitch { this: Bundle =>
  val linkactivereq = Output(Bool())
  val linkactiveack = Input(Bool())
}

trait HasPortSwitch { this: Bundle =>
  val txsactive = Output(Bool())
  val rxsactive = Input(Bool())
}

class DownwardsLinkIO extends Bundle with HasLinkSwitch {
  val req = ChannelIO(new CHIREQ)
  val rsp = ChannelIO(new CHIRSP)
  val dat = ChannelIO(new CHIDAT)
}

class UpwardsLinkIO extends Bundle with HasLinkSwitch {
  val rsp = ChannelIO(new CHIRSP)
  val dat = ChannelIO(new CHIDAT)
  val snp = ChannelIO(new CHISNP)
}

class DecoupledDownwardsLinkIO extends Bundle {
  val req = DecoupledIO(new CHIREQ)
  val rsp = DecoupledIO(new CHIRSP)
  val dat = DecoupledIO(new CHIDAT)
}

class DecoupledUpwardsLinkIO extends Bundle {
  val rsp = DecoupledIO(new CHIRSP)
  val dat = DecoupledIO(new CHIDAT)
  val snp = DecoupledIO(new CHISNP)
}

class DecoupledDownwardsNoSnpLinkIO extends Bundle {
  val req = DecoupledIO(new CHIREQ)
  val dat = DecoupledIO(new CHIDAT)
}

class DecoupledUpwardsNoSnpLinkIO extends Bundle {
  val rsp = DecoupledIO(new CHIRSP)
  val dat = DecoupledIO(new CHIDAT)
}

class PortIO extends Bundle with HasPortSwitch {
  val tx = new DownwardsLinkIO
  val rx = Flipped(new UpwardsLinkIO)
}

class DecoupledPortIO extends Bundle {
  val tx = new DecoupledDownwardsLinkIO
  val rx = Flipped(new DecoupledUpwardsLinkIO)
}

class DecoupledNoSnpPortIO extends Bundle {
  val tx = new DecoupledDownwardsNoSnpLinkIO
  val rx = Flipped(new DecoupledUpwardsNoSnpLinkIO)
}

object LinkStates {
  val width = 2

  def STOP        = 0.U(width.W)
  def ACTIVATE    = 1.U(width.W)
  def RUN         = 2.U(width.W)
  def DEACTIVATE  = 3.U(width.W)
}

class LinkState extends Bundle {
  val state = UInt(LinkStates.width.W)
}

object LinkState {
  def apply(s: UInt) = {
    val ls = Wire(new LinkState)
    ls.state := s
    ls
  }
  def onReset = LinkState(LinkStates.STOP)
}

class LCredit2Decoupled[T <: Bundle](
  gen: T,
  lcreditNum: Int = 4 // the number of L-Credits that a receiver can provide
) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(ChannelIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
    val state = Input(new LinkState())
    val reclaimLCredit = Output(Bool())
  })

  require(lcreditNum <= 15)

  val queue = Module(new Queue(gen.cloneType, entries = lcreditNum, pipe = true, flow = false))

  val state = io.state.state
  val enableLCredit = state === LinkStates.RUN

  val lcreditsWidth = log2Up(lcreditNum) + 1
  val lcreditInflight = RegInit(0.U(lcreditsWidth.W))
  val lcreditPool = RegInit(lcreditNum.U(lcreditsWidth.W))
  assert(lcreditInflight + lcreditPool === lcreditNum.U)
  val lcreditOut = (lcreditPool > queue.io.count) && enableLCredit

  val ready = lcreditInflight =/= 0.U
  val accept = ready && io.in.flitv && RegNext(io.in.flitpend)

  when (lcreditOut) {
    when (!accept) {
      lcreditInflight := lcreditInflight + 1.U
      lcreditPool := lcreditPool - 1.U
    }
  }.otherwise {
    when (accept) {
      lcreditInflight := lcreditInflight - 1.U
      lcreditPool := lcreditPool + 1.U
    }
  }

  queue.io.enq.valid := accept
  // queue.io.enq.bits := io.in.bits
  var lsb = 0
  queue.io.enq.bits.getElements.reverse.foreach { case e =>
    e := io.in.flit(lsb + e.asUInt.getWidth - 1, lsb).asTypeOf(e.cloneType)
    lsb += e.asUInt.getWidth
  }

  assert(!accept || queue.io.enq.ready)

  io.in.lcrdv := lcreditOut

  io.out <> queue.io.deq
  val opcodeElements = queue.io.deq.bits.elements.filter(_._1 == "opcode")
  require (opcodeElements.size == 1)
  for ((_, opcode) <- opcodeElements) {
    when (queue.io.deq.valid && opcode === 0.U) {
      // This is a *LCrdReturn flit
      queue.io.deq.ready := true.B
      io.out.valid := false.B
    }
  }
  io.reclaimLCredit := lcreditInflight === 0.U
}

object LCredit2Decoupled {
  val defaultLCreditNum = 4

  def apply[T <: Bundle](
    left: ChannelIO[T],
    right: DecoupledIO[T],
    state: LinkState,
    reclaimLCredit: Bool,
    suggestName: Option[String] = None,
    lcreditNum: Int = defaultLCreditNum
  ): Unit = {
    val mod = Module(new LCredit2Decoupled(right.bits.cloneType, lcreditNum))
    suggestName.foreach(name => mod.suggestName(s"LCredit2Decoupled_${name}"))

    mod.io.in <> left
    right <> mod.io.out
    mod.io.state := state
    reclaimLCredit := mod.io.reclaimLCredit
  }
}

class Decoupled2LCredit[T <: Bundle](gen: T) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(gen.cloneType))
    val out = ChannelIO(gen.cloneType)
    val state = Input(new LinkState())
  })

  val state = io.state.state
  val disableFlit = state === LinkStates.STOP || state === LinkStates.ACTIVATE
  val disableLCredit = state === LinkStates.STOP
  val acceptLCredit = io.out.lcrdv && !disableLCredit

  // The maximum number of L-Credits that a receiver can provide is 15.
  val lcreditsMax = 15
  val lcreditPool = RegInit(0.U(log2Up(lcreditsMax).W))

  val returnLCreditValid = !io.in.valid && state === LinkStates.DEACTIVATE && lcreditPool =/= 0.U

  when (acceptLCredit) {
    when (!io.out.flitv) {
      lcreditPool := lcreditPool + 1.U
      assert(lcreditPool + 1.U =/= 0.U, "L-Credit pool overflow")
    }
  }.otherwise {
    when (io.out.flitv) {
      lcreditPool := lcreditPool - 1.U
    }
  }

  io.in.ready := lcreditPool =/= 0.U && !disableFlit
  io.out.flitpend := true.B
  io.out.flitv := io.in.fire || returnLCreditValid
  io.out.flit := Mux(
    io.in.valid,
    Cat(io.in.bits.getElements.map(_.asUInt)),
    0.U // LCrdReturn
  )
}

object Decoupled2LCredit {
  def apply[T <: Bundle](
    left: DecoupledIO[T],
    right: ChannelIO[T],
    state: LinkState,
    suggestName: Option[String] = None
  ): Unit = {
    val mod = Module(new Decoupled2LCredit(left.bits.cloneType))
    suggestName.foreach(name => mod.suggestName(s"Decoupled2LCredit_${name}"))
    
    mod.io.in <> left
    right <> mod.io.out
    mod.io.state := state
  }
}

class LinkMonitor(implicit p: Parameters) extends L2Module with HasCHIMsgParameters {
  val io = IO(new Bundle() {
    val in = Flipped(new DecoupledPortIO())
    val out = new PortIO
    val nodeID = Input(UInt(NODEID_WIDTH.W))
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
  Decoupled2LCredit(setSrcID(io.in.tx.req, io.nodeID), io.out.tx.req, LinkState(txState), Some("txreq"))
  Decoupled2LCredit(setSrcID(io.in.tx.rsp, io.nodeID), io.out.tx.rsp, LinkState(txState), Some("txrsp"))
  Decoupled2LCredit(setSrcID(io.in.tx.dat, io.nodeID), io.out.tx.dat, LinkState(txState), Some("txdat"))
  LCredit2Decoupled(io.out.rx.snp, io.in.rx.snp, LinkState(rxState), rxsnpDeact, Some("rxsnp"))
  LCredit2Decoupled(io.out.rx.rsp, io.in.rx.rsp, LinkState(rxState), rxrspDeact, Some("rxrsp"))
  LCredit2Decoupled(io.out.rx.dat, io.in.rx.dat, LinkState(rxState), rxdatDeact, Some("rxdat"))

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