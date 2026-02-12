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

trait HasSystemCoherencyInterface { this: Bundle =>
  val syscoreq = Output(Bool())
  val syscoack = Input(Bool())
}

class DownwardsLinkIO(implicit p: Parameters) extends Bundle with HasLinkSwitch {
  val req = ChannelIO(new CHIREQ)
  val rsp = ChannelIO(new CHIRSP)
  val dat = ChannelIO(new CHIDAT)
}

class UpwardsLinkIO(implicit p: Parameters) extends Bundle with HasLinkSwitch {
  val rsp = ChannelIO(new CHIRSP)
  val dat = ChannelIO(new CHIDAT)
  val snp = ChannelIO(new CHISNP)
}

class DecoupledDownwardsLinkIO(implicit p: Parameters) extends Bundle {
  val req = DecoupledIO(new CHIREQ)
  val rsp = DecoupledIO(new CHIRSP)
  val dat = DecoupledIO(new CHIDAT)
}

class DecoupledUpwardsLinkIO(implicit p: Parameters) extends Bundle {
  val rsp = DecoupledIO(new CHIRSP)
  val dat = DecoupledIO(new CHIDAT)
  val snp = DecoupledIO(new CHISNP)
}

class DecoupledDownwardsNoSnpLinkIO(implicit p: Parameters) extends Bundle {
  val req = DecoupledIO(new CHIREQ)
  val dat = DecoupledIO(new CHIDAT)
}

class DecoupledUpwardsNoSnpLinkIO(implicit p: Parameters) extends Bundle {
  val rsp = DecoupledIO(new CHIRSP)
  val dat = DecoupledIO(new CHIDAT)
}

class PortIO(implicit p: Parameters) extends Bundle
  with HasPortSwitch
  with HasSystemCoherencyInterface {
  val tx = new DownwardsLinkIO
  val rx = Flipped(new UpwardsLinkIO)
}

class DecoupledPortIO(implicit p: Parameters) extends Bundle {
  val tx = new DecoupledDownwardsLinkIO
  val rx = Flipped(new DecoupledUpwardsLinkIO)
}

class DecoupledNoSnpPortIO(implicit p: Parameters) extends Bundle {
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
  lcreditNum: Int = 4, // the number of L-Credits that a receiver can provide
  blocking: Boolean = true
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(ChannelIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
    val state = Input(new LinkState())
    val reclaimLCredit = Output(Bool())
  })

  require(lcreditNum <= 15)

  val state = io.state.state
  val enableLCredit = state === LinkStates.RUN

  val lcreditsWidth = log2Up(lcreditNum) + 1
  val lcreditInflight = RegInit(0.U(lcreditsWidth.W))
  val lcreditPool = RegInit(lcreditNum.U(lcreditsWidth.W))
  val lcreditOut = Wire(Bool())
  assert(lcreditInflight + lcreditPool === lcreditNum.U)

  val ready = lcreditInflight =/= 0.U
  val accept = ready && io.in.flitv

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

  io.in.lcrdv := RegNext(lcreditOut, init = false.B)
  io.reclaimLCredit := lcreditInflight === 0.U

  /**
    * performance counters
    */
  XSPerfHistogram("lcrd_inflight", lcreditInflight, true.B, 0, lcreditNum + 1)
  XSPerfAccumulate("accept", accept)

  if (blocking) {
    val queue = Module(new Queue(gen.cloneType, entries = lcreditNum, pipe = true, flow = false))

    lcreditOut := (lcreditPool > queue.io.count) && enableLCredit

    queue.io.enq.valid := accept
    // queue.io.enq.bits := io.in.bits
    var lsb = 0
    queue.io.enq.bits.getElements.reverse.foreach { case e =>
      val elementWidth = e.asUInt.getWidth
      if (elementWidth > 0) {
        e := io.in.flit(lsb + elementWidth - 1, lsb).asTypeOf(e.cloneType)
        lsb += elementWidth
      }
    }

    assert(!accept || queue.io.enq.ready)

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

    QueuePerf(size = lcreditNum, utilization = queue.io.count, full = queue.io.count === lcreditNum.U)
  } else {
    val lcreditReturn = WireInit(false.B)
    lcreditOut := (lcreditPool > 0.U) && enableLCredit && io.out.ready

    //assert(!accept || io.out.ready) /*reuse this module and considering upstream may not always ready to accept*/

    io.out.valid := accept && !lcreditReturn
    var lsb = 0
    io.out.bits.getElements.reverse.foreach { case e =>
      val elementWidth = e.asUInt.getWidth
      if (elementWidth > 0) {
        e := io.in.flit(lsb + elementWidth - 1, lsb).asTypeOf(e.cloneType)
        lsb += elementWidth
      }
    }
    val opcodeElements = io.out.bits.elements.filter(_._1 == "opcode")
    require (opcodeElements.size == 1)
    for ((_, opcode) <- opcodeElements) {
      when (opcode === 0.U) {
        // This is a *LCrdReturn flit
        lcreditReturn := true.B
      }
    }
  }
}

object LCredit2Decoupled {
  val defaultLCreditNum = 4

  def apply[T <: Bundle](
    left: ChannelIO[T],
    right: DecoupledIO[T],
    state: LinkState,
    reclaimLCredit: Bool,
    suggestName: Option[String] = None,
    lcreditNum: Int = defaultLCreditNum,
    blocking: Boolean = true
  )(implicit p: Parameters): Unit = {
    val mod = Module(new LCredit2Decoupled(right.bits.cloneType, lcreditNum, blocking))
    suggestName.foreach(name => mod.suggestName(s"LCredit2Decoupled_${name}"))

    mod.io.in <> left
    right <> mod.io.out
    mod.io.state := state
    reclaimLCredit := mod.io.reclaimLCredit
  }
}

class Decoupled2LCredit[T <: Bundle](
  gen: T,
  overlcreditNum: Option[Int] = None,
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(gen.cloneType))
    val out = ChannelIO(gen.cloneType)
    val state = Input(new LinkState())
  })

  val out = Wire(io.out.cloneType)

  val state = io.state.state
  val disableFlit = state === LinkStates.STOP || state === LinkStates.ACTIVATE
  val disableLCredit = state === LinkStates.STOP
  val acceptLCredit = out.lcrdv && !disableLCredit

  // The maximum number of L-Credits that a receiver can provide is 15.
  val lcreditsMax = 15
  val overlcreditVal = overlcreditNum.getOrElse(0)
  val lcreditsMaxAll = lcreditsMax + overlcreditVal
  val lcreditPool = RegInit(overlcreditVal.U(log2Up(lcreditsMaxAll+1).W))

  val returnLCreditValid = !io.in.valid && state === LinkStates.DEACTIVATE && lcreditPool =/= overlcreditVal.U
  val flitv = io.in.fire || returnLCreditValid

  when (acceptLCredit) {
    when (!flitv) {
      lcreditPool := lcreditPool + 1.U
      assert(lcreditPool < lcreditsMaxAll.U, "L-Credit pool overflow")
    }
  }.otherwise {
    when (flitv) {
      lcreditPool := lcreditPool - 1.U
    }
  }

  io.in.ready := lcreditPool =/= 0.U && !disableFlit

  io.out <> out
  out.flitpend := RegNext(true.B, init = false.B) // TODO
  out.flitv := RegNext(flitv, init = false.B)
  out.flit := RegEnable(Mux(io.in.valid, Cat(io.in.bits.getElements.map(_.asUInt)), 0.U /* LCrdReturn */), flitv)

  /**
    * performance counters
    */
  XSPerfAccumulate("lcrd_received", acceptLCredit)
}

object Decoupled2LCredit {
  def apply[T <: Bundle](
    left: DecoupledIO[T],
    right: ChannelIO[T],
    state: LinkState,
    suggestName: Option[String] = None,
    overlcreditNum: Option[Int] = None
  )(implicit p: Parameters): Unit = {
    val mod = Module(new Decoupled2LCredit(left.bits.cloneType, overlcreditNum))
    suggestName.foreach(name => mod.suggestName(s"Decoupled2LCredit_${name}"))
    
    mod.io.in <> left
    right <> mod.io.out
    mod.io.state := state
  }
}

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
  Decoupled2LCredit(setSrcID(io.in.tx.req, io.nodeID), io.out.tx.req, LinkState(txState), Some("txreq"), Some(8))
  Decoupled2LCredit(setSrcID(io.in.tx.rsp, io.nodeID), io.out.tx.rsp, LinkState(txState), Some("txrsp"), Some(8))
  Decoupled2LCredit(setSrcID(io.in.tx.dat, io.nodeID), io.out.tx.dat, LinkState(txState), Some("txdat"), Some(8))
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
