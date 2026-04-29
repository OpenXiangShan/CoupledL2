/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncQueueParams, AsyncBundle, AsyncQueueSource, AsyncQueueSink}
import freechips.rocketchip.util.AsyncResetSynchronizerShiftReg
import org.chipsalliance.cde.config.Parameters
import coupledL2.L2ParamKey

class AsyncChannelIO[+T <: Data](gen: T, params: AsyncQueueParams = AsyncQueueParams()) extends Bundle {
  val flitpend = Output(Bool())
  val flit = new AsyncBundle(UInt(gen.getWidth.W), params)
  val lcrdv = Flipped(new AsyncBundle(UInt(0.W), params))
}

class AsyncDownwardsLinkIO(
  params: AsyncQueueParams = AsyncQueueParams()
)(implicit p: Parameters) extends Bundle with HasLinkSwitch {
  val req = new AsyncChannelIO(new CHIREQ, params)
  val rsp = new AsyncChannelIO(new CHIRSP, params)
  val dat = new AsyncChannelIO(new CHIDAT, params)
}

class AsyncUpwardsLinkIO(
  params: AsyncQueueParams = AsyncQueueParams()
)(implicit p: Parameters) extends Bundle with HasLinkSwitch {
  val rsp = new AsyncChannelIO(new CHIRSP, params)
  val dat = new AsyncChannelIO(new CHIDAT, params)
  val snp = new AsyncChannelIO(new CHISNP, params)
}

class AsyncPortIO(
  params: AsyncQueueParams = AsyncQueueParams()
)(implicit p: Parameters) extends Bundle with HasPortSwitch with HasSystemCoherencyInterface {
  val tx = new AsyncDownwardsLinkIO(params)
  val rx = Flipped(new AsyncUpwardsLinkIO(params))
}

class ChannelWithActive[T <: Data](gen: T) extends Bundle {
  val channel = new ChannelIO(gen)
  val active = Bool()
}

class LCrdyOut extends Bundle {
  val req = new Bundle { val rdy = Output(Bool())  }
  val dat = new Bundle { val rdy = Output(Bool())  }
  val rsp = new Bundle { val rdy = Output(Bool())  }
  val empty = Output(Bool())
}

class LCrdyIn extends Bundle {
  val req = new Bundle { val rdy = Input(Bool())  }
  val dat = new Bundle { val rdy = Input(Bool())  }
  val rsp = new Bundle { val rdy = Input(Bool())  }
  val empty = Input(Bool())
}
/*
 * This module enhances the standard async bridge by adding a front-end shadow buffer
 * to decouple local processing from asynchronous latency and provide instant credit
 * return to upstream modules
 *
 * rx: DownStream(CMN) → [Shadow Buffer (16)] → [AsyncQueueSink (4)] → [AsyncQueueSource (4)] → Upstream (L2)
 *                           ↑
 *                     Instant Credit return
 *
 * tx: UpStream(L2) → [Shadow Buffer (16)] → [AsyncQueueSource (4)] → [AsyncQueueSink (4)] → Downstream (CMN)
 *          ↑                                                               ↑
 *     CHI Credit + over Credit(4)                                       Credit manage to gen back-pressure
 *
 */
object ToAsyncBundleWithBuf {
  def channel[T <: Data](
    chn: ChannelIO[T],
    params: AsyncQueueParams = AsyncQueueParams(depth = 4),
    name: Option[String] = None,
    thresh: Int = 16
  ): (Data, Bool, Bool) = {
    /*
     1. Shadow Buffer (depth=16, flow mode for low latency)
     */
    val shadow_buffer = Module(new Queue(chiselTypeOf(chn.flit), 32, flow = true, pipe = false))
    if (name.isDefined) { shadow_buffer.suggestName("shadowBuffer_" + name.get) }
    shadow_buffer.io.enq.valid := chn.flitv
    shadow_buffer.io.enq.bits  := chn.flit
    /*
     2. For rx channel (CMN->L2), send out lcrdv right after a flit entering Shadow buffer if has space
     */
    val hasSpace = shadow_buffer.io.count <= thresh.U
    dontTouch(hasSpace)
    assert(!chn.flitv || shadow_buffer.io.enq.ready, s"${name.getOrElse("ToAsyncBundle")}: Shadow buffer overflow!")
    /*
     3. AsyncQueueSource (depth=4)
     */
    val source = Module(new AsyncQueueSource(chiselTypeOf(chn.flit), params))
    if (name.isDefined) { source.suggestName("asyncQSource_" + name.get) }
    source.io.enq <> shadow_buffer.io.deq

    val empty = shadow_buffer.io.count === 0.U && source.io.async.widx === source.io.async.ridx

    (source.io.async, hasSpace, empty)
  }

  def bitPulse(
    bit: Bool,
    params: AsyncQueueParams = AsyncQueueParams(),
    name: Option[String] = None
  ) = {
    /*
     1. Shadow Buffer (depth=16, flow mode for low latency)
     */
    class EmptyBundle extends Bundle {}
    val shadow_buffer = Module(new Queue(new EmptyBundle, 16, flow = true, pipe = false))
    if (name.isDefined) { shadow_buffer.suggestName("lcrdvShadowBuffer_" + name.get) }
    shadow_buffer.io.enq.valid := bit
    shadow_buffer.io.enq.bits  := DontCare
    /*
     2. AsyncQueueSource (depth =4)
     */
    val source = Module(new AsyncQueueSource(UInt(0.W), params))
    if (name.isDefined) { source.suggestName("asyncQBitSource_" + name.get) }
    source.io.enq.valid := shadow_buffer.io.deq.valid
    source.io.enq.bits := DontCare

    shadow_buffer.io.deq.ready := source.io.enq.ready

    source.io.async
  }

}
object ToAsyncBundle {
  def channel[T <: Data](
    chn: ChannelIO[T],
    params: AsyncQueueParams = AsyncQueueParams(),
    name: Option[String] = None
  ) = {
    val source = Module(new AsyncQueueSource(chiselTypeOf(chn.flit), params))
    if (name.isDefined) { source.suggestName("asyncQSource_" + name.get) }
    source.io.enq.valid := chn.flitv
    source.io.enq.bits := chn.flit
    source.io.async
  }

  def bitPulse(
    bit: Bool,
    params: AsyncQueueParams = AsyncQueueParams(),
    name: Option[String] = None
  ) = {
    val source = Module(new AsyncQueueSource(UInt(0.W), params))
    if (name.isDefined) { source.suggestName("asyncQBitSource_" + name.get) }
    source.io.enq.valid := bit
    source.io.enq.bits := DontCare
    source.io.async
  }
}

object FromAsyncBundleWithLCredit {
  def channel(
    async: AsyncBundle[UInt],
    params: AsyncQueueParams = AsyncQueueParams(),
    name: Option[String] = None,
    linkState: Option[LinkState] = None,  // NEW: needed for state-gated credit logic
    withPowerAck: Boolean = false
  ): Data = {
    val gen = chiselTypeOf(async.mem.head)
    val out = Wire(new ChannelIO(gen))
    val sink = Module(new AsyncQueueSink(gen, params))
    if (name.isDefined) { sink.suggestName("asyncQSink_" + name.get) }
    sink.io.async <> async

    // ---------------------------------------------------------------
    // L-Credit pool logic (mirrors Decoupled2LCredit)
    // ---------------------------------------------------------------
    val lcreditsMax = 15
    val lcreditPool = RegInit(0.U(log2Up(lcreditsMax + 1).W))

    // Resolve link state: fall back to always-RUN if not provided
    val state = linkState.map(_.state).getOrElse(LinkStates.RUN)
    val disableFlit   = state === LinkStates.STOP || state === LinkStates.ACTIVATE
    val disableLCredit = state === LinkStates.STOP

    // lcrdv comes in from the downstream receiver (input side of ChannelIO)
    val acceptLCredit = out.lcrdv && !disableLCredit

    // L-Credit return: send a null flit when deactivating and pool is non-empty
    // but only when there is nothing real to send
    val returnLCreditValid = !sink.io.deq.valid &&
                             state === LinkStates.DEACTIVATE &&
                             lcreditPool =/= 0.U

    // A real flit fires when the sink has data AND we have credit AND link is active
    val realFlitFire = sink.io.deq.valid && lcreditPool =/= 0.U && !disableFlit

    // Anything that consumes a credit
    val flitv = realFlitFire || returnLCreditValid

    // Credit pool update (same semantics as Decoupled2LCredit)
    when(acceptLCredit) {
      when(!flitv) {
        lcreditPool := lcreditPool + 1.U
        assert(lcreditPool + 1.U <= lcreditsMax.U, "L-Credit pool overflow")
      }
      // acceptLCredit && flitv => net zero, no register update needed
    }.otherwise {
      when(flitv) {
        lcreditPool := lcreditPool - 1.U
        assert(lcreditPool =/= 0.U, "L-Credit pool underflow")
      }
    }

    // Gate the async-queue dequeue on credit availability
    sink.io.deq.ready := lcreditPool =/= 0.U && !disableFlit

    // ---------------------------------------------------------------
    // Drive ChannelIO outputs (registered, matching Decoupled2LCredit)
    // ---------------------------------------------------------------
    out.flitpend := RegNext(true.B, init = false.B)   // TODO: tighten if needed
    out.flitv    := RegNext(flitv,  init = false.B)
    out.flit     := RegEnable(
                      Mux(sink.io.deq.valid,
                          sink.io.deq.bits,
                          0.U  /* LCrdReturn null flit */),
                      flitv)

    // lcrdv is an INPUT on ChannelIO at this side; leave undriven from here
    out.lcrdv    := DontCare

    // ---------------------------------------------------------------
    // Optional active flag for power-management handshake
    // ---------------------------------------------------------------
    if (withPowerAck) {
      val result = Wire(new ChannelWithActive(gen))
      result.channel <> out
      result.active := sink.io.deq.valid || lcreditPool =/= 0.U  // queue not empty OR credits outstanding
      result
    } else {
      out
    }
  }
}

object FromAsyncBundle {
  def channel(
    async: AsyncBundle[UInt],
    params: AsyncQueueParams = AsyncQueueParams(),
    name: Option[String] = None
  ) = {
    val gen = chiselTypeOf(async.mem.head)
    val out = Wire(new ChannelIO(gen))
    val sink = Module(new AsyncQueueSink(gen, params))
    if (name.isDefined) { sink.suggestName("asyncQSink_" + name.get) }
    sink.io.async <> async
    sink.io.deq.ready := true.B
    out.flitv := sink.io.deq.valid & sink.io.deq.ready
    out.flit := sink.io.deq.bits
    // flitpend and lcrdv are assigned independently
    out.flitpend := DontCare
    out.lcrdv := DontCare
    out
  }

  def bitPulse[T <: Data](
    async: AsyncBundle[T],
    params: AsyncQueueParams = AsyncQueueParams(),
    name: Option[String] = None
  ): Bool = {
    val gen = chiselTypeOf(async.mem.head)
    val sink = Module(new AsyncQueueSink(gen, params))
    if (name.isDefined) { sink.suggestName("asyncQBitSink_" + name.get) }
    sink.io.async <> async
    sink.io.deq.ready := true.B
    sink.io.deq.valid
  }
}

class CHIAsyncBridgeSource(params: AsyncQueueParams = AsyncQueueParams())(implicit p: Parameters) extends Module {

  val numSyncReg = params.sync

  val io = IO(new Bundle() {
    val enq = Flipped(new PortIO)
    val async = new AsyncPortIO(params)
    val resetFinish = Output(Bool())
    val lcrdy = new LCrdyOut
  })

  val async_tx_req = ToAsyncBundleWithBuf.channel(io.enq.tx.req, params, Some("txreq_flit"), 24)
  val async_tx_rsp = ToAsyncBundleWithBuf.channel(io.enq.tx.rsp, params, Some("txrsp_flit"), 24)
  val async_tx_dat = ToAsyncBundleWithBuf.channel(io.enq.tx.dat, params, Some("txdat_flit"), 24)

  io.lcrdy.req.rdy := async_tx_req._2
  io.lcrdy.rsp.rdy := async_tx_rsp._2
  io.lcrdy.dat.rdy := async_tx_dat._2
  io.lcrdy.empty := async_tx_req._3 && async_tx_rsp._3 && async_tx_dat._3

  io.async.tx.req.flit <> async_tx_req._1
  io.async.tx.rsp.flit <> async_tx_rsp._1
  io.async.tx.dat.flit <> async_tx_dat._1

  io.enq.tx.req.lcrdv <> FromAsyncBundle.bitPulse(io.async.tx.req.lcrdv, params, Some("txreq_lcrdv"))
  io.enq.tx.rsp.lcrdv <> FromAsyncBundle.bitPulse(io.async.tx.rsp.lcrdv, params, Some("txrsp_lcrdv"))
  io.enq.tx.dat.lcrdv <> FromAsyncBundle.bitPulse(io.async.tx.dat.lcrdv, params, Some("txdat_lcrdv"))

  io.enq.rx.rsp <> FromAsyncBundle.channel(io.async.rx.rsp.flit, params, Some("rxrsp_flit"))
  io.enq.rx.dat <> FromAsyncBundle.channel(io.async.rx.dat.flit, params, Some("rxdat_flit"))
  io.enq.rx.snp <> FromAsyncBundle.channel(io.async.rx.snp.flit, params, Some("rxsnp_flit"))

  io.async.rx.rsp.lcrdv <> ToAsyncBundle.bitPulse(io.enq.rx.rsp.lcrdv, params, Some("rxrsp_lcrdv"))
  io.async.rx.dat.lcrdv <> ToAsyncBundle.bitPulse(io.enq.rx.dat.lcrdv, params, Some("rxdat_lcrdv"))
  io.async.rx.snp.lcrdv <> ToAsyncBundle.bitPulse(io.enq.rx.snp.lcrdv, params, Some("rxsnp_lcrdv"))

  io.async.txsactive := io.enq.txsactive
  io.async.rx.linkactiveack := io.enq.rx.linkactiveack
  io.async.tx.linkactivereq := io.enq.tx.linkactivereq
  io.async.syscoreq := io.enq.syscoreq
  io.async.tx.req.flitpend := io.enq.tx.req.flitpend
  io.async.tx.dat.flitpend := io.enq.tx.dat.flitpend
  io.async.tx.rsp.flitpend := io.enq.tx.rsp.flitpend
  
  withClockAndReset(clock, reset) {
    val resetFinish = Wire(Bool())
    //
    // Below is a typical synchronizer with two registers
    //                                       │
    //                    ┌────┐  ┌────┐     │
    //  output signal ◄───┤ \/ │◄─┤ \/ │◄────│────── input signal
    //                    │    │  │    │     │
    //                    └────┘  └────┘     │
    //                     output clock      │
    //
    io.enq.rxsactive := AsyncResetSynchronizerShiftReg(io.async.rxsactive, numSyncReg, 0, Some("sync_rxsactive"))
    io.enq.tx.linkactiveack := AsyncResetSynchronizerShiftReg(io.async.tx.linkactiveack, numSyncReg, 0, Some("sync_tx_linkactiveack"))
    io.enq.rx.linkactivereq := AsyncResetSynchronizerShiftReg(io.async.rx.linkactivereq, numSyncReg, 0, Some("sync_rx_linkactivereq")) && resetFinish
    io.enq.syscoack := AsyncResetSynchronizerShiftReg(io.async.syscoack, numSyncReg, 0, Some("sync_syscoack"))

    io.enq.rx.rsp.flitpend := AsyncResetSynchronizerShiftReg(io.async.rx.rsp.flitpend, numSyncReg, 0, Some("sync_rx_rsp_flitpend"))
    io.enq.rx.dat.flitpend := AsyncResetSynchronizerShiftReg(io.async.rx.dat.flitpend, numSyncReg, 0, Some("sync_rx_dat_flitpend"))
    io.enq.rx.snp.flitpend := AsyncResetSynchronizerShiftReg(io.async.rx.snp.flitpend, numSyncReg, 0, Some("sync_rx_snp_flitpend"))

    val RESET_FINISH_MAX = 100
    val resetFinishCounter = withReset(reset.asAsyncReset)(RegInit(0.U((log2Ceil(RESET_FINISH_MAX) + 1).W)))
    when (resetFinishCounter < RESET_FINISH_MAX.U) {
      resetFinishCounter := resetFinishCounter + 1.U
    }
    resetFinish := resetFinishCounter >= RESET_FINISH_MAX.U
    io.resetFinish := resetFinish
  }

  dontTouch(io)
}

class CHIAsyncBridgeSink(params: AsyncQueueParams = AsyncQueueParams())(implicit p: Parameters) extends Module {

  val numSyncReg = params.sync

  val io = IO(new Bundle() {
    val async = Flipped(new AsyncPortIO(params))
    val deq = new PortIO
    val resetFinish = Output(Bool())
    val powerAck = new Bundle {
      val QACTIVE = Output(Bool())
      val QACCEPTn = Output(Bool())
      val QREQ = Input(Bool())
    }
  })
//  val txSourceReady = cacheParams.txSourceReady

  val txState = RegInit(LinkStates.STOP)
  val rxState = RegInit(LinkStates.STOP)

  val txreq = FromAsyncBundleWithLCredit.channel(io.async.tx.req.flit, params, Some("txreq_flit"), Some(LinkState(txState)), true).asInstanceOf[ChannelWithActive[UInt]]
  val txrsp = FromAsyncBundleWithLCredit.channel(io.async.tx.rsp.flit, params, Some("txrsp_flit"), Some(LinkState(txState)), true).asInstanceOf[ChannelWithActive[UInt]]
  val txdat = FromAsyncBundleWithLCredit.channel(io.async.tx.dat.flit, params, Some("txdat_flit"), Some(LinkState(txState)), true).asInstanceOf[ChannelWithActive[UInt]]

  io.deq.tx.req <> txreq.channel
  io.deq.tx.rsp <> txrsp.channel
  io.deq.tx.dat <> txdat.channel

  // Add handshake to confirm Sink Tx Queue is completely drained
  val txActive = txreq.active || txrsp.active || txdat.active
  io.powerAck.QACTIVE := txActive
  io.powerAck.QACCEPTn := !(io.powerAck.QREQ && !txActive && txState === LinkStates.STOP)

  io.async.tx.req.lcrdv <> ToAsyncBundleWithBuf.bitPulse(io.deq.tx.req.lcrdv, params, Some("txreq_lcrdv"))
  io.async.tx.rsp.lcrdv <> ToAsyncBundleWithBuf.bitPulse(io.deq.tx.rsp.lcrdv, params, Some("txrsp_lcrdv"))
  io.async.tx.dat.lcrdv <> ToAsyncBundleWithBuf.bitPulse(io.deq.tx.dat.lcrdv, params, Some("txdat_lcrdv"))

  val async_rx_rsp = ToAsyncBundleWithBuf.channel(io.deq.rx.rsp, params, Some("rxrsp_flit"), 16)
  val async_rx_dat = ToAsyncBundleWithBuf.channel(io.deq.rx.dat, params, Some("rxdat_flit"), 16)
  io.async.rx.rsp.flit <> async_rx_rsp._1
  io.async.rx.dat.flit <> async_rx_dat._1
  io.async.rx.snp.flit <> ToAsyncBundle.channel(io.deq.rx.snp, params, Some("rxsnp_flit"))

  io.deq.rx.rsp.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.rsp.lcrdv, params, Some("rxrsp_lcrdv"))
  io.deq.rx.dat.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.dat.lcrdv, params, Some("rxdat_lcrdv"))
  io.deq.rx.snp.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.snp.lcrdv, params, Some("rxsnp_lcrdv"))

  io.async.rxsactive := io.deq.rxsactive
  io.async.tx.linkactiveack := io.deq.tx.linkactiveack
  io.async.rx.linkactivereq := io.deq.rx.linkactivereq
  io.async.syscoack := io.deq.syscoack
  io.async.rx.snp.flitpend := io.deq.rx.snp.flitpend
  io.async.rx.dat.flitpend := io.deq.rx.dat.flitpend
  io.async.rx.rsp.flitpend := io.deq.rx.rsp.flitpend

  withClockAndReset(clock, reset) {
    val resetFinish = Wire(Bool())
    //
    // Below is a typical synchronizer with two registers
    //                                       │
    //                    ┌────┐  ┌────┐     │
    //  output signal ◄───┤ \/ │◄─┤ \/ │◄────│────── input signal
    //                    │    │  │    │     │
    //                    └────┘  └────┘     │
    //                     output clock      │
    //
    io.deq.txsactive := AsyncResetSynchronizerShiftReg(io.async.txsactive, numSyncReg, 0, Some("sync_txsactive"))
    io.deq.rx.linkactiveack := AsyncResetSynchronizerShiftReg(io.async.rx.linkactiveack, numSyncReg, 0, Some("sync_rx_linkactiveack")) && resetFinish
    io.deq.tx.linkactivereq := AsyncResetSynchronizerShiftReg(io.async.tx.linkactivereq, numSyncReg, 0, Some("sync_tx_linkactivereq")) && resetFinish
    io.deq.syscoreq := AsyncResetSynchronizerShiftReg(io.async.syscoreq, numSyncReg, 0, Some("sync_syscoreq"))

    io.deq.tx.req.flitpend := AsyncResetSynchronizerShiftReg(io.async.tx.req.flitpend, numSyncReg, 0, Some("sync_tx_req_flitpend"))
    io.deq.tx.dat.flitpend := AsyncResetSynchronizerShiftReg(io.async.tx.dat.flitpend, numSyncReg, 0, Some("sync_tx_dat_flitpend"))
    io.deq.tx.rsp.flitpend := AsyncResetSynchronizerShiftReg(io.async.tx.rsp.flitpend, numSyncReg, 0, Some("sync_tx_rsp_flitpend"))

    val RESET_FINISH_MAX = 100
    val resetFinishCounter = withReset(reset.asAsyncReset)(RegInit(0.U((log2Ceil(RESET_FINISH_MAX) + 1).W)))
    when (resetFinishCounter < RESET_FINISH_MAX.U) {
      resetFinishCounter := resetFinishCounter + 1.U
    }
    resetFinish := resetFinishCounter >= RESET_FINISH_MAX.U
    io.resetFinish := resetFinish
  }
  /*
   Duplicate Link Monitor tx/rx state FSM by using deq.rx deq.tx active signals which outuput to DownStream CHI
   */
  Seq(txState, rxState).zip(MixedVecInit(Seq(io.deq.tx, io.deq.rx))).foreach { case (state, link) =>
    state := MuxLookup(Cat(link.linkactivereq, link.linkactiveack), LinkStates.STOP)(Seq(
      Cat(true.B, false.B) -> LinkStates.ACTIVATE,
      Cat(true.B, true.B) -> LinkStates.RUN,
      Cat(false.B, true.B) -> LinkStates.DEACTIVATE,
      Cat(false.B, false.B) -> LinkStates.STOP
    ))
  }

  /*
   For rx channel, add l-credit manager module to generate lcrdv inside bridge
   a. Try to use io.deq.rx as LCredit i/f entering LCredit2Decoupled to generate lcrdv of io.deq.rx.dat and io.deq.rx.rsp
   b. Try to generate instant credit return if Shadow buffer has Space(async_rx_rsp._2 indicate space > 16)
   c. rxsnp is not in this practice and still use lcrdv generated in CoupledL2 since snoop may be unpredictablely blocked
   */
  val rxrspDeact, rxdatDeact = Wire(Bool())
  val rxin = WireInit(0.U asTypeOf(Flipped(new DecoupledPortIO()))) //fake Decoupled IO to provide ready
  rxin.rx.rsp.ready := async_rx_rsp._2
  rxin.rx.dat.ready := async_rx_dat._2
  LCredit2Decoupled(io.deq.rx.rsp, rxin.rx.rsp, LinkState(rxState), rxrspDeact, Some("rxrsp"), 15, false)
  LCredit2Decoupled(io.deq.rx.dat, rxin.rx.dat, LinkState(rxState), rxdatDeact, Some("rxdat"), 15, false)

  dontTouch(io)
}
