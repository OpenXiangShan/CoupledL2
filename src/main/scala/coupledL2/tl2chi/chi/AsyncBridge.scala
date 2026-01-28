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

/*
 * This module enhances the standard async bridge by adding a front-end shadow buffer
 * to decouple local processing from asynchronous latency and provide instant credit
 * return to upstream modules
 *
 *   Upstream → [Shadow Buffer (16)] → [AsyncQueueSource (4)] → Async Domain → Downstream
 *                 ↑                        ↑
 *            Instant Credit return         Async Credit
 *
 * The shadow buffer allows immediate credit return to upstream for flits accepted into
 * the shadow buffer, while actual credit recovery happens asynchronously when downstream
 * processes the flits.
 */
object ToAsyncBundleWithBuf {
  def channel[T <: Data](
    chn: ChannelIO[T],
    params: AsyncQueueParams = AsyncQueueParams(depth = 4),
    name: Option[String] = None
  ): (Data, Bool) = {
    /*
     1. Shadow Buffer (depth=16, flow mode for low latency)
     */
    val shadow_buffer = Module(new Queue(chiselTypeOf(chn.flit), 16, flow = true, pipe = false))
    if (name.isDefined) { shadow_buffer.suggestName("shadowBuffer_" + name.get) }
    shadow_buffer.io.enq.valid := chn.flitv
    shadow_buffer.io.enq.bits  := chn.flit
    /*
     2. For rx channel (CMN->L2), send out lcrdv right after a flit entering Shadow buffer(enqFire) 
     */
    val enqReady = shadow_buffer.io.enq.ready
    val enqFire = enqReady & chn.flitv
    dontTouch(enqFire)
    assert(!chn.flitv || shadow_buffer.io.enq.ready, s"${name.getOrElse("ToAsyncBundle")}: Shadow buffer overflow!")
    /*
     3. AsyncQueueSource (depth=4) 
     */
    val source = Module(new AsyncQueueSource(chiselTypeOf(chn.flit), params))
    if (name.isDefined) { source.suggestName("asyncQSource_" + name.get) }
    source.io.enq <> shadow_buffer.io.deq

    (source.io.async, enqFire)
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

object FromAsyncBundle {
  def channel(
    async: AsyncBundle[UInt],
    params: AsyncQueueParams = AsyncQueueParams(),
    name: Option[String] = None,
    lcrdvReady: Option[Bool]= None
  ) = {
    val gen = chiselTypeOf(async.mem.head)
    val out = Wire(new ChannelIO(gen))
    val sink = Module(new AsyncQueueSink(gen, params))
    if (name.isDefined) { sink.suggestName("asyncQSink_" + name.get) }
    sink.io.async <> async
    sink.io.deq.ready := lcrdvReady.getOrElse(true.B)
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
  })

  val async_tx_req = ToAsyncBundleWithBuf.channel(io.enq.tx.req, params, Some("txreq_flit"))
  val async_tx_rsp = ToAsyncBundleWithBuf.channel(io.enq.tx.rsp, params, Some("txrsp_flit"))
  val async_tx_dat = ToAsyncBundleWithBuf.channel(io.enq.tx.dat, params, Some("txdat_flit"))

  io.async.tx.req.flit <> async_tx_req._1
  io.async.tx.rsp.flit <> async_tx_rsp._1
  io.async.tx.dat.flit <> async_tx_dat._1

//  io.async.tx.req.flit <> ToAsyncBundle.channel(io.enq.tx.req, params, Some("txreq_flit"))
//  io.async.tx.rsp.flit <> ToAsyncBundle.channel(io.enq.tx.rsp, params, Some("txrsp_flit"))
//  io.async.tx.dat.flit <> ToAsyncBundle.channel(io.enq.tx.dat, params, Some("txdat_flit"))

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
  })

  val txreq_lcrdvReady = Wire(Bool())
  val txrsp_lcrdvReady = Wire(Bool())
  val txdat_lcrdvReady = Wire(Bool())
  io.deq.tx.req <> FromAsyncBundle.channel(io.async.tx.req.flit, params, Some("txreq_flit"), Some(txreq_lcrdvReady))
  io.deq.tx.rsp <> FromAsyncBundle.channel(io.async.tx.rsp.flit, params, Some("txrsp_flit"), Some(txrsp_lcrdvReady))
  io.deq.tx.dat <> FromAsyncBundle.channel(io.async.tx.dat.flit, params, Some("txdat_flit"), Some(txdat_lcrdvReady))
//  io.deq.tx.req <> FromAsyncBundle.channel(io.async.tx.req.flit, params, Some("txreq_flit"))
//  io.deq.tx.rsp <> FromAsyncBundle.channel(io.async.tx.rsp.flit, params, Some("txrsp_flit"))
//  io.deq.tx.dat <> FromAsyncBundle.channel(io.async.tx.dat.flit, params, Some("txdat_flit"))

  io.async.tx.req.lcrdv <> ToAsyncBundle.bitPulse(io.deq.tx.req.lcrdv, params, Some("txreq_lcrdv"))
  io.async.tx.rsp.lcrdv <> ToAsyncBundle.bitPulse(io.deq.tx.rsp.lcrdv, params, Some("txrsp_lcrdv"))
  io.async.tx.dat.lcrdv <> ToAsyncBundle.bitPulse(io.deq.tx.dat.lcrdv, params, Some("txdat_lcrdv"))

//  io.async.rx.rsp.flit <> ToAsyncBundleWithBuf.channel(io.deq.rx.rsp, params, Some("rxrsp_flit"))
//  io.async.rx.dat.flit <> ToAsyncBundleWithBuf.channel(io.deq.rx.dat, params, Some("rxdat_flit"))
  val async_rx_rsp = ToAsyncBundleWithBuf.channel(io.deq.rx.rsp, params, Some("rxrsp_flit"))
  val async_rx_dat = ToAsyncBundleWithBuf.channel(io.deq.rx.dat, params, Some("rxdat_flit"))
  io.async.rx.rsp.flit <> async_rx_rsp._1
  io.async.rx.dat.flit <> async_rx_dat._1

  io.async.rx.snp.flit <> ToAsyncBundle.channel(io.deq.rx.snp, params, Some("rxsnp_flit"))

  io.deq.rx.rsp.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.rsp.lcrdv, params, Some("rxrsp_lcrdv"))
  io.deq.rx.dat.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.dat.lcrdv, params, Some("rxdat_lcrdv"))
  io.deq.rx.snp.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.snp.lcrdv, params, Some("rxsnp_lcrdv"))

  io.deq.rx.rsp.lcrdv := RegNext(async_rx_rsp._2, false.B)
  io.deq.rx.dat.lcrdv := RegNext(async_rx_dat._2, false.B)


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
  val txState = RegInit(LinkStates.STOP)
  val rxState = RegInit(LinkStates.STOP)

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
   a. Try to use io.deq.rx as LCredit interface to output lcrdv right after rx flit received.
   b. The maximum number of L-Credits that CoupledL2 can provide is 4.
   c. rxsnp is not in this practice and still use lcrdv generated in CoupledL2 since snoop may be unpredictablely blocked   
   */
  /*val rxrspDeact, rxdatDeact = Wire(Bool())
   val rxin = WireInit(0.U asTypeOf(Flipped(new DecoupledPortIO()))) //fake Decoupled IO to provide ready
   rxin.rx.rsp.ready := async_rx_rsp._2
   rxin.rx.dat.ready := async_rx_dat._2
   LCredit2Decoupled(io.deq.rx.rsp, rxin.rx.rsp, LinkState(rxState), rxrspDeact, Some("rxrsp"), 15, false)
   LCredit2Decoupled(io.deq.rx.dat, rxin.rx.dat, LinkState(rxState), rxdatDeact, Some("rxdat"), 15, false)
  */

  /*
   For tx channel, add l-credit manager module to generate 'ready' to block tx flit to DownStream CHI
   a. The maximum number of L-Credits in tx channel is 4 inside bridge
   b. Use L-Credits number more than 4 in CoupledL2 to cover lcrdv sync delay from DownStream CHI to CoupledL2
   */
  val txin = WireInit(0.U asTypeOf(Flipped(new DecoupledPortIO()))) //fake Decoupled IO to provide flitv
  val txout = WireInit(0.U asTypeOf(new PortIO))//fake LCredit IO to provide lcrdv 
  txout.tx.req.lcrdv := io.deq.tx.req.lcrdv
  txout.tx.rsp.lcrdv := io.deq.tx.rsp.lcrdv
  txout.tx.dat.lcrdv := io.deq.tx.dat.lcrdv

  txin.tx.req.valid := io.deq.tx.req.flitv
  txin.tx.rsp.valid := io.deq.tx.rsp.flitv
  txin.tx.dat.valid := io.deq.tx.dat.flitv

  Decoupled2LCredit(txin.tx.req, txout.tx.req, LinkState(txState), Some("txreq"))
  Decoupled2LCredit(txin.tx.rsp, txout.tx.rsp, LinkState(txState), Some("txrsp"))
  Decoupled2LCredit(txin.tx.dat, txout.tx.dat, LinkState(txState), Some("txdat"))

  txreq_lcrdvReady := txin.tx.req.ready
  txrsp_lcrdvReady := txin.tx.rsp.ready
  txdat_lcrdvReady := txin.tx.dat.ready
 
  dontTouch(io)

}
