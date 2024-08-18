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
import freechips.rocketchip.util.SynchronizerShiftReg
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
    name: Option[String] = None
  ) = {
    val gen = chiselTypeOf(async.mem.head)
    val out = Wire(new ChannelIO(gen))
    val sink = Module(new AsyncQueueSink(gen, params))
    if (name.isDefined) { sink.suggestName("asyncQSink_" + name.get) }
    sink.io.async <> async
    sink.io.deq.ready := true.B
    out.flitv := sink.io.deq.valid
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

  io.async.tx.req.flit <> ToAsyncBundle.channel(io.enq.tx.req, params, Some("txreq_flit"))
  io.async.tx.rsp.flit <> ToAsyncBundle.channel(io.enq.tx.rsp, params, Some("txrsp_flit"))
  io.async.tx.dat.flit <> ToAsyncBundle.channel(io.enq.tx.dat, params, Some("txdat_flit"))

  io.enq.tx.req.lcrdv <> FromAsyncBundle.bitPulse(io.async.tx.req.lcrdv, params, Some("txreq_lcrdv"))
  io.enq.tx.rsp.lcrdv <> FromAsyncBundle.bitPulse(io.async.tx.rsp.lcrdv, params, Some("txrsp_lcrdv"))
  io.enq.tx.dat.lcrdv <> FromAsyncBundle.bitPulse(io.async.tx.dat.lcrdv, params, Some("txdat_lcrdv"))

  io.enq.rx.rsp <> FromAsyncBundle.channel(io.async.rx.rsp.flit, params, Some("rxrsp_flit"))
  io.enq.rx.dat <> FromAsyncBundle.channel(io.async.rx.dat.flit, params, Some("rxdat_flit"))
  io.enq.rx.snp <> FromAsyncBundle.channel(io.async.rx.snp.flit, params, Some("rxsnp_flit"))

  io.async.rx.rsp.lcrdv <> ToAsyncBundle.bitPulse(io.enq.rx.rsp.lcrdv, params, Some("rxrsp_lcrdv"))
  io.async.rx.dat.lcrdv <> ToAsyncBundle.bitPulse(io.enq.rx.dat.lcrdv, params, Some("rxdat_lcrdv"))
  io.async.rx.snp.lcrdv <> ToAsyncBundle.bitPulse(io.enq.rx.snp.lcrdv, params, Some("rxsnp_lcrdv"))

  withClockAndReset(clock, reset) {
    //
    // Below is a typical synchronizer with two registers
    //                                       │
    //                    ┌────┐  ┌────┐     │
    //  output signal ◄───┤ \/ │◄─┤ \/ │◄────│────── input signal
    //                    │    │  │    │     │
    //                    └────┘  └────┘     │
    //                     output clock      │
    //
    io.enq.rxsactive := SynchronizerShiftReg(io.async.rxsactive, numSyncReg, Some("sync_rxsactive"))
    io.enq.tx.linkactiveack := SynchronizerShiftReg(io.async.tx.linkactiveack, numSyncReg, Some("sync_tx_linkactiveack"))
    io.enq.rx.linkactivereq := SynchronizerShiftReg(io.async.rx.linkactivereq, numSyncReg, Some("sync_rx_linkactivereq"))
    io.enq.syscoack := SynchronizerShiftReg(io.async.syscoack, numSyncReg, Some("sync_syscoack"))

    io.enq.rx.rsp.flitpend := SynchronizerShiftReg(io.async.rx.rsp.flitpend, numSyncReg, Some("sync_rx_rsp_flitpend"))
    io.enq.rx.dat.flitpend := SynchronizerShiftReg(io.async.rx.dat.flitpend, numSyncReg, Some("sync_rx_dat_flitpend"))
    io.enq.rx.snp.flitpend := SynchronizerShiftReg(io.async.rx.snp.flitpend, numSyncReg, Some("sync_rx_snp_flitpend"))

    val RESET_FINISH_MAX = 100
    val resetFinishCounter = withReset(reset.asAsyncReset)(RegInit(0.U((log2Ceil(RESET_FINISH_MAX) + 1).W)))
    when (resetFinishCounter < RESET_FINISH_MAX.U) {
      resetFinishCounter := resetFinishCounter + 1.U
    }
    io.resetFinish := resetFinishCounter >= RESET_FINISH_MAX.U
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

  io.deq.tx.req <> FromAsyncBundle.channel(io.async.tx.req.flit, params, Some("txreq_flit"))
  io.deq.tx.rsp <> FromAsyncBundle.channel(io.async.tx.rsp.flit, params, Some("txrsp_flit"))
  io.deq.tx.dat <> FromAsyncBundle.channel(io.async.tx.dat.flit, params, Some("txdat_flit"))

  io.async.tx.req.lcrdv <> ToAsyncBundle.bitPulse(io.deq.tx.req.lcrdv, params, Some("txreq_lcrdv"))
  io.async.tx.rsp.lcrdv <> ToAsyncBundle.bitPulse(io.deq.tx.rsp.lcrdv, params, Some("txrsp_lcrdv"))
  io.async.tx.dat.lcrdv <> ToAsyncBundle.bitPulse(io.deq.tx.dat.lcrdv, params, Some("txdat_lcrdv"))

  io.async.rx.rsp.flit <> ToAsyncBundle.channel(io.deq.rx.rsp, params, Some("rxrsp_flit"))
  io.async.rx.dat.flit <> ToAsyncBundle.channel(io.deq.rx.dat, params, Some("rxdat_flit"))
  io.async.rx.snp.flit <> ToAsyncBundle.channel(io.deq.rx.snp, params, Some("rxsnp_flit"))

  io.deq.rx.rsp.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.rsp.lcrdv, params, Some("rxrsp_lcrdv"))
  io.deq.rx.dat.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.dat.lcrdv, params, Some("rxdat_lcrdv"))
  io.deq.rx.snp.lcrdv <> FromAsyncBundle.bitPulse(io.async.rx.snp.lcrdv, params, Some("rxsnp_lcrdv"))

  withClockAndReset(clock, reset) {
    //
    // Below is a typical synchronizer with two registers
    //                                       │
    //                    ┌────┐  ┌────┐     │
    //  output signal ◄───┤ \/ │◄─┤ \/ │◄────│────── input signal
    //                    │    │  │    │     │
    //                    └────┘  └────┘     │
    //                     output clock      │
    //
    io.deq.txsactive := SynchronizerShiftReg(io.async.txsactive, numSyncReg, Some("sync_txsactive"))
    io.deq.rx.linkactiveack := SynchronizerShiftReg(io.async.rx.linkactiveack, numSyncReg, Some("sync_rx_linkactiveack"))
    io.deq.tx.linkactivereq := SynchronizerShiftReg(io.async.tx.linkactivereq, numSyncReg, Some("sync_tx_linkactivereq"))
    io.deq.syscoreq := SynchronizerShiftReg(io.async.syscoreq, numSyncReg, Some("sync_syscoreq"))

    io.deq.tx.req.flitpend := SynchronizerShiftReg(io.async.tx.req.flitpend, numSyncReg, Some("sync_tx_req_flitpend"))
    io.deq.tx.dat.flitpend := SynchronizerShiftReg(io.async.tx.dat.flitpend, numSyncReg, Some("sync_tx_dat_flitpend"))
    io.deq.tx.rsp.flitpend := SynchronizerShiftReg(io.async.tx.rsp.flitpend, numSyncReg, Some("sync_tx_rsp_flitpend"))

    val RESET_FINISH_MAX = 100
    val resetFinishCounter = withReset(reset.asAsyncReset)(RegInit(0.U((log2Ceil(RESET_FINISH_MAX) + 1).W)))
    when (resetFinishCounter < RESET_FINISH_MAX.U) {
      resetFinishCounter := resetFinishCounter + 1.U
    }
    io.resetFinish := resetFinishCounter >= RESET_FINISH_MAX.U
  }

  dontTouch(io)
}