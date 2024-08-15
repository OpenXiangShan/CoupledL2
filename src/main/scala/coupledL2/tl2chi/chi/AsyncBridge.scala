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
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters

class AsyncBridgeCHI(implicit p: Parameters) extends RawModule {
    val enq_clock = IO(Input(Clock()))
    val enq_reset = IO(Input(Reset()))
    val deq_clock = IO(Input(Clock()))
    val deq_reset = IO(Input(Reset()))
    val io = IO(new Bundle {
        // val linkCtrl_enq    = Flipped(new CHILinkCtrlIO())
        // val linkCtrl_deq    = new CHILinkCtrlIO()
        // val chi_enq         = CHIBundleUpstream(Config.chiBundleParams, true)
        // val chi_deq         = CHIBundleDownstream(Config.chiBundleParams, true)
        val chi_enq = Flipped(new PortIO)
        val chi_deq = new PortIO
        val resetFinish_enq = Output(Bool())
        val resetFinish_deq = Output(Bool())
    })

    val numSyncReg = 3

    // A helper object for creating AsyncCreditBridge
    object AsyncConnect {
        //
        // case class AsyncQueueParams: (default parameters)
        //      depth: Int      = 8
        //      sync: Int       = 3
        //      safe: Boolean   = true    [[If safe is true, then effort is made to resynchronize the crossing indices when either side is reset.
        //                                  This makes it safe/possible to reset one side of the crossing (but not the other) when the queue is empty.]]
        //      narrow: Boolean = false   [[If narrow is true then the read mux is moved to the source side of the crossing.
        //                                  This reduces the number of level shifters in the case where the clock crossing is also a voltage crossing,
        //                                  at the expense of a combinational path from the sink to the source and back to the sink.]]
        //

        // Factory method for creating AsyncQueue between two clock domain(fifo structure)
        // clock domains:
        //      enq_clock   <--\           /--> deq_clock
        //                     |AsyncQueue|
        //      enq_reset  <--/           \--> deq_reset
        //
        def apply[T <: Data](in: ChannelIO[T], in_clock: Clock, in_reset: Reset, out_clock: Clock, out_reset: Reset, name: String = "Unknown", depth: Int = 4, sync: Int = 3): ChannelIO[T] = {
            val out    = WireInit(0.U.asTypeOf(chiselTypeOf(in)))
            val params = AsyncQueueParams(depth, sync)
            val q      = Module(new AsyncQueue(chiselTypeOf(in.flit), params))
            q.io.enq_clock := in_clock
            q.io.enq_reset := in_reset
            q.io.deq_clock := out_clock
            q.io.deq_reset := out_reset
            q.io.enq.bits  := in.flit
            q.io.enq.valid := in.flitv

            // q.io.enq.ready  ==> DontCare
            withClockAndReset(enq_clock, enq_reset) {
                assert(!(!q.io.enq.ready && in.flitv), s"AsyncConnect [${name}] enq when AsnycQueue is not ready!")
            }

            out.flit       := q.io.deq.bits
            out.flitv      := q.io.deq.valid
            q.io.deq.ready := true.B
            out
        }

        // Creating a 1-bit AsyncQueue between two clock domain, only used for bit pulse signals(e.g. lcrdv in CHI)
        def bitPulseConnect[T <: Data](in: Bool, in_clock: Clock, in_reset: Reset, out_clock: Clock, out_reset: Reset, name: String = "Unknown", depth: Int = 4, sync: Int = 3): Bool = {
            val out    = WireInit(false.B)
            val params = AsyncQueueParams(depth, sync)
            val q      = Module(new AsyncQueue(UInt(0.W), params))
            q.io.enq_clock := in_clock
            q.io.enq_reset := in_reset
            q.io.deq_clock := out_clock
            q.io.deq_reset := out_reset
            q.io.enq.bits  <> DontCare
            q.io.enq.valid := in

            // q.io.enq.ready  ==> DontCare
            withClockAndReset(enq_clock, enq_reset) {
                assert(!(!q.io.enq.ready && in), s"AsyncConnect [${name}] enq when AsnycQueue is not ready!")
            }

            out            := q.io.deq.valid
            q.io.deq.ready := true.B
            out
        }
    }

    //
    // CHI TX Channel: responsible for receiving L-Credit
    //
    io.chi_deq.tx.req <> AsyncConnect(io.chi_enq.tx.req, enq_clock, enq_reset, deq_clock, deq_reset, "enq_txreq_to_deq_txreq")
    io.chi_deq.tx.dat <> AsyncConnect(io.chi_enq.tx.dat, enq_clock, enq_reset, deq_clock, deq_reset, "enq_txdat_to_deq_txdat")
    io.chi_deq.tx.rsp <> AsyncConnect(io.chi_enq.tx.rsp, enq_clock, enq_reset, deq_clock, deq_reset, "enq_txrsp_to_deq_txrsp")

    io.chi_enq.tx.req.lcrdv <> AsyncConnect.bitPulseConnect(io.chi_deq.tx.req.lcrdv, deq_clock, deq_reset, enq_clock, enq_reset, "deq_txreq_lcrdv_to_enq_txreq_lcrdv")
    io.chi_enq.tx.dat.lcrdv <> AsyncConnect.bitPulseConnect(io.chi_deq.tx.dat.lcrdv, deq_clock, deq_reset, enq_clock, enq_reset, "deq_txdat_lcrdv_to_enq_txdat_lcrdv")
    io.chi_enq.tx.rsp.lcrdv <> AsyncConnect.bitPulseConnect(io.chi_deq.tx.rsp.lcrdv, deq_clock, deq_reset, enq_clock, enq_reset, "deq_txrsp_lcrdv_to_enq_txrsp_lcrdv")

    //
    // CHI RX Channel: responsible for sending L-Credit
    //
    io.chi_enq.rx.dat <> AsyncConnect(io.chi_deq.rx.dat, deq_clock, deq_reset, enq_clock, enq_reset, "deq_rxdat_to_enq_rxdat")
    io.chi_enq.rx.rsp <> AsyncConnect(io.chi_deq.rx.rsp, deq_clock, deq_reset, enq_clock, enq_reset, "deq_rxrsp_to_enq_rxrsp")
    io.chi_enq.rx.snp <> AsyncConnect(io.chi_deq.rx.snp, deq_clock, deq_reset, enq_clock, enq_reset, "deq_rxsnp_to_enq_rxsnp")

    io.chi_deq.rx.dat.lcrdv <> AsyncConnect.bitPulseConnect(io.chi_enq.rx.dat.lcrdv, enq_clock, enq_reset, deq_clock, deq_reset, "enq_rxdat_lcrdv_to_deq_rxdat_lcrdv")
    io.chi_deq.rx.rsp.lcrdv <> AsyncConnect.bitPulseConnect(io.chi_enq.rx.rsp.lcrdv, enq_clock, enq_reset, deq_clock, deq_reset, "enq_rxrsp_lcrdv_to_deq_rxrsp_lcrdv")
    io.chi_deq.rx.snp.lcrdv <> AsyncConnect.bitPulseConnect(io.chi_enq.rx.snp.lcrdv, enq_clock, enq_reset, deq_clock, deq_reset, "enq_rxsnp_lcrdv_to_deq_rxsnp_lcrdv")

    //
    // Input link control
    //
    withClockAndReset(enq_clock, enq_reset) {
        //
        // Below is a typical synchronizer with two registers
        //                                       │
        //                    ┌────┐  ┌────┐     │
        //  output signal ◄───┤ \/ │◄─┤ \/ │◄────│────── input signal
        //                    │    │  │    │     │
        //                    └────┘  └────┘     │
        //                     output clock      │
        //
        io.chi_enq.rxsactive := SynchronizerShiftReg(io.chi_deq.rxsactive, numSyncReg, Some("sync_LinkCtrl_rxsactive"))
        io.chi_enq.tx.linkactiveack := SynchronizerShiftReg(io.chi_deq.tx.linkactiveack, numSyncReg, Some("sync_LinkCtrl_txactiveack"))
        io.chi_enq.rx.linkactivereq := SynchronizerShiftReg(io.chi_deq.rx.linkactivereq, numSyncReg, Some("sync_LinkCrtl_rxactivereq"))
        io.chi_enq.syscoack := SynchronizerShiftReg(io.chi_deq.syscoack, numSyncReg, Some("sync_LinkCtrl_syscoack"))

        io.chi_enq.rx.dat.flitpend := SynchronizerShiftReg(io.chi_deq.rx.dat.flitpend, numSyncReg, Some("sync_enq_rxdat_flitpend"))
        io.chi_enq.rx.rsp.flitpend := SynchronizerShiftReg(io.chi_deq.rx.rsp.flitpend, numSyncReg, Some("sync_enq_rxrsp_flitpend"))
        io.chi_enq.rx.snp.flitpend := SynchronizerShiftReg(io.chi_deq.rx.snp.flitpend, numSyncReg, Some("sync_enq_rxsnp_flitpend"))

        val RESET_FINISH_MAX       = 100
        val resetFinishCounter_enq = withReset(enq_reset.asAsyncReset)(RegInit(0.U((log2Ceil(RESET_FINISH_MAX) + 1).W)))
        when (resetFinishCounter_enq < RESET_FINISH_MAX.U) {
            resetFinishCounter_enq := resetFinishCounter_enq + 1.U
        }
        io.resetFinish_enq := resetFinishCounter_enq >= RESET_FINISH_MAX.U
    }

    //
    // Output link controls
    //
    withClockAndReset(deq_clock, deq_reset) {
        io.chi_deq.txsactive := SynchronizerShiftReg(io.chi_enq.txsactive, numSyncReg, Some("sync_LinkCtrl_txsactive"))
        io.chi_deq.rx.linkactiveack := SynchronizerShiftReg(io.chi_enq.rx.linkactiveack, numSyncReg, Some("sync_LinkCtrl_rxactiveack"))
        io.chi_deq.tx.linkactivereq := SynchronizerShiftReg(io.chi_enq.tx.linkactivereq, numSyncReg, Some("sync_LinkCrtl_txactivereq"))
        io.chi_deq.syscoreq := SynchronizerShiftReg(io.chi_enq.syscoreq, numSyncReg, Some("sync_LinkCtrl_syscoreq"))

        io.chi_deq.tx.req.flitpend := SynchronizerShiftReg(io.chi_enq.tx.req.flitpend, numSyncReg, Some("sync_enq_txreq_flitpend"))
        io.chi_deq.tx.dat.flitpend := SynchronizerShiftReg(io.chi_enq.tx.dat.flitpend, numSyncReg, Some("sync_enq_txdat_flitpend"))
        io.chi_deq.tx.rsp.flitpend := SynchronizerShiftReg(io.chi_enq.tx.rsp.flitpend, numSyncReg, Some("sync_enq_txrsp_flitpend"))

        val RESET_FINISH_MAX       = 100
        val resetFinishCounter_deq = withReset(deq_reset.asAsyncReset)(RegInit(0.U((log2Ceil(RESET_FINISH_MAX) + 1).W)))
        when(resetFinishCounter_deq < RESET_FINISH_MAX.U) {
            resetFinishCounter_deq := resetFinishCounter_deq + 1.U
        }
        io.resetFinish_deq := resetFinishCounter_deq >= RESET_FINISH_MAX.U
    }

    dontTouch(enq_clock)
    dontTouch(deq_clock)
    dontTouch(enq_reset)
    dontTouch(deq_reset)
    dontTouch(io)
}