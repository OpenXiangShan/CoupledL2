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

class ChannelIO[+T <: Data](gen: T) extends Bundle {
  // Flit Pending. Early indication that a flit might be transmitted in the following cycle
  val flitpend = Output(Bool()) // To be confirmed: can this be ignored?
  // Flit Valid. The transmitter sets the signal HIGH to indicate when FLIT[(W-1):0] is valid.
  val flitv = Output(Bool())
  // Flit.
  val flit = Output(gen)
  // L-Credit Valid. The receiver sets this signal HIGH to return a channel L-Credit to a transmitter.
  val lcrdv = Input(Bool())

  def map[B <: Data](f: T => B): ChannelIO[B] = {
    val _map_flit = f(flit)
    val _map = Wire(ChannelIO(chiselTypeOf(_map_flit)))
    _map.flitpend := flitpend
    _map.flitv := flitv
    _map.flit := _map_flit
    lcrdv := _map.lcrdv
    _map
  }

  def bits: T = flit
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

class PortIO extends Bundle with HasPortSwitch {
  val tx = new DownwardsLinkIO
  val rx = Flipped(new UpwardsLinkIO)
}

// class CHITestModule extends Module {
//   val io = IO(new PortIO)
//   io <> DontCare
// }

class LCredit2Decoupled[T <: Data](
  gen: T,
  lCreditNum: Int = 4 // the number of L-Credits that a receiver can provide
) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(ChannelIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
  })

  val queue = Module(new Queue(gen.cloneType, entries = lCreditNum, pipe = true, flow = false))

  val lcreditsWidth = log2Up(lCreditNum) + 1
  val lcreditInflight = RegInit(0.U(lcreditsWidth.W))
  val lcreditPool = RegInit(lCreditNum.U(lcreditsWidth.W))
  val lcreditOut = lcreditPool > queue.io.count

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

  queue.io.enq.valid := accept
  queue.io.enq.bits := io.in.bits
  assert(!accept || queue.io.enq.ready)

  io.in.lcrdv := lcreditOut

  io.out <> queue.io.deq
}

object LCredit2Decoupled {
  def apply[T <: Data](
    left: ChannelIO[T],
    right: DecoupledIO[T],
    lCreditNum: Int = 4
  ): Unit = {
    val mod = Module(new LCredit2Decoupled(left.bits.cloneType, lCreditNum))
    mod.io.in <> left
    mod.io.out <> right
  }
}