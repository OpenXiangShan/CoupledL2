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

class PortIO extends Bundle with HasPortSwitch {
  val tx = new DownwardsLinkIO
  val rx = Flipped(new UpwardsLinkIO)
}

class DecoupledPortIO extends Bundle {
  val tx = new DecoupledDownwardsLinkIO
  val rx = Flipped(new DecoupledUpwardsLinkIO)
}

class LCredit2Decoupled[T <: Bundle](
  gen: T,
  lcreditNum: Int = 4 // the number of L-Credits that a receiver can provide
) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(ChannelIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
  })

  require(lcreditNum <= 15)

  val queue = Module(new Queue(gen.cloneType, entries = lcreditNum, pipe = true, flow = false))

  val lcreditsWidth = log2Up(lcreditNum) + 1
  val lcreditInflight = RegInit(0.U(lcreditsWidth.W))
  val lcreditPool = RegInit(lcreditNum.U(lcreditsWidth.W))
  assert(lcreditInflight + lcreditPool === lcreditNum.U)
  val lcreditOut = (lcreditPool > queue.io.count) && !reset.asUInt

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
}

object LCredit2Decoupled {
  val defaultLCreditNum = 4

  def apply[T <: Bundle](
    left: ChannelIO[T],
    right: DecoupledIO[T],
  ): Unit = {
    val mod = Module(new LCredit2Decoupled(right.bits.cloneType, defaultLCreditNum))
    mod.io.in <> left
    right <> mod.io.out
  }

  def apply[T <: Bundle](
    left: ChannelIO[T],
    right: DecoupledIO[T],
    suggestName: Option[String],
    lcreditNum: Int = 4
  ): Unit = {
    val mod = Module(new LCredit2Decoupled(right.bits.cloneType, lcreditNum))
    suggestName.foreach(name => mod.suggestName(name))

    mod.io.in <> left
    right <> mod.io.out
  }
}

class Decoupled2LCredit[T <: Bundle](gen: T) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(gen.cloneType))
    val out = ChannelIO(gen.cloneType)
  })

  // The maximum number of L-Credits that a receiver can provide is 15.
  val lcreditsMax = 15
  val lcreditPool = RegInit(0.U(log2Up(lcreditsMax).W))

  when (io.out.lcrdv) {
    when (!io.in.fire()) {
      lcreditPool := lcreditPool + 1.U
      assert(lcreditPool + 1.U =/= 0.U, "L-Credit pool overflow")
    }
  }.otherwise {
    when (io.in.fire()) {
      lcreditPool := lcreditPool - 1.U
    }
  }

  io.in.ready := lcreditPool =/= 0.U
  io.out.flitpend := true.B
  io.out.flitv := io.in.fire()
  io.out.flit := Cat(io.in.bits.getElements.map(_.asUInt))
}

object Decoupled2LCredit {
  def apply[T <: Bundle](left: DecoupledIO[T], right: ChannelIO[T]): Unit = {
    val mod = Module(new Decoupled2LCredit(left.bits.cloneType))
    mod.io.in <> left
    right <> mod.io.out
  }

  def apply[T <: Bundle](left: DecoupledIO[T], right: ChannelIO[T], suggestName: Option[String]): Unit = {
    val mod = Module(new Decoupled2LCredit(left.bits.cloneType))
    suggestName.foreach(name => mod.suggestName(name))
    
    mod.io.in <> left
    right <> mod.io.out
  }
}
