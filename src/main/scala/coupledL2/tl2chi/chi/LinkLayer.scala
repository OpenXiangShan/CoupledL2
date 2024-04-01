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

class LCredit2Decoupled[T <: Data](
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
    lcreditNum: Int = 4
  ): Unit = {
    val mod = Module(new LCredit2Decoupled(left.bits.cloneType, lcreditNum))
    mod.io.in <> left
    right <> mod.io.out
  }
}

class Decoupled2LCredit[T <: Data](gen: T) extends Module {
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
  io.out.flitpend := false.B // TODO
  io.out.flitv := io.in.fire()
  io.out.flit := io.in.bits
}

object Decoupled2LCredit {
  def apply[T <: Data](left: DecoupledIO[T], right: ChannelIO[T]): Unit = {
    val mod = Module(new Decoupled2LCredit(left.bits.cloneType))
    mod.io.in <> left
    right <> mod.io.out
  }
}

// class LCreditArbiterIO[T <: Data](private val gen: T, val n: Int) extends Bundle {
//   val in = Flipped(Vec(n, ChannelIO(gen)))
//   val out = ChannelIO(gen)
//   // val chosen = Output(UInt(log2Ceil(n).W))
// }

// class LCreditArbiter[T <: Data](val gen: T, val n: Int) extends Module {

//   override def desiredName = s"LCreditArbiter${n}_${gen.typeName}"

//   val io = IO(new LCreditArbiterIO(gen, n))

//   val lcreditsAverage = scala.math.ceil(lcreditsMax.toFloat / n).toInt

//   val participants = RegInit(VecInit(Seq.fill(n)(0.U(lcreditsAverage.W))))
//   val queues = Seq.fill(n)(Module(new Queue(gen.cloneType, entries = lcreditsAverage, pipe = true, flow = false)))

//   // which participant should an L-Credit be handed out
//   val grantlcrdv = participants.map(p => p === participants.reduce(_ & _))
//   val grantlcrdvOH = PriorityEncoderOH(grantlcrdv)
//   // which participant should be selected when multiple inputs burst
//   val queueValids = queues.map(_.io.deq.valid)
//   val inputValids = (io.in.map(_.flitv) zip participants.map(_.orR)).map(x => x._1 && x._2)
//   val flits = queues.map(_.io.deq.bits) ++ in.in.map(_.flit)
//   val valids = queueValids ++ inputValids
//   val grantv = valids.length match {
//     case 0 => Seq()
//     case 1 => Seq(true.B)
//     case _ => true.B +: valids.tail.init.scanLeft(valids.head)(_ || _).map(!_)
//   }
//   val fires = (valids zip grantv).map(x => x._1 && x._2)
//   // val grantvOH = PriorityEncoderOH(valids)

//   io.in.zip(grantlcrdvOH).foreach { case (in, g) => in.lcrdv := io.out.lcrdv && g }

//   /* 
//     When multiple inputs burst in the same cycle, only one could be chosen into output.
//     Other inputs should enqueue and the dequeue later.
//     */
//   io.out.flitv := Cat(valids).orR
//   io.out.flitpend := true.B // TODO: consider flitpend
//   io.out.flit := ParallelPriorityMux(valids, queues.map(_.io.deq.bits) ++ io.in.map(_.bits))

//   for (i <- 0 until n) {
//     queues(i).io.deq.ready := grantv(i)

//     queues(i).io.enq.valid := inputValids(i) && !(grantv.drop(n))(i)
//     queues(i).io.enq.bits := io.in(i).bits
//     assert(queues(i).io.enq.ready, "queues should always be ready")

//     val p = io.in(i).lcrdv // produce L-Credit
//     val v = fires(i) || fires(i + n) // consume L-Credit

//     when (p && !v) { participants(i) := produce(participants(i)) }
//     when (v && !p) { participants(i) := consume(participants(i)) }

//     assert(!(p && !v && participants(i).andR), s"L-Credit overflows for input-${i}")
//     assert(!(v && !participants(i).orR), s"L-Credit runs out for input-${i}")
//   }

//   // produce lcredits
//   def produce(x: UInt): UInt = Cat(x, 1.U(1.W))
//   // consume lcredits
//   def consume(x: UInt): UInt = x >> 1

//   assert(!io.out.lcrdv || Cat(grantlcrdv).orR)
// }
