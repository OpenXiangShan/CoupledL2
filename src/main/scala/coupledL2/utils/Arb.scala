package coupledL2.utils
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
import utility._

object ArbPerf {
    def apply(valids: Seq[Bool], readys: Seq[Bool], outRdy: Bool, name: String)(implicit p: Parameters): Unit = {
        require(valids.size == readys.size)
        XSPerfHistogram(s"${name}_myconflict", PopCount(valids), valids.asUInt.orR, 1, valids.size + 1)
        val n = valids.size
        val a = valids.zip(readys).zipWithIndex.map { case ((v, r), i) =>
            val w = RegInit(0.U(64.W))
            when (v && !r && outRdy) { w := w + 1.U }
            when (v && r) { w := 0.U }
            0.until(n*2).map(x => ((x.U === w && v && r).asUInt, x)) :+ (((w >= (n*2).U && v && r).asUInt, n*2))
        }.reduce { (left, right) =>
            left.zip(right).map { case (a, b) => (a._1 + b._1, a._2) }
        }.foreach { case (cnt, time) =>
            XSPerfAccumulate(s"${name}_mywait_${time}", cnt)
        }
    }

    def apply[T <: Data](arb: LockingRRArbiterInit[T], name: String)(implicit p: Parameters): Unit = {
        apply(arb.io.in.map(_.valid), arb.io.in.map(_.ready), arb.io.out.ready, name)
    }

    def apply[T <: Data](arb: FastArbiter[T], name: String)(implicit p: Parameters): Unit = {
        apply(arb.io.in.map(_.valid), arb.io.in.map(_.ready), arb.io.out.ready, name)
    }

    def apply[T <: Data](arb: L2FastArbiterBase[T], name: String)(implicit p: Parameters): Unit = {
        apply(arb.io.in.map(_.valid), arb.io.in.map(_.ready), arb.io.out.ready, name)
    }
}

class L2ArbiterIO[T <: Data](gen: T, n: Int) extends ArbiterIO[T](gen, n) {
    val chosenOH = Output(UInt(n.W))
}

abstract class L2FastArbiterBase[T <: Data](val gen: T, val n: Int) extends Module {
  val io = IO(new L2ArbiterIO[T](gen, n))

  def maskToOH(seq: Seq[Bool]) = {
    seq.zipWithIndex.map{
      case (b, 0) => b
      case (b, i) => b && !Cat(seq.take(i)).orR
    }
  }
}

class L2FastArbiter[T <: Data](gen: T, n: Int) extends L2FastArbiterBase[T](gen, n) {

  val chosenOH = Wire(UInt(n.W))
  val valids = VecInit(io.in.map(_.valid)).asUInt
  // the reqs that we didn't choose in last cycle
  val pendingMask = RegEnable(
    valids & (~chosenOH).asUInt, // make IDEA happy ...
    0.U(n.W),
    io.out.fire
  )
  // select a req from pending reqs by RR
  /*
      Eg: chosenOH  0100
       rrGrantMask  0011
   */
  val rrGrantMask = RegEnable(VecInit((0 until n) map { i =>
    if(i == 0) false.B else chosenOH(i - 1, 0).orR
  }).asUInt, 0.U(n.W), io.out.fire)
  val rrSelOH = VecInit(maskToOH((rrGrantMask & pendingMask).asBools)).asUInt
  val firstOneOH = VecInit(maskToOH(valids.asBools)).asUInt
  val rrValid = (rrSelOH & valids).orR
  chosenOH := Mux(rrValid, rrSelOH, firstOneOH)

  io.out.valid := valids.orR
  io.out.bits := Mux1H(chosenOH, io.in.map(_.bits))

  io.in.map(_.ready).zip(chosenOH.asBools).foreach{
    case (rdy, grant) => rdy := grant && io.out.ready
  }

  io.chosen := OHToUInt(chosenOH)
  io.chosenOH := chosenOH
}


// TODO: rename like TwoLevelRRArbiter
class HalfFastArbiter[T <: Data](gen: T, n: Int) extends L2FastArbiterBase[T](gen, n) {
  val mid = n / 2
  val rest = n - mid

  val chosenOHLow = Wire(UInt(mid.W))
  val chosenOHHigh = Wire(UInt(rest.W))
  val valids = VecInit(io.in.map(_.valid)).asUInt
  val validsLow = valids(mid - 1, 0)
  val validsHigh = valids(n - 1, mid)
  val fireLow, fireHigh = WireDefault(false.B)

  val selLow = RegInit(false.B)
  val selNext = WireInit(!selLow)
  when (validsLow.orR && validsHigh.orR) {
    selLow := !selLow
  }.elsewhen (validsLow.orR) {
    selLow := true.B
    selNext := true.B
  }.elsewhen (validsHigh.orR) {
    selLow := false.B
    selNext := false.B
  }

  val pendingMaskLow = RegEnable(
    validsLow & (~chosenOHLow).asUInt,
    0.U(mid.W),
    fireLow
  )
  val pendingMaskHigh = RegEnable(
    validsHigh & (~chosenOHHigh).asUInt,
    0.U(rest.W),
    fireHigh
  )
  val rrGrantMaskLow = RegEnable(VecInit((0 until mid) map { i =>
    if(i == 0) false.B else chosenOHLow(i - 1, 0).orR
  }).asUInt, 0.U(mid.W), fireLow)
  val rrGrantMaskHigh = RegEnable(VecInit((0 until rest) map { i =>
    if(i == 0) false.B else chosenOHHigh(i - 1, 0).orR
  }).asUInt, 0.U(rest.W), fireHigh)
  val rrSelOHLow = VecInit(maskToOH((rrGrantMaskLow & pendingMaskLow).asBools)).asUInt & Cat(Seq.fill(mid)(selLow))
  val rrSelOHHigh = VecInit(maskToOH((rrGrantMaskHigh & pendingMaskHigh).asBools)).asUInt & Cat(Seq.fill(rest)(!selLow))
  val rrSelOH = Cat(rrSelOHHigh, rrSelOHLow)
  assert(PopCount(rrSelOH) <= 1.U)
  val rrValid = (rrSelOH & valids).orR
  val firstOneOH = VecInit(maskToOH(valids.asBools)).asUInt
  val chosenOH = Mux(rrValid, rrSelOH, firstOneOH)
  chosenOHLow := chosenOH(mid - 1, 0)
  chosenOHHigh := chosenOH(n - 1, mid)
  assert(PopCount(chosenOH) <= 1.U)
  fireLow := (chosenOHLow & validsLow).orR
  fireHigh := (chosenOHHigh & validsHigh).orR

  io.out.valid := valids.orR
  io.out.bits := Mux1H(chosenOH, io.in.map(_.bits))

  io.in.map(_.ready).zip(chosenOH.asBools).foreach{
    case (rdy, grant) => rdy := grant && io.out.ready
  }

  io.chosen := OHToUInt(chosenOH)
  io.chosenOH := chosenOH
}