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


class TwoLevelRRArbiter[T <: Data](gen: T, n: Int) extends L2FastArbiterBase[T](gen, n) {
  if (n == 1) {
    // for better codegen
    io.out.valid := io.in.head.valid
    io.out.bits := io.in.head.bits
    io.in.head.ready := io.out.ready
    io.chosen := 0.U
    io.chosenOH := 1.U
  } else if (n == 0) {
    io.out.valid := false.B
    io.out.bits := 0.U.asTypeOf(io.out.bits)
    io.chosen := 0.U
    io.chosenOH := 0.U
  } else {
    val mid = n / 2
    val rest = n - mid
    val lowOut = Wire(Decoupled(gen))
    val highOut = Wire(Decoupled(gen))
    val selLow = RegInit(false.B)
    val finalSelLow = Wire(Bool())

    val chosenOHLow = Wire(UInt(mid.W))
    val chosenOHHigh = Wire(UInt(rest.W))
    val validsLow = VecInit(io.in.take(mid).map(_.valid)).asUInt
    val validsHigh = VecInit(io.in.drop(mid).map(_.valid)).asUInt

    val pendingMaskLow = RegEnable(
      validsLow & (~chosenOHLow).asUInt, // make IDEA happy ...
      0.U(mid.W),
      lowOut.fire
    )
    val pendingMaskHigh = RegEnable(
      validsHigh & (~chosenOHHigh).asUInt, // make IDEA happy ...
      0.U(rest.W),
      highOut.fire
    )

    val rrGrantMaskLow = RegEnable(VecInit((0 until mid) map { i =>
      if(i == 0) false.B else chosenOHLow(i - 1, 0).orR
    }).asUInt, 0.U(mid.W), lowOut.fire)
    val rrGrantMaskHigh = RegEnable(VecInit((0 until rest) map { i =>
      if(i == 0) false.B else chosenOHHigh(i - 1, 0).orR
    }).asUInt, 0.U(rest.W), highOut.fire)

    val rrSelOHLow = VecInit(maskToOH((rrGrantMaskLow & pendingMaskLow).asBools)).asUInt
    val rrSelOHHigh = VecInit(maskToOH((rrGrantMaskHigh & pendingMaskHigh).asBools)).asUInt
    val firstOneOHLow = VecInit(maskToOH(validsLow.asBools)).asUInt
    val firstOneOHHigh = VecInit(maskToOH(validsHigh.asBools)).asUInt
    val rrValidLow = (rrSelOHLow & validsLow).orR
    val rrValidHigh = (rrSelOHHigh & validsHigh).orR
    chosenOHLow := Mux(rrValidLow, rrSelOHLow, firstOneOHLow)
    chosenOHHigh := Mux(rrValidHigh, rrSelOHHigh, firstOneOHHigh)

    lowOut.valid := validsLow.orR
    lowOut.bits := Mux1H(chosenOHLow, io.in.take(mid).map(_.bits))
    highOut.valid := validsHigh.orR
    highOut.bits := Mux1H(chosenOHHigh, io.in.drop(mid).map(_.bits))

    io.in.take(mid).map(_.ready).zip(chosenOHLow.asBools).foreach{
      case (rdy, grant) => rdy := grant && lowOut.ready
    }
    io.in.drop(mid).map(_.ready).zip(chosenOHHigh.asBools).foreach{
      case (rdy, grant) => rdy := grant && highOut.ready
    }

    io.out.valid := lowOut.valid || highOut.valid
    finalSelLow := OHMux(Seq(
      (lowOut.valid && highOut.valid) -> selLow,
      (lowOut.valid && !highOut.valid) -> true.B,
      (!lowOut.valid && highOut.valid) -> false.B
    ))
    io.out.bits := Mux(finalSelLow, lowOut.bits, highOut.bits)
    lowOut.ready := io.out.ready && finalSelLow
    highOut.ready := io.out.ready && !finalSelLow

    io.chosenOH := Cat(Seq.fill(rest)(!finalSelLow).asUInt & chosenOHHigh, Seq.fill(mid)(finalSelLow).asUInt & chosenOHLow)
    io.chosen := OHToUInt(io.chosenOH)

    when (io.out.fire) {
      selLow := Mux(finalSelLow, !highOut.valid, lowOut.valid)
    }
  }
}