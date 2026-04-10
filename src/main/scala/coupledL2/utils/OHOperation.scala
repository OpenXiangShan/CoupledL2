package coupledL2.utils

import chisel3._
import chisel3.util._
import utility.ParallelOR

object MaskToOH {
  def apply(mask: UInt): UInt = {
    val width = mask.getWidth
    val oh = Wire(Vec(width, Bool()))
    for (i <- 0 until width) {
      if (i == 0) {
        oh(i) := mask(i)
      } else {
        oh(i) := mask(i) && !ParallelOR(mask(i-1, 0).asBools)
      }
    }
    assert(oh.asUInt === PriorityEncoderOH(mask), "MaskToOH should give the same result as PriorityEncoderOH")
    oh.asUInt
  }
}

object MaskToOH1 {
  def apply(in: UInt) = {
    def onehotWithMulti(x: UInt): UInt = {
      val w = x.getWidth
      if (w == 1) {
        x
      } else {
        val highWidth = w / 2
        val lowWidth  = w - highWidth
        val high = x(w - 1, lowWidth)
        val low  = x(lowWidth - 1, 0)

        val highOnehot = onehotWithMulti(high)
        val lowOnehot  = onehotWithMulti(low)

        val highHasOne = high.orR
        val lowHasOne  = low.orR

        val onehot = Mux(lowHasOne,
          Cat(0.U(highWidth.W), lowOnehot),
          Cat(highOnehot, 0.U(lowWidth.W))
        )

        onehot
      }
    }

    onehotWithMulti(in)
  }
}

object OHMux { 
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T = {
    assert(sel.size == in.size, s"input Seqs must have the same length, got sel ${sel.size} and in ${in.size}")
    apply(sel.zip(in))
  }

  def apply[T <: Data](in: Iterable[(Bool, T)]): T ={
    val typo = in.head._2
    (in.map { case (b, t) =>
      Cat(Seq.fill(t.getWidth)(b)) & t.asTypeOf(UInt())
    }.toSeq).reduce(_ | _).asTypeOf(typo)
  }

  def apply[T <: Data](sel: UInt, in: Seq[T]): T =
    apply((0 until in.size).map(sel(_)), in)

  def apply(sel: UInt, in: UInt): Bool = (sel & in).orR
}
