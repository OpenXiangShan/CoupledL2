package coupledL2.utils

import chisel3._
import chisel3.util._
import utility.ParallelOR
import freechips.rocketchip.util.SeqToAugmentedSeq

object MaskToOH {
  def apply(mask: Seq[Bool]): UInt = {
    val oh = mask.zipWithIndex.map{
      case (b, 0) => b
      case (b, i) => b && !Cat(mask.take(i)).orR
    }.asUInt
    assert(oh === PriorityEncoderOH(mask.asUInt), "MaskToOH should give the same result as PriorityEncoderOH")
    oh
  }

  def apply(mask: UInt): UInt = {
    apply(mask.asBools)
  }
}

object OHMux { 
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T = {
    assert(sel.size == in.size, s"input Seqs must have the same length, got sel ${sel.size} and in ${in.size}")
    apply(sel.zip(in))
  }

  def apply[T <: Data](in: Iterable[(Bool, T)]): T ={
    val typo = in.head._2
    ParallelOR(in.map { case (b, t) =>
      Cat(Seq.fill(t.getWidth)(b)) & t.asTypeOf(UInt())
    }.toSeq).asTypeOf(typo)
  }

  def apply[T <: Data](sel: UInt, in: Seq[T]): T =
    apply((0 until in.size).map(sel(_)), in)

  def apply(sel: UInt, in: UInt): Bool = (sel & in).orR
}