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