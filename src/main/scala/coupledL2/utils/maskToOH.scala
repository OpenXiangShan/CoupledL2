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
        oh(i) := mask(i) && !mask(i-1, 0).orR
      }
    }
    assert(oh.asUInt === PriorityEncoderOH(mask), "MaskToOH should give the same result as PriorityEncoderOH")
    oh.asUInt
  }
}