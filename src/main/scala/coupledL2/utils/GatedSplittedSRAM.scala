package coupledL2.utils

import chisel3._
import chisel3.util._
import utility.mbist.MbistClockGateCell
import utility.sram.{SramBroadcastBundle, SramHelper}

// SplittedSRAM with clockGate to each of the small splitted srams
// - this is a requirement from DFT, cause mbist needs to access each sram separately
//   and it will add addtional logic using ClockGate
// - so ClockGate need also be splitted

class GatedSplittedSRAM[T <: Data]
(
  gen: T, set: Int, way: Int,
  setSplit: Int = 1, waySplit: Int = 1, dataSplit: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = true, bypassWrite: Boolean = false,
  clkDivBy2: Boolean = false, readMCP2: Boolean = true,
  hasMbist:Boolean = false, hasSramCtl: Boolean = false,
  extraHold: Boolean = false, suffix: Option[String] = None,
  withClockGate: Boolean = true,
)(implicit valName: sourcecode.FullName) extends SplittedSRAM[T](
  gen = gen,
  set = set,
  way = way,
  setSplit = setSplit,
  waySplit = waySplit,
  dataSplit = dataSplit,
  shouldReset = shouldReset,
  holdRead = holdRead,
  singlePort = singlePort,
  bypassWrite = bypassWrite,
  clkDivBy2 = clkDivBy2,
  readMCP2 = readMCP2,
  hasMbist = hasMbist,
  hasSramCtl = hasSramCtl,
  extraHold = extraHold,
  extClockGate = withClockGate,
  suffix = Some(suffix.getOrElse(SramHelper.getSramSuffix(valName.value)))
) {
  // en is the actual r/w valid (last for one cycle)
  // en is used to generate gated_clock for each SRAM
  val io_en = IO(Input(Bool()))

  // Create a ClockGate module for each element in the array
  if (withClockGate) {
    array.map(_.map(_.map(a => {
      val cg = Module(new MbistClockGateCell(extraHold))
      cg.E := io_en
      cg.dft.fromBroadcast(a.io.broadcast.getOrElse(0.U.asTypeOf(new SramBroadcastBundle)))
      cg.mbist.req := a.io.mbistCgCtl.map(_.en).getOrElse(false.B)
      cg.mbist.writeen := a.io.mbistCgCtl.map(_.wckEn).getOrElse(false.B)
      cg.mbist.readen := a.io.mbistCgCtl.map(_.rckEn).getOrElse(false.B)
      a.io.mbistCgCtl.foreach(_.wclk := cg.out_clock)
      a.io.mbistCgCtl.foreach(_.rclk := cg.out_clock)
      a.clock := clock
    })))
  }

  // TODO: for now, all small SRAMs use the unified `io_en` signal for gating.
  // Theoretically, gating can be set based on whether the small SRAM is activated
  // (e.g., in cases of `setSplit` or `waySplit` is set).
  // However, since only `DataStorage` uses this module for now
  // and `DataStorage` reads/writes to all small SRAMs simultaneously,
  // a unified enable signal is used for simplicity in logic.
}
