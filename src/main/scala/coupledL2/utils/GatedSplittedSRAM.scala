package coupledL2.utils

import chisel3._
import chisel3.util._
import utility.ClockGate
import freechips.rocketchip.diplomacy.ValName

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
  clkDivBy2: Boolean = false, readMCP2: Boolean = true
)(implicit valName: ValName) extends SplittedSRAM[T](
  gen, set, way,
  setSplit, waySplit, dataSplit,
  shouldReset, holdRead, singlePort, bypassWrite, clkDivBy2, readMCP2
) {
  // en is the actual r/w valid (last for one cycle)
  // en is used to generate gated_clock for each SRAM
  val io_en = IO(Input(Bool()))

  // Create a ClockGate module for each element in the array
  array.map(_.map(_.map(_.clock := ClockGate(false.B, io_en, clock))))

  // TODO: for now, all small SRAMs use the unified `io_en` signal for gating.
  // Theoretically, gating can be set based on whether the small SRAM is activated
  // (e.g., in cases of `setSplit` or `waySplit` is set).
  // However, since only `DataStorage` uses this module for now
  // and `DataStorage` reads/writes to all small SRAMs simultaneously,
  // a unified enable signal is used for simplicity in logic.
}
