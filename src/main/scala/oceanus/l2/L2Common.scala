package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.compactchi.CCHIComponent
import org.chipsalliance.cde.config.Parameters
import oceanus.compactchi.CCHIOpcode
import oceanus.compactchi.CCHIChannelType
import utility.ParallelOR
import utility.ParallelMux
import oceanus.chi.EnumCHIChannel.SNP
import utility.XSPerfAccumulate
import utility.XSPerfHistogram

object L2Address {
  def set(pa: UInt)(implicit p: Parameters): UInt = pa // TODO: extract real set index from PA
}

object FSMPerfHistogram {
  def apply(name: String, prev: Bool, next: Bool)(implicit p: Parameters): Unit = {
    val counter = RegInit(0.U(32.W))
    when (prev) {
      when (!next) {
        counter := 0.U
      }.otherwise {
        counter := counter + 1.U
      }
    }
    XSPerfAccumulate(name + "_total", prev)
    XSPerfHistogram(name, counter, prev && !next, 0, 40, 2, right_strict = true)
    XSPerfHistogram(name, counter, prev && !next, 40, 800, 40, left_strict = true)
  }
}

object FSMTransPerfHistogram {
  def apply(name: String, prev: Bool, next: Bool)(implicit p: Parameters): Unit = {
    val counter = RegInit(0.U(32.W))
    when (prev) {
      when (next) {
        counter := 0.U
      }.otherwise {
        counter := counter + 1.U
      }
    }
    XSPerfAccumulate(name + "_cnt", prev && next)
    XSPerfHistogram(name, counter, prev && next, 0, 40, 2, right_strict = true)
    XSPerfHistogram(name, counter, prev && next, 40, 800, 40, left_strict = true)
  }
}