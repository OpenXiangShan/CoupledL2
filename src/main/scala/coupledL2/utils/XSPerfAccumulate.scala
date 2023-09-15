package coupledL2.utils

import chisel3._
import coupledL2.L2Param
import utility.{ChiselDB, LogPerfHelper, LogPerfIO}

object XSPerfAccumulate {
  def apply(params: L2Param, perfName: String, perfCnt: UInt) = {
    if (params.enablePerf) {
      val helper = Module(new LogPerfHelper)
      val perfClean = helper.io.clean
      val perfDump = helper.io.dump

      val counter = RegInit(0.U(64.W))
      val next_counter = counter + perfCnt
      counter := Mux(perfClean, 0.U, next_counter)

      when(perfDump) {
        XSPerfPrint(p"$perfName, $next_counter\n")(helper.io)
      }
    }
  }
}

object XSPerfHistogram {
  // instead of simply accumulating counters
  // this function draws a histogram
  def apply(
    params:   L2Param,
    perfName: String,
    perfCnt:  UInt,
    enable:   Bool,
    start:    Int,
    stop:     Int,
    step:     Int,
    left_strict: Boolean = false,
    right_strict: Boolean = false
  ) = {
    if (params.enablePerf) {
      val helper = Module(new LogPerfHelper)
      val perfClean = helper.io.clean
      val perfDump = helper.io.dump

      // drop each perfCnt value into a bin
      val nBins = (stop - start) / step
      require(start >= 0)
      require(stop > start)
      require(nBins > 0)

      (0 until nBins).map { i =>
        val binRangeStart = start + i * step
        val binRangeStop = start + (i + 1) * step
        val inRange = perfCnt >= binRangeStart.U && perfCnt < binRangeStop.U

        // if perfCnt < start, it will go to the first bin
        val leftOutOfRange = if(left_strict)
          false.B
        else
          perfCnt < start.U && i.U === 0.U
        // if perfCnt >= stop, it will go to the last bin
        val rightOutOfRange = if(right_strict)
          false.B
        else
          perfCnt >= stop.U && i.U === (nBins - 1).U
        val inc = inRange || leftOutOfRange || rightOutOfRange

        val counter = RegInit(0.U(64.W))
        when(perfClean) {
          counter := 0.U
        }.elsewhen(enable && inc) {
          counter := counter + 1.U
        }

        when(perfDump) {
          XSPerfPrint(p"${perfName}_${binRangeStart}_${binRangeStop}, $counter\n")(helper.io)
        }
      }
    }
  }
}

object XSPerfMax {
  def apply(params: L2Param, perfName: String, perfCnt: UInt, enable: Bool) = {
    if (params.enablePerf) {
      val helper = Module(new LogPerfHelper)
      val perfClean = helper.io.clean
      val perfDump = helper.io.dump

      val max = RegInit(0.U(64.W))
      val next_max = Mux(enable && (perfCnt > max), perfCnt, max)
      max := Mux(perfClean, 0.U, next_max)

      when(perfDump) {
        XSPerfPrint(p"${perfName}_max, $next_max\n")(helper.io)
      }
    }
  }
}

object TransactionLatencyCounter {
  // count the latency between start signal and stop signal
  // whenever stop signals comes, we create a latency sample
  def apply(start: Bool, stop: Bool): (Bool, UInt) = {
    assert(!(start && stop))
    val counter = RegInit(0.U(64.W))
    val next_counter = counter + 1.U
    counter := Mux(start || stop, 0.U, next_counter)
    (stop, next_counter)
  }
}

object XSPerfPrint {
  def apply(fmt: String, data: Bits*)(ctrlInfo: LogPerfIO): Any =
    apply(Printable.pack(fmt, data: _*))(ctrlInfo)

  def apply(pable: Printable)(ctrlInfo: LogPerfIO): Any = {
    val commonInfo = p"[PERF ][time=${ctrlInfo.timer}] 9527: "
    printf(commonInfo + pable)
  }
}

object GTimer {
  def apply() = {
    val c = RegInit(0.U(64.W))
    c := c + 1.U
    c
  }
}
