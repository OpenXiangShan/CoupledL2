/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package coupledL2.utils

import chisel3._
import coupledL2.L2Param
import utility.{ChiselDB, LogPerfHelper, LogPerfIO}

object XSPerfAccumulate {
  def apply(params: L2Param, perfName: String, perfCnt: UInt) = {
    if (params.enablePerf && !params.FPGAPlatform) {
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
    if (params.enablePerf && !params.FPGAPlatform) {
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
    if (params.enablePerf && !params.FPGAPlatform) {
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

object XSPerfRolling {

  class RollingEntry() extends Bundle {
    val xAxisPt = UInt(64.W)
    val yAxisPt = UInt(64.W)

    def apply(xAxisPt: UInt, yAxisPt: UInt): RollingEntry = {
      val e = Wire(new RollingEntry())
      e.xAxisPt := xAxisPt
      e.yAxisPt := yAxisPt
      e
    }
  }

  def apply(
    params: L2Param,
    perfName: String,
    perfCnt: UInt,
    granularity: Int,
    clock: Clock,
    reset: Reset
  ): Unit = {
    if (params.enableRollingDB && !params.FPGAPlatform) {
      val tableName = perfName + "_rolling_0"  // TODO: support naming hart id
      val rollingTable = ChiselDB.createTable(tableName, new RollingEntry(), basicDB=true)

      val xAxisCnt = RegInit(0.U(64.W))
      val yAxisCnt = RegInit(0.U(64.W))
      val xAxisPtReg = RegInit(0.U(64.W))
      val xAxisPt = WireInit(0.U(64.W))
      xAxisCnt := xAxisCnt + 1.U(64.W)  // increment per cycle
      yAxisCnt := yAxisCnt + perfCnt

      val triggerDB = xAxisCnt === granularity.U
      when(triggerDB) {
        xAxisCnt := 1.U(64.W)
        yAxisCnt := perfCnt
        xAxisPtReg := xAxisPtReg + granularity.U
        xAxisPt := xAxisPtReg + granularity.U
      }
      val rollingPt = new RollingEntry().apply(xAxisPt, yAxisCnt)
      rollingTable.log(rollingPt, triggerDB, "", clock, reset)
    }
  }

  def apply(
    params: L2Param,
    perfName: String,
    perfCnt: UInt,
    eventTrigger: UInt,
    granularity: Int,
    clock: Clock,
    reset: Reset
  ): Unit = {
    if (params.enableRollingDB && !params.FPGAPlatform) {
      val tableName = perfName + "_rolling_0"  // TODO: support naming hart id
      val rollingTable = ChiselDB.createTable(tableName, new RollingEntry(), basicDB=true)

      val xAxisCnt = RegInit(0.U(64.W))
      val yAxisCnt = RegInit(0.U(64.W))
      val xAxisPtReg = RegInit(0.U(64.W))
      val xAxisPt = WireInit(0.U(64.W))
      xAxisCnt := xAxisCnt + eventTrigger // increment when event triggers
      yAxisCnt := yAxisCnt + perfCnt

      val triggerDB = xAxisCnt >= granularity.U
      when(triggerDB) {
        xAxisCnt := xAxisCnt - granularity.U + eventTrigger
        yAxisCnt := perfCnt
        xAxisPtReg := xAxisPtReg + xAxisCnt
        xAxisPt := xAxisPtReg + xAxisCnt
      }
      val rollingPt = new RollingEntry().apply(xAxisPt, yAxisCnt)
      rollingTable.log(rollingPt, triggerDB, "", clock, reset)
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
    val commonInfo = p"[PERF ][time=${ctrlInfo.timer}] __PERCENTAGE_M__: "
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
