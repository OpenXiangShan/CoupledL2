/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
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
import chisel3.util._

/**
 * Assert the signal must hold for certain cycles when enable is high
 */
object HoldChecker {
  /**
   * signal holds at en and the next cycle
   */
  def check2(signal: UInt, en: Bool, name: String): Unit = {
    // at the 2nd cycle, signal changes
    assert(!(RegNext(en) && (signal =/= RegNext(signal))),
      s"signal changed at $name, fails to hold for 2 cycles")
  }
  def check2(signal: Record, en: Bool, name: String): Unit = {
    check2(signal.asUInt, en, name)
  }

  /**
   * signal holds for N cycles
   */
  def apply(signal: UInt, en: Bool, cycles: Int, name: String): Unit = {
    val counter = RegInit(0.U(log2Ceil(cycles).W))
    val data = RegEnable(signal, 0.U.asTypeOf(signal), en)

    when(en) {
      counter := cycles.U - 1.U
    }.otherwise {
      counter := Mux(counter === 0.U, 0.U, counter - 1.U)
    }

    assert((counter =/= 0.U) && (signal =/= data),
      s"Signal should hold for $cycles cycles, but it fails")
  }

  def apply(signal: Record, en: Bool, cycles: Int, name: String): Unit = {
    apply(signal.asUInt, en, cycles, name)
  }
}

