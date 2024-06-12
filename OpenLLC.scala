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

package openLLC

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2chi.{PortIO}

class OpenLLC(implicit p: Parameters) extends LazyModule with HasOpenLLCParameters {

  class OpenLLCImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val chi_upwards = Flipped(new PortIO)
      val chi_downwards = new NoSnpPortIO
      val nodeID = Input(UInt())
    })

    val slice = Module(new Slice)
    val upwardsLinkMonitor = Module(new UpwardsLinkMonitor)
    val downwardsLinkMonitor = Module(new DownwardsLinkMonitor)

    dontTouch(io)

    upwardsLinkMonitor.io.in <> slice.io.in
    upwardsLinkMonitor.io.nodeID := io.nodeID
    downwardsLinkMonitor.io.in <> slice.io.out
    downwardsLinkMonitor.io.nodeID := io.nodeID
    io.chi_upwards <> upwardsLinkMonitor.io.out
    io.chi_downwards <> downwardsLinkMonitor.io.out
  }
  lazy val module = new OpenLLCImp(this)
}
