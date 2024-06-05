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
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2chi.{PortIO}

class OpenLLC(implicit p: Parameters) extends LazyModule with HasOpenLLCParameters {

  val axi4node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "L3",
      id = IdRange(0, 1 << 14)
    ))
  )))

  class OpenLLCImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val chi = Flipped(new PortIO)
      val nodeID = Input(UInt())
    })

    val slice = Module(new Slice()(p.alterPartial {
      case EdgeOutKey => axi4node.out.head._2
    }))
    val linkMonitor = Module(new LLCLinkMonitor)

    dontTouch(axi4node.out.head._1)
    dontTouch(io)

    linkMonitor.io.in <> slice.io.in
    linkMonitor.io.nodeID := io.nodeID
    io.chi <> linkMonitor.io.out
    axi4node.out.head._1 <> slice.io.out
  }
  lazy val module = new OpenLLCImp(this)
}
