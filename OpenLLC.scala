package openLLC

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class OpenLLC(implicit p: Parameters) extends LazyModule {

  val node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "L3",
      id = IdRange(0, 1 << 14)
    ))
  )))

  class OpenLLCImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    node.out.map {
      case (out, edgeOut) =>
        dontTouch(out)
    }

  }
  lazy val module = new OpenLLCImp(this)
}
