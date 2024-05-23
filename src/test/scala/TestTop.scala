package openLLC

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import org.chipsalliance.cde.config._
import coupledL2._
import coupledL2.tl2chi._

class TestTop_L3()(implicit p: Parameters) extends LazyModule {
  override lazy val desiredName: String = "TestTop_L3"

  val l3 = LazyModule(new OpenLLC())
  val ram = LazyModule(new AXI4RAM(AddressSet(0, 0xffffL), beatBytes = 32))

  ram.node := l3.node

  lazy val module = new LazyModuleImp(this){

  }
}

object TestTop_L3 extends App {
  val config = new Config((_, _, _) => {
    case _ => 0
  })

  val top = DisableMonitors(p => LazyModule(new TestTop_L3()(p)))(config)
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))
}