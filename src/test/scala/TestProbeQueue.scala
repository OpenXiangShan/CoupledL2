package coupledL2

import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule


import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import scala.collection.mutable.ArrayBuffer
import huancun.DirtyField
import coupledL2.tl2tl._




object TestProbeQueue extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      echoField = Seq(DirtyField())
    )
  })

  val top_coupledl2 = DisableMonitors(p => LazyModule(new TestTop_L2()(p)) )(config)
  chisel3.stage.ChiselStage.elaborate(top_coupledl2.module)

  val arb_args = chisel3.aop.Select.collectDeep[ProbeQueue](top_coupledl2.module){
    case ds: ProbeQueue =>
      ds
  }.head

  (new chisel3.stage.ChiselStage).emitVerilog(new ProbeQueue()(arb_args.p))
}



/*
# To build a signle test Module @XiaBin
mill:
	mill -i CoupledL2.test.runMain coupledL2.TestProbeQueue
	-@mv *.v vsrc/ProbeQueue
	-@rm *.json *.fir
*/