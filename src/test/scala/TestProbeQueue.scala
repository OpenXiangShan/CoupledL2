package coupledL2

import chiseltest._
import freechips.rocketchip.diplomacy.LazyModule

import circt.stage.ChiselStage
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import scala.collection.mutable.ArrayBuffer





object TestProbeQueue extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      echoField = Seq(DirtyField())
    )
  })

  val top_coupledl2 = DisableMonitors(p => LazyModule(new TestTop_L2()(p)) )(config)
  ChiselStage.elaborate(top_coupledl2.module)

  val arb_args = chisel3.aop.Select.collectDeep[ProbeQueue](top_coupledl2.module){
    case ds: ProbeQueue =>
      ds
  }.head

  (new ChiselStage).execute(Array("--target", "verilog") ++ args, Seq(
    ChiselGeneratorAnnotation(() => new ProbeQueue()(arb_args.p))
  ))
}



/*
# To build a signle test Module @XiaBin
mill:
	mill -i CoupledL2.test.runMain coupledL2.TestProbeQueue
	-@mv *.v vsrc/ProbeQueue
	-@rm *.json *.fir
*/