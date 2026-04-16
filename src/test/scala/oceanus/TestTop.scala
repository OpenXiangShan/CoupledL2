package oceanus

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import utility._
import oceanus.l2._
import oceanus.chi._


class TestTop_L2TSHRAlloc()(implicit val p: Parameters) extends Module with HasL2Params {

  val io = IO(new Bundle {

    val fromTSHR = Flipped(Vec(paramL2.mshrSize, Valid(new PathFromTSHR)))
    val toTSHR = Output(Vec(paramL2.mshrSize, new PathToTSHR))

    val fromTSHRCtrl = new PathFromTSHRCtrl
  })

  val config = new L2TSHRAllocConfig(
    cluster = Seq(Seq(L2TSHRAllocTarget.EVT), Seq(L2TSHRAllocTarget.SNP, L2TSHRAllocTarget.REQ)),
    resv = Seq()
  )

  println(s"${config.cluster}")
  println(s"${config.cluster(0)}")

  val module = Module(new L2TSHRAlloc(config))

  module.io.fromTSHR := io.fromTSHR
  module.io.fromTSHRCtrl <> io.fromTSHRCtrl

  io.toTSHR := module.io.toTSHR
}

object TestTopFirtoolOptions {
  def apply() = Seq(
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--repl-seq-mem"),
    FirtoolOption("--repl-seq-mem-file=TestTop.sv.conf"),
    FirtoolOption("--lowering-options=explicitBitcast")
  )
}

object TestTop_L2TSHRAlloc extends App {

  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params (
      physicalAddrWidth = 48,
      mshrSize = 32
    )
    case CHIParametersKey => CHIParameters (
      issue = EnumCHIIssue.E,
      nodeIdWidth = 11,
      reqAddrWidth = 48,
      dataWidth = 256
    )
  })

  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => new TestTop_L2TSHRAlloc()(config)) +: TestTopFirtoolOptions())
}
