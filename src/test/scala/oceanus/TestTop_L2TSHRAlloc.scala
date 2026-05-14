package oceanus

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import utility._
import oceanus.l2._
import oceanus.chi._
import svsim.CommonCompilationSettings.Timescale.Unit.s
import scala.collection.mutable.ArrayBuffer


class TestTop_L2TSHRAlloc(val ccfg: Int)(implicit val p: Parameters) extends Module with HasL2Params {

  val log = IO(new Bundle {
    val dump = Input(Bool())
    val clean = Input(Bool())
  })

  val io = IO(new Bundle {

    val fromTSHR = Flipped(Vec(paramL2.mshrSize, Valid(new L2TSHRAlloc.PathFromTSHR)))
    val toTSHR = Output(Vec(paramL2.mshrSize, new L2TSHRAlloc.PathToTSHR))

    val fromTSHRCtrl = new L2TSHRAlloc.PathFromTSHRCtrl
  })

  val clusterConfigs = Seq(
    Seq(Seq(L2TSHRAllocTarget.EVT, L2TSHRAllocTarget.SNP, L2TSHRAllocTarget.REQ)),
    Seq(Seq(L2TSHRAllocTarget.EVT), Seq(L2TSHRAllocTarget.SNP, L2TSHRAllocTarget.REQ)),
    Seq(Seq(L2TSHRAllocTarget.EVT, L2TSHRAllocTarget.SNP), Seq(L2TSHRAllocTarget.REQ)),
    Seq(Seq(L2TSHRAllocTarget.SNP), Seq(L2TSHRAllocTarget.EVT, L2TSHRAllocTarget.REQ)),
    Seq(Seq(L2TSHRAllocTarget.EVT), Seq(L2TSHRAllocTarget.SNP), Seq(L2TSHRAllocTarget.REQ))
  )

  val config = new L2TSHRAllocConfig(
    cluster = clusterConfigs(ccfg),
    resv = Seq(
      (paramL2.mshrSize - 1, L2TSHRResvTarget.L2EVT),
      (paramL2.mshrSize - 2, L2TSHRResvTarget.L1EVT),
      (paramL2.mshrSize - 3, L2TSHRResvTarget.L3SNP))
  )

  println(s"${config.cluster}")
  println(s"${config.resv}")

  val timer = WireDefault(0.U(64.W))
  val logEnable = WireDefault(false.B)
  val clean = WireDefault(false.B)
  val dump = WireDefault(false.B)

  timer := 0.U
  logEnable := true.B
  clean := log.clean
  dump := log.dump

  dontTouch(timer)
  dontTouch(logEnable)
  dontTouch(clean)
  dontTouch(dump)

  val module = Module(new L2TSHRAlloc(config))

  module.io.fromTSHR := io.fromTSHR
  module.io.fromTSHRCtrl <> io.fromTSHRCtrl

  io.toTSHR := module.io.toTSHR

  //
  XSLog.collect(timer, logEnable, clean, dump)
}

object TestTopFirtoolOptions {
  def apply() = Seq(
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--repl-seq-mem"),
    FirtoolOption("--repl-seq-mem-file=TestTop.sv.conf"),
    FirtoolOption("--lowering-options=explicitBitcast"),
    FirtoolOption("--default-layer-specialization=enable")
  )
}

object TestTop_L2TSHRAlloc extends App {

  val usage = """
Usage: TestTop_CHIL2 [<--option> <values>]

      --ccfg <ccfg_index>       specify clustering config index, 0 by default
        clustering config list:
          0   ((EVT, SNP, REQ))
          1   ((EVT), (SNP, REQ))
          2   ((EVT, SNP), (REQ))
          3   ((SNP), (EVT, REQ))
          4   ((EVT), (SNP), (REQ))
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  var varArgs = ArrayBuffer(args.toIndexedSeq:_*)
  var varArgsDropped = 0

  var ccfg = 0

  val varArgsToDrop = args.sliding(2, 1).zipWithIndex.collect {
    case (Array("--ccfg", value), i) => (ccfg = value.toInt, i)
  }

  varArgsToDrop.map(_._2).foreach(i => {
    varArgs.remove(i - varArgsDropped, 2)
    varArgsDropped = varArgsDropped + 2
  })
  varArgs.trimToSize()

  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params (
      physicalAddrWidth = 48,
      mshrSize = 8
    )
    case CHIParametersKey => CHIParameters (
      issue = EnumCHIIssue.E,
      nodeIdWidth = 11,
      reqAddrWidth = 48,
      dataWidth = 256
    )
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = true,
      fpgaPlatform = false
    )
    case PerfCounterOptionsKey => PerfCounterOptions (
      enablePerfPrint = true,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE,
      0
    )
  })

  (new ChiselStage).execute(varArgs.toArray,
    ChiselGeneratorAnnotation(() => new TestTop_L2TSHRAlloc(ccfg)(config)) +: TestTopFirtoolOptions())
}
