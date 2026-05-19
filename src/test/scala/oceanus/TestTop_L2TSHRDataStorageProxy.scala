package oceanus

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import utility._
import oceanus.l2._
import oceanus.chi._

class TestTop_L2TSHRDataStorageProxy(implicit val p: Parameters) extends Module with HasL2Params {

  val module = Module(new L2TSHRDataStorageProxy(0))

  val io = IO(module.io.cloneType)

  io <> module.io

  //
  val log = IO(new Bundle {
    val dump = Input(Bool())
    val clean = Input(Bool())
  })

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

  XSLog.collect(timer, logEnable, clean, dump)
}

object TestTop_L2TSHRDataStorageProxy extends App {

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

  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => new TestTop_L2TSHRDataStorageProxy()(config)) +: TestTopFirtoolOptions())
}

