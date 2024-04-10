package coupledL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import huancun._
import coupledL2.prefetch._
import coupledL2.tl2chi._
import utility.{ChiselDB, FileRegisters, TLLogger}

class TestTop_CHIL2()(implicit p: Parameters) extends LazyModule {

  /*   L1D
   *    |
   *   L2
   */

  override lazy val desiredName: String = "TestTop"
  val delayFactor = 0.5
  val cacheParams = p(L2ParamKey)

  def createClientNode(name: String, sources: Int) = {
    val masterNode = TLClientNode(Seq(
      TLMasterPortParameters.v2(
        masters = Seq(
          TLMasterParameters.v1(
            name = name,
            sourceId = IdRange(0, sources),
            supportsProbe = TransferSizes(cacheParams.blockBytes)
          )
        ),
        channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
        minLatency = 1,
        echoFields = Nil,
        requestFields = Seq(AliasField(2)),
        responseKeys = cacheParams.respKey
      )
    ))
    masterNode
  }

  val l1d_nodes = (0 until 1) map( i => createClientNode(s"l1d$i", 128))
  val master_nodes = l1d_nodes

  val l2 = LazyModule(new TL2CHICoupledL2())
  val xbar = TLXbar()
  // val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

  for (l1d <- l1d_nodes) {
    xbar := TLBuffer() := l1d
  }

  // ram.node :=
  //   TLXbar() :=*
  //     TLFragmenter(32, 64) :=*
  //     TLCacheCork() :=*
  //     TLDelayer(delayFactor) :=*
  l2.node :=* xbar

  lazy val module = new LazyModuleImp(this){
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    master_nodes.zipWithIndex.foreach{
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }

    val io = IO(new Bundle {
      val chi = new PortIO
    })

    l2.module.io.chi <> io.chi
    dontTouch(l2.module.io)
  }

}

object TestTop_CHIL2 extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      enablePerf = false,
      enableRollingDB = false,
      enableMonitor = false,
      elaboratedTopDown = false,
      FPGAPlatform = true,
      // SAM for CMN 2X2 Mesh
      // sam = Seq(
      //   AddressSet(0x0L, 0xfffffffbfL) -> 8,
      //   AddressSet(0x0L, 0xfffffffffL) -> 40
      // )
    )
    case EnableCHI => true
  })

  val top = DisableMonitors(p => LazyModule(new TestTop_CHIL2()(p)))(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}
