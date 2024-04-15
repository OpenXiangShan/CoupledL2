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

class TestTop_CHIL2(numCores: Int = 1, numULAgents: Int = 0)(implicit p: Parameters) extends LazyModule {

  /*   L1D(L1I)* L1D(L1I)* ... L1D(L1I)*
   *       \         |          /
   *       L2        L2   ...  L2
   *         \       |        /
   *          \      |       /
   *             CMN or VIP
   */

  override lazy val desiredName: String = "TestTop"
  val delayFactor = 0.5
  val cacheParams = p(L2ParamKey)
  val banks = 1

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

  val l1d_nodes = (0 until numCores).map(i => createClientNode(s"l1d$i", 32))
  val l1i_nodes = (0 until numCores).map {i =>
    (0 until numULAgents).map { j =>
      TLClientNode(Seq(
        TLMasterPortParameters.v1(
          clients = Seq(TLMasterParameters.v1(
            name = s"l1i${i}_${j}",
            sourceId = IdRange(0, 32)
          ))
        )
      ))
    }
  }

  // val l2 = LazyModule(new TL2CHICoupledL2())
  val l2_nodes = (0 until numCores).map(i => LazyModule(new TL2CHICoupledL2()(new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      name = s"l2$i",
      ways = 4,
      sets = 128,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      // echoField = Seq(DirtyField),
      hartIds = Seq{i}
    )
    case EnableCHI => true
    case BankBitsKey => banks
  }).alter(p))))

  // val xbar = TLXbar()
  val bankBinders = (0 until numCores).map(_ => BankBinder(banks, 64))
  // val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

  // for (l1d <- l1d_nodes) {
  //   xbar := TLBuffer() := l1d
  // }
  // l1d_nodes.zip(l2_nodes).foreach { case (l1, l2) => l2 := l1 }

  // ram.node :=
  //   TLXbar() :=*
  //     TLFragmenter(32, 64) :=*
  //     TLCacheCork() :=*
  //     TLDelayer(delayFactor) :=*
  // l2.managerNode :=
  //   TLXbar() :=*
  //   bankBinder :*=
  //   l2.node :*=
  //   xbar

  l1d_nodes.zip(l2_nodes).zipWithIndex.foreach { case ((l1d, l2), i) =>
    val l1xbar = TLXbar()
    l1xbar := l1d
    for (l1i <- l1i_nodes(i)) { l1xbar := l1i }
    l2.managerNode :=
      TLXbar() :=*
      bankBinders(i) :*=
      l2.node :*=
      l1xbar
  }

  lazy val module = new LazyModuleImp(this){
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    l1d_nodes.zipWithIndex.foreach{
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
    if (numULAgents != 0) {
      l1i_nodes.zipWithIndex.foreach { case (core, i) =>
        core.zipWithIndex.foreach { case (node, j) =>
          node.makeIOs()(ValName(s"master_ul_port_${i}_${j}"))
        }
      }
    }

    // val io = IO(new Bundle {
    //   val chi = new PortIO
    // })
    val io = IO(Vec(numCores, new Bundle() {
      val chi = new PortIO
    }))

    // l2.module.io.chi <> io.chi
    // dontTouch(l2.module.io)

    // l2.module.io.hartId := 0.U
    l2_nodes.zipWithIndex.foreach { case (l2, i) =>
      l2.module.io.chi <> io(i).chi
      dontTouch(l2.module.io)

      l2.module.io.hartId := i.U
      l2.module.io.debugTopDown := DontCare
    }
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
      //   AddressSet(0x0L,  0xfffffffbfL) -> 8,
      //   AddressSet(0x40L, 0xfffffffbfL) -> 40
      // )
    )
    case EnableCHI => true
  })

  val top = DisableMonitors(p => LazyModule(new TestTop_CHIL2(numCores = 1)(p)))(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_CHI_DualCore extends App {
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
      //   AddressSet(0x0L,  0xfffffffbfL) -> 8,
      //   AddressSet(0x40L, 0xfffffffbfL) -> 40
      // )
    )
    case EnableCHI => true
  })

  val top = DisableMonitors(p => LazyModule(new TestTop_CHIL2(numCores = 2/*, numULAgents = 1*/)(p)))(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}
