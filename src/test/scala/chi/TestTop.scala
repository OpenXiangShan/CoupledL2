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

class TestTop_CHIL2(numCores: Int = 1, numULAgents: Int = 0, banks: Int = 1)(implicit p: Parameters) extends LazyModule
  with HasCHIMsgParameters {

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
      enablePerf = false,
      enableRollingDB = false,
      enableMonitor = false,
      enableTLLog = false,
      elaboratedTopDown = false,
      FPGAPlatform = false,
      // SAM for CMN 2X2 Mesh
      // sam = Seq(
      //   AddressSet(0x0L,  0xfffffffbfL) -> 8,
      //   AddressSet(0x40L, 0xfffffffbfL) -> 40
      // )
      hartIds = (0 until numCores)
    )
    case EnableCHI => true
    case BankBitsKey => log2Ceil(banks)
  }).alter(p))))

  val bankBinders = (0 until numCores).map(_ => BankBinder(banks, 64))

  l1d_nodes.zip(l2_nodes).zipWithIndex.foreach { case ((l1d, l2), i) =>
    val l1xbar = TLXbar()
    l1xbar := 
      TLLogger(s"L2_L1_CORE${i}_TLC", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) := 
      TLBuffer() := l1d

    l1i_nodes(i).zipWithIndex.foreach { case (l1i, j) =>
      l1xbar :=
        TLLogger(s"L2_L1_CORE${i}_TLUL${j}", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=
        TLBuffer() := l1i
    }
    
    l2.managerNode :=
      TLXbar() :=*
      bankBinders(i) :*=
      l2.node :*=
      l1xbar
    /**
      * MMIO: make diplomacy happy
      */
    val mmioClientNode = TLClientNode(Seq(
      TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          "uncache"
        ))
      )
    ))
    l2.mmioBridge.mmioNode := mmioClientNode
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

    val io = IO(Vec(numCores, new Bundle() {
      val chi = new PortIO
    }))

    l2_nodes.zipWithIndex.foreach { case (l2, i) =>
      l2.module.io.chi <> io(i).chi
      dontTouch(l2.module.io)

      l2.module.io.hartId := i.U
      l2.module.io.nodeID := i.U(NODEID_WIDTH.W)
      l2.module.io.debugTopDown := DontCare
    }
  }

}

object TestTop_CHIL2 extends App {
  val banks = 1
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      
    )
    case EnableCHI => true
    case BankBitsKey => log2Ceil(banks)
  })

  val top = DisableMonitors(p => LazyModule(new TestTop_CHIL2(numCores = 1, banks = banks)(p)))(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}


object TestTopCHIHelper {
  def gen(fTop: Parameters => TestTop_CHIL2)(args: Array[String]) = {
    implicit val config = new Config((_, _, _) => {
      case L2ParamKey => L2Param(
        FPGAPlatform = true
      )
    })

    val top = DisableMonitors(p => LazyModule(fTop(p)))

    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => top.module)
    ))

    ChiselDB.init(false)
    ChiselDB.addToFileRegisters
    FileRegisters.write("./build")
  }
}


object TestTop_CHI_DualCore_0UL extends App {

  TestTopCHIHelper.gen(p => new TestTop_CHIL2(
    numCores = 2,
    numULAgents = 0,
    banks = 1)(p)
  )(args)
}

object TestTop_CHI_DualCore_2UL extends App {

  TestTopCHIHelper.gen(p => new TestTop_CHIL2(
    numCores = 2,
    numULAgents = 0,
    banks = 1)(p)
  )(args)
}



object TestTop_CHI_QuadCore_0UL extends App {

  TestTopCHIHelper.gen(p => new TestTop_CHIL2(
    numCores = 4,
    numULAgents = 0,
    banks = 1)(p)
  )(args)
}

object TestTop_CHI_QuadCore_2UL extends App {

  TestTopCHIHelper.gen(p => new TestTop_CHIL2(
    numCores = 4,
    numULAgents = 2,
    banks = 1)(p)
  )(args)
}


object TestTop_CHI_OctaCore_0UL extends App {

  TestTopCHIHelper.gen(p => new TestTop_CHIL2(
    numCores = 8,
    numULAgents = 0,
    banks = 1)(p)
  )(args)
}

object TestTop_CHI_OctaCore_2UL extends App {

  TestTopCHIHelper.gen(p => new TestTop_CHIL2(
    numCores = 8,
    numULAgents = 2,
    banks = 1)(p)
  )(args)
}


object TestTop_CHI_HexaCore_0UL extends App {

  TestTopCHIHelper.gen(p => new TestTop_CHIL2(
    numCores = 16,
    numULAgents = 0,
    banks = 1)(p)
  )(args)
}

object TestTop_CHI_HexaCore_2UL extends App {

  TestTopCHIHelper.gen(p => new TestTop_CHIL2(
    numCores = 16,
    numULAgents = 2,
    banks = 1)(p)
  )(args)
}
