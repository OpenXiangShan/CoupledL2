package coupledL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.MaxHartIdBits
import huancun._
import coupledL2.prefetch._
import coupledL2.tl2chi._
import utility._
import scala.collection.mutable.ArrayBuffer

class TestTop_CHIL2(numCores: Int = 1, numULAgents: Int = 0, banks: Int = 1, issue: String = "B")(implicit p: Parameters) extends LazyModule
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
        requestFields = Seq(AliasField(2), VaddrField(36), PrefetchField()),
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
  val l2_nodes = (0 until numCores).map(i => LazyModule(new TL2CHICoupledL2()(new Config((site, here, up) => {
    case L2ParamKey => cacheParams.copy(
      name                = s"L2_$i",
      hartId              = i,
    )
    case EnableCHI => true
    case CHIIssue => issue
    case BankBitsKey => log2Ceil(banks)
    case MaxHartIdBits => log2Up(numCores)
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      i
    )
  }))))

  val bankBinders = (0 until numCores).map(_ => BankBinder(banks, 64))

  val mmio_nodes = l1d_nodes.zip(l2_nodes).zipWithIndex.map { case ((l1d, l2), i) =>
    val l1xbar = TLXbar()
    l1xbar := 
      TLBuffer() :=
      TLLogger(s"L2_L1[${i}].C[0]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) := 
      l1d

    l1i_nodes(i).zipWithIndex.foreach { case (l1i, j) =>
      l1xbar :=
        TLBuffer() :=
        TLLogger(s"L2_L1[${i}].UL[${j}]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=
        l1i
    }
    
    l2.managerNode :=
      TLXbar() :=*
      bankBinders(i) :*=
      l2.node :*=
      l1xbar

    val mmioClientNode = TLClientNode(Seq(
      TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          "uncache"
        ))
      )
    ))
    l2.mmioBridge.mmioNode := mmioClientNode

    mmioClientNode
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

    mmio_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"mmio_port_$i"))
    }

    val io = IO(Vec(numCores, new Bundle() {
      val chi = new PortIO()(p.alterPartial { case CHIIssue => issue })
    }))

    l2_nodes.zipWithIndex.foreach { case (l2, i) =>

      if (!cacheParams.FPGAPlatform && cacheParams.enableCHILog) {
        val chilogger = CHILogger(s"L3_L2[${i}]", issue, true)
        chilogger.io.up <> l2.module.io_chi
        chilogger.io.down <> io(i).chi
      }
      else {
        l2.module.io_chi <> io(i).chi
      }

      dontTouch(l2.module.io)

      l2.module.io.hartId := i.U
      l2.module.io_nodeID := (i + 1).U
      l2.module.io.debugTopDown := DontCare
      l2.module.io.l2_tlb_req <> DontCare
    }
  }
}


object TestTopCHIHelper {
  def gen(fTop: Parameters => TestTop_CHIL2,
          onFPGAPlatform: Boolean,
          enableChiselDB: Boolean,
          enableTLLog: Boolean,
          enableCHILog: Boolean)(args: Array[String]) = {

    val config = new Config((_, _, _) => {
      case L2ParamKey => L2Param(
        ways                = 4,
        sets                = 128,
        clientCaches        = Seq(L1Param(aliasBitsOpt = Some(2), vaddrBitsOpt = Some(36))),
        // echoField        = Seq(DirtyField),
        enablePerf          = false,
        enableRollingDB     = !onFPGAPlatform && enableChiselDB,
        enableMonitor       = !onFPGAPlatform && enableChiselDB,
        enableTLLog         = !onFPGAPlatform && enableChiselDB && enableTLLog,
        enableCHILog        = !onFPGAPlatform && enableChiselDB && enableCHILog,
        elaboratedTopDown   = false,
        FPGAPlatform        = onFPGAPlatform,

        // prefetch
        prefetch            = Seq(BOPParameters()),

        // using external RN-F SAM
        sam                 = Seq(AddressSet.everything -> 0)
      )
    })

    ChiselDB.init(enableChiselDB)
    Constantin.init(false)

    val top = DisableMonitors(p => LazyModule(fTop(p)))(config)

    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => top.module)
    ))

    ChiselDB.addToFileRegisters
    FileRegisters.write("./build")
  }
}


object TestTop_CHIL2 extends App {

  val usage = """
Usage: TestTop_CHIL2 [<--option> <values>]

      --issue <chi_issue>       specify AMBA CHI Issue of downstream, B by default
      --core <core_count>       specify core count, 2 by default
      --tl-ul <tl_ul_count>     specify TileLink-UL upstream count, 0 by default
      --bank <bank_count>       specify bank (slice) count, 4 by default
      --fpga <1>                generate for FPGA platform
      --chiseldb <1>            enable ChisleDB
      --tllog <1>               enable TLLogger under ChiselDB
      --chilog <1>              enable CHILogger under ChiselDB
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(-1)
  }

  var varArgs = ArrayBuffer(args:_*)
  var varArgsDropped = 0

  var numCores: Int = 2
  var numULAgents: Int = 0
  var numBanks: Int = 4
  var issue: String = "B"
  var onFPGAPlatform: Boolean = false
  var enableChiselDB: Boolean = false
  var enableTLLog: Boolean = false
  var enableCHILog: Boolean = false

  val varArgsToDrop = args.sliding(2, 1).zipWithIndex.collect {
    case (Array("--issue", value), i) => (issue = value, i)
    case (Array("--core", value), i) => (numCores = value.toInt, i)
    case (Array("--tl-ul", value), i) => (numULAgents = value.toInt, i)
    case (Array("--bank", value), i) => (numBanks = value.toInt, i)
    case (Array("--fpga", value), i) => (onFPGAPlatform = value.toInt != 0, i)
    case (Array("--chiseldb", value), i) => (enableChiselDB = value.toInt != 0, i)
    case (Array("--tllog", value), i) => (enableTLLog = value.toInt != 0, i)
    case (Array("--chilog", value), i) => (enableCHILog = value.toInt != 0, i)
  }

  varArgsToDrop.map(_._2).foreach(i => {
    varArgs.remove(i - varArgsDropped, 2)
    varArgsDropped = varArgsDropped + 2
  })
  varArgs.trimToSize

  TestTopCHIHelper.gen(
    p => new TestTop_CHIL2(
      numCores,
      numULAgents,
      numBanks,
      issue)(p), 
    onFPGAPlatform,
    enableChiselDB,
    enableTLLog,
    enableCHILog
  )(varArgs.toArray)
}
