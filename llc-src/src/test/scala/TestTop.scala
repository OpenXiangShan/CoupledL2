package openLLC

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.MaxHartIdBits
import org.chipsalliance.cde.config._
import coupledL2._
import coupledL2.tl2chi._
import utility.{FileRegisters, TLLogger, ChiselDB}

class TestTop_L3()(implicit p: Parameters) extends LazyModule {
  override lazy val desiredName: String = "TestTop_L3"

  val l3 = LazyModule(new OpenLLC())

  lazy val module = new LazyModuleImp(this){

  }
}

class TestTop_L2L3()(implicit p: Parameters) extends LazyModule with HasCHIMsgParameters {
  /* L1I    L1D
   *   \    /
   *     L2
   *      |
   *     L3
   */

  override lazy val desiredName: String = "TestTop"
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

  val l1d = createClientNode(s"l1d", 32)
  val l1i = TLClientNode(Seq(
    TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = s"l1i",
        sourceId = IdRange(0, 32)
      ))
    )
  ))
  val master_nodes = Seq(l1d, l1i)

  val l2 = LazyModule(new TL2CHICoupledL2()(new Config((_, _, _) => {
    case L2ParamKey => cacheParams.copy(
      name = s"L2",
      ways = 4,
      sets = 128,
      enableRollingDB = false,
      enablePerf = false,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2)))
    )
    case BankBitsKey => 1
    case EnableCHI => true
    case MaxHartIdBits => 1
  })))

  val l3 = LazyModule(new OpenLLC())

  val l1xbar = TLXbar()

  l1xbar :=
    TLLogger(s"L2_L1D", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=
    TLBuffer() :=
    l1d
  l1xbar :=
    TLLogger(s"L2_L1I", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=
    TLBuffer() :=
    l1i

  l2.managerNode :=
    TLXbar() :=*
    l2.node :=*
    l1xbar

  val mmioClientNode = TLClientNode(Seq(
    TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        "uncache"
      ))
    )
  ))
  l2.mmioBridge.mmioNode := mmioClientNode


  lazy val module = new LazyModuleImp(this) {
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    master_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }

    l2.module.io.hartId := DontCare
    l2.module.io.debugTopDown <> DontCare
    l2.module.io.l2_tlb_req <> DontCare
    l2.module.io_nodeID := 0.U(NODEID_WIDTH.W)
    l2.module.io_chi <> l3.module.io.chi_upwards

    l3.module.io.nodeID := 1.U(NODEID_WIDTH.W)
    l3.module.io.chi_downwards := DontCare
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

object TestTop_L2L3 extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2)))
    )
  })

  ChiselDB.init(true)

  val top = DisableMonitors(p => LazyModule(new TestTop_L2L3()(p)))(config)
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}