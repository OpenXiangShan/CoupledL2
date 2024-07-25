package coupledL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import huancun._
import coupledL2.prefetch._
import coupledL2.tl2tl._
import utility._

class TestTop_XSConfig()(implicit p: Parameters) extends LazyModule {

  /* L1D   (L1D)
   *  |      |
   * L2    (L2)
   *  \     /
   *     L3
   */

  override lazy val desiredName: String = "TestTop"
  val delayFactor = 0.2
  val cacheParams = p(L2ParamKey)

  val nrL2 = 1

  // ============ L1 ==============
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

  val l1d_nodes = (0 until nrL2).map(i => createClientNode(s"l1d$i", 32))
  val master_nodes = l1d_nodes

  // ============ L2 ==============
  val l2banks = 4
  val l2size = 1 * 1024 * 1024
  val l2ways = 8
  val l2sets = l2size / l2banks / l2ways / 64

  // TODO: customized TP params
  val l2tpAssoc = 4
  val l2tpTableEntries = l2sets * l2banks * 4

  val coupledL2 = (0 until nrL2).map(i => LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((site, here, up) => {
    case L2ParamKey => L2Param(
      name = s"l2$i",
      ways = l2ways,
      sets = l2sets,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      hartId = i,
      enableRollingDB = false,
      prefetch = Nil
      // prefetch = Seq(TPParameters(tpTableEntries = l2tpTableEntries, tpTableAssoc = l2tpAssoc))
    )
    case BankBitsKey => log2Ceil(l2banks)
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
  val l2_nodes = coupledL2.map(_.node)
  val l2_binder = (0 until nrL2).map(i => BankBinder(l2banks, 64))

  // ============ L3 ==============
  val l3banks = 4
  val l3size = 16 * 1024 * 1024
  val l3ways = 16
  val l3sets = l3size / l3banks / l3ways / 64

  val l3 = LazyModule(new HuanCun()(baseConfig(1).alter((site, here, up) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "L3",
      level = 3,
      ways = l3ways,
      sets = l3sets,
      inclusive = false,
      clientCaches = (0 until nrL2).map(i =>
        CacheParameters(
          name = s"l2",
          sets = 2 * nrL2 * l2sets,
          ways = l2ways + 2,
          blockGranularity = log2Ceil(l2sets)
        ),
      ),
      echoField = Seq(DirtyField()),
      simulation = true
    )
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(HCCacheParamsKey).enableDebug,
      here(HCCacheParamsKey).enablePerf,
      here(HCCacheParamsKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(HCCacheParamsKey).enablePerf && !here(HCCacheParamsKey).FPGAPlatform,
      false,
      0
    )
  })))
  val l3_binder = BankBinder(l3banks, 64)

  // ============ Misc ==============
  val l1_xbar = (0 until nrL2).map(i => TLXbar())
  val l2_xbar = (0 until nrL2).map(i => TLXbar())
  val l3_xbar = TLXbar()

  val ram = LazyModule(new TLRAM(AddressSet(0, 0xfffffffffL), beatBytes = 32))

  l1d_nodes.zip(l2_nodes).zipWithIndex map {
    case ((l1d, l2), i) =>
      l2_binder(i) :*=
      l2 :*=
      TLBuffer() :*=
      l1_xbar(i) :=*
      TLLogger(s"L2_L1_${i}", true) :=*
      l1d
  }

  l2_nodes.zipWithIndex map {
    case(l2, i) =>
      l3_xbar :=
      TLLogger(s"L3_L2_${i}", true) :=
      TLBuffer() :*=
      l2_xbar(i) :=*
      l2_binder(i)
  }

  ram.node :=
    TLXbar() :=
      TLFragmenter(32, 64) :=
      TLCacheCork() :=
      TLClientsMerger() :=
      TLDelayer(delayFactor) :=
      TLLogger(s"MEM_L3", true) :=
      TLXbar() :=*
      l3_binder :*=
      l3.node :*= l3_xbar

  lazy val module = new LazyModuleImp(this) {
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    coupledL2.foreach {
      case l2 => {
        l2.module.io.debugTopDown := DontCare
        l2.module.io.hartId := DontCare
        l2.module.io.l2_tlb_req <> DontCare
      }
    }

    master_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
  }
}

object TestTop_XSConfig extends App {
  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      // echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  ChiselDB.init(true)
  Constantin.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop_XSConfig()(p)))(config)
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}