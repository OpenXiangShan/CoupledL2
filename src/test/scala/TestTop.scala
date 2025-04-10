package coupledL2

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import huancun._
import coupledL2.prefetch._
import coupledL2.tl2tl._
import utility._
import scala.collection.mutable.ArrayBuffer

object baseConfig {
  def apply(maxHartIdBits: Int) = {
    new Config((_, _, _) => {
      case MaxHartIdBits => maxHartIdBits
    })
  }
}

class TestTop_L2()(implicit p: Parameters) extends LazyModule {

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

  val l1d_nodes = (0 until 1) map( i => createClientNode(s"l1d$i", 32))
  val master_nodes = l1d_nodes

  val l2 = LazyModule(new TL2TLCoupledL2()((baseConfig(1).alter((site, here, up) => {
    case BankBitsKey => 0
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  }))))
  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xff_ffffL), beatBytes = 32))

  for (l1d <- l1d_nodes) {
    xbar := TLBuffer() := l1d
  }

  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
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

    l2.module.io.hartId := DontCare
    l2.module.io.pfCtrlFromCore := DontCare
    l2.module.io.debugTopDown <> DontCare
    l2.module.io.l2_tlb_req <> DontCare
  }

}

class TestTop_L2L3()(implicit p: Parameters) extends LazyModule {
  /* L1I    L1D
   *   \    /
   *     L2
   *      |
   *     L3
   */

  override lazy val desiredName: String = "TestTop"
  val delayFactor = 0.2
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
        requestFields = Seq(AliasField(2), PrefetchField()),
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

  val l2 = LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((site, here, up) => {
    case L2ParamKey => L2Param(
      name = s"l2",
      ways = 4,
      sets = 128,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2), vaddrBitsOpt = Some(16))),
      echoField = Seq(DirtyField()),
      prefetch = Seq(BOPParameters(
        rrTableEntries = 16,
        rrTagBits = 6
      )),
      tagECC = Some("secded"),
      dataECC = Some("secded"),
      enableTagECC = true,
      enableDataECC = true,
      dataCheck = Some("oddparity"),
      enablePoison = true,
    )
    case BankBitsKey => 0
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))

  val l3 = LazyModule(new HuanCun()(baseConfig(1).alter((site, here, up) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "l3",
      level = 3,
      ways = 4,
      sets = 128,
      inclusive = false,
      clientCaches = Seq(
        CacheParameters(
          name = s"l2",
          sets = 128,
          ways = 4 + 2,
          blockGranularity = log2Ceil(128)
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
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))

  val xbar = TLXbar()
  val mem = new AXI4SlaveNode(Seq(new AXI4SlavePortParameters(
    slaves = Seq(new AXI4SlaveParameters(
      address = Seq(new AddressSet(0, 0x7ff_ffffL)),
      supportsWrite = new TransferSizes(1, 64),
      supportsRead = new TransferSizes(1, 64),
      regionType = RegionType.UNCACHED,
      executable = true
    )),
    beatBytes = 32
  )))

  xbar := TLBuffer() := l1i
  xbar := TLBuffer() := l1d

  mem :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(4096) :=
    TLToAXI4() :=*
    TLXbar() :=*
    TLCacheCork() :=*
    TLDelayer(delayFactor) :=*
    l3.node :=*
    TLBuffer() :=
    l2.node :=* xbar

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

    mem.makeIOs()(ValName("mem_axi"))

    l2.module.io.hartId := DontCare
    l2.module.io.pfCtrlFromCore := DontCare
    l2.module.io.debugTopDown <> DontCare
    l2.module.io.l2_tlb_req <> DontCare
  }

}

class TestTop_L2_Standalone()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1D (fake)
   *  \    /
   *    L2
   *    |
   *    L3 (fake, used for tl-test with salve)
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

  def createManagerNode(name: String, sources: Int) = {
    val xfer = TransferSizes(cacheParams.blockBytes, cacheParams.blockBytes)
    val slaveNode = TLManagerNode(Seq(
      TLSlavePortParameters.v1(Seq(
        TLSlaveParameters.v1(
          address          = Seq(AddressSet(0, 0xff_ffffL)),
          regionType       = RegionType.CACHED,
          executable       = true,
          supportsAcquireT = xfer,
          supportsAcquireB = xfer,
          fifoId           = None
        )),
        beatBytes = 32,
        minLatency = 2,
        responseFields = cacheParams.respField,
        requestKeys = cacheParams.reqKey,
        endSinkId = sources
      ))
    )
    slaveNode
  }

  val l1d_nodes = (0 until 1) map( i => createClientNode(s"l1d$i", 32))
  val master_nodes = l1d_nodes

  val l2 = LazyModule(new TL2TLCoupledL2()(new Config((site, here, up) => {
    case BankBitsKey => 0
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))
  val xbar = TLXbar()
  val l3 = createManagerNode("Fake_L3", 16)

  for(i <- 0 until 1) {
    xbar :=* TLBuffer() := l1d_nodes(i)
  }

  l3 :=
    TLBuffer() :=
    TLXbar() :=*
      TLDelayer(delayFactor) :=*
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
    l3.makeIOs()(ValName(s"slave_port"))
    l2.module.io.hartId := DontCare
    l2.module.io.pfCtrlFromCore := DontCare
    l2.module.io.debugTopDown <> DontCare
    l2.module.io.l2_tlb_req <> DontCare
  }
}

class TestTop_L2L3L2()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1D
   *  |     |
   * L2    L2
   *  \    /
   *    L3
   */

  override lazy val desiredName: String = "TestTop"
  val delayFactor = 0.2
  val cacheParams = p(L2ParamKey)

  val nrL2 = 2

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

  val coupledL2 = (0 until nrL2).map(i => LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((site, here, up) => {
    case L2ParamKey => L2Param(
      name = s"l2$i",
      ways = 4,
      sets = 128,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      hartId = i,
      tagECC = Some("secded"),
      dataECC = Some("secded"),
      enableTagECC = true,
      enableDataECC = true,
      dataCheck = Some("oddparity"),
      enablePoison = true,
    )
    case BankBitsKey => 0
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      i
    )
  }))))
  val l2_nodes = coupledL2.map(_.node)

  val l3 = LazyModule(new HuanCun()(baseConfig(1).alter((site, here, up) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "L3",
      level = 3,
      ways = 4,
      sets = 128,
      inclusive = false,
      clientCaches = (0 until nrL2).map(i =>
        CacheParameters(
          name = s"l2",
          sets = 128,
          ways = 4 + 2,
          blockGranularity = log2Ceil(128)
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
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))

  val xbar = TLXbar()
  val mem = new AXI4SlaveNode(Seq(new AXI4SlavePortParameters(
    slaves = Seq(new AXI4SlaveParameters(
      address = Seq(new AddressSet(0, 0x7ff_ffffL)),
      supportsWrite = new TransferSizes(1, 64),
      supportsRead = new TransferSizes(1, 64),
      regionType = RegionType.UNCACHED,
      executable = true
    )),
    beatBytes = 32
  )))

  l1d_nodes.zip(l2_nodes).zipWithIndex map {
    case ((l1d, l2), i) => l2 := 
        TLLogger(s"L2_L1[${i}].C[0]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) := 
        TLBuffer() := l1d
  }

  l2_nodes.zipWithIndex map {
    case(l2, i) => xbar := 
      TLLogger(s"L3_L2[${i}]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) := 
      TLBuffer() := l2
  }

  mem :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(4096) :=
      TLToAXI4() :=*
      TLXbar() :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
      TLLogger(s"MEM_L3", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=*
      l3.node :=* xbar

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
        l2.module.io.debugTopDown <> DontCare
        l2.module.io.hartId := DontCare
        l2.module.io.pfCtrlFromCore := DontCare
        l2.module.io.l2_tlb_req <> DontCare
      }
    }

    master_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }

    mem.makeIOs()(ValName("mem_axi"))
  }
}

// This is not yet applicable, just reference
class TestTop_fullSys()(implicit p: Parameters) extends LazyModule {

  /* L1D L1I L1D L1I (L1I sends Get)
   *  \  /    \  /
   *   L2      L2
   *    \     /
   *       L3
   */

  override lazy val desiredName: String = "TestTop"
  val delayFactor = 0.2
  val cacheParams = p(L2ParamKey)

  val nrL2 = 2

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

  val l2xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xff_ffffL), beatBytes = 32))
  var master_nodes: Seq[TLClientNode] = Seq() // TODO

  (0 until nrL2).map{i =>
    val l1d = createClientNode(s"l1d$i", 32)
    val l1i = TLClientNode(Seq(
      TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          name = s"l1i$i",
          sourceId = IdRange(0, 32)
        ))
      )
    ))
    master_nodes = master_nodes ++ Seq(l1d, l1i) // TODO

    val l1xbar = TLXbar()
    val l2 = LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((site, here, up) => {
      case L2ParamKey => L2Param(
        name = s"l2$i",
        ways = 4,
        sets = 128,
        clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
        echoField = Seq(DirtyField()),
        prefetch = Seq(BOPParameters(
          rrTableEntries = 16,
          rrTagBits = 6
        )),
        tagECC = Some("secded"),
        dataECC = Some("secded"),
        enableTagECC = true,
        enableDataECC = true,
        dataCheck = Some("oddparity"),
        enablePoison = true,
      )
      case BankBitsKey => 0
      case LogUtilsOptionsKey => LogUtilsOptions(
        false,
        here(L2ParamKey).enablePerf,
        here(L2ParamKey).FPGAPlatform
      )
      case PerfCounterOptionsKey => PerfCounterOptions(
        here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
        here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
        XSPerfLevel.withName("VERBOSE"),
        i
      )
    })))

    l1xbar := TLBuffer() := l1i
    l1xbar := TLBuffer() := l1d

    l2xbar := TLBuffer() := l2.node := l1xbar

    InModuleBody {
      l2.module.io.hartId := DontCare
      l2.module.io.pfCtrlFromCore := DontCare
    }
  }

  val l3 = LazyModule(new HuanCun()(baseConfig(1).alter((site, here, up) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "L3",
      level = 3,
      ways = 4,
      sets = 128,
      inclusive = false,
      clientCaches = (0 until nrL2).map(i =>
        CacheParameters(
          name = s"l2",
          sets = 128,
          ways = 4 + 2,
          blockGranularity = log2Ceil(128)
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
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))

  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
      l3.node :=* l2xbar

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
  }
}

private[coupledL2] object TestTopFirtoolOptions {
  def apply() = Seq(
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--repl-seq-mem"),
    FirtoolOption("--repl-seq-mem-file=TestTop.sv.conf"),
    FirtoolOption("--lowering-options=explicitBitcast")
  )
}

object TestTop_L2 extends App {
  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
  })
  ChiselDB.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop_L2()(p)) )(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopFirtoolOptions()
  )

  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_L2_Standalone extends App {
  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
  })
  ChiselDB.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop_L2_Standalone()(p)) )(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopFirtoolOptions()
  )

  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_L2L3 extends App {
  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  ChiselDB.init(true)
  Constantin.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop_L2L3()(p)) )(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopFirtoolOptions()
  )

  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_L2L3L2 extends App {
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

  val top = DisableMonitors(p => LazyModule(new TestTop_L2L3L2()(p)))(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopFirtoolOptions()
  )

  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_fullSys extends App {
  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  ChiselDB.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop_fullSys()(p)))(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopFirtoolOptions()
  )

  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}
