package coupledL2

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import huancun._
import coupledL2.prefetch._
import utility.{ChiselDB, FileRegisters}

import scala.collection.mutable.ArrayBuffer

class TestTop_L2()(implicit p: Parameters) extends LazyModule {

  /*   L1D
   *    | 
   *   L2
   */

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

  val l2 = LazyModule(new CoupledL2())
  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

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
    master_nodes.zipWithIndex.foreach{
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
  }

}

class TestTop_L2L3()(implicit p: Parameters) extends LazyModule {
  /* L1I    L1D
   *   \    /
   *     L2
   *      |
   *     L3
   */
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

  val l2 = LazyModule(new CoupledL2()(new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      name = s"l2",
      ways = 4,
      sets = 128,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      prefetch = Some(BOPParameters(
        rrTableEntries = 16,
        rrTagBits = 6
      ))
    )
  }))).node

  val l3 = LazyModule(new HuanCun()(new Config((_, _, _) => {
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
          ways = 4,
          blockGranularity = log2Ceil(128)
        ),
      ),
      echoField = Seq(DirtyField()),
      simulation = true
    )
  })))

  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

  xbar := TLBuffer() := l1i
  xbar := TLBuffer() := l1d

  ram.node :=
    TLXbar() :=*
    TLFragmenter(32, 64) :=*
    TLCacheCork() :=*
    TLDelayer(delayFactor) :=*
    l3.node :=*
    TLBuffer() :=
    l2 :=* xbar

  lazy val module = new LazyModuleImp(this) {
    master_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
  }

}

class TestTop_L2_Standalone()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1D (fake)
   *  \    /
   *    L2
   *    |
   *    L3 (fake, used for tl-test with salve)
   */

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
        echoFields = cacheParams.echoField,
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
          address          = Seq(AddressSet(0, 0xffffL)),
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

  val l2 = LazyModule(new CoupledL2())
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
    master_nodes.zipWithIndex.foreach{
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
    l3.makeIOs()(ValName(s"slave_port"))
  }

}

class TestTop_L2L3L2()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1D
   *  |     |
   * L2    L2
   *  \    /
   *    L3
   */

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

  val l2_nodes = (0 until nrL2).map(i => LazyModule(new CoupledL2()(new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      name = s"l2$i",
      ways = 4,
      sets = 128,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
  }))).node)

  val l3 = LazyModule(new HuanCun()(new Config((_, _, _) => {
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
          ways = 4,
          blockGranularity = log2Ceil(128)
        ),
      ),
      echoField = Seq(DirtyField()),
      simulation = true
    )
  })))

  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

  l1d_nodes.zip(l2_nodes).map {
    case (l1d, l2) => l2 := TLBuffer() := l1d
  }

  for (l2 <- l2_nodes) {
    xbar := TLBuffer() := l2
  }

  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
      l3.node :=* xbar

  lazy val module = new LazyModuleImp(this) {
    master_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
  }
}

class TestTop_fullSys()(implicit p: Parameters) extends LazyModule {

  /* L1D L1I L1D L1I (L1I sends Get)
   *  \  /    \  /
   *   L2      L2
   *    \     /
   *       L3
   */

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
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))
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
    val l2node = LazyModule(new CoupledL2()(new Config((_, _, _) => {
      case L2ParamKey => L2Param(
        name = s"l2$i",
        ways = 4,
        sets = 128,
        clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
        echoField = Seq(DirtyField()),
        prefetch = Some(BOPParameters(
          rrTableEntries = 16,
          rrTagBits = 6
        ))
      )
    }))).node

    l1xbar := TLBuffer() := l1i
    l1xbar := TLBuffer() := l1d

    l2xbar := TLBuffer() := l2node := l1xbar
  }

  val l3 = LazyModule(new HuanCun()(new Config((_, _, _) => {
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
          ways = 4,
          blockGranularity = log2Ceil(128)
        ),
      ),
      echoField = Seq(DirtyField()),
      simulation = true
    )
  })))

  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
      l3.node :=* l2xbar

  lazy val module = new LazyModuleImp(this) {
    master_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
  }
}

object TestTop_L2 extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new TestTop_L2()(p)) )(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_L2_Standalone extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new TestTop_L2_Standalone()(p)) )(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_L2L3 extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new TestTop_L2L3()(p)) )(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_L2L3L2 extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
     // echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new TestTop_L2L3L2()(p)))(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}

object TestTop_fullSys extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new TestTop_fullSys()(p)))(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}