package coupledL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import huancun._
import coupledL2.prefetch._
import utility.{ChiselDB, FileRegisters, TLClientsMerger, TLLogger}

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
        requestFields = Seq(AliasField(2), PrefetchField()),
        responseKeys = cacheParams.respKey
      )
    ))
    masterNode
  }

  val l1d_nodes = (0 until nrL2).map(i => createClientNode(s"l1d$i", 32))
  val l1i_nodes = (0 until nrL2).map(i => TLClientNode(Seq(
    TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = s"l1i$i",
        sourceId = IdRange(0, 32)
      ))
    )
  )))
  val master_nodes = l1d_nodes ++ l1i_nodes

  // ============ L2 ==============
  val l2banks = 4
  val l2size = 1 * 1024 * 1024
  val l2ways = 8
  val l2sets = l2size / l2banks / l2ways / 64

  val l2tpAssoc = 4
  val l2tpTableEntries = l2sets * l2banks * 4

  val coupledL2 = (0 until nrL2).map(i => LazyModule(new CoupledL2()(new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      name = s"l2$i",
      ways = l2ways,
      sets = l2sets,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      hartIds = Seq{i},
      prefetch = Some(TPParameters(tpTableEntries = l2tpTableEntries, tpTableAssoc = l2tpAssoc))
    )
  }))))
  val l2_nodes = coupledL2.map(_.node)
  val l2_binder = (0 until nrL2).map(i => BankBinder(l2banks, 64))

  // ============ L3 ==============
  val l3banks = 4
  val l3size = 16 * 1024 * 1024
  val l3ways = 16
  val l3sets = l3size / l3banks / l3ways / 64

  val l3 = LazyModule(new HuanCun()(new Config((_, _, _) => {
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
  })))
  val l3_binder = BankBinder(l3banks, 64)

  // ============ Misc ==============
  val l1_xbar = (0 until nrL2).map(i => TLXbar())
  val l2_xbar = (0 until nrL2).map(i => TLXbar())
  val l3_xbar = TLXbar()

  val ram = LazyModule(new TLRAM(AddressSet(0, 0xfffffffffL), beatBytes = 32))

  (0 until nrL2).map { i =>
    l1_xbar(i) :=* TLLogger(s"L2_L1D_${i}", true) :=* TLBuffer() :=* l1d_nodes(i)
    l1_xbar(i) :=* TLLogger(s"L2_L1I_${i}", true) :=* TLBuffer() :=* l1i_nodes(i)

    l2_binder(i) :*=
    l2_nodes(i) :*=
    TLBuffer() :*=
    l1_xbar(i)
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
    val timer = IO(Input(UInt(64.W)))
    val logEnable = IO(Input(Bool()))
    val clean = IO(Input(Bool()))
    val dump = IO(Input(Bool()))

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    coupledL2.foreach {
      case l2 => {
        l2.module.io.debugTopDown := DontCare
        l2.module.io.hartId := DontCare
      }
    }

    master_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
  }
}

object TestTop_XSConfig extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      // echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new TestTop_XSConfig()(p)))(config)

  ChiselDB.init(false)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}