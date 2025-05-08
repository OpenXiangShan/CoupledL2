package coupledL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import huancun._
import coupledL2.prefetch._
import coupledL2.tl2tl._
import utility._


import scala.collection.mutable.ArrayBuffer
import coupledL2.TestTop_CHIL2.enableTLLog

case object L2BanksKey extends Field[Int]
case object L3BanksKey extends Field[Int]
case object MNumKey extends Field[Int]
object baseConfigAME {
  def apply(maxHartIdBits: Int,l2_banks:Int,l3_banks:Int,m_num:Int) = {
    new Config((_, _, _) => {
      case MaxHartIdBits => maxHartIdBits
      case L2ParamKey => L2Param()
      case L2BanksKey => l2_banks
      case L3BanksKey => l3_banks
      case MNumKey => m_num
    })
  }
}

class TestTop_L2L3_AME()(implicit p: Parameters) extends LazyModule {

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
  val l2_banks=p(L2BanksKey)
  val l3_banks=p(L3BanksKey)
  val m_num=p(MNumKey)
  val l1d = createClientNode(s"l1d", 32)
  val l1i = TLClientNode(Seq(
    TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = s"l1i",
        sourceId = IdRange(0, 32)
      ))
    )
  ))

  val matrix_nodes = (0 until 1).flatMap { i =>
    (0 until m_num).map { j =>
      TLClientNode(Seq(
        TLMasterPortParameters.v1(
          clients = Seq(TLMasterParameters.v1(
            name = s"matrix${i}_${j}",
            sourceId = IdRange(0, 32),
            // supportsProbe = TransferSizes(cacheParams.blockBytes)// 缓存一致性管理
            // supportsProbe = TransferSizes.none,
            // supportsProbe = TransferSizes(1,cacheParams.blockBytes),
            // supportsGet = TransferSizes(1,cacheParams.blockBytes),
            // supportsPutFull = TransferSizes(1,cacheParams.blockBytes),
            // supportsPutPartial = TransferSizes(1,cacheParams.blockBytes),
            // requestFields = MatrixField
            )),
            requestFields = Seq(MatrixField(2))
        )
      ))
    }
  }

  val c_nodes = Seq(l1d)
  val l1i_nodes = Seq(l1i)

  // 2MB L2 Cache with 8 banks
  val l2 = LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((site, here, up) => {
    case L2ParamKey => L2Param(
      name = s"l2",
      ways = 8,
      sets = 512,
      channelBytes        = TLChannelBeatBytes(32),
      blockBytes          = 64,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2), vaddrBitsOpt = Some(16))),
      echoField = Seq(DirtyField()),
      // prefetch = Seq(BOPParameters(
      //   rrTableEntries = 16,
      //   rrTagBits = 6
      // )),
      // tagECC = Some("secded"),
      // dataECC = Some("secded"),
      enableTagECC = false, //XS use true
      enableDataECC = false, //XS use true
      enableMCP2 = false,
      // enablePerf = false,
      // dataCheck = Some("oddparity"),
    )
    // case huancun.BankBitsKey => log2Ceil(8)
    case BankBitsKey => log2Ceil(l2_banks)
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      0
    )
  })))

  // 16MB L3 Cache with 2 banks
  val l3 = LazyModule(new HuanCun()(baseConfig(1).alter((site, here, up) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "l3",
      level = 3,
      ways = 16,
      sets = 8192,
      inclusive = false,
      clientCaches = Seq(
        CacheParameters(
          name = s"l2",
          sets = 512 * 2, // l2 sets * 2, actually [l2 sets * 1] is enough for single core
          ways = 8 + 2,   // l2 ways + 2
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
      0
    )
  })))

  val l1xbar = TLXbar()
  val l2xbar = TLXbar()
  val l3xbar = TLXbar()
  val l2bankBinders = BankBinder(l2_banks, 64)
  val l3bankBinders = BankBinder(l3_banks, 64)
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xff_ffffL), beatBytes = 32))


  c_nodes.zipWithIndex map{ case(c,i) =>
        l1xbar := TLBuffer() := TLLogger(s"L2_L1D[${i}]", true) := c
  }

  l1i_nodes.zipWithIndex map{ case(ul,i) =>
        l1xbar := TLBuffer() := TLLogger(s"L2_L1I[${i}]", true) := ul
  }

  matrix_nodes.zipWithIndex map{ case(ul,i) =>
        l1xbar := TLBuffer() := TLLogger(s"L2_Matrix[${i}]", true) := ul
  }

  l2bankBinders :*= l2.node :*= TLBuffer() :*= l1xbar
  l3xbar :*= TLBuffer() :*=l2xbar :=*l2bankBinders
  ram.node :=
    TLXbar() :=
    TLFragmenter(32, 64) :=
    TLCacheCork() :=
    TLClientsMerger() :=
    TLDelayer(delayFactor) :=
    TLLogger(s"MEM_L3", true) :=
    TLXbar() :=*
    l3bankBinders :*=
    l3.node :*= l3xbar

  lazy val module = new LazyModuleImp(this) {
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    c_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
    l1i_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_ul_port_0_${i}"))
    }
    matrix_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_m_port_0_${i}"))

        require(node.out.size == 1)
        val logm = Module(new TLLoggerM(s"L2_Matrix[${i}]", true)(p.alterPartial {
          case EdgeOutKey => node.out.head._2
        }))
        logm.io.a.valid := node.out.head._1.a.valid
        logm.io.a.bits := node.out.head._1.a.bits
        logm.io.mdata_out.valid := l2.module.io.matrixDataOut512L2(i).valid
        logm.io.mdata_out.bits := l2.module.io.matrixDataOut512L2(i).bits
        logm
    }
    l2.module.io.hartId := DontCare
    l2.module.io.debugTopDown <> DontCare
    l2.module.io.l2_tlb_req <> DontCare
    l2.module.io.matrixDataOut512L2:= DontCare
    // For matrix get , l2 return data
    val matrix_data_out = IO(Vec(l2_banks, DecoupledIO(new MatrixDataBundle())))
    matrix_data_out <> l2.module.io.matrixDataOut512L2
  }

}

  /*  L1D L1I
   *  \  / 
   *   L2 -- (Matrix sends Get/Put)
   *   | 
   *  L3
   */

object TestTop_L2L3_AME extends App {
  val l2_banks=8
  val l3_banks=2
  val m_num=l2_banks

  val config = baseConfigAME(1,l2_banks,l3_banks,m_num).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      // enablePerf          = false,
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  ChiselDB.init(true)
  Constantin.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop_L2L3_AME()(p)) )(config)
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}

