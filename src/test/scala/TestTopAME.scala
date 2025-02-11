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
      case L2ParamKey => L2Param(
        ways                = 8,
        sets                = 512,
        channelBytes        = TLChannelBeatBytes(64),
        blockBytes = 128
      )
      case L2BanksKey => l2_banks
      case L3BanksKey => l3_banks
      case MNumKey => m_num
    })
  }
}

class TestTop_L2L3_AME()(implicit p: Parameters) extends LazyModule {
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
  // 使用 flatMap 生成扁平的 matrix_nodes 序列
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
  val master_nodes = Seq(l1d, l1i) ++ matrix_nodes
  val c_nodes = Seq(l1d)
  val l1i_nodes = Seq(l1i)
  val ul_nodes = l1i_nodes++matrix_nodes
  val l2 = LazyModule(new TL2TLCoupledL2()(baseConfig(1).alter((site, here, up) => {
    case L2ParamKey => L2Param(
      name = s"l2",
      ways = 8,
      sets = 512,
      channelBytes        = TLChannelBeatBytes(32),
      blockBytes          = 64,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2), vaddrBitsOpt = Some(16))),
      echoField = Seq(DirtyField()),
      prefetch = Seq(BOPParameters(
        rrTableEntries = 16,
        rrTagBits = 6
      )),
      // tagECC = Some("secded"),
      // dataECC = Some("secded"),
      enableTagECC = false, //XS use true
      enableDataECC = false, //XS use true
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

  val l3 = LazyModule(new HuanCun()(baseConfig(1).alter((site, here, up) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = "l3",
      level = 3,
      ways = 16,
      sets = 4096,
      inclusive = false,
      clientCaches = Seq(
        CacheParameters(
          name = s"l2",
          sets = 4096,
          ways = 16,
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

  // 连接所有 master_nodes 到 xbar，通过 TLBuffer
//   master_nodes.foreach { node =>
//     l1xbar := TLBuffer() := node
//   }
//   c_nodes.foreach { node =>
//     l1xbar := TLBuffer() := TLLogger(s"L2_L1D[0]", true) := node
//   }
  c_nodes.zipWithIndex map{ case(c,i) =>
        l1xbar := TLBuffer() := TLLogger(s"L2_L1D[${i}]", true) := c
  }
//   ul_nodes.foreach { node =>
//     l1xbar := TLBuffer() := TLLogger(s"L2_Matrix[0]", true) := node
//   }
  l1i_nodes.zipWithIndex map{ case(ul,i) =>
        l1xbar := TLBuffer() := TLLogger(s"L2_L1I[${i}]", true) := ul
  }
  matrix_nodes.zipWithIndex map{ case(ul,i) =>
        l1xbar := TLBuffer() := TLLogger(s"L2_Matrix[${i}]", true) := ul
  }
  // TLLogger(s"L2_L1[${i}].C[${j}]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=
  // l2bankBinders := TLBuffer() := l1xbar
  l2bankBinders :*= l2.node :*= TLBuffer() :*= l1xbar
  l3xbar :=TLBuffer() :*=l2xbar :=*l2bankBinders
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

    // master_nodes.zipWithIndex.foreach {
    //   case (node, i) =>
    //     node.makeIOs()(ValName(s"master_port_$i"))
    // }
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
    }
    l2.module.io.hartId := DontCare
    l2.module.io.debugTopDown <> DontCare
    l2.module.io.l2_tlb_req <> DontCare
    l2.module.io.matrix_valid.valid:= DontCare
    l2.module.io.matrix_valid.bits:= DontCare
    val matrix_valid = l2.module.io.matrix_valid
    // 连接到输出端口
    val matrix_valid_out = IO(Valid(Bool())).suggestName(s"matrix_valid_test")
    matrix_valid_out := matrix_valid

  }

}

  /*  L1D L1I
   *  \  / 
   *   L2 -- (Matrix sends Get/Put)
   *   | 
   *  L3
   */

object TestTop_L2L3_AME extends App {
  val config = baseConfigAME(1,8,8,8).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField())
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
    case MNumKey => 1
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

