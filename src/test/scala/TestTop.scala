package coupledL2

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}
import coupledL2.debug.TLLogger
import coupledL2.utils.ChiselDB

import scala.collection.mutable.ArrayBuffer

class TestTop_L2()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1D
   *  \    /
   *    L2
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
        requestFields = Nil,
        responseKeys = cacheParams.respKey
      )
    ))
    masterNode
  }

  val l1d_nodes = (0 until 1) map( i => createClientNode(s"l1d$i", 32))
  val l1d_l2_tllog_nodes = (0 until 1) map( i => TLLogger(s"L1D_L2_$i"))
  val master_nodes = l1d_nodes

  val l2 = LazyModule(new CoupledL2())
  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

  l1d_l2_tllog_nodes.zip(l1d_nodes).foreach{
    case (tllogger, l1d) =>
      tllogger := TLBuffer() := l1d
  }
  // for (i <- 0 until 1) {
  //   l1d_l2_tllog_nodes(i) := TLBuffer() := l1d_nodes(i)
  // }

  for (tllogger <- l1d_l2_tllog_nodes) {
    xbar :=* tllogger
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

object TestTop_L2 extends App with HasRocketChipStageUtils {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new TestTop_L2()(p)) )(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))
  ChiselDB.addToElaborationArtefacts
  ElaborationArtefacts.files.foreach{
    case (extension, contents) =>
      val prefix = extension match {
        case "h" | "cpp" => "chisel_db"
      }
      writeOutputFile("./build", s"$prefix.${extension}", contents())
  }
}