package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

object L2Directory {

  class PathToDirectory(implicit val p: Parameters) extends Bundle with HasL2Params {

    val TSHRADDR = UInt(mshrIndexWidth.W)
    val PADDR = UInt(paramL2.physicalAddrWidth.W)
    val META = Bool() // TODO: replace with meta bundle

    val DirRd = Bool()
    val DirWb = Bool()
    val ReplRd = Bool()
  }

  class PathFromDirectory(implicit val p: Parameters) extends Bundle with HasL2Params {

    val TSHRADDR = UInt(mshrIndexWidth.W)
    val META = Bool() // TODO: replace with meta bundle

    val DirRdArbComp = Bool()
    val DirRdResp = Bool()

    val DirWbArbComp = Bool()

    val ReplRdArbComp = Bool()
    val ReplRdResp = Bool()
    val ReplRdRetryAck = Bool()
  }
}