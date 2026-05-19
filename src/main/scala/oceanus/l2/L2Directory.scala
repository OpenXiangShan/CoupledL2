package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

object L2Directory {

  object MetaState {

    def apply() = UInt(2.W)

    def UU: UInt = 3.U(2.W)
    def US: UInt = 2.U(2.W)
    def S: UInt = 1.U(2.W)
    def I: UInt = 0.U(2.W)

    def isClientU(state: UInt, client: Bool) = {
      Mux(state === UU, client, false.B)
    }

    def isClientS(state: UInt, client: Bool) = {
      Mux(state === US || state === S, client, false.B)
    }

    def toLocal(state: UInt): L2CacheLocalState = {
      val value = Wire(new L2CacheLocalState)
      value.bits := ParallelLookUp(state, Seq(
        (UU, L2CacheStates.U),
        (US, L2CacheStates.U),
        (S , L2CacheStates.S),
        (I , L2CacheStates.I)
      ))
      value
    }
  }

  class Meta(implicit val p: Parameters) extends Bundle with HasL2Params {
    val way = UInt(4.W) // TODO: parameterize with l2 way count
    val state = MetaState()
    val dirty = Bool()
    val clients = UInt(1.W) // TODO: parameterize with coherent l2 client count
  }

  class Tag extends Bundle {

  }

  class PathToDirectoryUOPs extends Bundle {
    val DirRd = Bool()
    val DirWb = Bool()
    val ReplRd = Bool()
  }

  class PathToDirectory(implicit val p: Parameters) extends PathToDirectoryUOPs with HasL2Params {
    val TSHRADDR = UInt(mshrIndexWidth.W)
    val PADDR = UInt(paramL2.physicalAddrWidth.W)
    val META = Bool() // TODO: replace with meta bundle
  }

  class PathFromDirectoryUOPs extends Bundle {
    val DirRdArbComp = Bool()
    val DirRdResp = Bool()
    val DirWbArbComp = Bool()
    val ReplRdArbComp = Bool()
    val ReplRdResp = Bool()
    val ReplRdRetryAck = Bool()
  }

  class PathFromDirectory(implicit val p: Parameters) extends PathFromDirectoryUOPs with HasL2Params {
    val TSHRADDR = UInt(mshrIndexWidth.W)
    val META = new L2Directory.Meta // TODO: replace with meta bundle
  }
}