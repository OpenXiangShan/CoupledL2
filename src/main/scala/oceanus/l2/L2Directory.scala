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

    def atLeastS(state: UInt): Bool = state >= MetaState.S
    def atLeastU(state: UInt): Bool = state >= MetaState.US

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
    val state = MetaState()
    val dirty = Bool()
    val clients = Vec(1, Bool()) // TODO: parameterize with coherent l2 client count
    val alias = UInt(2.W) // TODO: parameterize with L2 alias width
  }

  class Tag(implicit val p: Parameters) extends Bundle with HasL2Params {
    
  }

  class MetaReadResult(implicit override val p: Parameters) extends Meta with HasL2Params {
    val way = UInt(4.W) // TODO: parameterize with l2 way count
    val hit = Bool()
  }

  class MetaWriteMask(implicit val p: Parameters) extends Bundle with HasL2Params {
    val state = Bool()
    val dirty = Bool()
    val clients = Vec(1, Bool()) // TODO: parameterize with coherent l2 client count

    def any = this.asUInt.orR

    def maskAndWrite(dst: Meta, src: Meta): Unit = {
      when (state) { dst.state := src.state }
      when (dirty) { dst.dirty := src.dirty }
      clients.zip(dst.clients.zip(src.clients)).foreach { case (mask, (dst, src)) => when (mask) { dst := src } }
    }

    def maskAndWrite(dst: Meta, dstFlag: MetaWriteMask, src: Meta): Unit = {
      maskAndWrite(dst, src)
      when (state) { dstFlag.state := true.B }
      when (dirty) { dstFlag.dirty := true.B }
      clients.zip(dstFlag.clients).foreach { case (mask, dst) => when (mask) { dst := true.B } }
    }

    def unmaskAndWrite(dst: Meta, src: Meta): Unit = {
      when (!state) { dst.state := src.state }
      when (!dirty) { dst.dirty := src.dirty }
      clients.zip(dst.clients.zip(src.clients)).foreach { case (mask, (dst, src)) => when (!mask) { dst := src } }
    }
  }

  object MetaWriteMask {
    def empty(implicit p: Parameters): MetaWriteMask = {
      val zero = new MetaWriteMask
      zero.elements.foreach(_._2 := 0.U)
      zero
    }
  }

  class ReplReadResult(implicit override val p: Parameters) extends Tag with HasL2Params {

  }

  class PathToDirectoryUOPs extends Bundle {
    val DirRd = Bool()
    val DirWb = Bool()
    val ReplRd = Bool()
  }

  class PathToDirectory(implicit val p: Parameters) extends PathToDirectoryUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val PADDR = UInt(paramL2.physicalAddrWidth.W)
    val WAY = UInt(4.W) // TODO: parameterize with l2 way count // only applicable for DirWb
    val META = new L2Directory.Meta // only applicable for DirWb
    val META_WEN = new L2Directory.MetaWriteMask // only applicable for DirWb
    val TAG_WEN = Bool()
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
    val TSHRID = UInt(mshrIndexWidth.W)
    val META = new L2Directory.MetaReadResult
    val REPL = new L2Directory.ReplReadResult // only applicable for ReplRd
  }
}