package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import oceanus.l2._
import oceanus.compactchi._

object L2VPipeEVT {

  class PathTSHRPayloadRead(clients: Seq[CCHIComponent]) extends Bundle {
    val p_paddr = UInt(48.W)
    val p_meta_state = new L2CacheState(clients)
  }
}

class L2VPipeEVT(clients: Seq[CCHIComponent]) extends Module {

  val io = IO(new Bundle {
    val flit = Flipped(Valid(new FlitEVT))
    val payloadRead = Input(new L2VPipeEVT.PathTSHRPayloadRead(clients))
  })


}
