package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import oceanus.compactchi.CCHIComponent
import oceanus.l2.L2CacheClientState

object L2SnoopAgent {

  class PathToSnoopAgentUOPs extends Bundle {
    val SnpMakeInvalid = Bool()
    val SnpToInvalid = Bool()
    val SnpToShared = Bool()
    val SnpToClean = Bool()

    val SnpCompAck = Bool()
  }

  class PathFromSnoopAgentUOPs extends Bundle {
    val SnpResp = Bool()
    val SnpRespData0 = Bool()
    val SnpRespData2 = Bool()
  }

  class PathToSnoopAgent extends PathToSnoopAgentUOPs {
    val CLIENTS = Vec(1, Bool())  
    val ALIAS = UInt(2.W)
  }

  class PathFromSnoopAgent extends PathFromSnoopAgentUOPs {
    val PASSDIRTY = Bool()
  }
}

class L2SnoopAgent {

}
