package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import oceanus.compactchi.CCHIComponent
import oceanus.l2.L2CacheClientState

object L2SnoopAgent {

  class PathTSHRSnp(client: CCHIComponent) extends Bundle {

    // operands
    val RESP = Output(new L2CacheClientState(client))

    // uOPs
    val SnpMakeInvalid = Input(Bool())
    val SnpToInvalid = Input(Bool())
    val SnpToShared = Input(Bool())
    val SnpToClean = Input(Bool())

    val SnpResp = Output(Bool())
    val SnpRespData0Ptl = Output(Bool())
    val SnpRespData2Ptl = Output(Bool())
  }

  class PathTSHR(clients: Seq[CCHIComponent]) extends Bundle {

    val REQ = clients.map(new PathTSHRSnp(_))
    val REQSnpToUniqueCompAck = Input(Bool())

    val SNP = clients.map(new PathTSHRSnp(_))
    val SNPSnpToUniqueCompAck = Input(Bool())
  }
}

class L2SnoopAgent {

  
}

