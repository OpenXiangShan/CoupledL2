package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._

object L2PCreditPool {

  class PathQuery extends Bundle {
    val valid = Bool()
    val pCrdType = UInt(4.W)
    val srcId = UInt(12.W) // TODO: parameterize with CHI node id width
  }

  class PathGrant extends Bundle {
    val grant = Bool()
  }
}

class L2PCreditPool {
  
}
