package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import oceanus.l2._
import oceanus.compactchi._

object L2RBE {

    class PathVPipeBlock extends Bundle {
        val EVT = Bool()
        val SNP = Bool()
        val REQ = Bool()
    }
}