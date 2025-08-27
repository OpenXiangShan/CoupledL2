package oceanus.chi

import chisel3._
import chisel3.util._

object CHICohResps {

    val WIDTH = 3

    def I       = "b000".U(WIDTH.W) // Invalid
    def SC      = "b001".U(WIDTH.W) // Shared Clean
    def UC      = "b010".U(WIDTH.W) // Unique Clean
    def UD      = "b010".U(WIDTH.W) // Unique Dirty
    def SD      = "b011".U(WIDTH.W) // Shared Dirty
    def I_PD    = "b100".U(WIDTH.W) // Invalid, Pass Dirty
    def SC_PD   = "b101".U(WIDTH.W) // Shared Clean, Pass Dirty
    def UC_PD   = "b110".U(WIDTH.W) // Unique Clean, Pass Dirty
    def SD_PD   = "b111".U(WIDTH.W) // Shared Dirty, Pass Dirty
}
