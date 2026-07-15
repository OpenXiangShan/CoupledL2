package oceanus.compactchi

import chisel3._
import chisel3.util._

class CCHIResp(val value: Int, val name: String) {
  def isPD: Boolean = value & 0b100
  def asUInt: UInt = value.U
  def U = asUInt
}

object CCHIResp {

  val I       = new CCHIResp(0b000, "I")
  val SC      = new CCHIResp(0b001, "SC")
  val UC      = new CCHIResp(0b010, "UC")
  val I_PD    = new CCHIResp(0b100, "I_PD")
  val SC_PD   = new CCHIResp(0b101, "SC_PD")
  val UC_PD   = new CCHIResp(0b110, "UC_PD")

  def isPD(value: UInt): Bool = value & 0b100.U

  def apply() = UInt(3.W)
}
