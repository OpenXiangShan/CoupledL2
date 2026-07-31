package oceanus.chi.field

import chisel3._

object CHIFieldResp {
  final class RespValue private[CHIFieldResp] (val value: Int, val name: String) {
    def U: UInt = value.U(3.W)
    def isPD: Boolean = (value & 0b100) != 0
  }

  private def resp(value: Int, name: String): RespValue =
    new RespValue(value, name)

  val I: RespValue = resp(0b000, "I")
  val SC: RespValue = resp(0b001, "SC")
  val UC: RespValue = resp(0b010, "UC")
  val I_PD: RespValue = resp(0b100, "I_PD")
  val SC_PD: RespValue = resp(0b101, "SC_PD")
  val UC_PD: RespValue = resp(0b110, "UC_PD")
  val UD_PD: RespValue = UC_PD

  val CopyBackWrData_I: RespValue = I
  val CopyBackWrData_SC: RespValue = SC
  val CopyBackWrData_UC: RespValue = UC
  val CopyBackWrData_UD_PD: RespValue = UD_PD

  val CompData_SC: RespValue = SC
  val CompData_UC: RespValue = UC
  val CompData_UD_PD: RespValue = UD_PD

  val DataSepResp_SC: RespValue = SC
  val DataSepResp_UC: RespValue = UC
  val DataSepResp_UD_PD: RespValue = UD_PD

  def isPD(value: UInt): Bool = value(2)
}
