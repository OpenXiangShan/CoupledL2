package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.compactchi.CCHIComponent

class L2DSReadMask extends Bundle {
    val S = Bool()
    val U = Bool()

    def test[T <: L2CacheStateTrait](state: T): Bool = {
        S && state.isS || U && state.isU
    }

    def testDirMeta(state: UInt): Bool = {
        test(L2Directory.MetaState.toLocal(state))
    }
}

object L2DSReadMask {

    def never = {
        val value = Wire(new L2DSReadMask)
        value.S := false.B
        value.U := false.B
        value
    }

    def whenS = {
        val value = Wire(new L2DSReadMask)
        value.S := true.B
        value.U := false.B
        value
    }

    def whenU = {
        val value = Wire(new L2DSReadMask)
        value.S := false.B
        value.U := true.B
        value
    }

    def always = {
        val value = Wire(new L2DSReadMask)
        value.S := true.B
        value.U := true.B
        value
    }
}
