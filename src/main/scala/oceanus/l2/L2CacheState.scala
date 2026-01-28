package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.compactchi.CCHIComponent

object L2CacheStates {
    def U: UInt = 2.U(2.W)
    def S: UInt = 1.U(2.W)
    def I: UInt = 0.U(2.W)
}

trait L2CacheStateTrait {
    def norm: UInt

    def satisfy(state: UInt) = norm >= state
    def satisfy(state: L2CacheStateTrait) = norm >= state.norm
    def satU = norm >= L2CacheStates.U
    def satS = norm >= L2CacheStates.S
    def satI = norm >= L2CacheStates.I

    def is(state: UInt) = norm == state
    def is(state: L2CacheStateTrait) = norm == state.norm
    def isU = norm == L2CacheStates.U
    def isS = norm == L2CacheStates.S
    def isI = norm == L2CacheStates.I

    def notI = norm =/= L2CacheStates.I

    def surpass(state: UInt) = norm > state
    def surpass(state: L2CacheStateTrait) = norm > state.norm
}

class L2CacheLocalState extends Bundle with L2CacheStateTrait {
    val state = UInt(2.W)

    def norm = state
}

class L2CacheClientState(val client: CCHIComponent) extends Bundle with L2CacheStateTrait {
    require(client.componentType.coherent, "Non-coherent component boasts no client state")
    val state = if (client.componentType.coherentUnique) UInt(2.W) else UInt(1.W)

    def hasUnique = client.componentType.coherentUnique
    def norm = if (hasUnique) state else Cat(0.U(1.W), state)
}

class L2CacheState(val clientComponents: Seq[CCHIComponent]) extends Bundle {
    val local = new L2CacheLocalState
    val clients = VecInit(clientComponents.map(new L2CacheClientState(_)))
}
