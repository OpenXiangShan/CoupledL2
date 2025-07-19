package oceanus.l2

import chisel3._
import chisel3.util._

class L2LocalDirectoryState extends Bundle {
    val bits = UInt(L2LocalDirectoryState.WIDTH.W)

    def isV: Bool = L2LocalDirectoryState.isV(bits)
    def isU: Bool = L2LocalDirectoryState.isU(bits)
    def isD: Bool = L2LocalDirectoryState.isD(bits)

    def asIntermediateState: L2LocalIntermediateState = {
        val state = Wire(new L2LocalIntermediateState)
        state.bits := L2LocalIntermediateState.fromDirectoryState(bits)
        state
    }

    def ===(that: L2LocalDirectoryState): Bool = bits === that.bits
    def =/=(that: L2LocalDirectoryState): Bool = bits =/= that.bits
}

object L2LocalDirectoryState {
    val WIDTH = 3

    def BIT_VALID  = "b001".U(WIDTH.W) // Valid bit
    def BIT_UNIQUE = "b010".U(WIDTH.W) // Unique bit
    def BIT_DIRTY  = "b100".U(WIDTH.W) // Dirty bit

    def I  = (0.U(WIDTH.W)                      ) // Invalid
    def SC = (BIT_VALID                         ) // Shared Clean
    def UC = (BIT_VALID | BIT_UNIQUE            ) // Unique Clean
    def SD = (BIT_VALID              | BIT_DIRTY) // Shared Dirty
    def UD = (BIT_VALID | BIT_UNIQUE | BIT_DIRTY) // Unique Dirty

    def isV(state: UInt): Bool = (state & BIT_VALID).orR
    def isU(state: UInt): Bool = (state & BIT_UNIQUE).orR
    def isD(state: UInt): Bool = (state & BIT_DIRTY).orR

    def apply(): L2LocalDirectoryState = new L2LocalDirectoryState
}

class L2LocalIntermediateState extends Bundle {
    val bits = UInt(L2LocalIntermediateState.WIDTH.W)

    def isV: Bool = L2LocalIntermediateState.isV(bits)
    def isU: Bool = L2LocalIntermediateState.isU(bits)
    def isD: Bool = L2LocalIntermediateState.isD(bits)
    def isPD: Bool = L2LocalIntermediateState.isPD(bits)

    def asDirectoryState: L2LocalDirectoryState = {
        val state = Wire(new L2LocalDirectoryState)
        state.bits := L2LocalIntermediateState.asDirectoryState(bits)
        state
    }

    def ===(that: L2LocalIntermediateState): Bool = bits === that.bits
    def =/=(that: L2LocalIntermediateState): Bool = bits =/= that.bits
}

object L2LocalIntermediateState {
    val WIDTH = 4

    def BIT_VALID       = "b0001".U(WIDTH.W) // Valid bit
    def BIT_UNIQUE      = "b0010".U(WIDTH.W) // Unique bit
    def BIT_DIRTY       = "b0100".U(WIDTH.W) // Dirty bit
    def BIT_PASSDIRTY   = "b1000".U(WIDTH.W) // Pass dirty bit

    def I     = (0.U(WIDTH.W)                                       ) // Invalid
    def I_PD  = (BIT_PASSDIRTY                                      ) // Invalid with Pass Dirty
    def SC    = (                BIT_VALID                          ) // Shared Clean
    def UC    = (                BIT_VALID | BIT_UNIQUE             ) // Unique Clean
    def SD    = (                BIT_VALID              | BIT_DIRTY ) // Shared Dirty
    def SD_PD = (BIT_PASSDIRTY | BIT_VALID              | BIT_DIRTY ) // Shared Dirty with Pass Dirty
    def UD    = (                BIT_VALID | BIT_UNIQUE | BIT_DIRTY ) // Unique Dirty
    def UD_PD = (BIT_PASSDIRTY | BIT_VALID | BIT_UNIQUE | BIT_DIRTY ) // Unique Dirty with Pass Dirty

    def isV(state: UInt): Bool = (state & BIT_VALID).orR
    def isU(state: UInt): Bool = (state & BIT_UNIQUE).orR
    def isD(state: UInt): Bool = (state & BIT_DIRTY).orR
    def isPD(state: UInt): Bool = (state & BIT_PASSDIRTY).orR

    def asDirectoryState(state: UInt): UInt = state(L2LocalDirectoryState.WIDTH - 1, 0)
    def fromDirectoryState(state: UInt): UInt = {
        require(state.getWidth == L2LocalDirectoryState.WIDTH)
        Cat(0.U((WIDTH - L2LocalDirectoryState.WIDTH).W), state)
    }

    def apply(): L2LocalIntermediateState = new L2LocalIntermediateState
}

class L2ClientState extends Bundle {
    val bits = UInt(L2ClientState.WIDTH.W)

    def isV: Bool = L2ClientState.isV(bits)
    def isU: Bool = L2ClientState.isU(bits)

    def ===(that: L2ClientState): Bool = bits === that.bits
    def =/=(that: L2ClientState): Bool = bits =/= that.bits
}

object L2ClientState {
    val WIDTH = 2

    def BIT_VALID   = "b01".U(WIDTH.W) // Valid bit
    def BIT_UNIQUE  = "b10".U(WIDTH.W) // Unique bit

    def I = (0.U(WIDTH.W)          ) // Invalid
    def S = (BIT_VALID             ) // Shared
    def U = (BIT_VALID | BIT_UNIQUE) // Unique

    def isV(state: UInt): Bool = (state & BIT_VALID).orR
    def isU(state: UInt): Bool = (state & BIT_UNIQUE).orR

    def apply(): L2ClientState = new L2ClientState
}
