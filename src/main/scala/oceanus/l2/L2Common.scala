package oceanus.l2

import chisel3._
import chisel3.util._

class L2LocalDirectoryState extends Bundle {
    val bits = UInt(L2LocalDirectoryState.WIDTH.W)

    def isV: Bool = L2LocalDirectoryState.isV(bits)
    def isU: Bool = L2LocalDirectoryState.isU(bits)
    def isD: Bool = L2LocalDirectoryState.isD(bits)
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
}

object L2ClientState {
    val WIDTH = 2

    def I = "b00".U(WIDTH.W) // Invalid
    def S = "b01".U(WIDTH.W) // Shared
    def U = "b11".U(WIDTH.W) // Unique
}
