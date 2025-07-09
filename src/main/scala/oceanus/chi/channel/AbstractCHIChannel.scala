package oceanus.chi.channel

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi._

abstract class AbstractCHIChannel[+T <: Data]
        (gen: T, val channelType: EnumCHIChannel) extends Bundle {

    // xFLITPEND    : Flit Pending.
    val flitpend    = Output(Bool())

    // xFLITV       : Flit Valid.
    val flitv       = Output(Bool())

    // xFLIT        : Flit.
    val flit        = Output(gen)

    // xLCRDV       : L-Credit Valid.
    val lcrdv       = Input(Bool())

    // utility functions
    def undirectedChiselType    = Output(chiselTypeOf(this))
}
