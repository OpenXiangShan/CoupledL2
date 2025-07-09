package oceanus.chi.channel

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.bundle._
import oceanus.chi.EnumCHIChannel


/*
* CHI Channel.
*/
class CHIChannel[+T <: AbstractCHIBundle](gen: T, channelType: EnumCHIChannel) 
        extends AbstractCHIChannel[T](gen, channelType)

class CHIRawChannel(gen: UInt, channelType: EnumCHIChannel) 
        extends AbstractCHIChannel[UInt](gen, channelType)


// TX CHI Channel
object CHIChannelTX {
    def apply[T <: AbstractCHIBundle](gen: T, channelType: EnumCHIChannel)
        = new CHIChannel(gen, channelType)
}

// RX CHI Channel
object CHIChannelRX {
    def apply[T <: AbstractCHIBundle](gen: T, channelType: EnumCHIChannel)
        = Flipped(new CHIChannel(gen, channelType))
}


// TX CHI Raw Channel
object CHIRawChannelTX {

    def apply(channelType: EnumCHIChannel)
        = new CHIRawChannel(UInt(), channelType)

    def apply(gen: UInt, channelType: EnumCHIChannel)
        = new CHIRawChannel(gen, channelType)
}

// RX CHI Raw Channel
object CHIRawChannelRX {

    def apply(channelType: EnumCHIChannel)
        = Flipped(new CHIRawChannel(UInt(), channelType))

    def apply(gen: UInt, channelType: EnumCHIChannel)
        = Flipped(new CHIRawChannel(gen, channelType))
}
