package oceanus.chi.channel

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.bundle._
import oceanus.chi.EnumCHIChannel


/*
* CHI RSP Channel.
*/
class CHIChannelRSP[+T <: CHIBundleRSP](gen: T)
        extends CHIChannel[T](gen, EnumCHIChannel.RSP)

class CHIRawChannelRSP(gen: UInt)
        extends CHIRawChannel(gen, EnumCHIChannel.RSP)


// TXRSP Channel.
object CHIChannelTXRSP {

    def apply[T <: CHIBundleRSP](gen: T) = new CHIChannelRSP(gen)

    def apply()(implicit p: Parameters) = new CHIChannelRSP(new CHIBundleRSP)
}

// RXRSP Channel.
object CHIChannelRXRSP {
    
    def apply[T <: CHIBundleRSP](gen: T) = Flipped(new CHIChannelRSP(gen))

    def apply()(implicit p: Parameters) = Flipped(new CHIChannelRSP(new CHIBundleRSP))
}


// Raw TXRSP Channel.
object CHIRawChannelTXRSP {
    def apply()(implicit p: Parameters)
        = new CHIRawChannelRSP(UInt((new CHIBundleRSP).getWidth.W))
}

// Raw RXRSP Channel.
object CHIRawChannelRXRSP {
    def apply()(implicit p: Parameters)
        = Flipped(new CHIRawChannelRSP(UInt((new CHIBundleRSP).getWidth.W)))
}
