package oceanus.chi.channel

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.bundle._
import oceanus.chi.EnumCHIChannel


/*
* CHI DAT Channel.
*/
class CHIChannelDAT[+T <: CHIBundleDAT](gen: T) 
        extends CHIChannel[T](gen, EnumCHIChannel.DAT)

class CHIRawChannelDAT(gen: UInt) 
        extends CHIRawChannel(gen, EnumCHIChannel.DAT)


// TXDAT Channel.
object CHIChannelTXDAT {

    def apply[T <: CHIBundleDAT](gen: T) = new CHIChannelDAT(gen)

    def apply()(implicit p: Parameters) = new CHIChannelDAT(new CHIBundleDAT)
}

// RXDAT Channel.
object CHIChannelRXDAT {

    def apply[T <: CHIBundleDAT](gen: T) = Flipped(new CHIChannelDAT(gen))

    def apply()(implicit p: Parameters) = Flipped(new CHIChannelDAT(new CHIBundleDAT))
}


// Raw TXDAT Channel.
object CHIRawChannelTXDAT {
    def apply()(implicit p: Parameters)
        = new CHIRawChannelDAT(UInt((new CHIBundleDAT).getWidth.W))
}

// Raw RXDAT Channel.
object CHIRawChannelRXDAT {
    def apply()(implicit p: Parameters)
        = Flipped(new CHIRawChannelDAT(UInt((new CHIBundleDAT).getWidth.W)))
}