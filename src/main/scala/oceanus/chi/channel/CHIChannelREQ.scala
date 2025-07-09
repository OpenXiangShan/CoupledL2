package oceanus.chi.channel

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.bundle._
import oceanus.chi.EnumCHIChannel


/*
* CHI REQ Channel.
*/
class CHIChannelREQ[+T <: CHIBundleREQ](gen: T)
        extends CHIChannel[T](gen, EnumCHIChannel.REQ)

class CHIRawChannelREQ(gen: UInt)
        extends CHIRawChannel(gen, EnumCHIChannel.REQ)


// TXREQ Channel.
object CHIChannelTXREQ {

    def apply[T <: CHIBundleREQ](gen: T) = new CHIChannelREQ(gen)

    def apply()(implicit p: Parameters) = new CHIChannelREQ(new CHIBundleREQ)
}

// RXREQ Channel.
object CHIChannelRXREQ {

    def apply[T <: CHIBundleREQ](gen: T) = Flipped(new CHIChannelREQ(gen))

    def apply()(implicit p: Parameters) = Flipped(new CHIChannelREQ(new CHIBundleREQ))
}


// Raw TXREQ Channel.
object CHIRawChannelTXREQ {
    def apply()(implicit p: Parameters)
        = new CHIRawChannelREQ(UInt((new CHIBundleREQ).getWidth.W))
}

// Raw RXREQ Channel.
object CHIRawChannelRXREQ {
    def apply()(implicit p: Parameters)
        = Flipped((new CHIRawChannelREQ(UInt((new CHIBundleREQ).getWidth.W))))
}
