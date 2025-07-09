package oceanus.chi.channel

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.bundle._
import oceanus.chi.EnumCHIChannel


/*
* CHI SNP Channel.
*/
class CHIChannelSNP[+T <: CHIBundleSNP](gen: T)
        extends CHIChannel[T](gen, EnumCHIChannel.SNP)

class CHIRawChannelSNP(gen: UInt)
        extends CHIRawChannel(gen, EnumCHIChannel.SNP)


// TXSNP Channel.
object CHIChannelTXSNP {

    def apply[T <: CHIBundleSNP](gen: T) = new CHIChannelSNP(gen)

    def apply()(implicit p: Parameters) = new CHIChannelSNP(new CHIBundleSNP)
}

// RXSNP Channel.
object CHIChannelRXSNP {
    
    def apply[T <: CHIBundleSNP](gen: T) = Flipped(new CHIChannelSNP(gen))

    def apply()(implicit p: Parameters) = Flipped(new CHIChannelSNP(new CHIBundleSNP))
}


// Raw TXSNP Channel.
object CHIRawChannelTXSNP {
    def apply()(implicit p: Parameters)
        = new CHIRawChannelSNP(UInt((new CHIBundleSNP).getWidth.W))
}

// Raw RXSNP Channel.
object CHIRawChannelRXSNP {
    def apply()(implicit p: Parameters)
        = Flipped(new CHIRawChannelSNP(UInt((new CHIBundleSNP).getWidth.W)))
}

