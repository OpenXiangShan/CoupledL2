package oceanus.chi

import chisel3._
import chisel3.util.Cat
import oceanus.chi.bundle.CHIBundleDAT
import oceanus.chi.bundle.AbstractCHIBundle
import freechips.rocketchip.util.SeqToAugmentedSeq
import freechips.rocketchip.util.DataToAugmentedData

package object channel {

    /* 
    * Connectors for CHI Channels and CHI Raw Channels
    */
    implicit class connectRawChannelFrom(dst: CHIRawChannel) {

        def :<<=[T <: Data](src: AbstractCHIChannel[T]) = {

            require(dst.channelType == src.channelType,
                s"channel type mismatch: " +
                s"${dst.channelType.canonicalName} :<= ${src.channelType.canonicalName}")

            if (dst.flit.isWidthKnown)
                require(dst.flit.getWidth == src.flit.getWidth,
                    s"width mismatch in raw channel connection: " +
                    s"(dst = ${dst.flit.getWidth}) :<= (src = ${src.flit.getWidth})")

            dst.flitpend    := src.flitpend
            dst.flitv       := src.flitv
            dst.flit        := Cat(src.flit.getElements.map(_.asUInt))
            src.lcrdv       := dst.lcrdv
        }
    }

    implicit class connectRawChannelTo(src: CHIRawChannel) {

        def :>>=[T <: Data](dst: AbstractCHIChannel[T]) = {

            require(dst.channelType == src.channelType,
                s"channel type mismatch: " +
                s"${dst.channelType.canonicalName} :>= ${src.channelType.canonicalName}")

            if (dst.flit.isWidthKnown)
                require(dst.flit.getWidth == src.flit.getWidth,
                    s"width mismatch in raw channel connection: " +
                    s"(dst = ${dst.flit.getWidth}) :>= (src = ${src.flit.getWidth})")

            dst.flitpend    := src.flitpend
            dst.flitv       := src.flitv
            src.lcrdv       := dst.lcrdv

            var scalaLsb = 0
            dst.flit.getElements.reverse.foreach(e => {
                e := src.flit(scalaLsb + e.getWidth - 1, scalaLsb)
                scalaLsb += e.getWidth
            })
        }
    }
}

