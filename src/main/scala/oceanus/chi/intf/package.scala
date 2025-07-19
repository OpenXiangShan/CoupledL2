package oceanus.chi

package object intf {

    /*
    * Connectors for CHI Interfaces and CHI Raw Interfaces. 
    */
    implicit class connectRawSNFInterfaceFrom(dst: CHISNFRawInterface) {

        def :<<=(src: CHISNFInterface) = {

            dst.rxreq   :>>= src.rxreq
            dst.txrsp   :<<= src.txrsp
            dst.txdat   :<<= src.txdat
            dst.rxdat   :>>= src.rxdat

            src.rxlinkactivereq := dst.rxlinkactivereq
            dst.rxlinkactiveack := src.rxlinkactiveack

            src.txlinkactiveack := dst.txlinkactiveack
            dst.txlinkactivereq := src.txlinkactivereq

            src.rxsactive   := dst.rxsactive
            dst.txsactive   := src.txsactive
        }
    }

    implicit class connectRawRNFInterfaceFrom(dst: CHIRNFRawInterface) {

        def :<<=(src: CHIRNFInterface) = {

            dst.txreq   :<<= src.txreq
            dst.rxsnp   :>>= src.rxsnp
            dst.txrsp   :<<= src.txrsp
            dst.txdat   :<<= src.txdat
            dst.rxrsp   :>>= src.rxrsp
            dst.rxdat   :>>= src.rxdat

            src.rxlinkactivereq := dst.rxlinkactivereq
            dst.rxlinkactiveack := src.rxlinkactiveack

            src.txlinkactiveack := dst.txlinkactiveack
            dst.txlinkactivereq := src.txlinkactivereq

            src.rxsactive   := dst.rxsactive
            dst.txsactive   := src.txsactive
        }
    }

    implicit class connectRawSNFInterfaceTo(src: CHISNFRawInterface) {

        def :>>=(dst: CHISNFInterface) = {

            src.rxreq   :<<= dst.rxreq
            src.txrsp   :>>= dst.txrsp
            src.txdat   :>>= dst.txdat
            src.rxdat   :<<= dst.rxdat

            src.rxlinkactivereq := dst.rxlinkactivereq
            dst.rxlinkactiveack := src.rxlinkactiveack

            src.txlinkactiveack := dst.txlinkactiveack
            dst.txlinkactivereq := src.txlinkactivereq

            src.rxsactive   := dst.rxsactive
            dst.txsactive   := src.txsactive
        }
    }

    implicit class connectRawRNFInterfaceTo(src: CHIRNFRawInterface) {

        def :>>=(dst: CHIRNFInterface) = {

            src.txreq   :>>= dst.txreq
            src.rxsnp   :<<= dst.rxsnp
            src.txrsp   :>>= dst.txrsp
            src.txdat   :>>= dst.txdat
            src.rxrsp   :<<= dst.rxrsp
            src.rxdat   :<<= dst.rxdat

            src.rxlinkactivereq := dst.rxlinkactivereq
            dst.rxlinkactiveack := src.rxlinkactiveack

            src.txlinkactiveack := dst.txlinkactiveack
            dst.txlinkactivereq := src.txlinkactivereq

            src.rxsactive   := dst.rxsactive
            dst.txsactive   := src.txsactive
        }
    }
}
