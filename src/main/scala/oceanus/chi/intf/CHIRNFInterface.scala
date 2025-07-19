package oceanus.chi.intf

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.channel._
import oceanus.chi.HasCHIParameters

/*
* CHI SN-F node interface. 
*/
class CHIRNFInterface(implicit val p: Parameters) extends Bundle with HasCHIParameters {

    // TXREQ
    val txreq               = CHIChannelTXREQ()

    // RXSNP
    val rxsnp               = CHIChannelRXSNP()

    // TXRSP
    val txrsp               = CHIChannelTXRSP()

    // TXDAT
    val txdat               = CHIChannelTXDAT()

    // RXRSP
    val rxrsp               = CHIChannelRXRSP()

    // RXDAT
    val rxdat               = CHIChannelRXDAT()

    // RXLINKACTIVE
    val rxlinkactivereq     = Input(Bool())
    val rxlinkactiveack     = Output(Bool())

    // TXLINKACTIVE
    val txlinkactivereq     = Output(Bool())
    val txlinkactiveack     = Input(Bool())

    // SACTIVE
    val txsactive           = Output(Bool())
    val rxsactive           = Input(Bool())


    // utility functions - convert to raw interface
    def asToRaw = {
        val raw = Wire(CHIRNFRawInterface())
        raw :<<= this
        raw
    }

    def awToRaw(raw: CHIRNFRawInterface) = {
        raw :<<= this
        raw
    }
    
    def asFromRaw = {
        val raw = Wire(CHIRNFRawInterface())
        raw :>>= this
        raw
    }

    def asFromRaw(raw: CHIRNFRawInterface) = {
        raw :>>= this
        raw
    }
}

object CHIRNFInterface {
    def apply()(implicit p: Parameters) = new CHIRNFInterface
}


// Raw interface.
class CHIRNFRawInterface(implicit val p: Parameters) extends Bundle with HasCHIParameters {

    // TXREQ
    val txreq               = CHIRawChannelTXREQ()

    // RXSNP
    val rxsnp               = CHIRawChannelRXSNP()

    // TXRSP
    val txrsp               = CHIRawChannelTXRSP()

    // TXDAT
    val txdat               = CHIRawChannelTXDAT()

    // RXRSP
    val rxrsp               = CHIRawChannelRXRSP()

    // RXDAT
    val rxdat               = CHIRawChannelRXDAT()
    
    // RXLINKACTIVE
    val rxlinkactivereq     = Input(Bool())
    val rxlinkactiveack     = Output(Bool())

    // TXLINKACTIVE
    val txlinkactivereq     = Output(Bool())
    val txlinkactiveack     = Input(Bool())

    // SACTIVE
    val txsactive           = Output(Bool())
    val rxsactive           = Input(Bool())
}

object CHIRNFRawInterface {
    def apply()(implicit p: Parameters) = new CHIRNFRawInterface
}
