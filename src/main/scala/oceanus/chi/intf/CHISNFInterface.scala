package oceanus.chi.intf

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.channel._
import oceanus.chi.HasCHIParameters


/*
* CHI SN-F node interface. 
*/
class CHISNFInterface(implicit val p: Parameters) extends Bundle with HasCHIParameters {

    // RXREQ
    val rxreq               = CHIChannelRXREQ()

    // TXRSP
    val txrsp               = CHIChannelTXRSP()

    // TXDAT
    val txdat               = CHIChannelTXDAT()

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
        val raw = Wire(CHISNFRawInterface())
        raw :<<= this
        raw
    }

    def asToRaw(raw: CHISNFRawInterface) = {
        raw :<<= this
        raw
    }

    def asFromRaw = {
        val raw = Wire(CHISNFRawInterface())
        raw :>>= this
        raw
    }

    def asFromRaw(raw: CHISNFRawInterface) = {
        raw :>>= this
        raw
    }
}

object CHISNFInterface {
    def apply()(implicit p: Parameters) = new CHISNFInterface
}


// Raw interface.
class CHISNFRawInterface(implicit val p: Parameters) extends Bundle with HasCHIParameters {

    // RXREQ
    val rxreq               = CHIRawChannelRXREQ()

    // TXRSP
    val txrsp               = CHIRawChannelTXRSP()

    // TXDAT
    val txdat               = CHIRawChannelTXDAT()

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

object CHISNFRawInterface {
    def apply()(implicit p: Parameters) = new CHISNFRawInterface
}