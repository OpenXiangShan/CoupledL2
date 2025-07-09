package oceanus.chi.field

import chisel3._
import oceanus.chi.field.CHIFieldOrder.{NoOrdering => NoOrdering}
import oceanus.chi.field.CHIFieldOrder.{RequestAccepted => RequestAccepted}
import oceanus.chi.field.CHIFieldOrder.{RequestOrder => RequestOrder}
import oceanus.chi.field.CHIFieldOrder.{EndpointOrder => EndpointOrder}
import oceanus.chi.bundle.CHIBundleREQ
import oceanus.chi.channel.AbstractCHIChannel


/*
* Order field helper. 
*/
sealed class EnumCHIFieldOrder(ordinal              : Int,
                               name                 : String,
                               val value            : Int,
                               val displayName      : String)
        extends Enum[EnumCHIFieldOrder](name, ordinal) {

    /*
    * Hardware decoder. 
    */
    def is(size: UInt)          : Bool  = size === value.U

    def is(req: CHIBundleREQ)   : Bool  = is(req.Order.get)

    def is[T <: CHIBundleREQ](req: AbstractCHIChannel[T]): Bool = is(req.flit)
    /**/
}

object CHIFieldOrder {

    /*
    * Order field value encodings.
    */
    val NO_ORDERING             : Int       = 0x00
    val REQUEST_ACCEPTED        : Int       = 0x01
    val REQUEST_ORDER           : Int       = 0x02
    val ENDPOINT_ORDER          : Int       = 0x03

    val NoOrdering              : EnumCHIFieldOrder = new EnumCHIFieldOrder(0, "NoOrdering"     , NO_ORDERING       , "No Ordering")
    val RequestAccepted         : EnumCHIFieldOrder = new EnumCHIFieldOrder(1, "RequestAccepted", REQUEST_ACCEPTED  , "Request Accepted")
    val RequestOrder            : EnumCHIFieldOrder = new EnumCHIFieldOrder(2, "RequestOrder"   , REQUEST_ORDER     , "Request Order / OWO")
    val EndpointOrder           : EnumCHIFieldOrder = new EnumCHIFieldOrder(3, "EndpointOrder"  , ENDPOINT_ORDER    , "Endpoint Order")
    /**/

    /*
    * Order field hardware comparators. 
    */
    def isNoOrdering                (order: UInt): Bool = order === NO_ORDERING.U
    def isRequestAccepted           (order: UInt): Bool = order === REQUEST_ACCEPTED.U
    def isRequestOrder              (order: UInt): Bool = order === REQUEST_ORDER.U
    def isEndpointOrder             (order: UInt): Bool = order === ENDPOINT_ORDER.U

    def isOrderedWriteObervation    (order: UInt): Bool = order === REQUEST_ORDER.U
    def isOWO                       (order: UInt): Bool = isOrderedWriteObervation(order)
    /**/
}

object EnumCHIFieldOrder {

    //
    def allElements: Seq[EnumCHIFieldOrder] 
        = Seq(NoOrdering, RequestAccepted, RequestOrder, EndpointOrder)

    def all = allElements.toBuffer
}
