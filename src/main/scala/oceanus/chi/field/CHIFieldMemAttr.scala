package oceanus.chi.field

import chisel3._
import oceanus.chi.bundle.CHIBundleREQ
import oceanus.chi.channel.AbstractCHIChannel


/* 
* MemAttr field helper. 
*/
sealed class EnumCHIFieldMemAttr(ordinal            : Int,
                                 name               : String,
                                 val bitIndex       : Int,
                                 val displayName    : String) 
        extends Enum[EnumCHIFieldMemAttr](name, ordinal) {

    /*
    * Hardware wire extraction. 
    */
    def is(u: UInt)             : Bool  = u(bitIndex)

    def is(req: CHIBundleREQ)   : Bool  = is(req.MemAttr.get)

    def is[T <: CHIBundleREQ](req: AbstractCHIChannel[T]): Bool = is(req.flit)
    /**/
}

object CHIFieldMemAttr {

    // Allocate hint bit offset.
    val BIT_ALLOCATE        : Int   = 3

    // Cacheable bit offset.
    val BIT_CACHEABLE       : Int   = 2

    // Device bit offset.
    val BIT_DEVICE          : Int   = 1

    // Early Write Acknowledge (EWA) bit offset
    val BIT_EWA             : Int   = 0

    /*
    * Enumeration types of MemAttr bits.
    */
    val Allocate            : EnumCHIFieldMemAttr   = new EnumCHIFieldMemAttr(0, "Allocate" , BIT_ALLOCATE  , "Allocate" )
    val Cacheable           : EnumCHIFieldMemAttr   = new EnumCHIFieldMemAttr(1, "Cacheable", BIT_CACHEABLE , "Cacheable")
    val Device              : EnumCHIFieldMemAttr   = new EnumCHIFieldMemAttr(2, "Device"   , BIT_DEVICE    , "Device"   )
    val EWA                 : EnumCHIFieldMemAttr   = new EnumCHIFieldMemAttr(3, "EWA"      , BIT_EWA       , "EWA"      )
    /**/

    /*
    * Check if this MemAttr field bits had Allocate Hint bit set.
    * 
    * @param memAttr Hardware UInt instance of MemAttr field bits 
    * @return Hardware Bool instance of Allocate Hint bit
    */
    def isAllocateHint(memAttr: UInt): Bool = memAttr(BIT_ALLOCATE)

    /*
    * Check if this MemAttr field bits had Cacheable bit set. 
    * 
    * @param memAttr Hardware UInt instance of MemAttr field bits 
    * @return Hardware Bool instance of Cacheable bit
    */
    def isCacheable(memAttr: UInt): Bool = memAttr(BIT_CACHEABLE)

    /*
    * Check if this MemAttr field bits had Device bit set. 
    * 
    * @param memAttr Hardware UInt instance of MemAttr field bits 
    * @return Hardware Bool instance of Device bit
    */
    def isDevice(memAttr: UInt): Bool = memAttr(BIT_DEVICE)

    /*
    * Check if this MemAttr field bits had EWA bit set. 
    * 
    * @param memAttr Hardware UInt instance of MemAttr field bits 
    * @return Hardware Bool instance of EWA bit
    */
    def isEWA(memAttr: UInt): Bool = memAttr(BIT_EWA)
}
