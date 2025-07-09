package oceanus.chi.field

import chisel3._
import oceanus.chi.bundle.CHIBundleRSP
import oceanus.chi.bundle.CHIBundleDAT
import oceanus.chi.channel.CHIChannel
import oceanus.chi.field.CHIFieldRespErr.{OK => OK}
import oceanus.chi.field.CHIFieldRespErr.{EXOK => EXOK}
import oceanus.chi.field.CHIFieldRespErr.{DERR => DERR}
import oceanus.chi.field.CHIFieldRespErr.{NDERR => NDERR}


/*
* RespErr field helper. 
*/
sealed class EnumCHIFieldRespErr(ordinal            : Int,
                                 name               : String,
                                 val value          : Int,
                                 val displayName    : String)
        extends Enum[EnumCHIFieldRespErr](name, ordinal) {

    /*
    * Hardware decoder. 
    */
    def is(size: UInt)          : Bool = size === value.U

    def is(rsp: CHIBundleRSP)   : Bool = is(rsp.RespErr.get)
    def is(dat: CHIBundleDAT)   : Bool = is(dat.RespErr.get)
    /**/
}

object CHIFieldRespErr {
    
    /*
    * RespErr field value encodings. 
    */
    val RESPERR_OK              : Int       = 0x00
    val RESPERR_EXOK            : Int       = 0x01
    val RESPERR_DERR            : Int       = 0x02
    val RESPERR_NDERR           : Int       = 0x03

    val OK                      : EnumCHIFieldRespErr   = new EnumCHIFieldRespErr(0, "OK"   , RESPERR_OK   , "Okay")
    val EXOK                    : EnumCHIFieldRespErr   = new EnumCHIFieldRespErr(1, "EXOK" , RESPERR_EXOK , "Exclusive Okay")
    val DERR                    : EnumCHIFieldRespErr   = new EnumCHIFieldRespErr(2, "DERR" , RESPERR_DERR , "Data Error")
    val NDERR                   : EnumCHIFieldRespErr   = new EnumCHIFieldRespErr(3, "NDERR", RESPERR_NDERR, "Non-data Error")
    /**/

    /*
    * RespErr field hardware comparators.
    */
    def isOK    (respErr: UInt): Bool   = respErr === RESPERR_OK   .U
    def isEXOK  (respErr: UInt): Bool   = respErr === RESPERR_EXOK .U
    def isDERR  (respErr: UInt): Bool   = respErr === RESPERR_DERR .U
    def isNDERR (respErr: UInt): Bool   = respErr === RESPERR_NDERR.U
    /**/
}

object EnumCHIFieldRespErr {

    //
    def allElements: Seq[EnumCHIFieldRespErr]
        = Seq(OK, EXOK, DERR, NDERR)

    def all = allElements.toBuffer
}
