package oceanus.chi.opcode

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi._
import oceanus.chi.EnumCHIIssue._
import oceanus.chi.EnumCHIChannel._
import oceanus.chi.opcode.CHIOpcodeDecoder


/* 
* CHI Opcodes permitted for RSP channel of SN-F interface
*/
trait CHISNFOpcodesRSP extends HasCHIParameters {

    //  =====================================================================================
    val RespLCrdReturn          = CHIOpcode(RSP, 0x00, "RespLCrdReturn"            , B, C, E)
    //  -------------------------------------------------------------------------------------
    val RetryAck                = CHIOpcode(RSP, 0x03, "RetryAck"                  , B, C, E)
    val PCrdGrant               = CHIOpcode(RSP, 0x07, "PCrdGrant"                 , B, C, E)
    val Comp                    = CHIOpcode(RSP, 0x04, "Comp"                      , B, C, E)
    val CompDBIDResp            = CHIOpcode(RSP, 0x05, "CompDBIDResp"              , B, C, E)
    //  -------------------------------------------------------------------------------------
    val CompCMO                 = CHIOpcode(RSP, 0x14, "CompCMO"                         , E)
    val ReadReceipt             = CHIOpcode(RSP, 0x08, "ReadReceipt"               , B, C, E)
    //  -------------------------------------------------------------------------------------
    val DBIDResp                = CHIOpcode(RSP, 0x06, "DBIDResp"                  , B, C, E)
    //  -------------------------------------------------------------------------------------
    val TagMatch                = CHIOpcode(RSP, 0x0A, "TagMatch"                        , E)
    //  -------------------------------------------------------------------------------------
    val Persist                 = CHIOpcode(RSP, 0x0C, "Persist"                         , E)
    //  -------------------------------------------------------------------------------------
    val CompPersist             = CHIOpcode(RSP, 0x0D, "CompPersist"                     , E)
    //  =====================================================================================


    /*
    * Decoder for CHI Opcodes of SN-F RSP 
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder}
    */
    class Decoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(RSP, paramOpcodeSupported, Seq(
        //  ========================
            RespLCrdReturn,
        //  ------------------------
            RetryAck,
            PCrdGrant,
            Comp,
            CompDBIDResp,
        //  ------------------------
            CompCMO,
            ReadReceipt,
        //  ------------------------
            DBIDResp,
        //  ------------------------
            TagMatch,
        //  ------------------------
            Persist,
        //  ------------------------
            CompPersist
        //  ========================
        ), paramEnableUnsupportedCheck)
}
