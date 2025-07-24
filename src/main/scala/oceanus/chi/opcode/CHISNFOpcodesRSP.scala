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

    //  =========================================================================================
    val CHI_RespLCrdReturn          = CHIOpcode(RSP, 0x00, "RespLCrdReturn"            , B, C, E)
    //  -------------------------------------------------------------------------------------
    val CHI_RetryAck                = CHIOpcode(RSP, 0x03, "RetryAck"                  , B, C, E)
    val CHI_PCrdGrant               = CHIOpcode(RSP, 0x07, "PCrdGrant"                 , B, C, E)
    val CHI_Comp                    = CHIOpcode(RSP, 0x04, "Comp"                      , B, C, E)
    val CHI_CompDBIDResp            = CHIOpcode(RSP, 0x05, "CompDBIDResp"              , B, C, E)
    //  -----------------------------------------------------------------------------------------
    val CHI_CompCMO                 = CHIOpcode(RSP, 0x14, "CompCMO"                         , E)
    val CHI_ReadReceipt             = CHIOpcode(RSP, 0x08, "ReadReceipt"               , B, C, E)
    //  -----------------------------------------------------------------------------------------
    val CHI_DBIDResp                = CHIOpcode(RSP, 0x06, "DBIDResp"                  , B, C, E)
    //  -----------------------------------------------------------------------------------------
    val CHI_TagMatch                = CHIOpcode(RSP, 0x0A, "TagMatch"                        , E)
    //  -----------------------------------------------------------------------------------------
    val CHI_Persist                 = CHIOpcode(RSP, 0x0C, "Persist"                         , E)
    //  -----------------------------------------------------------------------------------------
    val CHI_CompPersist             = CHIOpcode(RSP, 0x0D, "CompPersist"                     , E)
    //  =========================================================================================


    /*
    * Decoder for CHI Opcodes of SN-F RSP 
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder}
    */
    class RSPOpcodeDecoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(RSP, paramOpcodeSupported, Seq(
        //  ========================
            CHI_RespLCrdReturn,
        //  ------------------------
            CHI_RetryAck,
            CHI_PCrdGrant,
            CHI_Comp,
            CHI_CompDBIDResp,
        //  ------------------------
            CHI_CompCMO,
            CHI_ReadReceipt,
        //  ------------------------
            CHI_DBIDResp,
        //  ------------------------
            CHI_TagMatch,
        //  ------------------------
            CHI_Persist,
        //  ------------------------
            CHI_CompPersist
        //  ========================
        ), paramEnableUnsupportedCheck)
}
