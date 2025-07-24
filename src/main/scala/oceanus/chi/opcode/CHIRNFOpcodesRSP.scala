package oceanus.chi.opcode

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi._
import oceanus.chi.EnumCHIIssue._
import oceanus.chi.EnumCHIChannel._

/*
* CHI Opcodes permitted for RSP channel of RN-F interface
*/
trait CHIRNFOpcodesRSP extends HasCHIParameters {

    //  ==========================================================================================
    val CHI_RespLCrdReturn          = CHIOpcode(RSP, 0x00, "RespLCrdReturn"             , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_RetryAck                = CHIOpcode(RSP, 0x03, "RetryAck"                   , B, C, E)
    val CHI_DBIDResp                = CHIOpcode(RSP, 0x06, "DBIDResp"                   , B, C, E)
    val CHI_PCrdGrant               = CHIOpcode(RSP, 0x07, "PCrdGrant"                  , B, C, E)
    val CHI_Comp                    = CHIOpcode(RSP, 0x04, "Comp"                       , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_CompDBIDResp            = CHIOpcode(RSP, 0x05, "CompDBIDResp"               , B, C, E)
    val CHI_CompCMO                 = CHIOpcode(RSP, 0x14, "CompCMO"                          , E)
    val CHI_ReadReceipt             = CHIOpcode(RSP, 0x08, "ReadReceipt"                , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_RespSepData             = CHIOpcode(RSP, 0x0B, "RespSepData"                   , C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_DBIDRespOrd             = CHIOpcode(RSP, 0x0E, "DBIDRespOrd"                      , E)
    //  ------------------------------------------------------------------------------------------
    val CHI_StashDone               = CHIOpcode(RSP, 0x10, "StashDone"                        , E)
    val CHI_CompStashDone           = CHIOpcode(RSP, 0x11, "CompStashDone"                    , E)
    //  ------------------------------------------------------------------------------------------
    val CHI_TagMatch                = CHIOpcode(RSP, 0x0A, "TagMatch"                         , E)
    //  ------------------------------------------------------------------------------------------
    val CHI_Persist                 = CHIOpcode(RSP, 0x0C, "Persist"                          , E)
    //  ------------------------------------------------------------------------------------------
    val CHI_CompPersist             = CHIOpcode(RSP, 0x0D, "CompPersist"                      , E)
    //  ------------------------------------------------------------------------------------------
    val CHI_CompAck                 = CHIOpcode(RSP, 0x02, "CompAck"                    , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_SnpResp                 = CHIOpcode(RSP, 0x01, "SnpResp"                    , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_SnpRespFwded            = CHIOpcode(RSP, 0x09, "SnpRespFwded"               , B, C, E)
    //  ==========================================================================================


    /*
    * Decoder for CHI Opcodes of RN-F RSP
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder}
    */
    class RSPOpcodeDecoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(REQ, paramOpcodeSupported, Seq(
        //  ========================
            CHI_RespLCrdReturn,
        //  ------------------------
            CHI_RetryAck,
            CHI_DBIDResp,
            CHI_PCrdGrant,
            CHI_Comp,
        //  ------------------------
            CHI_CompDBIDResp,
            CHI_CompCMO,
            CHI_ReadReceipt,
        //  ------------------------
            CHI_RespSepData,
        //  ------------------------
            CHI_DBIDRespOrd,
        //  ------------------------
            CHI_StashDone,
            CHI_CompStashDone,
        //  ------------------------
            CHI_TagMatch,
        //  ------------------------
            CHI_Persist,
        //  ------------------------
            CHI_CompPersist,
        //  ------------------------
            CHI_CompAck,
        //  ------------------------
            CHI_SnpResp,
        //  ------------------------
            CHI_SnpRespFwded
        //  ========================
        ), paramEnableUnsupportedCheck)
}
