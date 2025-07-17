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

    //  ======================================================================================
    val RespLCrdReturn          = CHIOpcode(RSP, 0x00, "RespLCrdReturn"             , B, C, E)
    //  --------------------------------------------------------------------------------------
    val RetryAck                = CHIOpcode(RSP, 0x03, "RetryAck"                   , B, C, E)
    val DBIDResp                = CHIOpcode(RSP, 0x06, "DBIDResp"                   , B, C, E)
    val PCrdGrant               = CHIOpcode(RSP, 0x07, "PCrdGrant"                  , B, C, E)
    val Comp                    = CHIOpcode(RSP, 0x04, "Comp"                       , B, C, E)
    //  --------------------------------------------------------------------------------------
    val CompDBIDResp            = CHIOpcode(RSP, 0x05, "CompDBIDResp"               , B, C, E)
    val CompCMO                 = CHIOpcode(RSP, 0x14, "CompCMO"                          , E)
    val ReadReceipt             = CHIOpcode(RSP, 0x08, "ReadReceipt"                , B, C, E)
    //  --------------------------------------------------------------------------------------
    val RespSepData             = CHIOpcode(RSP, 0x0B, "RespSepData"                   , C, E)
    //  --------------------------------------------------------------------------------------
    val DBIDRespOrd             = CHIOpcode(RSP, 0x0E, "DBIDRespOrd"                      , E)
    //  --------------------------------------------------------------------------------------
    val StashDone               = CHIOpcode(RSP, 0x10, "StashDone"                        , E)
    val CompStashDone           = CHIOpcode(RSP, 0x11, "CompStashDone"                    , E)
    //  --------------------------------------------------------------------------------------
    val TagMatch                = CHIOpcode(RSP, 0x0A, "TagMatch"                         , E)
    //  --------------------------------------------------------------------------------------
    val Persist                 = CHIOpcode(RSP, 0x0C, "Persist"                          , E)
    //  --------------------------------------------------------------------------------------
    val CompPersist             = CHIOpcode(RSP, 0x0D, "CompPersist"                      , E)
    //  --------------------------------------------------------------------------------------
    val CompAck                 = CHIOpcode(RSP, 0x02, "CompAck"                    , B, C, E)
    //  --------------------------------------------------------------------------------------
    val SnpResp                 = CHIOpcode(RSP, 0x01, "SnpResp"                    , B, C, E)
    //  --------------------------------------------------------------------------------------
    val SnpRespFwded            = CHIOpcode(RSP, 0x09, "SnpRespFwded"               , B, C, E)
    //  ======================================================================================


    /*
    * Decoder for CHI Opcodes of RN-F RSP
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder}
    */
    class Decoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(REQ, paramOpcodeSupported, Seq(
        //  ========================
            RespLCrdReturn,
        //  ------------------------
            RetryAck,
            DBIDResp,
            PCrdGrant,
            Comp,
        //  ------------------------
            CompDBIDResp,
            CompCMO,
            ReadReceipt,
        //  ------------------------
            RespSepData,
        //  ------------------------
            DBIDRespOrd,
        //  ------------------------
            StashDone,
            CompStashDone,
        //  ------------------------
            TagMatch,
        //  ------------------------
            Persist,
        //  ------------------------
            CompPersist,
        //  ------------------------
            CompAck,
        //  ------------------------
            SnpResp,
        //  ------------------------
            SnpRespFwded
        //  ========================
        ), paramEnableUnsupportedCheck)
}
