package oceanus.chi.opcode

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi._
import oceanus.chi.EnumCHIIssue._
import oceanus.chi.EnumCHIChannel._


/*
* CHI Opcodes permitted for SNP channel of RN-F interface
*/
trait CHIRNFOpcodesSNP extends HasCHIParameters {

    //  ==========================================================================================
    val SnpLCrdReturn               = CHIOpcode(SNP, 0x00, "SnpLCrdReturn"              , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val SnpShared                   = CHIOpcode(SNP, 0x01, "SnpShared"                  , B, C, E)
    val SnpClean                    = CHIOpcode(SNP, 0x02, "SnpClean"                   , B, C, E)
    val SnpOnce                     = CHIOpcode(SNP, 0x03, "SnpOnce"                    , B, C, E)
    val SnpNotSharedDirty           = CHIOpcode(SNP, 0x04, "SnpNotSharedDirty"          , B, C, E)
    val SnpUnique                   = CHIOpcode(SNP, 0x07, "SnpUnique"                  , B, C, E)
    val SnpPreferUnique             = CHIOpcode(SNP, 0x15, "SnpPreferUnique"                  , E)
    val SnpCleanShared              = CHIOpcode(SNP, 0x08, "SnpCleanShared"             , B, C, E)
    val SnpCleanInvalid             = CHIOpcode(SNP, 0x09, "SnpCleanInvalid"            , B, C, E)
    val SnpMakeInvalid              = CHIOpcode(SNP, 0x0A, "SnpMakeInvalid"             , B, C, E)
    val SnpSharedFwd                = CHIOpcode(SNP, 0x11, "SnpSharedFwd"               , B, C, E)
    val SnpCleanFwd                 = CHIOpcode(SNP, 0x12, "SnpCleanFwd"                , B, C, E)
    val SnpOnceFwd                  = CHIOpcode(SNP, 0x13, "SnpOnceFwd"                 , B, C, E)
    val SnpNotSharedDirtyFwd        = CHIOpcode(SNP, 0x14, "SnpNotSharedDirtyFwd"       , B, C, E)
    val SnpUniqueFwd                = CHIOpcode(SNP, 0x17, "SnpUniqueFwd"               , B, C, E)
    val SnpPreferUniqueFwd          = CHIOpcode(SNP, 0x16, "SnpPreferUniqueFwd"               , E)
    val SnpUniqueStash              = CHIOpcode(SNP, 0x05, "SnpUniqueStash"             , B, C, E)
    val SnpMakeInvalidStash         = CHIOpcode(SNP, 0x06, "SnpMakeInvalidStash"        , B, C, E)
    val SnpStashUnique              = CHIOpcode(SNP, 0x0B, "SnpStashUnique"             , B, C, E)
    val SnpStashShared              = CHIOpcode(SNP, 0x0C, "SnpStashShared"             , B, C, E)
    val SnpQuery                    = CHIOpcode(SNP, 0x10, "SnpQuery"                         , E)
    //  ------------------------------------------------------------------------------------------
    val SnpDVMOp                    = CHIOpcode(SNP, 0x0D, "SnpDVMOp"                   , B, C, E)
    //  ==========================================================================================


    /*
    * Decoder for CHI Opcodes of RN-F SNP
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder} 
    */
    class Decoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(REQ, paramOpcodeSupported, Seq(
        //  ========================
            SnpLCrdReturn,
        //  ------------------------
            SnpShared,
            SnpClean,
            SnpOnce,
            SnpNotSharedDirty,
            SnpUnique,
            SnpPreferUnique,
            SnpCleanShared,
            SnpCleanInvalid,
            SnpMakeInvalid,
            SnpSharedFwd,
            SnpCleanFwd,
            SnpOnceFwd,
            SnpNotSharedDirtyFwd,
            SnpUniqueFwd,
            SnpPreferUniqueFwd,
            SnpUniqueStash,
            SnpMakeInvalidStash,
            SnpStashUnique,
            SnpStashShared,
            SnpQuery,
        //  ------------------------
            SnpDVMOp
        //  ========================
        ), paramEnableUnsupportedCheck)
}
