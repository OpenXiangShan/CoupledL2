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

    //  ==============================================================================================
    val CHI_SnpLCrdReturn               = CHIOpcode(SNP, 0x00, "SnpLCrdReturn"              , B, C, E)
    //  ----------------------------------------------------------------------------------------------
    val CHI_SnpShared                   = CHIOpcode(SNP, 0x01, "SnpShared"                  , B, C, E)
    val CHI_SnpClean                    = CHIOpcode(SNP, 0x02, "SnpClean"                   , B, C, E)
    val CHI_SnpOnce                     = CHIOpcode(SNP, 0x03, "SnpOnce"                    , B, C, E)
    val CHI_SnpNotSharedDirty           = CHIOpcode(SNP, 0x04, "SnpNotSharedDirty"          , B, C, E)
    val CHI_SnpUnique                   = CHIOpcode(SNP, 0x07, "SnpUnique"                  , B, C, E)
    val CHI_SnpPreferUnique             = CHIOpcode(SNP, 0x15, "SnpPreferUnique"                  , E)
    val CHI_SnpCleanShared              = CHIOpcode(SNP, 0x08, "SnpCleanShared"             , B, C, E)
    val CHI_SnpCleanInvalid             = CHIOpcode(SNP, 0x09, "SnpCleanInvalid"            , B, C, E)
    val CHI_SnpMakeInvalid              = CHIOpcode(SNP, 0x0A, "SnpMakeInvalid"             , B, C, E)
    val CHI_SnpSharedFwd                = CHIOpcode(SNP, 0x11, "SnpSharedFwd"               , B, C, E)
    val CHI_SnpCleanFwd                 = CHIOpcode(SNP, 0x12, "SnpCleanFwd"                , B, C, E)
    val CHI_SnpOnceFwd                  = CHIOpcode(SNP, 0x13, "SnpOnceFwd"                 , B, C, E)
    val CHI_SnpNotSharedDirtyFwd        = CHIOpcode(SNP, 0x14, "SnpNotSharedDirtyFwd"       , B, C, E)
    val CHI_SnpUniqueFwd                = CHIOpcode(SNP, 0x17, "SnpUniqueFwd"               , B, C, E)
    val CHI_SnpPreferUniqueFwd          = CHIOpcode(SNP, 0x16, "SnpPreferUniqueFwd"               , E)
    val CHI_SnpUniqueStash              = CHIOpcode(SNP, 0x05, "SnpUniqueStash"             , B, C, E)
    val CHI_SnpMakeInvalidStash         = CHIOpcode(SNP, 0x06, "SnpMakeInvalidStash"        , B, C, E)
    val CHI_SnpStashUnique              = CHIOpcode(SNP, 0x0B, "SnpStashUnique"             , B, C, E)
    val CHI_SnpStashShared              = CHIOpcode(SNP, 0x0C, "SnpStashShared"             , B, C, E)
    val CHI_SnpQuery                    = CHIOpcode(SNP, 0x10, "SnpQuery"                         , E)
    //  ----------------------------------------------------------------------------------------------
    val CHI_SnpDVMOp                    = CHIOpcode(SNP, 0x0D, "SnpDVMOp"                   , B, C, E)
    //  ==============================================================================================


    /*
    * Decoder for CHI Opcodes of RN-F SNP
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder} 
    */
    class SNPOpcodeDecoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(REQ, paramOpcodeSupported, Seq(
        //  ========================
            CHI_SnpLCrdReturn,
        //  ------------------------
            CHI_SnpShared,
            CHI_SnpClean,
            CHI_SnpOnce,
            CHI_SnpNotSharedDirty,
            CHI_SnpUnique,
            CHI_SnpPreferUnique,
            CHI_SnpCleanShared,
            CHI_SnpCleanInvalid,
            CHI_SnpMakeInvalid,
            CHI_SnpSharedFwd,
            CHI_SnpCleanFwd,
            CHI_SnpOnceFwd,
            CHI_SnpNotSharedDirtyFwd,
            CHI_SnpUniqueFwd,
            CHI_SnpPreferUniqueFwd,
            CHI_SnpUniqueStash,
            CHI_SnpMakeInvalidStash,
            CHI_SnpStashUnique,
            CHI_SnpStashShared,
            CHI_SnpQuery,
        //  ------------------------
            CHI_SnpDVMOp
        //  ========================
        ), paramEnableUnsupportedCheck)
}
