package oceanus.chi.opcode

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi._
import oceanus.chi.EnumCHIIssue._
import oceanus.chi.EnumCHIChannel._


/* 
* CHI Opcodes permitted for REQ channel of SN-F interface
*/
trait CHISNFOpcodesREQ extends HasCHIParameters {

    //  ==================================================================================================
    val CHI_ReqLCrdReturn               = CHIOpcode(REQ, 0x00, "ReqLCrdReturn"                  , B, C, E)
    //  --------------------------------------------------------------------------------------------------
    val CHI_ReadNoSnp                   = CHIOpcode(REQ, 0x04, "ReadNoSnp"                      , B, C, E)
    val CHI_WriteNoSnpFull              = CHIOpcode(REQ, 0x1D, "WriteNoSnpFull"                 , B, C, E)
    val CHI_WriteNoSnpPtl               = CHIOpcode(REQ, 0x1C, "WriteNoSnpPtl"                  , B, C, E)
    val CHI_WriteNoSnpZero              = CHIOpcode(REQ, 0x44, "WriteNoSnpZero"                       , E)
    //  --------------------------------------------------------------------------------------------------
    val CHI_ReadNoSnpSep                = CHIOpcode(REQ, 0x11, "ReadNoSnpSep"                      , C, E)
    //  --------------------------------------------------------------------------------------------------
    val CHI_CleanShared                 = CHIOpcode(REQ, 0x08, "CleanShared"                    , B, C, E)
    val CHI_CleanSharedPersist          = CHIOpcode(REQ, 0x27, "CleanSharedPersist"             , B, C, E)
    val CHI_CleanSharedPersistSep       = CHIOpcode(REQ, 0x13, "CleanSharedPersistSep"                , E)
    val CHI_CleanInvalid                = CHIOpcode(REQ, 0x09, "CleanInvalid"                   , B, C, E)
    val CHI_MakeInvalid                 = CHIOpcode(REQ, 0x0A, "MakeInvalid"                    , B, C, E)
    //  --------------------------------------------------------------------------------------------------
    val CHI_WriteNoSnpPtlCleanInv       = CHIOpcode(REQ, 0x61, "WriteNoSnpPtlCleanInv"                , E)
    val CHI_WriteNoSnpPtlCleanSh        = CHIOpcode(REQ, 0x60, "WriteNoSnpPtlCleanSh"                 , E)
    val CHI_WriteNoSnpPtlCleanShPerSep  = CHIOpcode(REQ, 0x62, "WriteNoSnpPtlCleanShPerSep"           , E)
    val CHI_WriteNoSnpFullCleanInv      = CHIOpcode(REQ, 0x51, "WriteNoSnpFullCleanInv"               , E)
    val CHI_WriteNoSnpFullCleanSh       = CHIOpcode(REQ, 0x50, "WriteNoSnpFullCleanSh"                , E)
    val CHI_WriteNoSnpFullCleanShPerSep = CHIOpcode(REQ, 0x52, "WriteNoSnpFullCleanShPerSep"          , E)
    //  --------------------------------------------------------------------------------------------------
    val CHI_PCrdReturn                  = CHIOpcode(REQ, 0x05, "PCrdReturn"                     , B, C, E)
    //  --------------------------------------------------------------------------------------------------
    val CHI_AtomicStore_ADD             = CHIOpcode(REQ, 0x28, "AtomicStore.ADD"                , B, C, E)
    val CHI_AtomicStore_CLR             = CHIOpcode(REQ, 0x29, "AtomicStore.CLR"                , B, C, E)
    val CHI_AtomicStore_EOR             = CHIOpcode(REQ, 0x2A, "AtomicStore.EOR"                , B, C, E)
    val CHI_AtomicStore_SET             = CHIOpcode(REQ, 0x2B, "AtomicStore.SET"                , B, C, E)
    val CHI_AtomicStore_SMAX            = CHIOpcode(REQ, 0x2C, "AtomicStore.SMAX"               , B, C, E)
    val CHI_AtomicStore_SMIN            = CHIOpcode(REQ, 0x2D, "AtomicStore.SMIN"               , B, C, E)
    val CHI_AtomicStore_UMAX            = CHIOpcode(REQ, 0x2E, "AtomicStore.UMAX"               , B, C, E)
    val CHI_AtomicStore_UMIN            = CHIOpcode(REQ, 0x2F, "AtomicStore.UMIN"               , B, C, E)
    val CHI_AtomicLoad_ADD              = CHIOpcode(REQ, 0x30, "AtomicLoad.ADD"                 , B, C, E)
    val CHI_AtomicLoad_CLR              = CHIOpcode(REQ, 0x31, "AtomicLoad.CLR"                 , B, C, E)
    val CHI_AtomicLoad_EOR              = CHIOpcode(REQ, 0x32, "AtomicLoad.EOR"                 , B, C, E)
    val CHI_AtomicLoad_SET              = CHIOpcode(REQ, 0x33, "AtomicLoad.SET"                 , B, C, E)
    val CHI_AtomicLoad_SMAX             = CHIOpcode(REQ, 0x34, "AtomicLoad.SMAX"                , B, C, E)
    val CHI_AtomicLoad_SMIN             = CHIOpcode(REQ, 0x35, "AtomicLoad.SMIN"                , B, C, E)
    val CHI_AtomicLoad_UMAX             = CHIOpcode(REQ, 0x36, "AtomicLoad.UMAX"                , B, C, E)
    val CHI_AtomicLoad_UMIN             = CHIOpcode(REQ, 0x37, "AtomicLoad.UMIN"                , B, C, E)
    val CHI_AtomicSwap                  = CHIOpcode(REQ, 0x38, "AtomicSwap"                     , B, C, E)
    val CHI_AtomicCompare               = CHIOpcode(REQ, 0x39, "AtomicCompare"                  , B, C, E)
    //  --------------------------------------------------------------------------------------------------
    val CHI_PrefetchTgt                 = CHIOpcode(REQ, 0x3A, "PrefetchTgt"                    , B, C, E)
    //  ==================================================================================================

    def isAtomic(opcode: CHIOpcode): Boolean 
        = (opcode.opcode >= CHI_AtomicStore_ADD.opcode) && (opcode.opcode <= CHI_AtomicCompare.opcode)

    def isAtomic(opcode: UInt): Bool 
        = (opcode >= CHI_AtomicStore_ADD.U) && (opcode <= CHI_AtomicCompare.U)

    def isAtomicStore(opcode: CHIOpcode): Boolean 
        = (opcode.opcode >= CHI_AtomicStore_ADD.opcode) && (opcode.opcode <= CHI_AtomicStore_UMIN.opcode)

    def isAtomicStore(opcode: UInt): Bool
        = (opcode >= CHI_AtomicStore_ADD.U) && (opcode <= CHI_AtomicStore_UMIN.U)

    def isAtomicLoad(opcode: CHIOpcode): Boolean 
        = (opcode.opcode >= CHI_AtomicLoad_ADD.opcode) && (opcode.opcode <= CHI_AtomicLoad_UMIN.opcode)

    def isAtomicLoad(opcode: UInt): Bool 
        = (opcode >= CHI_AtomicLoad_ADD.U) && (opcode <= CHI_AtomicLoad_UMIN.U)


    /*
    * Decoder for CHI Opcodes of SN-F REQ 
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder}
    */
    class REQOpcodeDecoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(REQ, paramOpcodeSupported, Seq(
        //  ========================
            CHI_ReqLCrdReturn,
        //  ------------------------
            CHI_ReadNoSnp,
            CHI_WriteNoSnpFull,
            CHI_WriteNoSnpPtl,
            CHI_WriteNoSnpZero,
        //  ------------------------
            CHI_ReadNoSnpSep,
        //  ------------------------
            CHI_CleanShared,
            CHI_CleanSharedPersist,
            CHI_CleanSharedPersistSep,
            CHI_CleanInvalid,
            CHI_MakeInvalid,
        //  ------------------------
            CHI_WriteNoSnpPtlCleanInv,
            CHI_WriteNoSnpPtlCleanSh,
            CHI_WriteNoSnpPtlCleanShPerSep,
            CHI_WriteNoSnpFullCleanInv,
            CHI_WriteNoSnpFullCleanSh,
            CHI_WriteNoSnpFullCleanShPerSep,
        //  ------------------------
            CHI_PCrdReturn,
        //  ------------------------
            CHI_AtomicStore_ADD,
            CHI_AtomicStore_CLR,
            CHI_AtomicStore_EOR,
            CHI_AtomicStore_SET,
            CHI_AtomicStore_SMAX,
            CHI_AtomicStore_SMIN,
            CHI_AtomicStore_UMAX,
            CHI_AtomicStore_UMIN,
            CHI_AtomicLoad_ADD,
            CHI_AtomicLoad_CLR,
            CHI_AtomicLoad_EOR,
            CHI_AtomicLoad_SET,
            CHI_AtomicLoad_SMAX,
            CHI_AtomicLoad_SMIN,
            CHI_AtomicLoad_UMAX,
            CHI_AtomicLoad_UMIN,
            CHI_AtomicSwap,
            CHI_AtomicCompare,
        //  ------------------------
            CHI_PrefetchTgt
        //  ========================
        ), paramEnableUnsupportedCheck)
}
