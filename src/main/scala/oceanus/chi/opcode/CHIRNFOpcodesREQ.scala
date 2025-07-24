package oceanus.chi.opcode

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi._
import oceanus.chi.EnumCHIIssue._
import oceanus.chi.EnumCHIChannel._


/* 
* CHI Opcodes permitted for REQ channel of RN-F interface
*/
trait CHIRNFOpcodesREQ extends HasCHIParameters {

    //  =====================================================================================================
    val CHI_ReqLCrdReturn               = CHIOpcode(REQ, 0x00       , "ReqLCrdReturn"              , B, C, E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_ReadNoSnp                   = CHIOpcode(REQ, 0x04       , "ReadNoSnp"                  , B, C, E)
    val CHI_WriteNoSnpFull              = CHIOpcode(REQ, 0x1D       , "WriteNoSnpFull"             , B, C, E)
    val CHI_WriteNoSnpPtl               = CHIOpcode(REQ, 0x1C       , "WriteNoSnpPtl"              , B, C, E)
    val CHI_WriteNoSnpZero              = CHIOpcode(REQ, 0x04 | 0x40, "WriteNoSnpZero"                   , E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_ReadClean                   = CHIOpcode(REQ, 0x02       , "ReadClean"                  , B, C, E)
    val CHI_ReadShared                  = CHIOpcode(REQ, 0x01       , "ReadShared"                 , B, C, E)
    val CHI_ReadNotSharedDirty          = CHIOpcode(REQ, 0x26       , "ReadNotSharedDirty"         , B, C, E)
    val CHI_ReadUnique                  = CHIOpcode(REQ, 0x07       , "ReadUnqiue"                 , B, C, E)
    val CHI_ReadPreferUnique            = CHIOpcode(REQ, 0x0C | 0x40, "ReadPreferUnique"                 , E)
    val CHI_MakeReadUnique              = CHIOpcode(REQ, 0x01 | 0x40, "MakeReadUnique"                   , E)
    val CHI_CleanUnique                 = CHIOpcode(REQ, 0x0B       , "CleanUnique"                , B, C, E)
    val CHI_MakeUnique                  = CHIOpcode(REQ, 0x0C       , "MakeUnique"                 , B, C, E)
    val CHI_Evict                       = CHIOpcode(REQ, 0x0D       , "Evict"                      , B, C, E)
    val CHI_WriteBackFull               = CHIOpcode(REQ, 0x1B       , "WriteBackFull"              , B, C, E)
    val CHI_WriteBackPtl                = CHIOpcode(REQ, 0x1A       , "WriteBackPtl"               , B, C, E)
    val CHI_WriteEvictFull              = CHIOpcode(REQ, 0x15       , "WriteEvictFull"             , B, C, E)
    val CHI_WriteCleanFull              = CHIOpcode(REQ, 0x17       , "WriteCleanFull"             , B, C, E)
    val CHI_WriteEvictOrEvict           = CHIOpcode(REQ, 0x02 | 0x40, "WriteEvictOrEvict"                , E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_ReadOnce                    = CHIOpcode(REQ, 0x03       , "ReadOnce"                   , B, C, E)
    val CHI_ReadOnceCleanInvalid        = CHIOpcode(REQ, 0x24       , "ReadOnceCleanInvalid"       , B, C, E)
    val CHI_ReadOnceMakeInvalid         = CHIOpcode(REQ, 0x25       , "ReadOnceMakeInvalid"        , B, C, E)
    val CHI_StashOnceUnique             = CHIOpcode(REQ, 0x23       , "StashOnceUnique"            , B, C, E)
    val CHI_StashOnceShared             = CHIOpcode(REQ, 0x22       , "StashOnceShared"            , B, C, E)
    val CHI_StashOnceSepUnique          = CHIOpcode(REQ, 0x08 | 0x40, "StashOnceSepUnique"               , E)
    val CHI_StashOnceSepShared          = CHIOpcode(REQ, 0x07 | 0x40, "StashOnceSepShared"               , E)
    val CHI_WriteUniqueFull             = CHIOpcode(REQ, 0x19       , "WriteUniqueFull"            , B, C, E)
    val CHI_WriteUniqueFullStash        = CHIOpcode(REQ, 0x20       , "WriteUniqueFullStash"       , B, C, E)
    val CHI_WriteUniquePtl              = CHIOpcode(REQ, 0x18       , "WriteUniquePtl"             , B, C, E)
    val CHI_WriteUniquePtlStash         = CHIOpcode(REQ, 0x21       , "WriteUniquePtlStash"        , B, C, E)
    val CHI_WriteUniqueZero             = CHIOpcode(REQ, 0x03 | 0x40, "WriteUniqueZero"                  , E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_CleanShared                 = CHIOpcode(REQ, 0x08       , "CleanShared"                , B, C, E)
    val CHI_CleanSharedPersist          = CHIOpcode(REQ, 0x27       , "CleanSharedPersist"         , B, C, E)
    val CHI_CleanSharedPersistSep       = CHIOpcode(REQ, 0x13       , "CleanSharedPersistSep"            , E)
    val CHI_CleanInvalid                = CHIOpcode(REQ, 0x09       , "CleanInvalid"               , B, C, E)
    val CHI_MakeInvalid                 = CHIOpcode(REQ, 0x0A       , "MakeInvalid"                , B, C, E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_WriteUniquePtlCleanSh       = CHIOpcode(REQ, 0x24 | 0x40, "WriteUniquePtlCleanSh"            , E)
    val CHI_WriteUniquePtlCleanShPerSep = CHIOpcode(REQ, 0x26 | 0x40, "WriteUniquePtlCleanShPerSep"      , E)
    val CHI_WriteUniqueFullCleanSh      = CHIOpcode(REQ, 0x14 | 0x40, "WriteUniqueFullCleanSh"           , E)
    val CHI_WriteUniqueFullCleanShPerSep= CHIOpcode(REQ, 0x16 | 0x40, "WriteUniqueFullCleanShPerSep"     , E)
    val CHI_WriteBackFullCleanInv       = CHIOpcode(REQ, 0x19 | 0x40, "WriteBackFullCleanInv"            , E)
    val CHI_WriteBackFullCleanSh        = CHIOpcode(REQ, 0x18 | 0x40, "WriteBackFullCleanSh"             , E)
    val CHI_WriteBackFullCleanShPerSep  = CHIOpcode(REQ, 0x1A | 0x40, "WriteBackFullCleanShPerSep"       , E)
    val CHI_WriteCleanFullCleanSh       = CHIOpcode(REQ, 0x1C | 0x40, "WriteCleanFullCleanSh"            , E)
    val CHI_WriteCleanFullCleanShPerSep = CHIOpcode(REQ, 0x1E | 0x40, "WriteCleanFullCleanShPerSep"      , E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_WriteNoSnpPtlCleanInv       = CHIOpcode(REQ, 0x21 | 0x40, "WriteNoSnpPtlCleanInv"            , E)
    val CHI_WriteNoSnpPtlCleanSh        = CHIOpcode(REQ, 0x20 | 0x40, "WriteNoSnpPtlCleanSh"             , E)
    val CHI_WriteNoSnpPtlCleanShPerSep  = CHIOpcode(REQ, 0x22 | 0x40, "WriteNoSnpPtlCleanShPerSep"       , E)
    val CHI_WriteNoSnpFullCleanInv      = CHIOpcode(REQ, 0x11 | 0x40, "WriteNoSnpFullCleanInv"           , E)
    val CHI_WriteNoSnpFullCleanSh       = CHIOpcode(REQ, 0x10 | 0x40, "WriteNoSnpFullCleanSh"            , E)
    val CHI_WriteNoSnpFullCleanShPerSep = CHIOpcode(REQ, 0x12 | 0x40, "WriteNoSnpFullCleanShPerSep"      , E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_DVMOp                       = CHIOpcode(REQ, 0x14       , "DVMOp"                      , B, C, E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_PCrdReturn                  = CHIOpcode(REQ, 0x05       , "PCrdReturn"                 , B, C, E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_AtomicStore_ADD             = CHIOpcode(REQ, 0x28       , "AtomicStore.ADD"            , B, C, E)
    val CHI_AtomicStore_CLR             = CHIOpcode(REQ, 0x29       , "AtomicStore.CLR"            , B, C, E)
    val CHI_AtomicStore_EOR             = CHIOpcode(REQ, 0x2A       , "AtomicStore.EOR"            , B, C, E)
    val CHI_AtomicStore_SET             = CHIOpcode(REQ, 0x2B       , "AtomicStore.SET"            , B, C, E)
    val CHI_AtomicStore_SMAX            = CHIOpcode(REQ, 0x2C       , "AtomicStore.SMAX"           , B, C, E)
    val CHI_AtomicStore_SMIN            = CHIOpcode(REQ, 0x2D       , "AtomicStore.SMIN"           , B, C, E)
    val CHI_AtomicStore_UMAX            = CHIOpcode(REQ, 0x2E       , "AtomicStore.UMAX"           , B, C, E)
    val CHI_AtomicStore_UMIN            = CHIOpcode(REQ, 0x2F       , "AtomicStore.UMIN"           , B, C, E)
    val CHI_AtomicLoad_ADD              = CHIOpcode(REQ, 0x30       , "AtomicLoad.ADD"             , B, C, E)
    val CHI_AtomicLoad_CLR              = CHIOpcode(REQ, 0x31       , "AtomicLoad.CLR"             , B, C, E)
    val CHI_AtomicLoad_EOR              = CHIOpcode(REQ, 0x32       , "AtomicLoad.EOR"             , B, C, E)
    val CHI_AtomicLoad_SET              = CHIOpcode(REQ, 0x33       , "AtomicLoad.SET"             , B, C, E)
    val CHI_AtomicLoad_SMAX             = CHIOpcode(REQ, 0x34       , "AtomicLoad.SMAX"            , B, C, E)
    val CHI_AtomicLoad_SMIN             = CHIOpcode(REQ, 0x35       , "AtomicLoad.SMIN"            , B, C, E)
    val CHI_AtomicLoad_UMAX             = CHIOpcode(REQ, 0x36       , "AtomicLoad.UMAX"            , B, C, E)
    val CHI_AtomicLoad_UMIN             = CHIOpcode(REQ, 0x37       , "AtomicLoad.UMIN"            , B, C, E)
    val CHI_AtomicSwap                  = CHIOpcode(REQ, 0x38       , "AtomicSwap"                 , B, C, E)
    val CHI_AtomicCompare               = CHIOpcode(REQ, 0x39       , "AtomicCompare"              , B, C, E)
    //  -----------------------------------------------------------------------------------------------------
    val CHI_PrefetchTgt                 = CHIOpcode(REQ, 0x3A       , "PrefetchTgt"                , B, C, E)
    //  =====================================================================================================

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
    * Decoder for CHI Opcodes of RN-F REQ
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
            CHI_ReadClean,
            CHI_ReadShared,
            CHI_ReadNotSharedDirty,
            CHI_ReadUnique,
            CHI_ReadPreferUnique,
            CHI_MakeReadUnique,
            CHI_CleanUnique,
            CHI_MakeUnique,
            CHI_Evict,
            CHI_WriteBackFull,
            CHI_WriteBackPtl,
            CHI_WriteEvictFull,
            CHI_WriteCleanFull,
            CHI_WriteEvictOrEvict,
        //  ------------------------
            CHI_ReadOnce,
            CHI_ReadOnceCleanInvalid,
            CHI_ReadOnceMakeInvalid,
            CHI_StashOnceUnique,
            CHI_StashOnceShared,
            CHI_StashOnceSepUnique,
            CHI_StashOnceSepShared,
            CHI_WriteUniqueFull,
            CHI_WriteUniqueFullStash,
            CHI_WriteUniquePtl,
            CHI_WriteUniquePtlStash,
            CHI_WriteUniqueZero,
        //  ------------------------
            CHI_CleanShared,
            CHI_CleanSharedPersist,
            CHI_CleanSharedPersistSep,
            CHI_CleanInvalid,
            CHI_MakeInvalid,
        //  ------------------------
            CHI_WriteUniquePtlCleanSh,
            CHI_WriteUniquePtlCleanShPerSep,
            CHI_WriteUniqueFullCleanSh,
            CHI_WriteUniqueFullCleanShPerSep,
            CHI_WriteBackFullCleanInv,
            CHI_WriteBackFullCleanSh,
            CHI_WriteBackFullCleanShPerSep,
            CHI_WriteCleanFullCleanSh,
            CHI_WriteCleanFullCleanShPerSep,
        //  ------------------------
            CHI_WriteNoSnpPtlCleanInv,
            CHI_WriteNoSnpPtlCleanSh,
            CHI_WriteNoSnpPtlCleanShPerSep,
            CHI_WriteNoSnpFullCleanInv,
            CHI_WriteNoSnpFullCleanSh,
            CHI_WriteNoSnpFullCleanShPerSep,
        //  ------------------------
            CHI_DVMOp,
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
