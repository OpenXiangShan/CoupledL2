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

    //  =================================================================================================
    val ReqLCrdReturn               = CHIOpcode(REQ, 0x00       , "ReqLCrdReturn"              , B, C, E)
    //  -------------------------------------------------------------------------------------------------
    val ReadNoSnp                   = CHIOpcode(REQ, 0x04       , "ReadNoSnp"                  , B, C, E)
    val WriteNoSnpFull              = CHIOpcode(REQ, 0x1D       , "WriteNoSnpFull"             , B, C, E)
    val WriteNoSnpPtl               = CHIOpcode(REQ, 0x1C       , "WriteNoSnpPtl"              , B, C, E)
    val WriteNoSnpZero              = CHIOpcode(REQ, 0x04 | 0x40, "WriteNoSnpZero"                   , E)
    //  -------------------------------------------------------------------------------------------------
    val ReadClean                   = CHIOpcode(REQ, 0x02       , "ReadClean"                  , B, C, E)
    val ReadShared                  = CHIOpcode(REQ, 0x01       , "ReadShared"                 , B, C, E)
    val ReadNotSharedDirty          = CHIOpcode(REQ, 0x26       , "ReadNotSharedDirty"         , B, C, E)
    val ReadUnique                  = CHIOpcode(REQ, 0x07       , "ReadUnqiue"                 , B, C, E)
    val ReadPreferUnique            = CHIOpcode(REQ, 0x0C | 0x40, "ReadPreferUnique"                 , E)
    val MakeReadUnique              = CHIOpcode(REQ, 0x01 | 0x40, "MakeReadUnique"                   , E)
    val CleanUnique                 = CHIOpcode(REQ, 0x0B       , "CleanUnique"                , B, C, E)
    val MakeUnique                  = CHIOpcode(REQ, 0x0C       , "MakeUnique"                 , B, C, E)
    val Evict                       = CHIOpcode(REQ, 0x0D       , "Evict"                      , B, C, E)
    val WriteBackFull               = CHIOpcode(REQ, 0x1B       , "WriteBackFull"              , B, C, E)
    val WriteBackPtl                = CHIOpcode(REQ, 0x1A       , "WriteBackPtl"               , B, C, E)
    val WriteEvictFull              = CHIOpcode(REQ, 0x15       , "WriteEvictFull"             , B, C, E)
    val WriteCleanFull              = CHIOpcode(REQ, 0x17       , "WriteCleanFull"             , B, C, E)
    val WriteEvictOrEvict           = CHIOpcode(REQ, 0x02 | 0x40, "WriteEvictOrEvict"                , E)
    //  -------------------------------------------------------------------------------------------------
    val ReadOnce                    = CHIOpcode(REQ, 0x03       , "ReadOnce"                   , B, C, E)
    val ReadOnceCleanInvalid        = CHIOpcode(REQ, 0x24       , "ReadOnceCleanInvalid"       , B, C, E)
    val ReadOnceMakeInvalid         = CHIOpcode(REQ, 0x25       , "ReadOnceMakeInvalid"        , B, C, E)
    val StashOnceUnique             = CHIOpcode(REQ, 0x23       , "StashOnceUnique"            , B, C, E)
    val StashOnceShared             = CHIOpcode(REQ, 0x22       , "StashOnceShared"            , B, C, E)
    val StashOnceSepUnique          = CHIOpcode(REQ, 0x08 | 0x40, "StashOnceSepUnique"               , E)
    val StashOnceSepShared          = CHIOpcode(REQ, 0x07 | 0x40, "StashOnceSepShared"               , E)
    val WriteUniqueFull             = CHIOpcode(REQ, 0x19       , "WriteUniqueFull"            , B, C, E)
    val WriteUniqueFullStash        = CHIOpcode(REQ, 0x20       , "WriteUniqueFullStash"       , B, C, E)
    val WriteUniquePtl              = CHIOpcode(REQ, 0x18       , "WriteUniquePtl"             , B, C, E)
    val WriteUniquePtlStash         = CHIOpcode(REQ, 0x21       , "WriteUniquePtlStash"        , B, C, E)
    val WriteUniqueZero             = CHIOpcode(REQ, 0x03 | 0x40, "WriteUniqueZero"                  , E)
    //  -------------------------------------------------------------------------------------------------
    val CleanShared                 = CHIOpcode(REQ, 0x08       , "CleanShared"                , B, C, E)
    val CleanSharedPersist          = CHIOpcode(REQ, 0x27       , "CleanSharedPersist"         , B, C, E)
    val CleanSharedPersistSep       = CHIOpcode(REQ, 0x13       , "CleanSharedPersistSep"            , E)
    val CleanInvalid                = CHIOpcode(REQ, 0x09       , "CleanInvalid"               , B, C, E)
    val MakeInvalid                 = CHIOpcode(REQ, 0x0A       , "MakeInvalid"                , B, C, E)
    //  -------------------------------------------------------------------------------------------------
    val WriteUniquePtlCleanSh       = CHIOpcode(REQ, 0x24 | 0x40, "WriteUniquePtlCleanSh"            , E)
    val WriteUniquePtlCleanShPerSep = CHIOpcode(REQ, 0x26 | 0x40, "WriteUniquePtlCleanShPerSep"      , E)
    val WriteUniqueFullCleanSh      = CHIOpcode(REQ, 0x14 | 0x40, "WriteUniqueFullCleanSh"           , E)
    val WriteUniqueFullCleanShPerSep= CHIOpcode(REQ, 0x16 | 0x40, "WriteUniqueFullCleanShPerSep"     , E)
    val WriteBackFullCleanInv       = CHIOpcode(REQ, 0x19 | 0x40, "WriteBackFullCleanInv"            , E)
    val WriteBackFullCleanSh        = CHIOpcode(REQ, 0x18 | 0x40, "WriteBackFullCleanSh"             , E)
    val WriteBackFullCleanShPerSep  = CHIOpcode(REQ, 0x1A | 0x40, "WriteBackFullCleanShPerSep"       , E)
    val WriteCleanFullCleanSh       = CHIOpcode(REQ, 0x1C | 0x40, "WriteCleanFullCleanSh"            , E)
    val WriteCleanFullCleanShPerSep = CHIOpcode(REQ, 0x1E | 0x40, "WriteCleanFullCleanShPerSep"      , E)
    //  -------------------------------------------------------------------------------------------------
    val WriteNoSnpPtlCleanInv       = CHIOpcode(REQ, 0x21 | 0x40, "WriteNoSnpPtlCleanInv"            , E)
    val WriteNoSnpPtlCleanSh        = CHIOpcode(REQ, 0x20 | 0x40, "WriteNoSnpPtlCleanSh"             , E)
    val WriteNoSnpPtlCleanShPerSep  = CHIOpcode(REQ, 0x22 | 0x40, "WriteNoSnpPtlCleanShPerSep"       , E)
    val WriteNoSnpFullCleanInv      = CHIOpcode(REQ, 0x11 | 0x40, "WriteNoSnpFullCleanInv"           , E)
    val WriteNoSnpFullCleanSh       = CHIOpcode(REQ, 0x10 | 0x40, "WriteNoSnpFullCleanSh"            , E)
    val WriteNoSnpFullCleanShPerSep = CHIOpcode(REQ, 0x12 | 0x40, "WriteNoSnpFullCleanShPerSep"      , E)
    //  -------------------------------------------------------------------------------------------------
    val DVMOp                       = CHIOpcode(REQ, 0x14       , "DVMOp"                      , B, C, E)
    //  -------------------------------------------------------------------------------------------------
    val PCrdReturn                  = CHIOpcode(REQ, 0x05       , "PCrdReturn"                 , B, C, E)
    //  -------------------------------------------------------------------------------------------------
    val AtomicStore_ADD             = CHIOpcode(REQ, 0x28       , "AtomicStore.ADD"            , B, C, E)
    val AtomicStore_CLR             = CHIOpcode(REQ, 0x29       , "AtomicStore.CLR"            , B, C, E)
    val AtomicStore_EOR             = CHIOpcode(REQ, 0x2A       , "AtomicStore.EOR"            , B, C, E)
    val AtomicStore_SET             = CHIOpcode(REQ, 0x2B       , "AtomicStore.SET"            , B, C, E)
    val AtomicStore_SMAX            = CHIOpcode(REQ, 0x2C       , "AtomicStore.SMAX"           , B, C, E)
    val AtomicStore_SMIN            = CHIOpcode(REQ, 0x2D       , "AtomicStore.SMIN"           , B, C, E)
    val AtomicStore_UMAX            = CHIOpcode(REQ, 0x2E       , "AtomicStore.UMAX"           , B, C, E)
    val AtomicStore_UMIN            = CHIOpcode(REQ, 0x2F       , "AtomicStore.UMIN"           , B, C, E)
    val AtomicLoad_ADD              = CHIOpcode(REQ, 0x30       , "AtomicLoad.ADD"             , B, C, E)
    val AtomicLoad_CLR              = CHIOpcode(REQ, 0x31       , "AtomicLoad.CLR"             , B, C, E)
    val AtomicLoad_EOR              = CHIOpcode(REQ, 0x32       , "AtomicLoad.EOR"             , B, C, E)
    val AtomicLoad_SET              = CHIOpcode(REQ, 0x33       , "AtomicLoad.SET"             , B, C, E)
    val AtomicLoad_SMAX             = CHIOpcode(REQ, 0x34       , "AtomicLoad.SMAX"            , B, C, E)
    val AtomicLoad_SMIN             = CHIOpcode(REQ, 0x35       , "AtomicLoad.SMIN"            , B, C, E)
    val AtomicLoad_UMAX             = CHIOpcode(REQ, 0x36       , "AtomicLoad.UMAX"            , B, C, E)
    val AtomicLoad_UMIN             = CHIOpcode(REQ, 0x37       , "AtomicLoad.UMIN"            , B, C, E)
    val AtomicSwap                  = CHIOpcode(REQ, 0x38       , "AtomicSwap"                 , B, C, E)
    val AtomicCompare               = CHIOpcode(REQ, 0x39       , "AtomicCompare"              , B, C, E)
    //  -------------------------------------------------------------------------------------------------
    val PrefetchTgt                 = CHIOpcode(REQ, 0x3A       , "PrefetchTgt"                , B, C, E)
    //  =================================================================================================

    def isAtomic(opcode: CHIOpcode): Boolean 
        = (opcode.opcode >= AtomicStore_ADD.opcode) && (opcode.opcode <= AtomicCompare.opcode)

    def isAtomic(opcode: UInt): Bool 
        = (opcode >= AtomicStore_ADD.U) && (opcode <= AtomicCompare.U)

    def isAtomicStore(opcode: CHIOpcode): Boolean 
        = (opcode.opcode >= AtomicStore_ADD.opcode) && (opcode.opcode <= AtomicStore_UMIN.opcode)

    def isAtomicStore(opcode: UInt): Bool
        = (opcode >= AtomicStore_ADD.U) && (opcode <= AtomicStore_UMIN.U)

    def isAtomicLoad(opcode: CHIOpcode): Boolean 
        = (opcode.opcode >= AtomicLoad_ADD.opcode) && (opcode.opcode <= AtomicLoad_UMIN.opcode)

    def isAtomicLoad(opcode: UInt): Bool 
        = (opcode >= AtomicLoad_ADD.U) && (opcode <= AtomicLoad_UMIN.U)


    /*
    * Decoder for CHI Opcodes of RN-F REQ
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder} 
    */
    class Decoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(REQ, paramOpcodeSupported, Seq(
        //  ========================
            ReqLCrdReturn,
        //  ------------------------
            ReadNoSnp,
            WriteNoSnpFull,
            WriteNoSnpPtl,
            WriteNoSnpZero,
        //  ------------------------
            ReadClean,
            ReadShared,
            ReadNotSharedDirty,
            ReadUnique,
            ReadPreferUnique,
            MakeReadUnique,
            CleanUnique,
            MakeUnique,
            Evict,
            WriteBackFull,
            WriteBackPtl,
            WriteEvictFull,
            WriteCleanFull,
            WriteEvictOrEvict,
        //  ------------------------
            ReadOnce,
            ReadOnceCleanInvalid,
            ReadOnceMakeInvalid,
            StashOnceUnique,
            StashOnceShared,
            StashOnceSepUnique,
            StashOnceSepShared,
            WriteUniqueFull,
            WriteUniqueFullStash,
            WriteUniquePtl,
            WriteUniquePtlStash,
            WriteUniqueZero,
        //  ------------------------
            CleanShared,
            CleanSharedPersist,
            CleanSharedPersistSep,
            CleanInvalid,
            MakeInvalid,
        //  ------------------------
            WriteUniquePtlCleanSh,
            WriteUniquePtlCleanShPerSep,
            WriteUniqueFullCleanSh,
            WriteUniqueFullCleanShPerSep,
            WriteBackFullCleanInv,
            WriteBackFullCleanSh,
            WriteBackFullCleanShPerSep,
            WriteCleanFullCleanSh,
            WriteCleanFullCleanShPerSep,
        //  ------------------------
            WriteNoSnpPtlCleanInv,
            WriteNoSnpPtlCleanSh,
            WriteNoSnpPtlCleanShPerSep,
            WriteNoSnpFullCleanInv,
            WriteNoSnpFullCleanSh,
            WriteNoSnpFullCleanShPerSep,
        //  ------------------------
            DVMOp,
        //  ------------------------
            PCrdReturn,
        //  ------------------------
            AtomicStore_ADD,
            AtomicStore_CLR,
            AtomicStore_EOR,
            AtomicStore_SET,
            AtomicStore_SMAX,
            AtomicStore_SMIN,
            AtomicStore_UMAX,
            AtomicStore_UMIN,
            AtomicLoad_ADD,
            AtomicLoad_CLR,
            AtomicLoad_EOR,
            AtomicLoad_SET,
            AtomicLoad_SMAX,
            AtomicLoad_SMIN,
            AtomicLoad_UMAX,
            AtomicLoad_UMIN,
            AtomicSwap,
            AtomicCompare,
        //  ------------------------
            PrefetchTgt
        //  ========================
        ), paramEnableUnsupportedCheck)
}
