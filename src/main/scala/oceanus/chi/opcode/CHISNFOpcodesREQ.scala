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

    //  ==============================================================================================
    val ReqLCrdReturn               = CHIOpcode(REQ, 0x00, "ReqLCrdReturn"                  , B, C, E)
    //  ----------------------------------------------------------------------------------------------
    val ReadNoSnp                   = CHIOpcode(REQ, 0x04, "ReadNoSnp"                      , B, C, E)
    val WriteNoSnpFull              = CHIOpcode(REQ, 0x1D, "WriteNoSnpFull"                 , B, C, E)
    val WriteNoSnpPtl               = CHIOpcode(REQ, 0x1C, "WriteNoSnpPtl"                  , B, C, E)
    val WriteNoSnpZero              = CHIOpcode(REQ, 0x44, "WriteNoSnpZero"                       , E)
    //  ----------------------------------------------------------------------------------------------
    val ReadNoSnpSep                = CHIOpcode(REQ, 0x11, "ReadNoSnpSep"                      , C, E)
    //  ----------------------------------------------------------------------------------------------
    val CleanShared                 = CHIOpcode(REQ, 0x08, "CleanShared"                    , B, C, E)
    val CleanSharedPersist          = CHIOpcode(REQ, 0x27, "CleanSharedPersist"             , B, C, E)
    val CleanSharedPersistSep       = CHIOpcode(REQ, 0x13, "CleanSharedPersistSep"                , E)
    val CleanInvalid                = CHIOpcode(REQ, 0x09, "CleanInvalid"                   , B, C, E)
    val MakeInvalid                 = CHIOpcode(REQ, 0x0A, "MakeInvalid"                    , B, C, E)
    //  ----------------------------------------------------------------------------------------------
    val WriteNoSnpPtlCleanInv       = CHIOpcode(REQ, 0x61, "WriteNoSnpPtlCleanInv"                , E)
    val WriteNoSnpPtlCleanSh        = CHIOpcode(REQ, 0x60, "WriteNoSnpPtlCleanSh"                 , E)
    val WriteNoSnpPtlCleanShPerSep  = CHIOpcode(REQ, 0x62, "WriteNoSnpPtlCleanShPerSep"           , E)
    val WriteNoSnpFullCleanInv      = CHIOpcode(REQ, 0x51, "WriteNoSnpFullCleanInv"               , E)
    val WriteNoSnpFullCleanSh       = CHIOpcode(REQ, 0x50, "WriteNoSnpFullCleanSh"                , E)
    val WriteNoSnpFullCleanShPerSep = CHIOpcode(REQ, 0x52, "WriteNoSnpFullCleanShPerSep"          , E)
    //  ----------------------------------------------------------------------------------------------
    val PCrdReturn                  = CHIOpcode(REQ, 0x05, "PCrdReturn"                     , B, C, E)
    //  ----------------------------------------------------------------------------------------------
    val AtomicStore_ADD             = CHIOpcode(REQ, 0x28, "AtomicStore.ADD"                , B, C, E)
    val AtomicStore_CLR             = CHIOpcode(REQ, 0x29, "AtomicStore.CLR"                , B, C, E)
    val AtomicStore_EOR             = CHIOpcode(REQ, 0x2A, "AtomicStore.EOR"                , B, C, E)
    val AtomicStore_SET             = CHIOpcode(REQ, 0x2B, "AtomicStore.SET"                , B, C, E)
    val AtomicStore_SMAX            = CHIOpcode(REQ, 0x2C, "AtomicStore.SMAX"               , B, C, E)
    val AtomicStore_SMIN            = CHIOpcode(REQ, 0x2D, "AtomicStore.SMIN"               , B, C, E)
    val AtomicStore_UMAX            = CHIOpcode(REQ, 0x2E, "AtomicStore.UMAX"               , B, C, E)
    val AtomicStore_UMIN            = CHIOpcode(REQ, 0x2F, "AtomicStore.UMIN"               , B, C, E)
    val AtomicLoad_ADD              = CHIOpcode(REQ, 0x30, "AtomicLoad.ADD"                 , B, C, E)
    val AtomicLoad_CLR              = CHIOpcode(REQ, 0x31, "AtomicLoad.CLR"                 , B, C, E)
    val AtomicLoad_EOR              = CHIOpcode(REQ, 0x32, "AtomicLoad.EOR"                 , B, C, E)
    val AtomicLoad_SET              = CHIOpcode(REQ, 0x33, "AtomicLoad.SET"                 , B, C, E)
    val AtomicLoad_SMAX             = CHIOpcode(REQ, 0x34, "AtomicLoad.SMAX"                , B, C, E)
    val AtomicLoad_SMIN             = CHIOpcode(REQ, 0x35, "AtomicLoad.SMIN"                , B, C, E)
    val AtomicLoad_UMAX             = CHIOpcode(REQ, 0x36, "AtomicLoad.UMAX"                , B, C, E)
    val AtomicLoad_UMIN             = CHIOpcode(REQ, 0x37, "AtomicLoad.UMIN"                , B, C, E)
    val AtomicSwap                  = CHIOpcode(REQ, 0x38, "AtomicSwap"                     , B, C, E)
    val AtomicCompare               = CHIOpcode(REQ, 0x39, "AtomicCompare"                  , B, C, E)
    //  ----------------------------------------------------------------------------------------------
    val PrefetchTgt                 = CHIOpcode(REQ, 0x3A, "PrefetchTgt"                    , B, C, E)
    //  ==============================================================================================

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
    * Decoder for CHI Opcodes of SN-F REQ 
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder}
    */
    class REQOpcodeDecoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
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
            ReadNoSnpSep,
        //  ------------------------
            CleanShared,
            CleanSharedPersist,
            CleanSharedPersistSep,
            CleanInvalid,
            MakeInvalid,
        //  ------------------------
            WriteNoSnpPtlCleanInv,
            WriteNoSnpPtlCleanSh,
            WriteNoSnpPtlCleanShPerSep,
            WriteNoSnpFullCleanInv,
            WriteNoSnpFullCleanSh,
            WriteNoSnpFullCleanShPerSep,
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
