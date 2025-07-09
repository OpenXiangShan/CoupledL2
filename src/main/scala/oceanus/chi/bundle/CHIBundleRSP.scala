package oceanus.chi.bundle

import org.chipsalliance.cde.config.Parameters
import chisel3._
import oceanus.chi._
import oceanus.chi.field._


/*
* CHI (TX/RX) RSP channel signals bundle. 
*/
class CHIBundleRSP(implicit p: Parameters) extends AbstractCHIBundle {

    //  ================================================================
    val QoS             = CHIFieldUInt(paramCHI.rspQoSWidth)
    //  ----------------------------------------------------------------
    val TgtID           = CHIFieldUInt(paramCHI.rspTgtIDWidth)
    //  ----------------------------------------------------------------
    val SrcID           = CHIFieldUInt(paramCHI.rspSrcIDWidth)
    //  ----------------------------------------------------------------
    val TxnID           = CHIFieldUInt(paramCHI.rspTxnIDWidth)
    //  ----------------------------------------------------------------
    val Opcode          = CHIFieldUInt(paramCHI.rspOpcodeWidth)
    //  ----------------------------------------------------------------
    val RespErr         = CHIFieldUInt(paramCHI.rspRespErrWidth)
    //  ----------------------------------------------------------------
    val Resp            = CHIFieldUInt(paramCHI.rspRespWidth)
    //  ----------------------------------------------------------------
    val FwdState_DataPull   = CHIFieldUInt(
            paramCHI.rspFwdStateWidth 
        max paramCHI.rspDataPullWidth)

    def FwdState        = CHIFieldUInt(paramCHI.rspFwdStateWidth, 0, FwdState_DataPull)
    def DataPull        = CHIFieldUInt(paramCHI.rspDataPullWidth, 0, FwdState_DataPull)

    def FwdState    (fwdState   : UInt) = CHIFieldAssign(FwdState_DataPull,
                                            (paramCHI.rspFwdStateWidth, 0, fwdState))
    def DataPull    (dataPull   : UInt) = CHIFieldAssign(FwdState_DataPull,
                                            (paramCHI.rspDataPullWidth, 0, dataPull))
    //  ----------------------------------------------------------------
    val CBusy           = CHIFieldUInt(paramCHI.rspCBusyWidth, EnumCHIIssue.E)
    //  ----------------------------------------------------------------
    val DBID_PGroupID_StashGroupID_TagGroupID   = CHIFieldUInt(
            paramCHI.rspDBIDWidth
        max paramCHI.rspPGroupIDWidth
        max paramCHI.rspStashGroupIDWidth
        max paramCHI.rspTagGroupIDWidth)

    def DBID            = CHIFieldUInt(paramCHI.rspDBIDWidth        , 0, DBID_PGroupID_StashGroupID_TagGroupID)
    def PGroupID        = CHIFieldUInt(paramCHI.rspPGroupIDWidth    , 0, DBID_PGroupID_StashGroupID_TagGroupID, EnumCHIIssue.E)
    def StashGroupID    = CHIFieldUInt(paramCHI.rspStashGroupIDWidth, 0, DBID_PGroupID_StashGroupID_TagGroupID, EnumCHIIssue.E)
    def TagGroupID      = CHIFieldUInt(paramCHI.rspTagGroupIDWidth  , 0, DBID_PGroupID_StashGroupID_TagGroupID, EnumCHIIssue.E)

    def DBID        (dbid           : UInt) = CHIFieldAssign(DBID_PGroupID_StashGroupID_TagGroupID,
                                                (paramCHI.rspDBIDWidth          , 0, dbid))
    def PGroupID    (pGroupID       : UInt) = CHIFieldAssign(DBID_PGroupID_StashGroupID_TagGroupID,
                                                (paramCHI.rspPGroupIDWidth      , 0, pGroupID))
    def StashGroupID(stashGroupID   : UInt) = CHIFieldAssign(DBID_PGroupID_StashGroupID_TagGroupID,
                                                (paramCHI.rspStashGroupIDWidth  , 0, stashGroupID))
    def TagGroupID  (tagGroupID     : UInt) = CHIFieldAssign(DBID_PGroupID_StashGroupID_TagGroupID,
                                                (paramCHI.rspTagGroupIDWidth    , 0, tagGroupID))
    //  ----------------------------------------------------------------
    val PCrdType        = CHIFieldUInt(paramCHI.rspPCrdTypeWidth)
    //  ----------------------------------------------------------------
    val TagOp           = CHIFieldUInt(paramCHI.rspTagOpWidth, EnumCHIIssue.E)
    //  ----------------------------------------------------------------
    val TraceTag        = CHIFieldUInt(paramCHI.rspTraceTagWidth)
    //  ================================================================
}