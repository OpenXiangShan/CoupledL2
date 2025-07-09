package oceanus.chi.bundle

import org.chipsalliance.cde.config.Parameters
import chisel3._
import oceanus.chi._
import oceanus.chi.field._


/*
* CHI (TX/RX) REQ channel signals bundle.
*/
class CHIBundleREQ(implicit p: Parameters) extends AbstractCHIBundle {

    //  ================================================================
    val QoS             = CHIFieldUInt(paramCHI.reqQoSWidth)
    //  ----------------------------------------------------------------
    val TgtID           = CHIFieldUInt(paramCHI.reqTgtIDWidth)
    //  ----------------------------------------------------------------
    val SrcID           = CHIFieldUInt(paramCHI.reqSrcIDWidth)
    //  ----------------------------------------------------------------
    val TxnID           = CHIFieldUInt(paramCHI.reqTxnIDWidth)
    //  ----------------------------------------------------------------
    val ReturnNID_StashNID_SLCRepHint           = CHIFieldUInt(
            paramCHI.reqReturnNIDWidth 
        max paramCHI.reqStashNIDWidth
        max paramCHI.reqSLCRepHintWidth)

    def ReturnNID       = CHIFieldUInt(paramCHI.reqReturnNIDWidth , 0, ReturnNID_StashNID_SLCRepHint)
    def StashNID        = CHIFieldUInt(paramCHI.reqStashNIDWidth  , 0, ReturnNID_StashNID_SLCRepHint)
    def SLCRepHint      = CHIFieldUInt(paramCHI.reqSLCRepHintWidth, 0, ReturnNID_StashNID_SLCRepHint, EnumCHIIssue.E)

    def ReturnNID   (returnNID  : UInt) = CHIFieldAssign(ReturnNID_StashNID_SLCRepHint,
                                            (paramCHI.reqReturnNIDWidth , 0, returnNID))
    def StashNID    (stashNID   : UInt) = CHIFieldAssign(ReturnNID_StashNID_SLCRepHint,
                                            (paramCHI.reqStashNIDWidth  , 0, stashNID))
    def SLCRepHint  (slcRepHint : UInt) = CHIFieldAssign(ReturnNID_StashNID_SLCRepHint,
                                            (paramCHI.reqSLCRepHintWidth, 0, slcRepHint))
    //  ----------------------------------------------------------------
    val StashNIDValid_Endian_Deep               = CHIFieldUInt(
            paramCHI.reqStashNIDValidWidth
        max paramCHI.reqEndianWidth
        max paramCHI.reqDeepWidth)

    def StashNIDValid   = CHIFieldUInt(paramCHI.reqStashNIDValidWidth, 0, StashNIDValid_Endian_Deep)
    def Endian          = CHIFieldUInt(paramCHI.reqEndianWidth       , 0, StashNIDValid_Endian_Deep)
    def Deep            = CHIFieldUInt(paramCHI.reqDeepWidth         , 0, StashNIDValid_Endian_Deep, EnumCHIIssue.E)

    def StashNIDValid   (stashNIDValid  : UInt) = CHIFieldAssign(StashNIDValid_Endian_Deep,
                                                    (paramCHI.reqStashNIDValidWidth , 0, stashNIDValid))
    def Endian          (endian         : UInt) = CHIFieldAssign(StashNIDValid_Endian_Deep,
                                                    (paramCHI.reqEndianWidth        , 0, endian))
    def Deep            (deep           : UInt) = CHIFieldAssign(StashNIDValid_Endian_Deep,
                                                    (paramCHI.reqDeepWidth          , 0, deep))
    //  ----------------------------------------------------------------
    val ReturnTxnID_StashLPIDValid_StashLPID    = CHIFieldUInt(
            paramCHI.reqReturnTxnIDWidth
        max(paramCHI.reqStashLPIDWidth + paramCHI.reqStashLPIDValidWidth))

    def ReturnTxnID     = CHIFieldUInt(paramCHI.reqReturnTxnIDWidth   ,                          0, ReturnTxnID_StashLPIDValid_StashLPID)
    def StashLPIDValid  = CHIFieldUInt(paramCHI.reqStashLPIDValidWidth, paramCHI.reqStashLPIDWidth, ReturnTxnID_StashLPIDValid_StashLPID)
    def StashLPID       = CHIFieldUInt(paramCHI.reqStashLPIDWidth     ,                          0, ReturnTxnID_StashLPIDValid_StashLPID)

    def ReturnTxnID     (returnTxnID    : UInt) = CHIFieldAssign(ReturnTxnID_StashLPIDValid_StashLPID,
                                                    (paramCHI.reqReturnTxnIDWidth   , 0, returnTxnID))
    def StashLPID       (stashLPIDValid : UInt,
                         stashLPID      : UInt) = CHIFieldAssign(ReturnTxnID_StashLPIDValid_StashLPID,
                                                    (paramCHI.reqStashLPIDValidWidth, paramCHI.reqStashLPIDWidth, stashLPIDValid),
                                                    (paramCHI.reqStashLPIDWidth     ,                          0, stashLPID))
    //  ----------------------------------------------------------------
    val Opcode          = CHIFieldUInt(paramCHI.reqOpcodeWidth)
    //  ----------------------------------------------------------------
    val Size            = CHIFieldUInt(paramCHI.reqSizeWidth)
    //  ----------------------------------------------------------------
    val Addr            = CHIFieldUInt(paramCHI.reqAddrWidth)
    //  ----------------------------------------------------------------
    val NS              = CHIFieldUInt(paramCHI.reqNSWidth)
    //  ----------------------------------------------------------------
    val LikelyShared    = CHIFieldUInt(paramCHI.reqLikelySharedWidth)
    //  ----------------------------------------------------------------
    val AllowRetry      = CHIFieldUInt(paramCHI.reqAllowRetryWidth)
    //  ----------------------------------------------------------------
    val Order           = CHIFieldUInt(paramCHI.reqOrderWidth)
    //  ----------------------------------------------------------------
    val PCrdType        = CHIFieldUInt(paramCHI.reqPCrdTypeWidth)
    //  ----------------------------------------------------------------
    val MemAttr         = CHIFieldUInt(paramCHI.reqMemAttrWidth)
    //  ----------------------------------------------------------------
    val SnpAttr_DoDWT   = CHIFieldUInt(
            paramCHI.reqSnpAttrWidth
        max paramCHI.reqDoDWTWidth)

    def SnpAttr         = CHIFieldUInt(paramCHI.reqSnpAttrWidth, 0, SnpAttr_DoDWT)
    def DoDWT           = CHIFieldUInt(paramCHI.reqDoDWTWidth  , 0, SnpAttr_DoDWT, EnumCHIIssue.E)

    def SnpAttr (snpAttr: UInt) = CHIFieldAssign(SnpAttr_DoDWT,
                                    (paramCHI.reqSnpAttrWidth, 0, snpAttr))
    def DoDWT   (doDWT  : UInt) = CHIFieldAssign(SnpAttr_DoDWT,
                                    (paramCHI.reqDoDWTWidth  , 0, doDWT))
    //  ----------------------------------------------------------------
    val LPID_PGroupID_StashGroupID_TagGroupID   = CHIFieldUInt(
            paramCHI.reqLPIDWidth
        max paramCHI.reqPGroupIDWidth
        max paramCHI.reqStashGroupIDWidth
        max paramCHI.reqTagGroupIDWidth)

    def LPID            = CHIFieldUInt(paramCHI.reqLPIDWidth        , 0, LPID_PGroupID_StashGroupID_TagGroupID)
    def PGroupID        = CHIFieldUInt(paramCHI.reqPGroupIDWidth    , 0, LPID_PGroupID_StashGroupID_TagGroupID, EnumCHIIssue.E)
    def StashGroupID    = CHIFieldUInt(paramCHI.reqStashGroupIDWidth, 0, LPID_PGroupID_StashGroupID_TagGroupID, EnumCHIIssue.E)
    def TagGroupID      = CHIFieldUInt(paramCHI.reqTagGroupIDWidth  , 0, LPID_PGroupID_StashGroupID_TagGroupID, EnumCHIIssue.E)

    def LPID            (lpid           : UInt) = CHIFieldAssign(LPID_PGroupID_StashGroupID_TagGroupID,
                                                    (paramCHI.reqLPIDWidth          , 0, lpid))
    def PGroupID        (pGroupID       : UInt) = CHIFieldAssign(LPID_PGroupID_StashGroupID_TagGroupID,
                                                    (paramCHI.reqPGroupIDWidth      , 0, pGroupID))
    def StashGroupID    (stashGroupID   : UInt) = CHIFieldAssign(LPID_PGroupID_StashGroupID_TagGroupID,
                                                    (paramCHI.reqStashGroupIDWidth  , 0, stashGroupID))
    def TagGroupID      (tagGroupID     : UInt) = CHIFieldAssign(LPID_PGroupID_StashGroupID_TagGroupID,
                                                    (paramCHI.reqTagGroupIDWidth    , 0, tagGroupID))
    //  ----------------------------------------------------------------
    val Excl_SnoopMe    = CHIFieldUInt(
            paramCHI.reqExclWidth 
        max paramCHI.reqSnoopMeWidth)

    def Excl            = CHIFieldUInt(paramCHI.reqExclWidth   , 0, Excl_SnoopMe)
    def SnoopMe         = CHIFieldUInt(paramCHI.reqSnoopMeWidth, 0, Excl_SnoopMe)

    def Excl    (excl   : UInt) = CHIFieldAssign(Excl_SnoopMe,
                                    (paramCHI.reqExclWidth      , 0, excl))
    def SnoopMe (snoopMe: UInt) = CHIFieldAssign(Excl_SnoopMe,
                                    (paramCHI.reqSnoopMeWidth   , 0, snoopMe))
    //  ----------------------------------------------------------------
    val ExpCompAck      = CHIFieldUInt(paramCHI.reqExpCompAckWidth)
    //  ----------------------------------------------------------------
    val TagOp           = CHIFieldUInt(paramCHI.reqTagOpWidth, EnumCHIIssue.E)
    //  ----------------------------------------------------------------
    val TraceTag        = CHIFieldUInt(paramCHI.reqTraceTagWidth)
    //  ----------------------------------------------------------------
    val MPAM            = CHIFieldUInt(paramCHI.reqMPAMWidth, EnumCHIIssue.E)
    //  ----------------------------------------------------------------
    val RSVDC           = CHIFieldUInt(paramCHI.reqRsvdcWidth)
    //  ================================================================
}
