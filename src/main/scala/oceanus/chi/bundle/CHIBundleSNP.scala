package oceanus.chi.bundle

import org.chipsalliance.cde.config.Parameters
import chisel3._
import oceanus.chi._
import oceanus.chi.field._


/* 
* CHI (TX/RX) SNP channel signals bundle.
*/
class CHIBundleSNP(implicit p: Parameters) extends AbstractCHIBundle {

    //  ================================================================
    val QoS             = CHIFieldUInt(paramCHI.snpQoSWidth)
    //  ----------------------------------------------------------------
    val SrcID           = CHIFieldUInt(paramCHI.snpSrcIDWidth)
    //  ----------------------------------------------------------------
    val TxnID           = CHIFieldUInt(paramCHI.snpTxnIDWidth)
    //  ----------------------------------------------------------------
    val FwdNID          = CHIFieldUInt(paramCHI.snpFwdNIDWidth)
    //  ----------------------------------------------------------------
    val FwdTxnID_StashLPIDValid_StashLPID_VMIDExt   = CHIFieldUInt(
            paramCHI.snpFwdNIDWidth 
        max(paramCHI.snpStashLPIDValidWidth + paramCHI.snpStashLPIDWidth)
        max paramCHI.snpVMIDExtWidth)

    def FwdTxnID        = CHIFieldUInt(paramCHI.snpFwdNIDWidth        , 0                           , FwdTxnID_StashLPIDValid_StashLPID_VMIDExt)
    def StashLPIDValid  = CHIFieldUInt(paramCHI.snpStashLPIDValidWidth, paramCHI.snpStashLPIDWidth  , FwdTxnID_StashLPIDValid_StashLPID_VMIDExt)
    def StashLPID       = CHIFieldUInt(paramCHI.snpStashLPIDWidth     , 0                           , FwdTxnID_StashLPIDValid_StashLPID_VMIDExt)
    def VMIDExt         = CHIFieldUInt(paramCHI.snpVMIDExtWidth       , 0                           , FwdTxnID_StashLPIDValid_StashLPID_VMIDExt)

    def FwdTxnID    (fwdTxnID       : UInt) = CHIFieldAssign(FwdTxnID_StashLPIDValid_StashLPID_VMIDExt,
                                                (paramCHI.snpFwdNIDWidth        , 0, fwdTxnID))
    def StashLPID   (stashLPIDValid : UInt,
                     stashLPID      : UInt) = CHIFieldAssign(FwdTxnID_StashLPIDValid_StashLPID_VMIDExt,
                                                (paramCHI.snpStashLPIDValidWidth, paramCHI.snpStashLPIDWidth, stashLPIDValid),
                                                (paramCHI.snpStashLPIDWidth     ,                          0, stashLPID))
    def VMIDExt     (vmidExt        : UInt) = CHIFieldAssign(FwdTxnID_StashLPIDValid_StashLPID_VMIDExt,
                                                (paramCHI.snpVMIDExtWidth, 0, vmidExt))
    //  ----------------------------------------------------------------
    val Opcode          = CHIFieldUInt(paramCHI.snpOpcodeWidth)
    //  ----------------------------------------------------------------
    val Addr            = CHIFieldUInt(paramCHI.snpAddrWidth)
    //  ----------------------------------------------------------------
    val NS              = CHIFieldUInt(paramCHI.snpNSWidth)
    //  ----------------------------------------------------------------
    val DoNotGoToSD_DoNotDataPull   = CHIFieldUInt(
            paramCHI.snpDoNotGoToSDWidth 
        max paramCHI.snpDoNotDataPullWidth)

    def DoNotGoToSD     = CHIFieldUInt(paramCHI.snpDoNotGoToSDWidth  , 0, DoNotGoToSD_DoNotDataPull)
    def DoNotDataPull   = CHIFieldUInt(paramCHI.snpDoNotDataPullWidth, 0, DoNotGoToSD_DoNotDataPull, EnumCHIIssue.B)

    def DoNotGoToSD     (doNotGoToSD    : UInt) = CHIFieldAssign(DoNotGoToSD_DoNotDataPull,
                                                    (paramCHI.snpDoNotGoToSDWidth   , 0, doNotGoToSD))
    def DoNotDataPull   (doNotDataPull  : UInt) = CHIFieldAssign(DoNotGoToSD_DoNotDataPull,
                                                    (paramCHI.snpDoNotDataPullWidth , 0, doNotDataPull))
    //  ----------------------------------------------------------------
    val RetToSrc        = CHIFieldUInt(paramCHI.snpRetToSrcWidth)
    //  ----------------------------------------------------------------
    val TraceTag        = CHIFieldUInt(paramCHI.snpTraceTagWidth)
    //  ----------------------------------------------------------------
    val MPAM            = CHIFieldUInt(paramCHI.snpMPAMWidth, EnumCHIIssue.E)
    //  ================================================================
}
