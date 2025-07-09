package oceanus.chi

import org.chipsalliance.cde.config.Field
import dataclass.data


case class CHIParameters (

    // CHI Essentials
    /*
    * issue: Issue version of CHI. 
    * 
    * -------------------------------------------------------------------
    * For NCB-200:
    *   * Functionally fully-supported CHI Issue:
    *       - Issue B
    *       - Issue C
    *       - Issue E (E.b) mostly (except WriteNoSnpZero, WriteNoSnp*Clean*, CleanSharedPersistSep)
    * 
    *   * Connectional (flit fields & interface) supported CHI Issue:
    *       - Issue B
    *       - Issue C
    *       - Issue E (E.b)
    * 
    *   * Future version feature: Issue E.b functionality support:
    *       1) Support for <b>WriteNoSnpZero</b>
    *       2) Support for <b>WriteNoSnp*Clean*</b>
    *       3) Recognizing <b>CleanSharedPersistSep</b>
    * -------------------------------------------------------------------
    */
    issue               : EnumCHIIssue      = EnumCHIIssue.B,
    //


    // CHI Flit configurations
    /*
    * nodeIdWidth: Width of Node ID (NodeID_Width)
    * 
    * * Legal values: 7 to 11
    */
    nodeIdWidth         : Int               = 7,

    /*
    * reqAddrWidth: Width of REQ Address (Req_Addr_Width)
    * 
    * * Legal values: 44 to 52
    */
    reqAddrWidth        : Int               = 44,

    /*
    * reqRsvdcWidth: Width of REQ RSVDC
    * 
    * * Legal values: 0 or 4, 8, 12, 16, 24, 32
    */
    reqRsvdcWidth       : Int               = 0,

    /* 
    * datRsvdcWidth: Width of DAT RSVDC
    * 
    * * Legal values: 0 or 4, 8, 12, 16, 24, 32
    */
    datRsvdcWidth       : Int               = 0,

    /*
    * dataWidth: Width of DAT Data (Data_Width)
    * 
    * * Legal values: 128, 256 or 512
    */
    dataWidth           : Int               = 128,

    /*
    * dataCheckPresent: Support for DAT.DataCheck field (Data_Check, Check_Type)
    */
    dataCheckPresent    : Boolean           = false,

    /* 
    * poisonPresent: Support for DAT.Poison field (Data_Poison)
    */
    poisonPresent       : Boolean           = false,

    /* 
    * mpamPresent: Support for REQ.MPAM and SNP.MPAM field (MPAM_Support)
    */
    mpamPresent         : Boolean           = false
    //
)
/*
* Elaborated width of all CHI Flit fields.
*/
{
    //
    private def unknownIssue = 
        throw new IllegalArgumentException(s"unknown or unsupported CHI Issue: ${issue}")
    //

    //
    require(nodeIdWidth >= 7 && nodeIdWidth <= 11,
        s"Legal values of 'nodeIdWidth' are 7 to 11: nodeIdWidth = ${nodeIdWidth}")

    require(reqAddrWidth >= 44 && reqAddrWidth <= 52,
        s"Legal values of 'reqAddrWidth' are 44 to 52: reqAddrWidth = ${reqAddrWidth}")

    require(dataWidth == 128 || dataWidth == 256 || dataWidth == 512,
        s"Legal values of 'dataWidth' are 128, 256 and 512: dataWidth = ${dataWidth}")

    require(datRsvdcWidth == 0 || datRsvdcWidth == 4  || datRsvdcWidth == 8
                               || datRsvdcWidth == 12 || datRsvdcWidth == 16
                               || datRsvdcWidth == 24 || datRsvdcWidth == 32,
        s"Legal values of 'datRsvdcWidth' are 0 or 4, 8, 12, 16, 24, 32: datRsvdcWidth = ${datRsvdcWidth}")

    require(reqRsvdcWidth == 0 || reqRsvdcWidth == 4  || reqRsvdcWidth == 8
                               || reqRsvdcWidth == 12 || reqRsvdcWidth == 16
                               || reqRsvdcWidth == 24 || reqRsvdcWidth == 32,
        s"Legal values of 'reqRsvdcWidth' are 0 or 4, 8, 12, 16, 24, 32: reqRsvdcWidth = ${reqRsvdcWidth}")
    //


    /*
    * REQ Flit fields width.
    */
    //  reqQoSWidth: Width of REQ.QoS
    def reqQoSWidth             : Int       = 4

    //  reqTgtIDWidth: Width of REQ.TgtID
    def reqTgtIDWidth           : Int       = nodeIdWidth

    //  reqSrcIDWidth: Width of REQ.SrcID
    def reqSrcIDWidth           : Int       = nodeIdWidth

    //  reqTxnIDWidth: Width of REQ.TxnID
    def reqTxnIDWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 8
        case EnumCHIIssue.C     => 8
        case EnumCHIIssue.E     => 12
        case _: EnumCHIIssue    => unknownIssue
    }

    //  reqReturnNIDWidth: Width of REQ.ReturnNID
    def reqReturnNIDWidth       : Int       = nodeIdWidth

    //  reqStashNIDWidth: Width of REQ.StashNID
    def reqStashNIDWidth        : Int       = nodeIdWidth

    //  reqSLCRepHintWidth: Width of REQ.SLCRepHint
    def reqSLCRepHintWidth      : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 7
        case _: EnumCHIIssue    => unknownIssue
    }

    //  reqStashNIDValidWidth: Width of REQ.StashNIDValid
    def reqStashNIDValidWidth   : Int       = 1

    //  reqEndianWidth: Width of REQ.Endian
    def reqEndianWidth          : Int       = 1

    //  reqDeepWidth: Width of REQ.Deep
    def reqDeepWidth            : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 1
        case _: EnumCHIIssue    => unknownIssue
    }

    //  reqReturnTxnIDWidth: Width of REQ.ReturnTxnID
    def reqReturnTxnIDWidth     : Int       = issue match {
        case EnumCHIIssue.B     => 8
        case EnumCHIIssue.C     => 8
        case EnumCHIIssue.E     => 12
        case _: EnumCHIIssue    => unknownIssue
    }

    //  reqStashLPIDWidth: Width of REQ.StashLPID
    def reqStashLPIDWidth       : Int       = 5

    //  reqStashLPIDValidWidth: Width of REQ.StashLPIDValid
    def reqStashLPIDValidWidth  : Int       = 1

    //  reqOpcodeWidth: Width of REQ.Opcode
    def reqOpcodeWidth          : Int       = issue match {
        case EnumCHIIssue.B     => 6
        case EnumCHIIssue.C     => 6
        case EnumCHIIssue.E     => 7
        case _: EnumCHIIssue    => unknownIssue 
    }

    //  reqSizeWidth: Width of REQ.Size
    def reqSizeWidth            : Int       = 3

    //  reqAddrWidth: Width of REQ.Addr
//  def reqAddrWidth

    //  reqNSWidth: Width of REQ.NS
    def reqNSWidth              : Int       = 1

    //  reqLikelySharedWidth: Width of REQ.LikelyShared
    def reqLikelySharedWidth    : Int       = 1

    //  reqAllowRetryWidth: Width of REQ.AllowRetry
    def reqAllowRetryWidth      : Int       = 1

    //  reqOrderWidth: Width of REQ.Order
    def reqOrderWidth           : Int       = 2

    //  reqPCrdTypeWidth: Width of REQ.PCrdType
    def reqPCrdTypeWidth        : Int       = 4

    //  reqMemAttrWidth: Width of REQ.MemAttr
    def reqMemAttrWidth         : Int       = 4

    //  reqSnpAttrWidth: Width of REQ.SnpAttr
    def reqSnpAttrWidth         : Int       = 1

    //  reqDoDWTWidth: Width of REQ.DoDWT
    def reqDoDWTWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 1
        case _: EnumCHIIssue    => unknownIssue 
    }

    //  reqLPIDWidth: Width of REQ.LPID
    def reqLPIDWidth            : Int       = 5

    //  reqPGroupIDWidth: Width of REQ.PGroupID
    def reqPGroupIDWidth        : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 8
        case _: EnumCHIIssue    => unknownIssue 
    }

    //  reqStashGroupIDWidth: Width of REQ.StashGroupID
    def reqStashGroupIDWidth    : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 8
        case _: EnumCHIIssue    => unknownIssue 
    }

    //  reqTagGroupIDWidth: Width of REQ.TagGroupID
    def reqTagGroupIDWidth      : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 8
        case _: EnumCHIIssue    => unknownIssue
    }

    //  reqExclWidth: Width of REQ.Excl
    def reqExclWidth            : Int       = 1

    //  reqSnoopMeWidth: Width of REQ.SnoopMe
    def reqSnoopMeWidth         : Int       = 1

    //  reqExpCompAckWidth: Width of REQ.ExpCompAck
    def reqExpCompAckWidth      : Int       = 1

    //  reqTagOpWidth: Width of REQ.TagOp
    def reqTagOpWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 2
        case _: EnumCHIIssue    => unknownIssue
    }

    //  reqTraceTagWidth: Width of REQ.TraceTag
    def reqTraceTagWidth        : Int       = 1

    //  reqMPAMWidth: Width of REQ.MPAM
    def reqMPAMWidth            : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => if (mpamPresent) 11 else 0
        case _: EnumCHIIssue    => unknownIssue
    }

    //  reqRsvdcWidth: Width of REQ.RSVDC
//  def reqRsvdcWidth
    /**/


    /* 
    * DAT Flit fields width.
    */
    //  datQoSWidth: Width of DAT.QoS
    def datQoSWidth             : Int       = 4

    //  datTgtIDWidth: Width of DAT.TgtID
    def datTgtIDWidth           : Int       = nodeIdWidth

    //  datSrcIDWidth: Width of DAT.SrcID
    def datSrcIDWidth           : Int       = nodeIdWidth

    //  datTxnIDWidth: Width of DAT.TxnID
    def datTxnIDWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 8
        case EnumCHIIssue.C     => 8
        case EnumCHIIssue.E     => 12
        case _: EnumCHIIssue    => unknownIssue
    }

    //  datHomeNIDWidth: Width of DAT.HomeNID
    def datHomeNIDWidth         : Int       = nodeIdWidth

    //  datOpcodeWidth: Width of DAT.Opcode
    def datOpcodeWidth          : Int       = issue match {
        case EnumCHIIssue.B     => 3
        case EnumCHIIssue.C     => 4
        case EnumCHIIssue.E     => 4
        case _: EnumCHIIssue    => unknownIssue 
    }

    //  datRespErrWidth: Width of DAT.RespErr
    def datRespErrWidth         : Int       = 2

    //  datRespWidth: Width of DAT.Resp
    def datRespWidth            : Int       = 3

    //  datFwdStateWidth: Width of DAT.FwdState
    def datFwdStateWidth        : Int       = 3

    //  datDataPullWidth: Width of DAT.DataPull
    def datDataPullWidth        : Int       = 3

    //  datDataSourceWidth: Width of DAT.DataSource
    def datDataSourceWidth      : Int       = issue match {
        case EnumCHIIssue.B     => 3
        case EnumCHIIssue.C     => 3
        case EnumCHIIssue.E     => 4
        case _: EnumCHIIssue    => unknownIssue
    }

    //  datCBusyWidth: Width of DAT.CBusy
    def datCBusyWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 3
        case _: EnumCHIIssue    => unknownIssue
    }

    //  datDBIDWidth: Width of DAT.DBID
    def datDBIDWidth            : Int       = issue match {
        case EnumCHIIssue.B     => 8
        case EnumCHIIssue.C     => 8
        case EnumCHIIssue.E     => 12
        case _: EnumCHIIssue    => unknownIssue
    }

    //  datCCIDWidth: Width of DAT.CCID
    def datCCIDWidth            : Int       = 2

    //  datDataIDWidth: Width of DAT.DataID
    def datDataIDWidth          : Int       = 2

    //  datTagOpWidth: Width of DAT.TagOp
    def datTagOpWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 2
        case _: EnumCHIIssue    => unknownIssue
    }

    //  datTagWidth: Width of DAT.Tag
    def datTagWidth             : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => dataWidth / 32
        case _: EnumCHIIssue    => unknownIssue
    }

    //  datTUWidth: Width of DAT.TU
    def datTUWidth              : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => dataWidth / 128
        case _: EnumCHIIssue    => unknownIssue
    }

    //  datTraceTagWidth: Width of DAT.TraceTag
    def datTraceTagWidth        : Int       = 1

    //  datRsvdcWidth: Width of DAT.RSVDC
//  def datRsvdcWidth

    //  datBEWidth: Width of DAT.BE
    def datBEWidth              : Int       = dataWidth / 8

    //  datDataWidth: Width of DAT.Data
    def datDataWidth            : Int       = dataWidth

    //  datDataCheckWidth: Width of DAT.DataCheck
    def datDataCheckWidth       : Int       = if (dataCheckPresent) dataWidth / 8 else 0

    //  datPoisonWidth: Width of DAT.Poison
    def datPoisonWidth          : Int       = if (poisonPresent) dataWidth / 64 else 0
    /**/


    /* 
    * RSP Flit fields width.
    */
    //  rspQoSWidth: Width of RSP.QoS
    def rspQoSWidth             : Int       = 4

    //  rspTgtIDWidth: Width of RSP.TgtID
    def rspTgtIDWidth           : Int       = nodeIdWidth

    //  rspSrcIDWidth: Width of RSP.SrcID
    def rspSrcIDWidth           : Int       = nodeIdWidth

    //  rspTxnIDWidth: Width of RSP.TxnID
    def rspTxnIDWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 8
        case EnumCHIIssue.C     => 8
        case EnumCHIIssue.E     => 12
        case _: EnumCHIIssue    => unknownIssue
    }

    //  rspOpcodeWidth: Width of RSP.Opcode
    def rspOpcodeWidth          : Int       = issue match {
        case EnumCHIIssue.B     => 4
        case EnumCHIIssue.C     => 4
        case EnumCHIIssue.E     => 5
        case _: EnumCHIIssue    => unknownIssue
    }

    //  rspRespErrWidth: Width of RSP.RespErr
    def rspRespErrWidth         : Int       = 2

    //  rspRespWidth: Width of RSP.Resp
    def rspRespWidth            : Int       = 3

    //  rspFwdStateWidth: Width of RSP.FwdState
    def rspFwdStateWidth        : Int       = 3

    //  rspDataPullWidth: Width of RSP.DataPull
    def rspDataPullWidth        : Int       = 3

    //  rspCBusyWidth: Width of RSP.CBusy
    def rspCBusyWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 3
        case _: EnumCHIIssue    => unknownIssue
    }

    //  rspDBIDWidth: Width of RSP.DBID
    def rspDBIDWidth            : Int       = issue match {
        case EnumCHIIssue.B     => 8
        case EnumCHIIssue.C     => 8
        case EnumCHIIssue.E     => 12
        case _: EnumCHIIssue    => unknownIssue
    }

    //  rspPGroupIDWidth: Width of RSP.PGroupID
    def rspPGroupIDWidth        : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 8
        case _: EnumCHIIssue    => unknownIssue
    }

    //  rspStashGroupIDWidth: Width of RSP.StashGroupID
    def rspStashGroupIDWidth    : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 8
        case _: EnumCHIIssue    => unknownIssue
    }

    //  rspTagGroupIDWidth: Width of RSP.TagGroupID
    def rspTagGroupIDWidth      : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 8
        case _: EnumCHIIssue    => unknownIssue
    }

    //  rspPCrdTypeWidth: Width of RSP.PCrdType
    def rspPCrdTypeWidth        : Int       = 4

    //  rspTagOpWidth: Width of RSP.TagOp
    def rspTagOpWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => 2
        case _: EnumCHIIssue    => unknownIssue
    }

    //  rspTraceTagWidth: Width of RSP.TraceTag
    def rspTraceTagWidth        : Int       = 1
    /**/


    /* 
    * SNP Flit fields width.
    */
    //  snpQoSWidth: Width of SNP.QoS
    def snpQoSWidth             : Int       = 4

    //  snpSrcIDWidth: Width of SNP.SrcID
    def snpSrcIDWidth           : Int       = nodeIdWidth

    //  snpTxnIDWidth: Width of SNP.TxnID
    def snpTxnIDWidth           : Int       = issue match {
        case EnumCHIIssue.B     => 8
        case EnumCHIIssue.C     => 8
        case EnumCHIIssue.E     => 12
        case _: EnumCHIIssue    => unknownIssue
    }

    //  snpFwdNIDWidth: Width of SNP.FwdNID
    def snpFwdNIDWidth          : Int       = nodeIdWidth

    //  snpFwdTxnIDWidth: Width of SNP.FwdTxnID
    def snpFwdTxnIDWidth        : Int       = issue match {
        case EnumCHIIssue.B     => 8
        case EnumCHIIssue.C     => 8
        case EnumCHIIssue.E     => 12
        case _: EnumCHIIssue    => unknownIssue
    }

    //  snpStashLPIDWidth: Width of SNP.StashLPID
    def snpStashLPIDWidth       : Int       = 5

    //  snpStashLPIDValidWidth: Width of SNP.StashLPIDValid
    def snpStashLPIDValidWidth  : Int       = 1

    //  snpVMIDExtWidth: Width of SNP.VMIDExt
    def snpVMIDExtWidth         : Int       = 8

    //  snpOpcodeWidth: Width of SNP.Opcode
    def snpOpcodeWidth          : Int       = 5

    //  snpAddrWidth: Width of SNP.Addr
    def snpAddrWidth            : Int       = reqAddrWidth - 3

    //  snpNSWidth: Width of SNP.NS
    def snpNSWidth              : Int       = 1

    //  snpDoNotGoToSDWidth: Width of SNP.DoNotGoToSD
    def snpDoNotGoToSDWidth     : Int       = 1

    //  snpDoNotDataPullWidth: Width of SNP.DoNotDataPull
    def snpDoNotDataPullWidth   : Int       = issue match {
        case EnumCHIIssue.B     => 1
        case EnumCHIIssue.C     => 1
        case EnumCHIIssue.E     => 0
        case _: EnumCHIIssue    => unknownIssue
    }

    //  snpRetToSrcWidth: Width of SNP.RetToSrc
    def snpRetToSrcWidth        : Int       = 1

    //  snpTraceTagWidth: Width of SNP.TraceTag
    def snpTraceTagWidth        : Int       = 1

    //  snpMPAMWidth: Width of SNP.MPAM
    def snpMPAMWidth            : Int       = issue match {
        case EnumCHIIssue.B     => 0
        case EnumCHIIssue.C     => 0
        case EnumCHIIssue.E     => if (mpamPresent) 11 else 0
        case _: EnumCHIIssue    => unknownIssue
    }
    /**/
}


case object CHIParametersKey extends Field[CHIParameters]
