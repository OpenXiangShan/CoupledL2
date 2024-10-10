/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.cde.config.Field
import scala.math.max
import coupledL2.TaskBundle
import coupledL2.tl2chi.CHICohStates._
import coupledL2.tl2chi.CHICohStateTrans._
import coupledL2.tl2chi.CHICohStateFwdedTrans._

case object CHIIssue extends Field[String](Issue.B)

object CHICohStates {
  val width = 3

  def I = "b000".U(width.W)
  def SC = "b001".U(width.W)
  def UC = "b010".U(width.W)
  def UD = "b010".U(width.W)
  def SD = "b011".U(width.W)

  def PassDirty = "b100".U(width.W)

  def I_PD = setPD(I)
  def SC_PD = setPD(SC)
  def UC_PD = setPD(UC)
  def UD_PD = setPD(UD)
  def SD_PD = setPD(SD)

  def setPD(state: UInt, pd: Bool = true.B): UInt = {
    require(state.getWidth == width)
    state | Mux(pd, PassDirty, 0.U)
  }
}

class CHICohStateTrans(val resp: () => UInt)

object CHICohStateTrans {

  val CopyBackWrData_I = new CHICohStateTrans(() => I)
  val CopyBackWrData_UC = new CHICohStateTrans(() => UC)
  val CopyBackWrData_SC = new CHICohStateTrans(() => SC)
  val CopyBackWrData_UD_PD = new CHICohStateTrans(() => UD_PD)
  val CopyBackWrData_SD_PD = new CHICohStateTrans(() => SD_PD)

  val CompData_I = new CHICohStateTrans(() => I)
  val CompData_UC = new CHICohStateTrans(() => UC)
  val CompData_SC = new CHICohStateTrans(() => SC)
  val CompData_UD_PD = new CHICohStateTrans(() => UD_PD)
  val CompData_SD_PD = new CHICohStateTrans(() => SD_PD)

  val DataSepResp_I = new CHICohStateTrans(() => I)
  val DataSepResp_UC = new CHICohStateTrans(() => UC)
  val DataSepResp_SC = new CHICohStateTrans(() => SC)
  val DataSepResp_UD_PD = new CHICohStateTrans(() => UD_PD)

  val RespSepData_I = new CHICohStateTrans(() => I)
  val RespSepData_UC = new CHICohStateTrans(() => UC)
  val RespSepData_SC = new CHICohStateTrans(() => SC)
  val RespSepData_UD_PD = new CHICohStateTrans(() => UD_PD)

  val SnpResp_I = new CHICohStateTrans(() => I)
  val SnpResp_SC = new CHICohStateTrans(() => SC)
  val SnpResp_UC = new CHICohStateTrans(() => UC)
  val SnpResp_UD = new CHICohStateTrans(() => UD)
  val SnpResp_SD = new CHICohStateTrans(() => SD)

  val SnpRespData_I = new CHICohStateTrans(() => I)
  val SnpRespData_UC_UD = new CHICohStateTrans(() => UC)
  val SnpRespData_SC = new CHICohStateTrans(() => SC)
  val SnpRespData_SD = new CHICohStateTrans(() => SD)
  val SnpRespData_I_PD = new CHICohStateTrans(() => I_PD)
  val SnpRespData_UC_PD = new CHICohStateTrans(() => UC_PD)
  val SnpRespData_SC_PD = new CHICohStateTrans(() => SC_PD)

  val SnpRespDataPtl_I_PD = new CHICohStateTrans(() => I_PD)
  val SnpRespDataPtl_UD = new CHICohStateTrans(() => UD)
}

class CHICohStateTransSet(val channel: () => UInt, val opcode: UInt, val set: Seq[CHICohStateTrans])

object CHICohStateTransSet {

  def isValid(set: CHICohStateTransSet, channel: UInt, opcode: UInt, resp: UInt): Bool = {
    channel =/= set.channel() || opcode =/= set.opcode || VecInit(set.set.map(t => t.resp() === resp)).asUInt.orR
  }

  def ofCopyBackWrData(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXDAT, opcode, Seq(
    CopyBackWrData_I, CopyBackWrData_UC, CopyBackWrData_SC,
    CopyBackWrData_UD_PD, CopyBackWrData_SD_PD
  ))
  def ofCompData(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXDAT, opcode, Seq(
    CompData_I, CompData_UC, CompData_SC, 
    CompData_UD_PD, CompData_SD_PD
  ))
  def ofDataSepResp(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXDAT, opcode, Seq(
    DataSepResp_I, DataSepResp_UC, DataSepResp_SC, DataSepResp_UD_PD
  ))
  def ofRespSepData(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXRSP, opcode, Seq(
    RespSepData_I, RespSepData_UC, RespSepData_SC, RespSepData_UD_PD
  ))
  def ofSnpResp(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXRSP, opcode, Seq(
    SnpResp_I, SnpResp_SC, SnpResp_UC, SnpResp_UD, SnpResp_SD,
  ))
  def ofSnpRespData(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXDAT, opcode, Seq(
    SnpRespData_I, SnpRespData_UC_UD, SnpRespData_SC, SnpRespData_SD,
    SnpRespData_I_PD, SnpRespData_UC_PD, SnpRespData_SC_PD
  ))
  def ofSnpRespDataPtl(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXDAT, opcode, Seq(
    SnpRespDataPtl_I_PD, SnpRespDataPtl_UD
  ))
  def ofNonCopyBackWrData(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXDAT, opcode, Seq(new CHICohStateTrans(() => 0.U)))
  def ofNCBWrDataCompAck(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXDAT, opcode, Seq(new CHICohStateTrans(() => 0.U)))
  def ofWriteDataCancel(opcode: UInt) = new CHICohStateTransSet(() => CHIChannel.TXDAT, opcode, Seq(new CHICohStateTrans(() => 0.U)))
}

class CHICohStateFwdedTrans(val resp: () => UInt, val fwdState: () => UInt)

object CHICohStateFwdedTrans {

  val SnpResp_I_Fwded_I = new CHICohStateFwdedTrans(() => I, () => I)
  val SnpResp_I_Fwded_SC = new CHICohStateFwdedTrans(() => I, () => SC)
  val SnpResp_I_Fwded_UC = new CHICohStateFwdedTrans(() => I, () => UC)
  val SnpResp_I_Fwded_UD_PD = new CHICohStateFwdedTrans(() => I, () => UD_PD)
  val SnpResp_I_Fwded_SD_PD = new CHICohStateFwdedTrans(() => I, () => SD_PD)
  val SnpResp_SC_Fwded_I = new CHICohStateFwdedTrans(() => SC, () => I)
  val SnpResp_SC_Fwded_SC = new CHICohStateFwdedTrans(() => SC, () => SC)
  val SnpResp_SC_Fwded_SD_PD = new CHICohStateFwdedTrans(() => SC, () => SD_PD)
  val SnpResp_UC_UD_Fwded_I = new CHICohStateFwdedTrans(() => UC, () => I)
  val SnpResp_SD_Fwded_I = new CHICohStateFwdedTrans(() => SD, () => I)
  val SnpResp_SD_Fwded_SC = new CHICohStateFwdedTrans(() => SD, () => SC)

  val SnpRespData_I_Fwded_SC = new CHICohStateFwdedTrans(() => I, () => SC)
  val SnpRespData_I_Fwded_SD_PD = new CHICohStateFwdedTrans(() => I, () => SD_PD)
  val SnpRespData_SC_Fwded_SC = new CHICohStateFwdedTrans(() => SC, () => SC)
  val SnpRespData_SC_Fwded_SD_PD = new CHICohStateFwdedTrans(() => SC, () => SD_PD)
  val SnpRespData_SD_Fwded_SC = new CHICohStateFwdedTrans(() => SD, () => SC)
  val SnpRespData_I_PD_Fwded_I = new CHICohStateFwdedTrans(() => I_PD, () => I)
  val SnpRespData_I_PD_Fwded_SC = new CHICohStateFwdedTrans(() => I_PD, () => SC)
  val SnpRespData_SC_PD_Fwded_I = new CHICohStateFwdedTrans(() => SC_PD, () => I)
  val SnpRespData_SC_PD_Fwded_SC = new CHICohStateFwdedTrans(() => SC_PD, () => SC)
}

class CHICohStateFwdedTransSet(val channel: () => UInt, val opcode: UInt, val set: Seq[CHICohStateFwdedTrans])

object CHICohStateFwdedTransSet {
  
  def isValid(set: CHICohStateFwdedTransSet, channel: UInt, opcode: UInt, resp: UInt, fwdState: UInt): Bool =
    channel =/= set.channel() || opcode =/= set.opcode || VecInit(set.set.map(t => t.resp() === resp && t.fwdState() === fwdState)).asUInt.orR
  
  def ofSnpResp(opcode: UInt) = new CHICohStateFwdedTransSet(() => CHIChannel.TXRSP, opcode, Seq(
    SnpResp_I_Fwded_I, SnpResp_I_Fwded_SC, SnpResp_I_Fwded_UC,
    SnpResp_I_Fwded_UD_PD, SnpResp_I_Fwded_SD_PD,
    SnpResp_SC_Fwded_I, SnpResp_SC_Fwded_SC, SnpResp_SC_Fwded_SD_PD,
    SnpResp_UC_UD_Fwded_I,
    SnpResp_SD_Fwded_I, SnpResp_SD_Fwded_SC
  ))
  def ofSnpRespData(opcode: UInt)  = new CHICohStateFwdedTransSet(() => CHIChannel.TXDAT, opcode, Seq(
    SnpRespData_I_Fwded_SC, SnpRespData_I_Fwded_SD_PD,
    SnpRespData_SC_Fwded_SC, SnpRespData_SC_Fwded_SD_PD,
    SnpRespData_SD_Fwded_SC,
    SnpRespData_I_PD_Fwded_I, SnpRespData_I_PD_Fwded_SC,
    SnpRespData_SC_PD_Fwded_I, SnpRespData_SC_PD_Fwded_SC
  ))
}

object OrderEncodings {
  val width = 2

  def None = "b00".U(width.W)
  def RequestAccepted = "b01".U(width.W)
  def RequestOrder = "b10".U(width.W)
  def OWO = "b10".U(width.W) // Ordered Write Observation
  def EndpointOrder = "b11".U(width.W)

  def isRequestOrder(order: UInt): Bool = order >= RequestOrder
}

object RespErrEncodings {
  val width = 2

  def OK = "b00".U(width.W) // Okay
  def EXOK = "b01".U(width.W) // Exclusive Okay
  def DERR = "b10".U(width.W) // Data Error
  def NDERR = "b11".U(width.W) // Non-data Error
}


/**
  * This object collects constants and related helper methods
  * to support different CHI issues (versions).
  */
object Issue {
  val B = "B"
  val Eb = "E.b"
}

trait HasCHIMsgParameters {
  implicit val p: Parameters

  def ENABLE_ISSUE_Eb = p(CHIIssue) == Issue.Eb
  def ENABLE_ISSUE_B = p(CHIIssue) == Issue.B

  val ISSUE_B_CONFIG = Map(
    "NODEID_WIDTH" -> 7,
    "TXNID_WIDTH" -> 8,
    "LPID_WITH_PADDING_WIDTH" -> 5,
    "REQ_OPCODE_WIDTH" -> 6,
    "RSP_OPCODE_WIDTH" -> 4,
    "SNP_OPCODE_WIDTH" -> 5,
    "DAT_OPCODE_WIDTH" -> 3
  )

  val ISSUE_Eb_CONFIG = Map(
    "NODEID_WIDTH" -> 11,
    "TXNID_WIDTH" -> 12,
    "LPID_WITH_PADDING_WIDTH" -> 8,
    "REQ_OPCODE_WIDTH" -> 7,
    "RSP_OPCODE_WIDTH" -> 5,
    "SNP_OPCODE_WIDTH" -> 5,
    "DAT_OPCODE_WIDTH" -> 4
  )

  val SUPPORT_ISSUE_CONFIG = Map(
    Issue.B -> ISSUE_B_CONFIG,
    Issue.Eb -> ISSUE_Eb_CONFIG
  )

  def issue_config(key: String) = SUPPORT_ISSUE_CONFIG(p(CHIIssue))(key)

  // Scala by-name argument
  def field_Eb[T <: Data](d: T): Option[T] = {
    if (ENABLE_ISSUE_Eb) Some(d) else None
  }

  def NODEID_WIDTH = issue_config("NODEID_WIDTH")
  require(NODEID_WIDTH >= 7 && NODEID_WIDTH <= 11)

  // Transaction request fields
  def QOS_WIDTH = 4
  def TGTID_WIDTH = NODEID_WIDTH
  def SRCID_WIDTH = NODEID_WIDTH
  def TXNID_WIDTH = issue_config("TXNID_WIDTH") // An 8-bit field is defined for the TxnID to accommodate up to 256 outstanding transactions
  def LPID_WITH_PADDING_WIDTH = issue_config("LPID_WITH_PADDING_WIDTH")
  def LPID_WIDTH = 5
  def RETURNNID_WIDTH = NODEID_WIDTH
  def RETURNTXNID_WIDTH = TXNID_WIDTH
  def STASHNID_WIDTH = NODEID_WIDTH
  def STASHLPID_WIDTH = LPID_WIDTH
  def TAGOP_WIDTH = 2


  def REQ_OPCODE_WIDTH = issue_config("REQ_OPCODE_WIDTH")
  def RSP_OPCODE_WIDTH = issue_config("RSP_OPCODE_WIDTH")
  def SNP_OPCODE_WIDTH = issue_config("SNP_OPCODE_WIDTH")
  def DAT_OPCODE_WIDTH = issue_config("DAT_OPCODE_WIDTH")
  def OPCODE_WIDTH = max(REQ_OPCODE_WIDTH, max(RSP_OPCODE_WIDTH, max(SNP_OPCODE_WIDTH, DAT_OPCODE_WIDTH)))

  def ADDR_WIDTH = 48
  def SNP_ADDR_WIDTH = ADDR_WIDTH - 3
  def SIZE_WIDTH = 3
  def PCRDTYPE_WIDTH = 4
  def MEMATTR_WIDTH = 4
  def ORDER_WIDTH = OrderEncodings.width

  // Snoop request fields
  def FWDNID_WIDTH = NODEID_WIDTH
  def FWDTXNID_WIDTH = TXNID_WIDTH
  def VMIDEXT_WIDTH = 8

  // Data fields && Response fields
  def HOMENID_WIDTH = NODEID_WIDTH
  def DBID_WIDTH = TXNID_WIDTH
  def RESPERR_WIDTH = RespErrEncodings.width
  def RESP_WIDTH = CHICohStates.width
  def FWDSTATE_WIDTH = CHICohStates.width
  def DATAPULL_WIDTH = 3
  def DATASOURCE_WIDTH = if (ENABLE_ISSUE_Eb) 4 else 3
  def CCID_WIDTH = 2
  def DATAID_WIDTH = 2
  def BE_WIDTH = DATA_WIDTH / 8
  def DATA_WIDTH = 256
  def DATACHECK_WIDTH = DATA_WIDTH / 8
  def TAG_WIDTH = DATA_WIDTH / 32
  def TAG_UPDATE_WIDTH = DATA_WIDTH / 128

  // User defined
  /*
  * Currently don't care about *::RSVDC, and the width is tied to 4.
  */
  def REQ_RSVDC_WIDTH = 4 // Permitted RSVDC bus widths X = 0, 4, 12, 16, 24, 32
  def DAT_RSVDC_WIDTH = 4 // Permitted RSVDC bus widths Y = 0, 4, 12, 16, 24, 32
  def CBUSY_WIDTH = 3 // E.b field. The width is tied to 3. Custom completer state indicator.
  def MPAM_WIDTH = 11 // E.b field. Optional, width 0 or 11. Memory Performance and Monitoring.
}

class MemAttr extends Bundle {
  // The Allocate attribute is a an allocation hint.
  // It indicates the recommended allocation policy for a transaction.
  val allocate = Bool()
  // The Cacheable attribute indicates if a transaction must perform a cache lookup.
  val cacheable = Bool()
  // Device attribute indicates if the memory type is either Device or Normal.
  val device = Bool()
  // Early Write Acknowledge (EWA)
  // EWA indicates whether the write completion response for a transaction:
  // If true, comp is permitted to come from an intermediate point in the interconnect, such as a Home Node.
  // If false, comp must come from the final endpoint that a transaction is destined for.
  val ewa = Bool()
}

object MemAttr {
  def apply(allocate: Bool, cacheable: Bool, device: Bool, ewa: Bool): MemAttr = {
    val memAttr = Wire(new MemAttr)
    memAttr.allocate := allocate
    memAttr.cacheable := cacheable
    memAttr.device := device
    memAttr.ewa := ewa
    memAttr
  }
  def apply(): MemAttr = apply(false.B, false.B, false.B, false.B)
}

abstract class CHIBundle(implicit val p: Parameters) extends Bundle with HasCHIMsgParameters

class CHIREQ(implicit p: Parameters) extends CHIBundle {
  // BE CAUTIOUS with the order of the flit fields

  /* LSB */
  val qos = UInt(QOS_WIDTH.W)
  val tgtID = UInt(TGTID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)

  val returnNID = UInt(RETURNNID_WIDTH.W) // Used for DMT
  def stashNID = returnNID // Used for Stash
  def slcRepHint = returnTxnID(6, 0)  // E.b field

  val stashNIDValid = Bool() // Used for Stash
  def endian = stashNIDValid // Used for Atomic
  def deep = stashNIDValid // E.b field

  val returnTxnID = UInt(RETURNTXNID_WIDTH.W)
  def stashLPID = returnTxnID(STASHLPID_WIDTH - 1, 0)
  def stashLPIDValid = returnTxnID(STASHLPID_WIDTH).asBool

  val opcode = UInt(REQ_OPCODE_WIDTH.W)
  val size = UInt(SIZE_WIDTH.W)
  val addr = UInt(ADDR_WIDTH.W)
  val ns = Bool()
  val likelyshared = Bool()
  val allowRetry = Bool()
  val order = UInt(ORDER_WIDTH.W)
  val pCrdType = UInt(PCRDTYPE_WIDTH.W)
  val memAttr = new MemAttr()
  val snpAttr = Bool()
  def doDWT = snpAttr // E.b field
  val lpIDWithPadding = UInt(LPID_WITH_PADDING_WIDTH.W)
  def lpID = lpIDWithPadding(LPID_WIDTH - 1, 0) // E.b field
  def pGroupID = lpIDWithPadding // E.b field
  def stashGroupID = lpIDWithPadding // E.b field
  def tagGroupID = lpIDWithPadding // E.b field

  val snoopMe = Bool() // Used for Atomic
  def excl = snoopMe // Used for Exclusive transactions

  val expCompAck = Bool()
  val tagOp = field_Eb(UInt(TAGOP_WIDTH.W))
  val traceTag = Bool()
  val mpam = field_Eb(UInt(MPAM_WIDTH.W))
  val rsvdc = UInt(REQ_RSVDC_WIDTH.W)

  /* MSB */
}

class CHISNP(implicit p: Parameters) extends CHIBundle {
  // BE CAUTIOUS with the order of the flit fields

  /* LSB */
  val qos = UInt(QOS_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val fwdNID = UInt(FWDNID_WIDTH.W)

  val fwdTxnID = UInt(FWDTXNID_WIDTH.W)
  def stashLPID = fwdTxnID(STASHLPID_WIDTH - 1, 0)
  def stashLPIDValid = fwdTxnID(STASHLPID_WIDTH)
  def vmIDExt = fwdTxnID(VMIDEXT_WIDTH - 1, 0)

  val opcode = UInt(SNP_OPCODE_WIDTH.W)
  val addr = UInt(SNP_ADDR_WIDTH.W)
  val ns = Bool()

  val doNotGoToSD = Bool()
  def doNotDataPull = doNotGoToSD

  val retToSrc = Bool()
  val traceTag = Bool()
  val mpam = field_Eb(UInt(MPAM_WIDTH.W))

  /* MSB */
}

class CHIDAT(implicit p: Parameters) extends CHIBundle {
  // BE CAUTIOUS with the order of the flit fields

  /* LSB */
  val qos = UInt(QOS_WIDTH.W)
  val tgtID = UInt(TGTID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val homeNID = UInt(HOMENID_WIDTH.W)
  val opcode = UInt(DAT_OPCODE_WIDTH.W)
  val respErr = UInt(RESPERR_WIDTH.W)
  val resp = UInt(RESP_WIDTH.W)

  val fwdState = UInt(FWDSTATE_WIDTH.W) // Used for DCT
  val dataSourceHi = field_Eb(UInt((DATASOURCE_WIDTH - FWDSTATE_WIDTH).W))
  def dataPull = fwdState // Used for Stash
  def dataSource = if (ENABLE_ISSUE_Eb) Cat(dataSourceHi.get, fwdState) else fwdState // Indicates Data source in a response

  val cBusy = field_Eb(UInt(CBUSY_WIDTH.W))

  val dbID = UInt(DBID_WIDTH.W)
  val ccID = UInt(CCID_WIDTH.W)
  val dataID = UInt(DATAID_WIDTH.W)

  val tagOp = field_Eb(UInt(TAGOP_WIDTH.W))
  val tag = field_Eb(UInt(TAG_WIDTH.W))
  val tu = field_Eb(UInt(TAG_UPDATE_WIDTH.W))

  val traceTag = Bool()
  val rsvdc = UInt(DAT_RSVDC_WIDTH.W)
  val be = UInt(BE_WIDTH.W)
  val data = UInt(DATA_WIDTH.W)

  /* MSB */
}

class CHIRSP(implicit p: Parameters) extends CHIBundle {
  // BE CAUTIOUS with the order of the flit fields

  /* LSB */
  val qos = UInt(QOS_WIDTH.W)
  val tgtID = UInt(TGTID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val opcode = UInt(RSP_OPCODE_WIDTH.W)
  val respErr = UInt(RESPERR_WIDTH.W)
  val resp = UInt(RESP_WIDTH.W)

  val fwdState = UInt(FWDSTATE_WIDTH.W)
  def dataPull = fwdState

  val cBusy = field_Eb(UInt(CBUSY_WIDTH.W))

  val dbID = UInt(DBID_WIDTH.W)
  def pGroupID = dbID(LPID_WITH_PADDING_WIDTH - 1, 0)
  def stashGroupID = dbID(LPID_WITH_PADDING_WIDTH - 1, 0)
  def tagGroupID = dbID(LPID_WITH_PADDING_WIDTH - 1, 0)

  val pCrdType = UInt(PCRDTYPE_WIDTH.W)

  val tagOp = field_Eb(UInt(TAGOP_WIDTH.W))

  val traceTag = Bool()
  /* MSB */
}
