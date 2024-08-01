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
    if (p(CHIIssue) == Issue.Eb) Some(d) else None
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
  def DATASOURCE_WIDTH = if (p(CHIIssue) == Issue.Eb) 4 else 3
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
  def dataSource = if (p(CHIIssue) == Issue.Eb) Cat(dataSourceHi.get, fwdState) else fwdState // Indicates Data source in a response

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
