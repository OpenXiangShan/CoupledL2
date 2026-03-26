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
import coupledL2.{L2ParamKey, TaskBundle}
import coupledL2.tl2chi.CHICohStates._
import coupledL2.tl2chi.CHICohStateTrans._
import coupledL2.tl2chi.CHICohStateFwdedTrans._

case object CHIIssue extends Field[String](Issue.B)

case object NonSecureKey extends Field[Boolean](false)

case object CHIAddrWidthKey extends Field[Int](48)

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
  
  def ofSnpRespFwded(opcode: UInt) = new CHICohStateFwdedTransSet(() => CHIChannel.TXRSP, opcode, Seq(
    SnpResp_I_Fwded_I, SnpResp_I_Fwded_SC, SnpResp_I_Fwded_UC,
    SnpResp_I_Fwded_UD_PD, SnpResp_I_Fwded_SD_PD,
    SnpResp_SC_Fwded_I, SnpResp_SC_Fwded_SC, SnpResp_SC_Fwded_SD_PD,
    SnpResp_UC_UD_Fwded_I,
    SnpResp_SD_Fwded_I, SnpResp_SD_Fwded_SC
  ))
  def ofSnpRespDataFwded(opcode: UInt)  = new CHICohStateFwdedTransSet(() => CHIChannel.TXDAT, opcode, Seq(
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
  val C = "C"
  val Eb = "E.b"
}

trait HasCHIMsgParameters {
  implicit val p: Parameters

  val issue = p(CHIIssue)
  val l2CacheParams = p(L2ParamKey)

  val DEFAULT_CONFIG = Map(
    "QOS_WIDTH" -> 4,
    "NODEID_WIDTH" -> 7,
    "TXNID_WIDTH" -> 8,
    "LPID_WITH_PADDING_WIDTH" -> 5,
    "LPID_WIDTH" -> 5,
    "REQ_OPCODE_WIDTH" -> 6,
    "RSP_OPCODE_WIDTH" -> 4,
    "SNP_OPCODE_WIDTH" -> 5,
    "DAT_OPCODE_WIDTH" -> 3,
    "ADDR_WIDTH" -> p(CHIAddrWidthKey),
    "SIZE_WIDTH" -> 3,
    "PCRDTYPE_WIDTH" -> 4,
    "MEMATTR_WIDTH" -> 4,
    "ORDER_WIDTH" -> 2,
    "VMIDEXT_WIDTH" -> 8,
    "RESPERR_WIDTH" -> 2,
    "RESP_WIDTH" -> 3,
    "FWDSTATE_WIDTH" -> 3,
    "DATAPULL_WIDTH" -> 3,
    "DATASOURCE_WIDTH" -> 3,
    "CCID_WIDTH" -> 2,
    "DATAID_WIDTH" -> 2,
    "DATA_WIDTH" -> 256,
    "REQ_RSVDC_WIDTH" -> 4,
    "DAT_RSVDC_WIDTH" -> 4
  )

  val B_CONFIG = DEFAULT_CONFIG

  val C_CONFIG = B_CONFIG ++ Map(
    // new width def for existing fields
    "NODEID_WIDTH" -> 9,
    "DAT_OPCODE_WIDTH" -> 4,
  )

  val Eb_CONFIG = C_CONFIG ++ Map(
    // new width def for existing fields
    "NODEID_WIDTH" -> 11,
    "TXNID_WIDTH" -> 12,
    "LPID_WITH_PADDING_WIDTH" -> 8,
    "DATASOURCE_WIDTH" -> 4,
    "REQ_OPCODE_WIDTH" -> 7,
    "RSP_OPCODE_WIDTH" -> 5,
    "SNP_OPCODE_WIDTH" -> 5,
    "DAT_OPCODE_WIDTH" -> 4,
    // new fields
    "CBUSY_WIDTH" -> 3,
    "MPAM_WIDTH" -> 11,
    "SLCREPHINT_WIDTH" -> 7,
    "TAGOP_WIDTH" -> 2
  )

  val params = Map(
    Issue.B -> B_CONFIG,
    Issue.C -> C_CONFIG,
    Issue.Eb -> Eb_CONFIG
  )(issue)

  def CONFIG(key: String): Int = {
    if (params.contains(key)) params(key) else 0
  }

  def after(x: String, y: String): Boolean = x.compareTo(y) >= 0

  def B_FIELD[T <: Data](x: T): T = x
  def C_FIELD[T <: Data](x: T): Option[T] = if (after(issue, Issue.C)) Some(x) else None
  // def D_FIELD[T <: Data](x: T): Option[T] = if (after(issue, Issue.D)) Some(x) else None
  // def Ea_FIELD[T <: Data](x: T): Option[T] = if (after(issue, Issue.Ea)) Some(x) else None
  def Eb_FIELD[T <: Data](x: T): Option[T] = if (after(issue, Issue.Eb)) Some(x) else None

  def dataCheckMethod : Int = l2CacheParams.dataCheck.getOrElse("none").toLowerCase match {
    case "none" => 0
    case "oddparity" => 1
    case "secded" => 2
    case _ => 0
  }
  def enableDataCheck = dataCheckMethod != 0
  def enablePoison = l2CacheParams.enablePoison

  def NODEID_WIDTH = CONFIG("NODEID_WIDTH")

  // Transaction request fields
  def QOS_WIDTH = CONFIG("QOS_WIDTH")
  def TGTID_WIDTH = NODEID_WIDTH
  def SRCID_WIDTH = NODEID_WIDTH
  def TXNID_WIDTH = CONFIG("TXNID_WIDTH") // An 8-bit field is defined for the TxnID to accommodate up to 256 outstanding transactions
  def LPID_WITH_PADDING_WIDTH = CONFIG("LPID_WITH_PADDING_WIDTH")
  def LPID_WIDTH = CONFIG("LPID_WIDTH")
  def RETURNNID_WIDTH = NODEID_WIDTH
  def SLCREPHINT_WIDTH = CONFIG("SLCREPHINT_WIDTH")
  def RETURNTXNID_WIDTH = TXNID_WIDTH
  def STASHNID_WIDTH = NODEID_WIDTH
  def STASHLPID_WIDTH = LPID_WIDTH
  def TAGOP_WIDTH = CONFIG("TAGOP_WIDTH")

  def REQ_OPCODE_WIDTH = CONFIG("REQ_OPCODE_WIDTH")
  def RSP_OPCODE_WIDTH = CONFIG("RSP_OPCODE_WIDTH")
  def SNP_OPCODE_WIDTH = CONFIG("SNP_OPCODE_WIDTH")
  def DAT_OPCODE_WIDTH = CONFIG("DAT_OPCODE_WIDTH")
  def OPCODE_WIDTH = max(REQ_OPCODE_WIDTH, max(RSP_OPCODE_WIDTH, max(SNP_OPCODE_WIDTH, DAT_OPCODE_WIDTH)))

  def ADDR_WIDTH = CONFIG("ADDR_WIDTH")
  def SNP_ADDR_WIDTH = ADDR_WIDTH - 3
  def SIZE_WIDTH = CONFIG("SIZE_WIDTH")
  def PCRDTYPE_WIDTH = CONFIG("PCRDTYPE_WIDTH")
  def MEMATTR_WIDTH = CONFIG("MEMATTR_WIDTH")
  def ORDER_WIDTH = CONFIG("ORDER_WIDTH")

  // Snoop request fields
  def FWDNID_WIDTH = NODEID_WIDTH
  def FWDTXNID_WIDTH = TXNID_WIDTH
  def VMIDEXT_WIDTH = CONFIG("VMIDEXT_WIDTH")

  // Data fields && Response fields
  def HOMENID_WIDTH = NODEID_WIDTH
  def DBID_WIDTH = TXNID_WIDTH
  def RESPERR_WIDTH = CONFIG("RESPERR_WIDTH")
  def RESP_WIDTH = CONFIG("RESP_WIDTH")
  def FWDSTATE_WIDTH = CONFIG("FWDSTATE_WIDTH")
  def DATAPULL_WIDTH = CONFIG("DATAPULL_WIDTH")
  def DATASOURCE_WIDTH = CONFIG("DATASOURCE_WIDTH")
  def CCID_WIDTH = CONFIG("CCID_WIDTH")
  def DATAID_WIDTH = CONFIG("DATAID_WIDTH")
  def BE_WIDTH = DATA_WIDTH / 8
  def DATA_WIDTH = CONFIG("DATA_WIDTH")
  def DATACHECK_WIDTH = DATA_WIDTH / 8
  def POISON_WIDTH = DATA_WIDTH / 64
  def TAG_WIDTH = DATA_WIDTH / 32
  def TAG_UPDATE_WIDTH = DATA_WIDTH / 128

  // DataID is assigned with the granularity of a 16-byte chunk
  def ChunkBytes = 16
  def ChunkOffsetWidth = log2Up(ChunkBytes)

  // User defined
  /*
  * Currently don't care about *::RSVDC, and the width is tied to 4.
  */
  def REQ_RSVDC_WIDTH = CONFIG("REQ_RSVDC_WIDTH") // Permitted RSVDC bus widths X = 0, 4, 12, 16, 24, 32
  def DAT_RSVDC_WIDTH = CONFIG("DAT_RSVDC_WIDTH") // Permitted RSVDC bus widths Y = 0, 4, 12, 16, 24, 32
  def CBUSY_WIDTH = CONFIG("CBUSY_WIDTH") // E.b field. The width is tied to 3. Custom completer state indicator.
  def MPAM_WIDTH = CONFIG("MPAM_WIDTH") // E.b field. Optional, width 0 or 11. Memory Performance and Monitoring.

  def enableNS = p(NonSecureKey)

  require(ADDR_WIDTH >= 44 && ADDR_WIDTH <= 52)
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

class MPAM extends Bundle {
  // The resources are partitioned among users by Partition ID (PartID) and Performance Monitoring Group (PerfMonGroup)
  val perfMonGroup = UInt(1.W)
  val partID = UInt(9.W)
  //  A Non-secure bit in the MPAM field, this is in addition to and different from the NS bit of the request.
  //  The polarity of the MPAMNS bit encoding is the same as that of the NS bit.
  val mpamNS = Bool()
}

object MPAM {
  def apply(perfMonGroup: UInt, partID: UInt, mpamNS: Bool): MPAM = {
    val mpam = Wire(new MPAM)
    mpam.perfMonGroup := perfMonGroup
    mpam.partID := partID
    mpam.mpamNS := mpamNS
    mpam
  }
  def apply(mpamNS: Bool): MPAM = apply(0.U, 0.U, mpamNS) // See Default values for MPAM subfields 
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
  def slcRepHint = Eb_FIELD(returnNID(SLCREPHINT_WIDTH - 1, 0)) // Used in cache line replacement algorithms

  val stashNIDValid = Bool() // Used for Stash
  def endian = stashNIDValid // Used for Atomic
  def deep = Eb_FIELD(stashNIDValid) // Used in CleanSharedPersist* transactions

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
  def doDWT = Eb_FIELD(snpAttr) // Used for DWT

  val lpIDWithPadding = UInt(LPID_WITH_PADDING_WIDTH.W)
  def lpID = lpIDWithPadding(LPID_WIDTH - 1, 0)
  def pGroupID = Eb_FIELD(lpIDWithPadding) // Used in Persistent CMO transactions
  def stashGroupID = Eb_FIELD(lpIDWithPadding) // Used in the StashOnceSep transaction
  def tagGroupID = Eb_FIELD(lpIDWithPadding) // Used for Memory Tagging

  val snoopMe = Bool() // Used for Atomic
  def excl = snoopMe // Used for Exclusive transactions

  val expCompAck = Bool()
  val tagOp = Eb_FIELD(UInt(TAGOP_WIDTH.W))
  val traceTag = Bool()
  val mpam = Eb_FIELD(new MPAM())
  val rsvdc = UInt(REQ_RSVDC_WIDTH.W)

  /* MSB */

  require(mpam.map(_.getWidth).orElse(Some(0)).get == MPAM_WIDTH, "configured MPAM width mismatch")
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
  def stashLPIDValid = fwdTxnID(STASHLPID_WIDTH).asBool
  def vmIDExt = fwdTxnID(VMIDEXT_WIDTH - 1, 0)

  val opcode = UInt(SNP_OPCODE_WIDTH.W)
  val addr = UInt(SNP_ADDR_WIDTH.W)
  val ns = Bool()

  val doNotGoToSD = Bool()
  def doNotDataPull = doNotGoToSD

  val retToSrc = Bool()
  val traceTag = Bool()
  val mpam = Eb_FIELD(new MPAM())

  /* MSB */

  require(mpam.map(_.getWidth).orElse(Some(0)).get == MPAM_WIDTH, "configured MPAM width mismatch")
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

  val dataSource = UInt(DATASOURCE_WIDTH.W)
  def fwdState = dataSource(FWDSTATE_WIDTH - 1, 0)
  def dataPull = dataSource(DATAPULL_WIDTH - 1, 0)
  def setFwdState(s: UInt): Unit = {
    require(s.getWidth == FWDSTATE_WIDTH)
    dataSource := s
  }

  val cBusy = Eb_FIELD(UInt(CBUSY_WIDTH.W))

  val dbID = UInt(DBID_WIDTH.W)
  val ccID = UInt(CCID_WIDTH.W)
  val dataID = UInt(DATAID_WIDTH.W)

  val tagOp = Eb_FIELD(UInt(TAGOP_WIDTH.W))
  val tag = Eb_FIELD(UInt(TAG_WIDTH.W))
  val tu = Eb_FIELD(UInt(TAG_UPDATE_WIDTH.W))

  val traceTag = Bool()
  val rsvdc = UInt(DAT_RSVDC_WIDTH.W)
  val be = UInt(BE_WIDTH.W)
  val data = UInt(DATA_WIDTH.W)

  val dataCheck = Option.when(enableDataCheck) {
    UInt(DATACHECK_WIDTH.W)
  }
  val poison = Option.when(enablePoison) {
    UInt(POISON_WIDTH.W)
  }

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

  val cBusy = Eb_FIELD(UInt(CBUSY_WIDTH.W))

  val dbID = UInt(DBID_WIDTH.W)
  def pGroupID = Eb_FIELD(dbID(LPID_WITH_PADDING_WIDTH - 1, 0))
  def stashGroupID = Eb_FIELD(dbID(LPID_WITH_PADDING_WIDTH - 1, 0))
  def tagGroupID = Eb_FIELD(dbID(LPID_WITH_PADDING_WIDTH - 1, 0))

  val pCrdType = UInt(PCRDTYPE_WIDTH.W)

  val tagOp = Eb_FIELD(UInt(TAGOP_WIDTH.W))

  val traceTag = Bool()
  /* MSB */
}

// CHIRSP_withAddr is CHIRSP with an additional 'addr' field which is used for release to loadQueueRAR
class CHIRSP_withAddr(implicit p: Parameters) extends CHIRSP {
  val addr = UInt(ADDR_WIDTH.W)
}
object CHIRSP_withAddr{
  def apply(chirsp: CHIRSP, addr: UInt)(implicit p: Parameters): CHIRSP_withAddr = {
    val chirsp_with_addr = Wire(new CHIRSP_withAddr)
    chirsp.elements.foreach { case (name, data) =>
      chirsp_with_addr.elements(name) := data
    }
    chirsp_with_addr.addr := addr
    chirsp_with_addr
  }
  def toCHIRSP(chirsp_with_addr: CHIRSP_withAddr)(implicit p: Parameters): CHIRSP = {
    val chirsp = Wire(new CHIRSP)
    chirsp_with_addr.elements.foreach { case (name, data) =>
      if (chirsp.elements.contains(name)) {
        chirsp.elements(name) := data
      }
    }
    chirsp
  }
}

// CHIDAT_withAddr is CHIDAT with an additional 'addr' field which is used for release to loadQueueRAR
class CHIDAT_withAddr(implicit p: Parameters) extends CHIDAT {
  val addr = UInt(ADDR_WIDTH.W)
}
object CHIDAT_withAddr{
  def apply(chidat: CHIDAT, addr: UInt)(implicit p: Parameters): CHIDAT_withAddr = {
    val chidat_with_addr = Wire(new CHIDAT_withAddr)
    chidat.elements.foreach { case (name, data) =>
      chidat_with_addr.elements(name) := data
    }
    chidat_with_addr.addr := addr
    chidat_with_addr
  }
  def toCHIDAT(chidat_with_addr: CHIDAT_withAddr)(implicit p: Parameters): CHIDAT = {
    val chidat = Wire(new CHIDAT)
    chidat_with_addr.elements.foreach { case (name, data) =>
      if (chidat.elements.contains(name)) {
        chidat.elements(name) := data
      }
    }
    chidat
  }
}