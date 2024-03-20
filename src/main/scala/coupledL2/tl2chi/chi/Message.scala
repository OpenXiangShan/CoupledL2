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
import scala.math.max
import coupledL2.TaskBundle

object CHICohStates {
  val width = 3

  def I = "b000".U
  def SC = "b001".U
  def UC = "b010".U
  def UD = "b010".U
  def SD = "b011".U

  def PassDirty = "b100".U

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

  def None = "b00".U
  def RequestAccepted = "b01".U
  def RequestOrder = "b10".U
  def OWO = "b10".U // Ordered Write Observation
  def EndpointOrder = "b11".U

  def isRequestOrder(order: UInt): Bool = order >= RequestOrder
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

// object MemAttr extends HasCHIMsgParameters {
//   def apply(memAttr: UInt): MemAttr = {
//     require(memAttr.getWidth >= MEMATTR_WIDTH)
//     val attr = Wire(new MemAttr)
//     attr.ewa := memAttr(0).asBool
//     attr.device := memAttr(1).asBool
//     attr.cacheable := memAttr(2).asBool
//     attr.allocate := memAttr(3).asBool
//     attr
//   }
//   def apply(): MemAttr = apply(0.U(MEMATTR_WIDTH.W))
// }

trait HasCHIMsgParameters {
  // TODO: Comfirm the fields and their corresponding width
  def NODEID_WIDTH = 7
  require(NODEID_WIDTH >= 7 && NODEID_WIDTH <= 11)

  // Transaction request fields
  def QOS_WIDTH = 4
  def TGTID_WIDTH = NODEID_WIDTH
  def SRCID_WIDTH = NODEID_WIDTH
  def TXNID_WIDTH = 8 // An 8-bit field is defined for the TxnID to accommodate up to 256 outstanding transactions
  def LPID_WIDTH = 4 // TODO: To be confirmed
  def RETURNNID_WIDTH = NODEID_WIDTH
  def RETURNTXNID_WIDTH = TXNID_WIDTH
  def STASHNID_WIDTH = NODEID_WIDTH
  def STASHLPID_WIDTH = LPID_WIDTH

  def REQ_OPCODE_WIDTH = CHIOpcode.REQOpcodes.width
  def RSP_OPCODE_WIDTH = CHIOpcode.RSPOpcodes.width
  def SNP_OPCODE_WIDTH = CHIOpcode.SNPOpcodes.width
  def DAT_OPCODE_WIDTH = CHIOpcode.DATOpcodes.width
  def OPCODE_WIDTH = max(REQ_OPCODE_WIDTH, max(RSP_OPCODE_WIDTH, max(SNP_OPCODE_WIDTH, DAT_OPCODE_WIDTH)))

  def ADDR_WIDTH = 44 // TODO: To be confirmed
  def SIZE_WIDTH = 3
  def PCRDTYPE_WIDTH = 4
  def MEMATTR_WIDTH = 4
  def ORDER_WIDTH = OrderEncodings.width

  // Snoop request fields
  def FWDNID_WIDTH = NODEID_WIDTH
  def FWDTXNID_WIDTH = TXNID_WIDTH
  def VMIDEXT_WIDTH = 8 // TODO: To be confirmed

  // Data fields && Response fields
  def HOMENID_WIDTH = NODEID_WIDTH
  def DBID_WIDTH = TXNID_WIDTH
  def RESPERR_WIDTH = 2
  def RESP_WIDTH = CHICohStates.width
  def FWDSTATE_WIDTH = CHICohStates.width
  def DATASOURCE_WIDTH = 3
  def CCID_WIDTH = 2 // TODO: To be confirmed
  def DATAID_WIDTH = 2 // TODO: To be confirmed
  def BE_WIDTH = DATA_WIDTH / 8
  def DATA_WIDTH = 256
  def DATACHECK_WIDTH = DATA_WIDTH / 8

}

abstract class CHIBundle extends Bundle with HasCHIMsgParameters

class CHIREQ extends CHIBundle {
  val tgtID = UInt(TGTID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val opcode = UInt(REQ_OPCODE_WIDTH.W)
  val addr = UInt(ADDR_WIDTH.W)
  val allowRetry = Bool()
  val pCrdType = UInt(PCRDTYPE_WIDTH.W)
  val expCompAck = Bool()
  val memAttr = new MemAttr()
  val snpAttr = Bool()
  val order = UInt(ORDER_WIDTH.W)
  // TODO: Finish this
}

class CHISNP extends CHIBundle {
  val txnID = UInt(TXNID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val opcode = UInt(SNP_OPCODE_WIDTH.W)
  val addr = UInt(ADDR_WIDTH.W)
  val retToSrc = Bool()
  // TODO: Finish this
}

class CHIDAT extends CHIBundle {
  val tgtID = UInt(TGTID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val dbID = UInt(DBID_WIDTH.W)
  val opcode = UInt(DAT_OPCODE_WIDTH.W)
  val ccID = UInt(CCID_WIDTH.W)
  val dataID = UInt(DATAID_WIDTH.W)
  val be = UInt(BE_WIDTH.W)
  val data = UInt(DATA_WIDTH.W)
  val resp = UInt(RESP_WIDTH.W)
  val fwdState = UInt(FWDSTATE_WIDTH.W)
  // TODO: Finish this
}

class CHIRSP extends CHIBundle {
  val tgtID = UInt(TGTID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val dbID = UInt(DBID_WIDTH.W)
  val pCrdType = UInt(PCRDTYPE_WIDTH.W)
  val opcode = UInt(RSP_OPCODE_WIDTH.W)
  val resp = UInt(RESP_WIDTH.W)
  val fwdState = UInt(FWDSTATE_WIDTH.W)
  // TODO: Finish this
}