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

package openLLC

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2chi._

abstract class LLCModule(implicit val p: Parameters) extends Module
  with HasOpenLLCParameters
  with HasCHIMsgParameters
abstract class LLCBundle(implicit val p: Parameters) extends Bundle
  with HasOpenLLCParameters
  with HasCHIMsgParameters

class ReplacerInfo(implicit p: Parameters) extends LLCBundle {
  val opcode = UInt(REQ_OPCODE_WIDTH.W)
  val refill = Bool()
}

class Task(implicit p: Parameters) extends LLCBundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val off = UInt(offsetBits.W)
  val size = UInt(SIZE_WIDTH.W)

  // MSHR task
  val mshrTask = Bool()             // is task from mshr
  val mshrId = UInt(mshrBits.W)     // mshr entry index (used only in mshr-task)
  val metaWen = Bool()
  val tagWen = Bool()
  val dataWen = Bool()

  // CHI
  val tgtID = UInt(TGTID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val homeNID = UInt(SRCID_WIDTH.W) // TODO: required?
  val dbID = UInt(DBID_WIDTH.W)
  val fwdNID = UInt(FWDNID_WIDTH.W)
  val fwdTxnID = UInt(FWDTXNID_WIDTH.W)
  val chiOpcode = UInt(OPCODE_WIDTH.W)
  val resp = UInt(RESP_WIDTH.W)
  val fwdState = UInt(FWDSTATE_WIDTH.W)
  val pCrdType = UInt(PCRDTYPE_WIDTH.W)
  val retToSrc = Bool() // only used in snoop
  val expCompAck = Bool()
  val allowRetry = Bool()
  val order = UInt(ORDER_WIDTH.W)
  val memAttr = new MemAttr
  val snpAttr = Bool()

  def toCHIREQBundle(): CHIREQ = {
    val req = WireInit(0.U.asTypeOf(new CHIREQ()))
    req.tgtID := tgtID
    req.srcID := srcID
    req.txnID := txnID
    req.opcode := chiOpcode
    req.addr := Cat(tag, set, 0.U(offsetBits.W))
    req.allowRetry := allowRetry //TODO: consider retry
    req.pCrdType := pCrdType
    req.expCompAck := expCompAck
    req.memAttr := memAttr
    req.snpAttr := true.B
    req.order := OrderEncodings.None
    req
  }
}

class TaskWithData(implicit p: Parameters) extends LLCBundle {
  val task = new Task()
  val data = new DSBlock()
}

class Resp(implicit p: Parameters) extends LLCBundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
}

class FSMState(implicit p: Parameters) extends LLCBundle {
  // schedule
  val s_refill = Bool()   // write to DS, and evict the old block if necessary
  val s_retry = Bool()    // need retry when conflict

  // wait
  val w_snpresp = Bool()  // wait for the clients to return the snoop response
}

// MSHR allocation request that MainPipe sends to MSHRCtl
class MSHRRequest(implicit p: Parameters) extends LLCBundle {
  val dirResult = new DirResult()
  val state = new FSMState()
  val task = new Task()
}
