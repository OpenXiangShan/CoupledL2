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

  // Refill task
  val refillTask = Bool() // is task from RefillUnit
  val bufId = UInt(mshrBits.W)

  // Identify the transaction from LLC
  val reqId = UInt(TXNID_WIDTH.W)

  // Snoop Info
  val replSnp = Bool() // indicates whether the snoop is caused by a replacement
  val snpVec  = Vec(clientBits, Bool())

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
  val doNotGoToSD = Bool() // only used in snoop
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
    req.size := size
    req.addr := Cat(tag, set, 0.U(offsetBits.W))
    req.allowRetry := allowRetry //TODO: consider retry
    req.pCrdType := pCrdType
    req.expCompAck := expCompAck
    req.memAttr := memAttr
    req.snpAttr := snpAttr
    req.order := order
    req
  }

  def toCHISNPBundle(): CHISNP = {
    val snp = WireInit(0.U.asTypeOf(new CHISNP()))
    snp.srcID := srcID
    snp.txnID := txnID
    snp.fwdNID := fwdNID
    snp.fwdTxnID := fwdTxnID
    snp.opcode := chiOpcode
    snp.addr := Cat(tag, set, 0.U(offsetBits.W))
    snp.doNotGoToSD := doNotGoToSD
    snp.retToSrc := retToSrc
    snp
  }

  def toCHIRSPBundle(): CHIRSP = {
    val rsp = WireInit(0.U.asTypeOf(new CHIRSP()))
    rsp.tgtID := tgtID
    rsp.srcID := srcID
    rsp.txnID := txnID
    rsp.opcode := chiOpcode
    rsp.resp := resp
    rsp.dbID := dbID
    rsp.pCrdType := pCrdType
    rsp.fwdState := fwdState
    rsp
  }

}

class TaskWithData(implicit p: Parameters) extends LLCBundle {
  val task = new Task()
  val data = new DSBlock()

  def toCHIDATBundle(beatId: Int): CHIDAT = {
    val dat = WireInit(0.U.asTypeOf(new CHIDAT()))
    dat.tgtID := task.tgtID
    dat.srcID := task.srcID
    dat.txnID := task.txnID
    dat.homeNID := task.homeNID
    dat.fwdState := task.fwdState
    dat.opcode := task.chiOpcode
    dat.resp := task.resp
    dat.dbID := task.dbID
    dat.be := Fill(BE_WIDTH, true.B)
    dat.data := Seq.tabulate(beatSize)(
      i => data.data((i + 1) * beatBytes * 8 - 1, i * beatBytes * 8)
    )(beatId)
    dat
  }
}

class Resp(implicit p: Parameters) extends LLCBundle {
  val txnId = UInt(TXNID_WIDTH.W)
  val opcode = UInt(OPCODE_WIDTH.W)
  val resp  = UInt(RESP_WIDTH.W)
}

class RespWithData(implicit p: Parameters) extends Resp {
  val data = new DSBlock()
}

class TaskEntry(implicit p: Parameters) extends LLCBundle {
  val valid = Bool()
  val ready = Bool()
  val task  = new Task()
  val data  = new DSBlock()
}
