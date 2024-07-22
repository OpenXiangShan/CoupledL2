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
  val bank = UInt(bankBits.W)
  val tag = UInt(tagBits.W)
  val off = UInt(offsetBits.W)
  val size = UInt(SIZE_WIDTH.W)

  // Refill task
  val refillTask = Bool() // is task from RefillUnit
  val bufID = UInt(mshrBits.W)

  // Identify the transaction from LLC
  val reqID = UInt(TXNID_WIDTH.W)

  // Snoop Info
  val replSnp = Bool() // indicates whether the snoop is caused by a replacement
  val snpVec  = Vec(numRNs, Bool())

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
    req.addr := Cat(tag, set, bank, 0.U(offsetBits.W))
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
    snp.addr := Cat(tag, set, bank, 0.U(offsetBits.W))
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
    dat.data := data.data(beatId).data
    dat.dataID := (beatBytes * beatId * 8).U(log2Ceil(blockBytes * 8) - 1, log2Ceil(blockBytes * 8) - 2)
    dat
  }
}

class Resp(implicit p: Parameters) extends LLCBundle {
  val txnID = UInt(TXNID_WIDTH.W)
  val dbID = UInt(DBID_WIDTH.W)
  val opcode = UInt(OPCODE_WIDTH.W)
  val resp  = UInt(RESP_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
}

class RespWithData(implicit p: Parameters) extends Resp {
  val dataID = UInt(DATAID_WIDTH.W)
  val data = new DSBeat()
}

class TaskEntry(implicit p: Parameters) extends LLCBundle {
  val valid = Bool()
  val ready = Bool()
  val task  = new Task()
}

class PipeStatus(implicit p: Parameters) extends LLCBundle {
  val tags = Vec(5, UInt(tagBits.W))
  val sets = Vec(5, UInt(setBits.W))
  val valids = Vec(5, Bool())

  def s2_tag = tags(0)
  def s3_tag = tags(1)
  def s4_tag = tags(2)
  def s5_tag = tags(3)
  def s6_tag = tags(4)

  def s2_set = sets(0)
  def s3_set = sets(1)
  def s4_set = sets(2)
  def s5_set = sets(3)
  def s6_set = sets(4)

  def s2_valid = valids(0)
  def s3_valid = valids(1)
  def s4_valid = valids(2)
  def s5_valid = valids(3)
  def s6_valid = valids(4)
}

class BlockInfo(implicit p: Parameters) extends LLCBundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val opcode = UInt(REQ_OPCODE_WIDTH.W)
  val reqID = UInt(TXNID_WIDTH.W)
}
