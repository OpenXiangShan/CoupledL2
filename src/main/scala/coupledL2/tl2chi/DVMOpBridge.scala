
/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
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
import utility._
import org.chipsalliance.cde.config.Parameters
import coupledL2.HasCoupledL2Parameters
import coupledL2.tl2chi._
import coupledL2.tl2chi.RespErrEncodings._
import coupledL2.tl2chi.CHICohStates._


// Default outstanding SnpDVMOp depth of 4 (including Sync and non-Sync).
// This parameters should be consistent with that configured in SoC.
class DVMOpBridge(val depth: Int = 4)(implicit p: Parameters) extends TL2CHIL2Module {

  val io = IO(new Bundle() {
    val rxsnp = Flipped(DecoupledIO(new CHISNP()))
    val txrsp = DecoupledIO(new CHIRSP())
  })

  val entries = Seq.fill(depth) { Module(new DVMOpBridgeEntry) }

  // rx channels
  io.rxsnp.ready := ParallelOR(entries.map(e => {
    e.io.req.bits := io.rxsnp.bits
    e.io.req.valid := io.rxsnp.valid
    e.io.req.ready
  }))

  // tx channels
  arb(entries.map(_.io.resp), io.txrsp, Some("dvmop_txrsp"))
}

class DVMOpBridgeEntry()(implicit p: Parameters) extends TL2CHIL2Module with HasCHIOpcodes {

  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new CHISNP))
    val resp = DecoupledIO(new CHIRSP)
  })

  val w_snpdvmop_p1 = RegInit(true.B)
  val w_snpdvmop_p2 = RegInit(true.B)
  val s_txrsp = RegInit(true.B)

  val no_schedule = s_txrsp
  val no_wait = w_snpdvmop_p1 && w_snpdvmop_p2

  //
  val io_req_is_p1 = io.req.bits.addr(0).asUInt === 0.U
  val io_req_is_p2 = io.req.bits.addr(0).asUInt === 1.U

  val req_p1 = RegEnable(io.req.bits, io.req.fire && io_req_is_p1)
  val req_p2 = RegEnable(io.req.bits, io.req.fire && io_req_is_p2)
  val req_valid = !no_schedule || !no_wait

  val txnID = Mux(w_snpdvmop_p1, req_p1.txnID, req_p2.txnID)
  val srcID = Mux(w_snpdvmop_p1, req_p1.srcID, req_p2.srcID)
  val traceTag = (w_snpdvmop_p1 && req_p1.traceTag) || (w_snpdvmop_p2 && req_p2.traceTag)

  val io_req_hit = io.req.bits.txnID === txnID && io.req.bits.srcID === srcID

  when (io.req.fire) {
    when (!req_valid) {
      w_snpdvmop_p1 := false.B
      w_snpdvmop_p2 := false.B
      s_txrsp := false.B
    }
    when (io_req_is_p1) {
      w_snpdvmop_p1 := true.B
    }
    when (io_req_is_p2) {
      w_snpdvmop_p2 := true.B
    }
  }

  assert(!(io.req.fire && io.req.bits.opcode =/= SnpDVMOp), "receiving non SnpDVMOp")
  assert(!(io.req.fire && io_req_is_p1 && w_snpdvmop_p1), "receiving more than one SnpDVMOp_P1")
  assert(!(io.req.fire && io_req_is_p2 && w_snpdvmop_p2), "receiving more than one SnpDVMOp_P2")

  io.req.ready := Mux(!req_valid, true.B, io_req_hit)

  //
  val txrsp = io.resp

  when (txrsp.fire) {
    s_txrsp := true.B
  }

  txrsp.valid := !s_txrsp && w_snpdvmop_p1 && w_snpdvmop_p2
  txrsp.bits := 0.U.asTypeOf(txrsp.bits.cloneType)
  txrsp.bits.qos := 0.U
  txrsp.bits.tgtID := srcID
//txrsp.bits.srcID
  txrsp.bits.txnID := txnID
  txrsp.bits.opcode := SnpResp
  txrsp.bits.respErr := NDERR
  txrsp.bits.resp := I
  txrsp.bits.traceTag := traceTag
}
