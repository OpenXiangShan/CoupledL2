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
import utility.{FastArbiter}

class MMIODiverger(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    val in = Vec(numRNs, Flipped(new DecoupledPortIO()))
    val out = new Bundle() {
      val cache = Vec(numRNs, new DecoupledPortIO())
      val uncache = new DecoupledNoSnpPortIO()
    }
  })

  def reqFromMMIO(req: CHIREQ): Bool = req.txnID(TXNID_WIDTH - 1)
  def datFromMMIO(dat: CHIDAT): Bool = dat.opcode === NonCopyBackWrData

  val mmioReqArb = Module(new FastArbiter(new CHIREQ(), numRNs))
  val mmioDatArb = Module(new FastArbiter(new CHIDAT(), numRNs))
  val receiptQueue = Module(new Queue(new CHIRSP(), entries = numRNs, pipe = true, flow = true))

  receiptQueue.io.enq.valid := io.out.uncache.tx.req.fire && io.out.uncache.tx.req.bits.opcode === ReadNoSnp
  receiptQueue.io.enq.bits := {
    val receipt = WireInit(0.U.asTypeOf(new CHIRSP()))
    receipt.opcode := ReadReceipt
    receipt.txnID := io.out.uncache.tx.req.bits.txnID
    receipt.tgtID := io.out.uncache.tx.req.bits.srcID
    receipt
  }
  receiptQueue.io.deq.ready := Cat(io.in.zipWithIndex.map { case (in, i) =>
    in.rx.rsp.ready && receiptQueue.io.deq.bits.tgtID === i.U
  }).orR

  mmioReqArb.io.in.zip(io.in.map(_.tx.req)).foreach { case (in, e) =>
    in.valid := e.valid && reqFromMMIO(e.bits)
    in.bits := e.bits
  }
  mmioReqArb.io.out.ready := io.out.uncache.tx.req.ready

  mmioDatArb.io.in.zip(io.in.map(_.tx.dat)).foreach { case (in, e) =>
    in.valid := e.valid && datFromMMIO(e.bits)
    in.bits := e.bits
  }
  mmioDatArb.io.out.ready := io.out.uncache.tx.dat.ready

  io.in.zip(io.out.cache).zipWithIndex.foreach { case ((in, out), i) =>
    out.tx.req.valid := in.tx.req.valid && !reqFromMMIO(in.tx.req.bits)
    out.tx.req.bits := in.tx.req.bits
    out.tx.dat.valid := in.tx.dat.valid && !datFromMMIO(in.tx.dat.bits)
    out.tx.dat.bits := in.tx.dat.bits
    out.tx.rsp <> in.tx.rsp

    out.rx.rsp.ready := in.rx.rsp.ready && (!receiptQueue.io.deq.valid || receiptQueue.io.deq.bits.tgtID =/= i.U) &&
      (!io.out.uncache.rx.rsp.valid || io.out.uncache.rx.rsp.bits.tgtID =/= i.U)
    out.rx.dat.ready := in.rx.dat.ready && (!io.out.uncache.rx.dat.valid || io.out.uncache.rx.dat.bits.tgtID =/= i.U)

    in.tx.req.ready := io.out.uncache.tx.req.fire && mmioReqArb.io.chosen === i.U || out.tx.req.fire
    in.tx.dat.ready := io.out.uncache.tx.dat.fire && mmioDatArb.io.chosen === i.U || out.tx.dat.fire

    in.rx.rsp.valid := out.rx.rsp.valid || io.out.uncache.rx.rsp.valid && io.out.uncache.rx.rsp.bits.tgtID === i.U ||
      receiptQueue.io.deq.valid && receiptQueue.io.deq.bits.tgtID === i.U
    in.rx.rsp.bits := Mux(
      receiptQueue.io.deq.valid && receiptQueue.io.deq.bits.tgtID === i.U,
      receiptQueue.io.deq.bits,
      Mux(
        io.out.uncache.rx.rsp.valid && io.out.uncache.rx.rsp.bits.tgtID === i.U,
        io.out.uncache.rx.rsp.bits,
        out.rx.rsp.bits
      )
    )
    in.rx.dat.valid := out.rx.dat.valid || io.out.uncache.rx.dat.valid && io.out.uncache.rx.dat.bits.tgtID === i.U
    in.rx.dat.bits := Mux(
      io.out.uncache.rx.dat.valid && io.out.uncache.rx.dat.bits.tgtID === i.U,
      io.out.uncache.rx.dat.bits,
      out.rx.dat.bits
    )
    in.rx.snp <> out.rx.snp
  }

  io.out.uncache.tx.req.valid := mmioReqArb.io.out.valid && receiptQueue.io.enq.ready
  io.out.uncache.tx.req.bits := mmioReqArb.io.out.bits
  io.out.uncache.tx.req.bits.order := OrderEncodings.None
  io.out.uncache.tx.dat.valid := mmioDatArb.io.out.valid
  io.out.uncache.tx.dat.bits := mmioDatArb.io.out.bits

  io.out.uncache.rx.rsp.ready := Cat(io.in.zipWithIndex.map { case (in, i) =>
    in.rx.rsp.ready && io.out.uncache.rx.rsp.bits.tgtID === i.U &&
    (!receiptQueue.io.deq.valid || receiptQueue.io.deq.bits.tgtID =/= i.U)
  }).orR
  io.out.uncache.rx.dat.ready := Cat(io.in.zipWithIndex.map { case (in, i) =>
    in.rx.dat.ready && io.out.uncache.rx.dat.bits.tgtID === i.U }).orR
}

class MMIOMerger(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val in = new Bundle() {
      val cache = Flipped(new DecoupledNoSnpPortIO())
      val uncache = Flipped(new DecoupledNoSnpPortIO())
    }
    val out = new DecoupledNoSnpPortIO()
  })

  val in = io.in
  val out = io.out

  def datToMMIO(dat: CHIDAT): Bool = dat.txnID(TXNID_WIDTH - 1)
  def rspToMMIO(rsp: CHIRSP): Bool = rsp.txnID(TXNID_WIDTH - 1)

  out.tx.req.valid := in.cache.tx.req.valid || in.uncache.tx.req.valid
  out.tx.req.bits := Mux(in.uncache.tx.req.valid, in.uncache.tx.req.bits, in.cache.tx.req.bits)
  out.tx.dat.valid := in.cache.tx.dat.valid || in.uncache.tx.dat.valid
  out.tx.dat.bits := Mux(in.uncache.tx.dat.valid, in.uncache.tx.dat.bits, in.cache.tx.dat.bits)
  out.rx.rsp.ready := in.cache.rx.rsp.fire || in.uncache.rx.rsp.fire
  out.rx.dat.ready := in.cache.rx.dat.fire || in.uncache.rx.dat.fire
  in.cache.tx.req.ready := out.tx.req.ready && !in.uncache.tx.req.valid
  in.cache.tx.dat.ready := out.tx.dat.ready && !in.uncache.tx.dat.valid
  in.cache.rx.dat.valid := out.rx.dat.valid && !datToMMIO(out.rx.dat.bits)
  in.cache.rx.dat.bits := out.rx.dat.bits
  in.cache.rx.rsp.valid := out.rx.rsp.valid && !rspToMMIO(out.rx.rsp.bits)
  in.cache.rx.rsp.bits := out.rx.rsp.bits

  in.uncache.tx.req.ready := out.tx.req.ready
  in.uncache.tx.dat.ready := out.tx.dat.ready
  in.uncache.rx.dat.valid := out.rx.dat.valid && datToMMIO(out.rx.dat.bits)
  in.uncache.rx.dat.bits := out.rx.dat.bits
  in.uncache.rx.rsp.valid := out.rx.rsp.valid && rspToMMIO(out.rx.rsp.bits)
  in.uncache.rx.rsp.bits := out.rx.rsp.bits
}
