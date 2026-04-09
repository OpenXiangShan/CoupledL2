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

class RNXbar(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val in = Vec(numRNs, Flipped(new DecoupledPortIO()))
    val out = Vec(banks, new DecoupledPortIO())
    val snpMasks = Input(Vec(banks, Vec(numRNs, Bool())))
  })

  val txreqArbs = Seq.fill(banks)(Module(new FastArbiter(new CHIREQ(), numRNs)))
  val txrspArbs = Seq.fill(banks)(Module(new FastArbiter(new CHIRSP(), numRNs)))
  val txdatArbs = Seq.fill(banks)(Module(new FastArbiter(new CHIDAT(), numRNs)))
  val rxsnpArbs = Seq.fill(numRNs)(Module(new FastArbiter(new CHISNP(), banks)))
  val rxrspArbs = Seq.fill(numRNs)(Module(new FastArbiter(new CHIRSP(), banks)))
  val rxdatArbs = Seq.fill(numRNs)(Module(new FastArbiter(new CHIDAT(), banks)))

  val snpMasks = RegInit(VecInit(Seq.fill(banks)(VecInit(Seq.fill(numRNs)(false.B)))))
  val snpReqs  = RegInit(VecInit(Seq.fill(banks)(0.U.asTypeOf(Valid(new CHISNP())))))

  for (i <- 0 until banks) {
    txreqArbs(i).io.in.zip(io.in.map(_.tx.req)).foreach { case (in, e) =>
      in.valid := e.valid && { if (banks == 1) true.B else parseAddress(e.bits.addr)._3 === i.U }
      in.bits  := e.bits
    }
    txreqArbs(i).io.out.ready := io.out(i).tx.req.ready
    io.out(i).tx.req.valid    := txreqArbs(i).io.out.valid
    io.out(i).tx.req.bits     := txreqArbs(i).io.out.bits

    txrspArbs(i).io.in.zip(io.in.map(_.tx.rsp)).foreach { case (in, e) =>
      in.valid := e.valid && { if (banks == 1) true.B else
        e.bits.txnID(TXNID_WIDTH - 2, TXNID_WIDTH - bankBits - 1) === i.U }
      in.bits  := e.bits
    }
    txrspArbs(i).io.out.ready := io.out(i).tx.rsp.ready
    io.out(i).tx.rsp.valid    := txrspArbs(i).io.out.valid
    io.out(i).tx.rsp.bits     := txrspArbs(i).io.out.bits

    txdatArbs(i).io.in.zip(io.in.map(_.tx.dat)).foreach { case (in, e) =>
      in.valid := e.valid && { if (banks == 1) true.B else
        e.bits.txnID(TXNID_WIDTH - 2, TXNID_WIDTH - bankBits - 1) === i.U }
      in.bits  := e.bits
    }
    txdatArbs(i).io.out.ready := io.out(i).tx.dat.ready
    io.out(i).tx.dat.valid    := txdatArbs(i).io.out.valid
    io.out(i).tx.dat.bits     := txdatArbs(i).io.out.bits

    io.out(i).rx.rsp.ready := Cat(rxrspArbs.map(arb => arb.io.out.fire && arb.io.chosen === i.U)).orR
    io.out(i).rx.dat.ready := Cat(rxdatArbs.map(arb => arb.io.out.fire && arb.io.chosen === i.U)).orR

    val newSnpMask = ~VecInit(rxsnpArbs.map(arb => arb.io.out.fire && arb.io.chosen === i.U)).asUInt &
      snpMasks(i).asUInt
    snpMasks(i) := VecInit(newSnpMask.asBools)

    io.out(i).rx.snp.ready := snpReqs(i).valid && !newSnpMask.orR || !snpMasks(i).asUInt.orR
    when(io.out(i).rx.snp.fire) {
      snpReqs(i).valid := true.B
      snpReqs(i).bits  := io.out(i).rx.snp.bits
      snpMasks(i)      := io.snpMasks(i)
    }
  }

  for (j <- 0 until numRNs) {
    rxsnpArbs(j).io.in.zip(snpReqs).zip(snpMasks).foreach { case ((in, req), mask) =>
      in.valid := req.valid && mask(j)
      in.bits  := req.bits
    }
    rxsnpArbs(j).io.out.ready := io.in(j).rx.snp.ready
    io.in(j).rx.snp.valid     := rxsnpArbs(j).io.out.valid
    io.in(j).rx.snp.bits      := rxsnpArbs(j).io.out.bits

    rxrspArbs(j).io.in.zip(io.out.map(_.rx.rsp)).foreach { case (in, e) =>
      in.valid := e.valid && e.bits.srcID === j.U
      in.bits  := e.bits
    }
    rxrspArbs(j).io.out.ready := io.in(j).rx.rsp.ready
    io.in(j).rx.rsp.valid     := rxrspArbs(j).io.out.valid
    io.in(j).rx.rsp.bits      := rxrspArbs(j).io.out.bits

    rxdatArbs(j).io.in.zip(io.out.map(_.rx.dat)).foreach { case (in, e) =>
      in.valid := e.valid && e.bits.srcID === j.U
      in.bits  := e.bits
    }
    rxdatArbs(j).io.out.ready := io.in(j).rx.dat.ready
    io.in(j).rx.dat.valid     := rxdatArbs(j).io.out.valid
    io.in(j).rx.dat.bits      := rxdatArbs(j).io.out.bits

    io.in(j).tx.req.ready := Cat(txreqArbs.map(arb => arb.io.out.fire && arb.io.chosen === j.U)).orR
    io.in(j).tx.rsp.ready := Cat(txrspArbs.map(arb => arb.io.out.fire && arb.io.chosen === j.U)).orR
    io.in(j).tx.dat.ready := Cat(txdatArbs.map(arb => arb.io.out.fire && arb.io.chosen === j.U)).orR
  }

}

class SNXbar(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val in = Vec(banks, Flipped(new DecoupledNoSnpPortIO))
    val out = new DecoupledNoSnpPortIO
  })

  val txreqArb = Module(new FastArbiter(new CHIREQ(), banks))
  val txdatArb = Module(new FastArbiter(new CHIDAT(), banks))

  for (i <- 0 until banks) {
    txreqArb.io.in(i).valid := io.in(i).tx.req.valid
    txreqArb.io.in(i).bits  := io.in(i).tx.req.bits
    txdatArb.io.in(i).valid := io.in(i).tx.dat.valid
    txdatArb.io.in(i).bits  := io.in(i).tx.dat.bits

    io.in(i).tx.req.ready := txreqArb.io.out.fire && txreqArb.io.chosen === i.U
    io.in(i).tx.dat.ready := txdatArb.io.out.fire && txdatArb.io.chosen === i.U
    io.in(i).rx.dat.valid := io.out.rx.dat.valid && { if (banks == 1) true.B else
      io.out.rx.dat.bits.txnID(TXNID_WIDTH - 2, TXNID_WIDTH - bankBits - 1) === i.U }
    io.in(i).rx.dat.bits  := io.out.rx.dat.bits
    io.in(i).rx.rsp.valid := io.out.rx.rsp.valid && { if (banks == 1) true.B else
      io.out.rx.rsp.bits.txnID(TXNID_WIDTH - 2, TXNID_WIDTH - bankBits - 1) === i.U }
    io.in(i).rx.rsp.bits  := io.out.rx.rsp.bits
  }

  txreqArb.io.out.ready := io.out.tx.req.ready
  txdatArb.io.out.ready := io.out.tx.dat.ready

  io.out.tx.req.valid := txreqArb.io.out.valid
  io.out.tx.req.bits  := txreqArb.io.out.bits
  io.out.tx.dat.valid := txdatArb.io.out.valid
  io.out.tx.dat.bits  := txdatArb.io.out.bits
  io.out.rx.dat.ready := Cat(io.in.zipWithIndex.map { case (e, i) =>
    e.rx.dat.fire && { if (banks == 1) true.B else
      io.out.rx.dat.bits.txnID(TXNID_WIDTH - 2, TXNID_WIDTH - bankBits - 1) === i.U }
  }).orR
  io.out.rx.rsp.ready := Cat(io.in.zipWithIndex.map { case (e, i) =>
    e.rx.rsp.fire && { if (banks == 1) true.B else
      io.out.rx.rsp.bits.txnID(TXNID_WIDTH - 2, TXNID_WIDTH - bankBits - 1) === i.U }
  }).orR

}
