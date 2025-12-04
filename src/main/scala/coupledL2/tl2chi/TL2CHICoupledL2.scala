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
import utility.{FastArbiter, Pipeline, ParallelPriorityMux, RegNextN, RRArbiterInit, XSPerfAccumulate}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import scala.math.max
import coupledL2._
import coupledL2.prefetch._

abstract class TL2CHIL2Bundle(implicit val p: Parameters) extends Bundle
  with HasCoupledL2Parameters
  with HasCHIMsgParameters
abstract class TL2CHIL2Module(implicit val p: Parameters) extends Module
  with HasCoupledL2Parameters
  with HasCHIMsgParameters


class TL2CHICoupledL2(implicit p: Parameters) extends CoupledL2Base {

  val addressRange = Seq(AddressSet(0x00000000L, 0xffffffffffffL)) // TODO: parameterize this
  val managerParameters = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = addressRange,
      regionType = RegionType.CACHED,
      supportsAcquireT = xfer,
      supportsAcquireB = xfer,
      supportsArithmetic = atom,
      supportsLogical = atom,
      supportsGet = access,
      supportsPutFull = access,
      supportsPutPartial = access,
      supportsHint = access,
      fifoId = None
    )),
    beatBytes = 32,
    minLatency = 2,
    responseFields = cacheParams.respField,
    requestKeys = cacheParams.reqKey,
    endSinkId = idsAll * (1 << bankBits)
  )
  val managerNode = TLManagerNode(Seq(managerParameters))

  val mmioBridge = LazyModule(new MMIOBridge)
  val mmioNode = mmioBridge.mmioNode

  class CoupledL2Imp(wrapper: LazyModule) extends BaseCoupledL2Imp(wrapper)
    with HasCHIOpcodes {

    val io_chi = IO(new PortIO)
    val io_nodeID = IO(Input(UInt()))
    val io_cpu_halt = Option.when(cacheParams.enableL2Flush) (IO(Input(Bool())))

    // Check port width
    require(io_chi.tx.rsp.getWidth == io_chi.rx.rsp.getWidth);
    require(io_chi.tx.dat.getWidth == io_chi.rx.dat.getWidth);

    // Display info
    println(s"CHI Issue Version: ${p(CHIIssue)}")
    println(s"CHI REQ Flit Width: ${io_chi.tx.req.flit.getWidth}")
    println(s"CHI RSP Flit Width: ${io_chi.tx.rsp.flit.getWidth}")
    println(s"CHI SNP Flit Width: ${io_chi.rx.snp.flit.getWidth}")
    println(s"CHI DAT Flit Width: ${io_chi.rx.dat.flit.getWidth}")
    println(s"CHI Port Width: ${io_chi.getWidth}")

    println(s"MMIO:")
    mmioNode.edges.in.headOption.foreach { n =>
      n.client.clients.zipWithIndex.foreach {
        case (c, i) =>
          println(s"\t${i} <= ${c.name};" +
            s"\tsourceRange: ${c.sourceId.start}~${c.sourceId.end}")
      }
    }

    val mmio = mmioBridge.module

    // Outer interface connection
    /**
      * TxnID space arrangement:
      * If this is a cacheable request:
      * +----------------+-----------+---------------+
      * |    0.U(1.W)    |  SliceID  |  Inner TxnID  |
      * +----------------+-----------+---------------+
      * Otherwise this is an MMIO request:
      * +----------------+-----------+---------------+
      * |    1.U(1.W)    |        Inner TxnID        |
      * +----------------+---------------------------+  
      *
      */
    def setSliceID(txnID: UInt, sliceID: UInt, mmio: Bool): UInt = {
      Mux(
        mmio,
        Cat(1.U(1.W), txnID.tail(1)),
        Cat(0.U(1.W), if (banks <= 1) txnID.tail(1) else Cat(sliceID(bankBits - 1, 0), txnID.tail(bankBits + 1)))
      )
    }
    def getSliceID(txnID: UInt): UInt = if (banks <= 1) 0.U else txnID.tail(1).head(bankBits)
    def restoreTXNID(txnID: UInt): UInt = {
      val mmio = txnID.head(1).asBool
      Mux(
        mmio || (banks <= 1).B,
        Cat(0.U(1.W), txnID.tail(1)),
        Cat(0.U(1.W), 0.U(bankBits.W), txnID.tail(bankBits + 1))
      )
    }

    slices match {
      case slices: Seq[Slice] =>
        // DVMOp Bridge
        val dvmop = Module(new DVMOpBridge)

        // TXREQ
        val txreq_arb = Module(new RRArbiterInit(new CHIREQ, slices.size + 1)) // plus 1 for MMIO
        val txreq = Wire(DecoupledIO(new CHIREQ))
        slices.zip(txreq_arb.io.in.init).foreach { case (s, in) => in <> s.io.out.tx.req }
        txreq_arb.io.in.last <> mmio.io.tx.req
        txreq <> txreq_arb.io.out
        txreq.bits.txnID := setSliceID(txreq_arb.io.out.bits.txnID, txreq_arb.io.chosen, mmio.io.tx.req.fire)

        // TXRSP
        val txrsp = Wire(DecoupledIO(new CHIRSP))
        fastArb(slices.map(_.io.out.tx.rsp) :+ dvmop.io.txrsp, txrsp, Some("txrsp"))

        // TXDAT
        val txdat = Wire(DecoupledIO(new CHIDAT))
        fastArb(slices.map(_.io.out.tx.dat) :+ mmio.io.tx.dat, txdat, Some("txdat"))

        // RXSNP
        val rxsnp = Wire(DecoupledIO(new CHISNP))
        val rxsnpDVMOp = rxsnp.bits.opcode === SnpDVMOp
        val rxsnpSliceID = if (banks <= 1) 0.U else (rxsnp.bits.addr >> (offsetBits - 3))(bankBits - 1, 0)
        slices.zipWithIndex.foreach { case (s, i) =>
          s.io.out.rx.snp.valid := rxsnp.valid && !rxsnpDVMOp && rxsnpSliceID === i.U
          s.io.out.rx.snp.bits := rxsnp.bits
        }
        dvmop.io.rxsnp.valid := rxsnp.valid && rxsnpDVMOp
        dvmop.io.rxsnp.bits := rxsnp.bits
        rxsnp.ready := Cat(
          slices.zipWithIndex.map { case (s, i) => s.io.out.rx.snp.ready && rxsnpSliceID === i.U } :+
          (dvmop.io.rxsnp.ready && rxsnpDVMOp)
        ).orR

        // RXRSP
        val rxrsp = Wire(DecoupledIO(new CHIRSP))
        val rxrspIsMMIO = rxrsp.bits.txnID.head(1).asBool
        val isPCrdGrant = rxrsp.valid && rxrsp.bits.opcode === PCrdGrant

        // PCredit queue
        class EmptyBundle extends Bundle

        class PCrdGranted extends Bundle {
          val pCrdType = UInt(PCRDTYPE_WIDTH.W)
          val srcID = UInt(SRCID_WIDTH.W)
        }

        val (mmioQuerys, mmioGrants) = mmio.io_pCrd.map { case x => (x.query, x.grant) }.unzip
        val (slicesQuerys, slicesGrants) = slices.map { case s =>
          (s.io_pCrd.map(_.query), s.io_pCrd.map(_.grant))
        }.unzip
        val mshrPCrdQuerys = mmioQuerys ++ slicesQuerys.flatten
        val mshrPCrdGrants = mmioGrants ++ slicesGrants.flatten

        val mshrEntryCount = mshrPCrdQuerys.length

        val pCrdQueue_s2 = Module(new Queue(new PCrdGranted, entries = mshrEntryCount - 2))
        val pCrdQueue_s3 = Module(new Queue(new PCrdGranted, entries = 2))

        pCrdQueue_s3.io.enq <> pCrdQueue_s2.io.deq

        // PCredit hit by MSHRs
        val mshrPCrdHits = mshrPCrdQuerys.map((_, pCrdQueue_s3.io.deq)).map { case (q, h) => {
          q.valid && h.valid && q.bits.pCrdType === h.bits.pCrdType && q.bits.srcID === h.bits.srcID
        }}

        // PCredit dispatch arbitration
        val mshrPCrdArbGrants = Wire(Vec(mshrEntryCount, Bool()))
        val mshrPCrdArbIn = mshrPCrdHits.zip(mshrPCrdArbGrants).map { case (hit, grant) => {
          val arbPort = Wire(Decoupled(new EmptyBundle))
          arbPort.valid := hit
          grant := arbPort.ready
          arbPort
        }}

        val mshrPCrdArbOut = {
          val arbPort = Wire(Decoupled(new EmptyBundle))
          arbPort.ready := true.B
          pCrdQueue_s3.io.deq.ready := arbPort.valid
          arbPort
        }

        fastArb(mshrPCrdArbIn, mshrPCrdArbOut, Some("pcrdgrant"))

        mshrPCrdGrants.zip(mshrPCrdArbGrants).foreach { case (grant, arb) => grant := arb }

        // PCredit receive
        val pCrdGrantValid_s1 = RegNext(isPCrdGrant)
        val pCrdGrantType_s1 = RegNext(rxrsp.bits.pCrdType)
        val pCrdGrantSrcID_s1 = RegNext(rxrsp.bits.srcID)

        pCrdQueue_s2.io.enq.valid := pCrdGrantValid_s1
        pCrdQueue_s2.io.enq.bits.pCrdType := pCrdGrantType_s1
        pCrdQueue_s2.io.enq.bits.srcID := pCrdGrantSrcID_s1

        val grantCnt = RegInit(0.U(64.W))
        when (pCrdQueue_s3.io.deq.ready) {
          grantCnt := grantCnt + 1.U
        }
        dontTouch(grantCnt)

        val rxrspSliceID = getSliceID(rxrsp.bits.txnID)
        slices.zipWithIndex.foreach { case (s, i) =>
          s.io.out.rx.rsp.valid := rxrsp.valid && rxrspSliceID === i.U && !rxrspIsMMIO && !isPCrdGrant
          s.io.out.rx.rsp.bits := rxrsp.bits
          s.io.out.rx.rsp.bits.txnID := restoreTXNID(rxrsp.bits.txnID)
        }
        mmio.io.rx.rsp.valid := rxrsp.valid && rxrspIsMMIO && !isPCrdGrant
        mmio.io.rx.rsp.bits := rxrsp.bits
        mmio.io.rx.rsp.bits.txnID := restoreTXNID(rxrsp.bits.txnID)
        rxrsp.ready := rxrsp.bits.opcode === PCrdGrant || Mux(
          rxrspIsMMIO,
          mmio.io.rx.rsp.ready,
          Cat(slices.zipWithIndex.map { case (s, i) => s.io.out.rx.rsp.ready && rxrspSliceID === i.U }).orR
        )

        // RXDAT
        val rxdat = Wire(DecoupledIO(new CHIDAT))
        val rxdatIsMMIO = rxdat.bits.txnID.head(1).asBool
        val rxdatSliceID = getSliceID(rxdat.bits.txnID)
        slices.zipWithIndex.foreach { case (s, i) =>
          s.io.out.rx.dat.valid := rxdat.valid && rxdatSliceID === i.U && !rxdatIsMMIO
          s.io.out.rx.dat.bits := rxdat.bits
          s.io.out.rx.dat.bits.txnID := restoreTXNID(rxdat.bits.txnID)
        }
        mmio.io.rx.dat.valid := rxdat.valid && rxdatIsMMIO
        mmio.io.rx.dat.bits := rxdat.bits
        mmio.io.rx.dat.bits.txnID := restoreTXNID(rxdat.bits.txnID)
        rxdat.ready := Mux(
          rxdatIsMMIO,
          mmio.io.rx.dat.ready,
          Cat(slices.zipWithIndex.map { case (s, i) => s.io.out.rx.dat.ready && rxdatSliceID === i.U}).orR
        )

        val linkMonitor = Module(new LinkMonitor)
        val rxdatPipe = Pipeline(linkMonitor.io.in.rx.dat)
        val rxrspPipe = Pipeline(linkMonitor.io.in.rx.rsp)
        linkMonitor.io.in.tx.req <> txreq
        linkMonitor.io.in.tx.rsp <> txrsp
        linkMonitor.io.in.tx.dat <> txdat
        rxsnp <> linkMonitor.io.in.rx.snp
        rxrsp <> rxrspPipe
        rxdat <> rxdatPipe
        io_chi <> linkMonitor.io.out
        linkMonitor.io.nodeID := io_nodeID
        /* exit coherency when: l2 flush of all slices is done and core is in WFI state */
        linkMonitor.io.exitco.foreach { _ :=
          Cat(slices.zipWithIndex.map { case (s, i) => s.io.l2FlushDone.getOrElse(false.B)}).andR && io_cpu_halt.getOrElse(false.B)
        }

        /**
          * performance counters
          */
        XSPerfAccumulate("pcrd_count", pCrdQueue_s2.io.enq.fire)
    }
  }

  lazy val module = new CoupledL2Imp(this)
}
