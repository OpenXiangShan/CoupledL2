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
import utility.{FastArbiter, Pipeline, ParallelPriorityMux, RegNextN}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import scala.math.max
import coupledL2._
import coupledL2.tl2chi.CHIOpcode.RSPOpcodes._
import coupledL2.prefetch._
import coupledL2.utils.XSPerfAccumulate

abstract class TL2CHIL2Bundle(implicit val p: Parameters) extends Bundle
  with HasCoupledL2Parameters
  with HasCHIMsgParameters
abstract class TL2CHIL2Module(implicit val p: Parameters) extends Module
  with HasCoupledL2Parameters
  with HasCHIMsgParameters

class TL2CHICoupledL2(implicit p: Parameters) extends CoupledL2Base {

  val addressRange = AddressSet(0x00000000L, 0xfffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL)) // TODO: parameterize this
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

  class CoupledL2Imp(wrapper: LazyModule) extends BaseCoupledL2Imp(wrapper) {

    val io_chi = IO(new PortIO)
    val io_nodeID = IO(Input(UInt()))

    // Display info
    println(s"CHI REQ Width: ${(new CHIREQ).getWidth}")
    println(s"CHI RSP Width: ${(new CHIRSP).getWidth}")
    println(s"CHI SNP Width: ${(new CHISNP).getWidth}")
    println(s"CHI DAT Width: ${(new CHIDAT).getWidth}")
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
        // TXREQ
        val txreq_arb = Module(new Arbiter(new CHIREQ, slices.size + 1)) // plus 1 for MMIO
        val txreq = Wire(DecoupledIO(new CHIREQ))
        slices.zip(txreq_arb.io.in.init).foreach { case (s, in) => in <> s.io.out.tx.req }
        txreq_arb.io.in.last <> mmio.io.tx.req
        txreq <> txreq_arb.io.out
        txreq.bits.txnID := setSliceID(txreq_arb.io.out.bits.txnID, txreq_arb.io.chosen, mmio.io.tx.req.fire)

        // TXRSP
        val txrsp = Wire(DecoupledIO(new CHIRSP))
        arb(slices.map(_.io.out.tx.rsp), txrsp, Some("txrsp"))

        // TXDAT
        val txdat = Wire(DecoupledIO(new CHIDAT))
        arb(slices.map(_.io.out.tx.dat) :+ mmio.io.tx.dat, txdat, Some("txdat"))

        // RXSNP
        val rxsnp = Wire(DecoupledIO(new CHISNP))
        val rxsnpSliceID = if (banks <= 1) 0.U else (rxsnp.bits.addr >> (offsetBits - 3))(bankBits - 1, 0)
        slices.zipWithIndex.foreach { case (s, i) =>
          s.io.out.rx.snp.valid := rxsnp.valid && rxsnpSliceID === i.U
          s.io.out.rx.snp.bits := rxsnp.bits
        }
        rxsnp.ready := Cat(slices.zipWithIndex.map { case (s, i) => s.io.out.rx.snp.ready && rxsnpSliceID === i.U }).orR

        // RXRSP
        val rxrsp = Wire(DecoupledIO(new CHIRSP))
        val rxrspIsMMIO = rxrsp.bits.txnID.head(1).asBool
        val isPCrdGrant = rxrsp.valid && (rxrsp.bits.opcode === PCrdGrant)
        val pArb = Module(new RRArbiter(UInt(), banks))
        /*
        when PCrdGrant, give credit to one Slice that:
        1. got RetryAck and not Reissued
        2. match srcID and PCrdType
        3. use Round-Robin arbiter if multi-Slice match
        */
        val matchPCrdGrant = Wire(Vec(banks, UInt(mshrsAll.W)))
        slices.zipWithIndex.foreach { case (s, i) =>
          matchPCrdGrant(i) := VecInit(s.io_waitPCrdInfo.map(p =>
            p.valid && isPCrdGrant &&
            p.srcID.get === rxrsp.bits.srcID &&
            p.pCrdType.get === rxrsp.bits.pCrdType
          )).asUInt
        }
        val pCrdIsWait = VecInit(matchPCrdGrant.map(_.asUInt.orR)).asUInt

        pArb.io.in.zipWithIndex.foreach {
          case (in, i) =>
            in.valid := pCrdIsWait(i)
            in.bits := 0.U
        }
        pArb.io.out.ready := true.B
        val pCrdSliceID = pArb.io.chosen 
        val rxrspSliceID = Mux(isPCrdGrant, pCrdSliceID, getSliceID(rxrsp.bits.txnID))
        slices.zipWithIndex.foreach { case (s, i) =>
          s.io.out.rx.rsp.valid := rxrsp.valid && rxrspSliceID === i.U && !rxrspIsMMIO
          s.io.out.rx.rsp.bits := rxrsp.bits
          s.io.out.rx.rsp.bits.txnID := restoreTXNID(rxrsp.bits.txnID)
        }
        mmio.io.rx.rsp.valid := rxrsp.valid && rxrspIsMMIO
        mmio.io.rx.rsp.bits := rxrsp.bits
        mmio.io.rx.rsp.bits.txnID := restoreTXNID(rxrsp.bits.txnID)
        rxrsp.ready := Mux(
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
        linkMonitor.io.in.tx.req <> txreq
        linkMonitor.io.in.tx.rsp <> txrsp
        linkMonitor.io.in.tx.dat <> txdat
        rxsnp <> linkMonitor.io.in.rx.snp
        rxrsp <> linkMonitor.io.in.rx.rsp
        rxdat <> linkMonitor.io.in.rx.dat
        io_chi <> linkMonitor.io.out
        linkMonitor.io.nodeID := io_nodeID
    }
  }

  lazy val module = new CoupledL2Imp(this)
}
