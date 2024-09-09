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
import utility.{FastArbiter, Pipeline, ParallelPriorityMux, RegNextN, RRArbiterInit}
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

  val addressRange = AddressSet(0x00000000L, 0xffffffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL)) // TODO: parameterize this
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
        val isPCrdGrant = rxrsp.valid && rxrsp.bits.opcode === PCrdGrant

        // PCredit arbitration
        val (mmioQuerys, mmioGrants) = mmio.io_pCrd.map { case x => (x.query, x.grant) }.unzip
        val (slicesQuerys, slicesGrants) = slices.map { case s =>
          (s.io_pCrd.map(_.query), s.io_pCrd.map(_.grant))
        }.unzip
        val querys = mmioQuerys ++ slicesQuerys.flatten
        val grants = mmioGrants ++ slicesGrants.flatten
        val entries = querys.length
        val pCrdValids = RegInit(VecInit(Seq.fill(entries)(false.B)))
        val pCrdTypes = Reg(Vec(entries, UInt(PCRDTYPE_WIDTH.W)))
        val pCrdSrcIDs = Reg(Vec(entries, UInt(SRCID_WIDTH.W)))
        val pCrdMatch = Wire(Vec(entries, Vec(entries, Bool())))
        val pCrdMatchEntryVec = pCrdMatch.map(_.asUInt.orR)
        val pCrdMatchEntryOH = PriorityEncoderOH(pCrdMatchEntryVec)
        val pCrdFreeOH = ParallelPriorityMux(
          pCrdMatchEntryVec,
          pCrdMatch.map(x => VecInit(PriorityEncoderOH(x)))
        )

        // Insert a PCredit
        val groupSize = 8
        val ohs_s0 = Seq.tabulate(entries)(i => (BigInt(1) << i).asUInt(entries.W)) :+ 0.U(entries.W)
        val avail_s0 = Wire(Vec(entries, Bool()))
        val groups_s0 = (avail_s0 :+ true.B).zip(ohs_s0).grouped(groupSize).toList
        val select_s0 = Wire(Vec(groups_s0.length, Bool()))
        select_s0.zip(groups_s0).foreach { case (s, g) => s := g.map(_._1).reduce(_ || _) }
        val select_s1 = RegInit(VecInit(Seq.fill(groups_s0.length)(false.B)))
        when (isPCrdGrant) { select_s1 := select_s0 }
        val ohs_s1 = RegInit(VecInit(Seq.fill(groups_s0.length)(0.U(entries.W))))
        ohs_s1.zip(groups_s0).foreach { case (oh, g) => when (isPCrdGrant) { oh := ParallelPriorityMux(g) } }
        val pCrdInsertOH_s1 = ParallelPriorityMux(select_s1, ohs_s1)
        val isPCrdGrant_s1 = RegNext(isPCrdGrant)
        val pCrdType_s1 = RegEnable(rxrsp.bits.pCrdType, isPCrdGrant)
        val srcID_s1 = RegEnable(rxrsp.bits.srcID, isPCrdGrant)
        pCrdValids.zipWithIndex.foreach { case (v, i) =>
          avail_s0(i) := !v && (!isPCrdGrant_s1 || !pCrdInsertOH_s1(i))
          val t = pCrdTypes(i)
          val srcID = pCrdSrcIDs(i)
          val insert_s1 = pCrdInsertOH_s1(i)
          when (isPCrdGrant_s1) {
            when (insert_s1) {
              v := true.B
              t := pCrdType_s1
              srcID := srcID_s1
            }
            assert(!(v && insert_s1), "P-Credit overflow")
          }
        }

        // Free a PCredit
        pCrdValids.zipWithIndex.foreach { case (v, i) =>
          val free = pCrdFreeOH(i)
          when (free) { v := false.B }
          assert(!free || v, "invalid entry should not be free")
        }

        for (i <- 0 until entries) {
          pCrdMatch(i) := VecInit(pCrdValids.zip(pCrdTypes).zip(pCrdSrcIDs).map { case ((v, t), s) =>
            querys(i).valid && v &&
            querys(i).bits.pCrdType === t &&
            querys(i).bits.srcID === s
          })
          grants(i) := RegNext(pCrdMatchEntryOH(i))
        }
        val grantCnt = RegInit(0.U(64.W))
        grantCnt := grantCnt + PopCount(grants)
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
