
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
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.HasCoupledL2Parameters
import coupledL2.{MemBackTypeMM, MemPageTypeNC}

class MMIOBridge()(implicit p: Parameters) extends LazyModule
  with HasCoupledL2Parameters
  with HasCHIMsgParameters {

  override def shouldBeInlined: Boolean = false

  /**
    * MMIO node
    */
  val beuRange = AddressSet(0x38010000, 4096 - 1)
  val peripheralRange = AddressSet(
    0x0, 0xffffffffffffL
  ).subtract(beuRange)

  val mmioNode = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = peripheralRange,
      regionType = RegionType.UNCACHED,
      supportsGet = TransferSizes(1, 8),
      supportsPutFull = TransferSizes(1, 8),
      supportsPutPartial = TransferSizes(1, 8)
    )),
    beatBytes = 8,
    requestKeys = Seq(MemBackTypeMM, MemPageTypeNC)
  )))

  lazy val module = new MMIOBridgeImp(this)

}

class MMIOBridgeEntry(edge: TLEdgeIn)(implicit p: Parameters) extends TL2CHIL2Module with HasCHIOpcodes {

  val needRR = true

  // *NOTICE: DO NOT set 'bufferableNC = true' when 'needRR = false', 
  //          since the ordering between NC and IO described in SvPBMT was maintained by 'fence iorw, iorw'.
  //          And alias with NC and IO must not cause loss of coherency due to SvPBMT, which was NOT guaranteed
  //          by CHI specification in which case obtaining different EWA from different agents.
  //
  //          1) For Memory backend, the observability of possible weakly-ordered intermediate states were determined
  //          by the core (the processor pipeline, store buffer, etc.) because there is no ordering guarantee from HN.
  //
  //          2) For Device backend, the observability of possible weakly-ordered intermediate states were determined
  //          by the HN (Home Node) on bus with Endpoint Ordering.
  val bufferableNC = true

  require(!bufferableNC || needRR , "DO NOT set 'bufferableNC = true' when 'needRR = false'")

  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new TLBundleA(edge.bundle)))
    val resp = DecoupledIO(new TLBundleD(edge.bundle))
    val chi = new DecoupledNoSnpPortIO
    val id = Input(UInt())
    val pCrd = new PCrdQueryBundle
    val waitOnReadReceipt = Option.when(needRR)(Output(Bool()))
  })

  val s_txreq = RegInit(true.B)
  val s_ncbwrdata = RegInit(true.B)
  // val s_readrecript = RegInit(true.B) // TODO
  // val s_compack = RegInit(true.B) // TODO
  val s_resp = RegInit(true.B)
  val w_comp = RegInit(true.B)
  val w_dbidresp = RegInit(true.B)
  val w_compdata = RegInit(true.B)
  val w_pcrdgrant = RegInit(true.B)
  val w_readreceipt = Option.when(needRR)(RegInit(true.B))

  val no_schedule = s_txreq && s_ncbwrdata && s_resp
  val no_wait = w_comp && w_dbidresp && w_compdata && w_pcrdgrant && w_readreceipt.getOrElse(true.B)

  val req = RegEnable(io.req.bits, io.req.fire)
  val req_valid = !no_schedule || !no_wait
  val rdata = Reg(UInt(DATA_WIDTH.W))
  val srcID = Reg(UInt(SRCID_WIDTH.W))
  val dbID = Reg(UInt(DBID_WIDTH.W))
  val allowRetry = RegInit(true.B)
  val pCrdType = Reg(UInt(PCRDTYPE_WIDTH.W))
  val denied = Reg(Bool())
  val corrupt = Reg(Bool())
  val traceTag = Reg(Bool())
  val isRead = req.opcode === Get
  val isBackTypeMM = req.user.lift(MemBackTypeMM).getOrElse(false.B)
  val isPageTypeNC = req.user.lift(MemPageTypeNC).getOrElse(false.B)

  val wordBits = io.req.bits.data.getWidth // 64
  val wordBytes = wordBits / 8
  val words = DATA_WIDTH / wordBits
  val wordIdxBits = log2Ceil(words)
  require(wordBits == 64)
  require(wordIdxBits == 2)
  val reqWordIdx = (req.address >> log2Ceil(wordBytes))(wordIdxBits - 1, 0)

  val txreq = io.chi.tx.req
  val txdat = io.chi.tx.dat
  val rxdat = io.chi.rx.dat
  val rxrsp = io.chi.rx.rsp
  
  /**
    * Entry allocation
    */
  when (io.req.fire) {
    s_txreq := false.B
    s_resp := false.B
    allowRetry := true.B
    denied := false.B
    corrupt := false.B
    traceTag := false.B
    when (io.req.bits.opcode === Get) {
      w_compdata := false.B
      w_readreceipt.foreach(_ := false.B)
    }.elsewhen (io.req.bits.opcode === PutFullData || io.req.bits.opcode === PutPartialData) {
      w_comp := false.B
      w_dbidresp := false.B
      s_ncbwrdata := false.B
    }
  }

  /**
    * State flags recover
    */
  when (txreq.fire) {
    s_txreq := true.B
  }
  when (rxdat.fire) {
    w_compdata := true.B
    rdata := rxdat.bits.data
    val nderr = rxdat.bits.respErr === RespErrEncodings.NDERR
    val derr = rxdat.bits.respErr === RespErrEncodings.DERR
    denied := denied || nderr
    corrupt := corrupt || derr || nderr
  }
  when (io.resp.fire) {
    s_resp := true.B
  }
  when (rxrsp.fire) {
    when (rxrsp.bits.opcode === CompDBIDResp || rxrsp.bits.opcode === Comp) {
      w_comp := true.B
    }
    when (
      rxrsp.bits.opcode === CompDBIDResp || rxrsp.bits.opcode === DBIDResp ||
      ENABLE_ISSUE_Eb.B && rxrsp.bits.opcode === DBIDRespOrd
    ) {
      w_dbidresp := true.B
      srcID := rxrsp.bits.srcID
      dbID := rxrsp.bits.dbID
      traceTag := rxrsp.bits.traceTag
    }
    when (rxrsp.bits.opcode === CompDBIDResp || rxrsp.bits.opcode === Comp) {
      denied := denied || rxrsp.bits.respErr === RespErrEncodings.NDERR
      // TODO: d_corrupt is reserved and must be 0 in TileLink
    }
    when (rxrsp.bits.opcode === RetryAck) {
      s_txreq := false.B
      w_pcrdgrant := false.B
      allowRetry := false.B
      pCrdType := rxrsp.bits.pCrdType
      srcID := rxrsp.bits.srcID
    }
    when (rxrsp.bits.opcode === ReadReceipt) {
      w_readreceipt.foreach(_ := true.B)
    }
  }
  when (txdat.fire) {
    s_ncbwrdata := true.B
  }
  when (io.pCrd.grant) {
    w_pcrdgrant := true.B
  }

  /**
    * IO Assignment
    */
  io.req.ready := no_schedule && no_wait
  txreq.valid := !s_txreq && w_pcrdgrant
  txreq.bits := 0.U.asTypeOf(txreq.bits.cloneType)
  txreq.bits.tgtID := SAM(sam).lookup(txreq.bits.addr)
  txreq.bits.txnID := io.id
  txreq.bits.opcode := ParallelLookUp(req.opcode, Seq(
    Get -> ReadNoSnp,
    PutFullData -> WriteNoSnpPtl,
    PutPartialData -> WriteNoSnpPtl
  ))
  txreq.bits.size := req.size
  txreq.bits.addr := req.address
  txreq.bits.allowRetry := allowRetry
  txreq.bits.pCrdType := Mux(allowRetry, 0.U, pCrdType)
  txreq.bits.expCompAck := false.B
  // *Ordering and MemAttr: 
  // ---------------------------------------------------------
  // [when 'bufferableNC' configured to false]
  //    PMA = MM   , PBMT = NC -> Non-cacheable Non-bufferable
  //    PMA = MM   , PBMT = IO -> Non-cacheable Non-bufferable
  //    PMA = NC/IO, PBMT = NC -> Device nRnE (no reorder, no early acknowlegment)
  //    PMA = NC/IO, PBMT = IO -> Device nRnE (no reorder, no early acknowlegment)
  // ---------------------------------------------------------
  // [when 'bufferableNC' configured to true]
  //    PMA = MM   , PBMT = NC -> Non-cacheable Bufferable
  //    PMA = MM   , PBMT = IO -> Non-cacheable Bufferable
  //    PMA = NC/IO, PBMT = NC -> Device nRE  (no reorder, early acknowlegment)
  //    PMA = NC/IO, PBMT = IO -> Device nRnE (no reorder, no early acknowlegment)
  txreq.bits.order := {
    if (needRR) 
      Mux(!isBackTypeMM, OrderEncodings.EndpointOrder, OrderEncodings.RequestOrder)
    else 
      OrderEncodings.None
  }
  txreq.bits.memAttr := MemAttr(
    allocate = false.B,
    cacheable = false.B,
    device = !isBackTypeMM,
    ewa = if (bufferableNC) (isPageTypeNC || isBackTypeMM) else false.B
  )

  io.resp.valid := !s_resp && Mux(isRead, w_compdata, w_comp && w_dbidresp && s_ncbwrdata)
  io.resp.bits.opcode := Mux(isRead, AccessAckData, AccessAck)
  io.resp.bits.param := 0.U // reserved
  io.resp.bits.size := req.size
  io.resp.bits.source := req.source
  io.resp.bits.sink := 0.U // ignored
  io.resp.bits.denied := denied
  io.resp.bits.corrupt := isRead && corrupt
  io.resp.bits.data := ParallelLookUp(
    reqWordIdx,
    List.tabulate(words)(i => i.U -> rdata((i + 1) * wordBits - 1, i * wordBits))
  )

  txdat.valid := !s_ncbwrdata && w_dbidresp
  txdat.bits := 0.U.asTypeOf(txdat.bits.cloneType)
  txdat.bits.tgtID := srcID
  txdat.bits.txnID := dbID
  txdat.bits.opcode := NonCopyBackWrData
  txdat.bits.ccID := req.address(log2Ceil(beatBytes), log2Ceil(beatBytes) - CCID_WIDTH + 1)
  txdat.bits.dataID := Cat(req.address(log2Ceil(beatBytes)), 0.U(1.W))
  txdat.bits.be := ParallelLookUp(
    reqWordIdx,
    List.tabulate(words)(i => i.U -> (ZeroExt(req.mask, BE_WIDTH) << (i * wordBytes)))
  )
  txdat.bits.data := Fill(words, req.data) & FillInterleaved(8, txdat.bits.be)
  txdat.bits.traceTag := traceTag

  rxrsp.ready := (!w_comp || !w_dbidresp || !w_readreceipt.getOrElse(true.B)) && s_txreq
  rxdat.ready := !w_compdata && s_txreq

  io.pCrd.query.valid := !w_pcrdgrant
  io.pCrd.query.bits.pCrdType := pCrdType
  io.pCrd.query.bits.srcID := srcID

  io.waitOnReadReceipt.foreach(_ := !w_readreceipt.get && s_txreq)
}

class MMIOBridgeImp(outer: MMIOBridge) extends LazyModuleImp(outer)
  with HasCoupledL2Parameters
  with HasCHIOpcodes {

  val (bus, edge) = outer.mmioNode.in.head

  val io = IO(new DecoupledNoSnpPortIO)
  val io_pCrd = IO(Vec(mmioBridgeSize, new PCrdQueryBundle))

  val entries = Seq.fill(mmioBridgeSize) { Module(new MMIOBridgeEntry(edge)) }
  val readys = VecInit(entries.map(_.io.req.ready))
  val selectOH = ParallelPriorityMux(readys.zipWithIndex.map { case (ready, i) =>
    ready -> (1 << i).U
  }).asBools

  /**
    * When a ReadNoSnp requires RequestOrder or Endpoint Order, the requester requires a ReadReceipt to determine
    * when it can send the next ordered request.
    */
  val waitOnReadReceiptVec = entries.map(e => e.io.waitOnReadReceipt.getOrElse(false.B))
  val waitOnReadReceipt = Cat(waitOnReadReceiptVec).orR

  /**
    * Protocol Retry
    */
  val isPCrdGrant = io_pCrd.map(_.grant).reduce(_ || _)
  io_pCrd.zip(entries).foreach(x => x._1 <> x._2.io.pCrd)

  entries.zipWithIndex.foreach { case (entry, i) =>
    entry.io.req.valid := bus.a.valid && selectOH(i)
    entry.io.req.bits := bus.a.bits

    entry.io.chi.rx.dat.valid := io.rx.dat.valid && io.rx.dat.bits.txnID === i.U
    entry.io.chi.rx.dat.bits := io.rx.dat.bits

    entry.io.chi.rx.rsp.valid := io.rx.rsp.valid && io.rx.rsp.bits.txnID === i.U
    entry.io.chi.rx.rsp.bits := io.rx.rsp.bits

    entry.io.id := i.U
  }

  val txreqArb = Module(new Arbiter(chiselTypeOf(io.tx.req.bits), mmioBridgeSize))
  for ((a, req) <- txreqArb.io.in.zip(entries.map(_.io.chi.tx.req))) {
    a <> req
    val isReadNoSnp = req.bits.opcode === ReadNoSnp
    val block = isReadNoSnp && waitOnReadReceipt
    req.ready := a.ready && !block
    a.valid := req.valid && !block
  }
  io.tx.req <> txreqArb.io.out
  // arb(entries.map(_.io.chi.tx.req), io.tx.req, Some("mmio_txreq"))
  arb(entries.map(_.io.chi.tx.dat), io.tx.dat, Some("mmio_txdat"))
  arb(entries.map(_.io.resp), bus.d, Some("mmio_channel_D"))

  bus.a.ready := Cat(readys).orR

  io.rx.dat.ready := Cat(entries.zipWithIndex.map { case (entry, i) =>
    entry.io.chi.rx.dat.ready && io.rx.dat.bits.txnID === i.U
  }).orR
  io.rx.rsp.ready := true.B
  assert(!io.rx.rsp.valid || Cat(entries.zipWithIndex.map { case (entry, i) =>
    entry.io.chi.rx.rsp.ready && io.rx.rsp.bits.txnID === i.U }).orR)

  dontTouch(io)
  dontTouch(bus)
}
