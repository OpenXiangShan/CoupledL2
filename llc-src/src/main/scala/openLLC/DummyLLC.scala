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
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4.AXI4Parameters._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2chi._
import coupledL2.tl2chi.CHICohStates._
import utility._

class DummyLLC(numRNs: Int = 1)(implicit p: Parameters) extends LazyModule with HasOpenLLCParameters {
  val axi4node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "LLC",
      id = IdRange(0, 16),
      aligned = true,
      maxFlight = Some(1)
    ))
  )))

  lazy val module = new DummyLLCImp(numRNs)(this)
}

class DummyLLCImp(numRNs: Int)(wrapper: DummyLLC) extends LazyModuleImp(wrapper)
  with HasOpenLLCParameters 
  with HasCHIOpcodes {

  val io = IO(new Bundle {
    val rn = Vec(numRNs, Flipped(new PortIO))
  })

  println(s"====== DummyLLC ======")
  dontTouch(io.rn)

  val (bus, edge) = wrapper.axi4node.out.head
  val aw = bus.aw
  val w = bus.w
  val b = bus.b
  val ar = bus.ar
  val r = bus.r

  val s_ar, w_r, s_aw, s_w, w_b = RegInit(true.B)
  val s_snp, w_snpresp = RegInit(VecInit(Seq.fill(numRNs)(true.B)))
  val s_readreceipt, s_comp, s_compdata, w_compack, w_wrdata = RegInit(true.B)
  val noSchedule = s_ar && s_aw && s_w && s_snp.asUInt.andR && s_comp && s_compdata && s_readreceipt
  val noWait = w_r && w_b && w_snpresp.asUInt.andR && w_compack && w_wrdata

  // LinkMonitor: transform between LCredit handshake and Decouple handshake
  val rn = Wire(Vec(numRNs, new DecoupledPortIO))
  for (i <- 0 until numRNs) {
    val linkMonitor = Module(new ReceiverLinkMonitor)
    linkMonitor.io.in <> io.rn(i)
    rn(i) <> linkMonitor.io.out
  }

  println(s"CHI REQ Flit Width: ${io.rn(0).tx.req.flit.getWidth}")
  println(s"CHI RSP Flit Width: ${io.rn(0).tx.rsp.flit.getWidth}")
  println(s"CHI SNP Flit Width: ${io.rn(0).rx.snp.flit.getWidth}")
  println(s"CHI DAT Flit Width: ${io.rn(0).rx.dat.flit.getWidth}")

  val releaseBuf = Reg(Vec(beatSize, UInt((beatBytes * 8).W)))
  val refillBuf = Reg(Vec(beatSize, UInt((beatBytes * 8).W)))
  val wBeatCnt = RegInit(0.U(log2Ceil(beatSize).W))
  val rBeatCnt = RegInit(0.U(log2Ceil(beatSize).W))
  val snpBeatCnt = RegInit(VecInit(Seq.fill(numRNs)(0.U(log2Ceil(beatSize).W))))
  val releaseBeatCnt = RegInit(0.U(log2Ceil(beatSize).W))
  val refillBeatCnt = RegInit(0.U(log2Ceil(beatSize).W))

  val snpGotData = RegInit(false.B)
  val snpGotDirty = RegInit(false.B)
  val cbwrdataValid = RegInit(false.B)

  val be = Reg(UInt(beatBytes.W))

  // 1. Choose a request from all the RNs
  val reqArb = Module(new RRArbiter(new CHIREQ, numRNs))
  reqArb.io.in <> VecInit(rn.map(_.tx.req))
  val valid = RegInit(false.B)
  val req = Reg(new CHIREQ)
  val alloc = !valid && reqArb.io.out.valid
  val chosenOH = VecInit(reqArb.io.in.map(_.fire))
  val chosenIdx = RegEnable(OHToUInt(chosenOH), alloc)

  when (alloc) {
    valid := true.B
    req := reqArb.io.out.bits
    wBeatCnt := 0.U
    rBeatCnt := 0.U
    snpBeatCnt.foreach(_ := 0.U)
    releaseBeatCnt := 0.U
    refillBeatCnt := 0.U
    snpGotData := false.B
    snpGotDirty := false.B
    cbwrdataValid := false.B
  }.elsewhen (valid && noSchedule && noWait) {
    valid := false.B
  }
  reqArb.io.out.ready := !valid

  // 2. Set task registers
  when (alloc) {
    val opcode = reqArb.io.out.bits.opcode
    val isSnoopableRead = opcode === ReadUnique || opcode === ReadNotSharedDirty
    val isDataless = opcode === MakeUnique
    val isWriteBackFull = opcode === WriteBackFull
    val isEvict = opcode === Evict
    val isReadNoSnp = opcode === ReadNoSnp
    val isWriteNoSnp = opcode === WriteNoSnpFull || opcode === WriteNoSnpPtl
    val isNoSnp = isReadNoSnp || isWriteNoSnp
    val isRead = isSnoopableRead || isReadNoSnp
    val isWrite = isWriteBackFull || isWriteNoSnp
    assert(isSnoopableRead || isDataless || isWriteBackFull || isEvict || isNoSnp, "Unsupported opcode")

    when (isNoSnp) {
      assert((1.U << reqArb.io.out.bits.size) <= beatBytes.U)
    }

    when (isRead) {
      s_ar := false.B
      w_r := false.B
    }
    
    when (isWrite) {
      s_aw := false.B
      s_w := false.B
      w_b := false.B
    }

    when (isSnoopableRead || isDataless) {
      s_snp := chosenOH
      w_snpresp := chosenOH
    }

    when (isReadNoSnp && reqArb.io.out.bits.order =/= OrderEncodings.None) {
      s_readreceipt := false.B
    }

    when (isRead) {
      s_compdata := false.B
    }

    when (isDataless || isWrite || isEvict) {
      s_comp := false.B
    }

    when (isSnoopableRead || isDataless || isReadNoSnp && reqArb.io.out.bits.expCompAck) {
      w_compack := false.B
    }

    when (isWrite) {
      w_wrdata := false.B
    }
  }

  // 3. Send out s_* tasks
  ar.valid := !s_ar
  aw.valid := !s_aw
  w.valid := !s_w && w_wrdata

  for (i <- 0 until numRNs) {
    rn(i).rx.snp.valid := !s_snp(i)
    rn(i).rx.rsp.valid := (!s_comp && w_snpresp.asUInt.andR || !s_readreceipt) && chosenIdx === i.U
    rn(i).rx.dat.valid := !s_compdata && w_snpresp.asUInt.andR && w_r && chosenIdx === i.U
  }

  val isReadNoSnp = req.opcode === ReadNoSnp
  ar.bits.id := 0.U
  ar.bits.addr := req.addr
  ar.bits.len := Mux(isReadNoSnp, 0.U, (beatSize - 1).U)
  ar.bits.size := Mux(isReadNoSnp, req.size, log2Ceil(beatBytes).U)
  ar.bits.burst := BURST_INCR
  ar.bits.lock := 0.U
  ar.bits.cache := Mux(
    isReadNoSnp,
    0.U,
    CACHE_RALLOCATE | CACHE_WALLOCATE | CACHE_MODIFIABLE | CACHE_BUFFERABLE
  )
  ar.bits.prot := 0.U
  ar.bits.qos := 0.U

  val isWriteNoSnp = req.opcode === WriteNoSnpFull || req.opcode === WriteNoSnpPtl
  aw.bits.id := 0.U
  aw.bits.addr := req.addr
  aw.bits.len := Mux(isWriteNoSnp, 0.U, (beatSize - 1).U)
  aw.bits.size := Mux(isWriteNoSnp, req.size, log2Ceil(beatBytes).U)
  aw.bits.burst := BURST_INCR
  aw.bits.lock := 0.U
  aw.bits.cache := Mux(
    isWriteNoSnp,
    0.U,
    CACHE_RALLOCATE | CACHE_WALLOCATE | CACHE_MODIFIABLE | CACHE_BUFFERABLE
  )
  aw.bits.prot := 0.U
  aw.bits.qos := 0.U

  w.bits.data := releaseBuf(wBeatCnt)
  w.bits.strb := Mux(isWriteNoSnp, be, Fill(beatBytes, cbwrdataValid || snpGotDirty))
  w.bits.last := isWriteNoSnp || wBeatCnt === (beatSize - 1).U

  for (i <- 0 until numRNs) {
    val snp = rn(i).rx.snp.bits
    snp := 0.U.asTypeOf(new CHISNP)
    snp.addr := req.addr >> 3
    snp.srcID := req.tgtID
    snp.txnID := 0.U
    snp.opcode := ParallelLookUp(req.opcode, Seq(
      ReadUnique -> SnpUnique,
      ReadNotSharedDirty -> SnpNotSharedDirty,
      MakeUnique -> SnpMakeInvalid
    ))
    snp.retToSrc := false.B

    val rsp = rn(i).rx.rsp.bits
    rsp := 0.U.asTypeOf(new CHIRSP)
    rsp.txnID := req.txnID
    rsp.opcode := Mux(
      !s_readreceipt,
      ReadReceipt,
      ParallelLookUp(req.opcode, Seq(
        MakeUnique -> Comp,
        Evict -> Comp,
        WriteBackFull -> CompDBIDResp,
        WriteNoSnpFull -> CompDBIDResp,
        WriteNoSnpPtl -> CompDBIDResp
      ))
    )
    rsp.srcID := req.tgtID
    rsp.dbID := 0.U
    rsp.resp := Mux(
      !s_readreceipt,
      0.U,
      ParallelLookUp(req.opcode, Seq(
        MakeUnique -> CHICohStates.UC,
        Evict -> CHICohStates.I,
        WriteBackFull -> CHICohStates.I,
        WriteNoSnpFull -> CHICohStates.I,
        WriteNoSnpPtl -> CHICohStates.I
      ))
    )

    val dat = rn(i).rx.dat.bits
    val buf = Mux(snpGotData, releaseBuf, refillBuf)
    dat := 0.U.asTypeOf(new CHIDAT)
    dat.dataID := refillBeatCnt << 1
    dat.txnID := req.txnID
    dat.data := buf(refillBeatCnt)
    dat.opcode := CompData
    dat.srcID := req.tgtID
    dat.homeNID := req.tgtID
    dat.dbID := 0.U
    dat.resp := Mux(
      req.opcode === ReadUnique,
      Mux(snpGotDirty, CHICohStates.UD_PD, CHICohStates.UC),
      Mux(isReadNoSnp, CHICohStates.I, CHICohStates.SC)
    )
  }

  when (ar.fire) { s_ar := true.B }
  when (aw.fire) { s_aw := true.B }
  when (w.fire) {
    wBeatCnt := wBeatCnt + 1.U
    when (w.bits.last) { s_w := true.B }
  }

  for (i <- 0 until numRNs) {
    when (rn(i).rx.snp.fire) { s_snp(i) := true.B }
    when (rn(i).rx.rsp.fire) {
      s_comp := true.B
      s_readreceipt := true.B
    }
    when (rn(i).rx.dat.fire) {
      refillBeatCnt := refillBeatCnt + 1.U
      when (isReadNoSnp || refillBeatCnt === (beatSize - 1).U) {
        s_compdata := true.B
      }
    }
  }

  // 4. Wait for w_* tasks
  when (r.fire) {
    rBeatCnt := rBeatCnt + 1.U
    refillBuf(rBeatCnt) := r.bits.data
    when (r.bits.last) {
      w_r := true.B
    }
  }

  when (b.fire) {
    w_b := true.B
  }

  for (i <- 0 until numRNs) {
    when (rn(i).tx.rsp.fire && rn(i).tx.rsp.bits.opcode === SnpResp) {
      w_snpresp(i) := true.B
    }.elsewhen (rn(i).tx.dat.fire && rn(i).tx.dat.bits.opcode === SnpRespData) {
      val beatIdx = rn(i).tx.dat.bits.dataID >> 1
      val data = rn(i).tx.dat.bits.data
      val resp = rn(i).tx.dat.bits.resp
      snpBeatCnt(i) := snpBeatCnt(i) + 1.U
      releaseBuf(beatIdx) := data
      snpGotData := true.B

      when (snpBeatCnt(i) === (beatSize - 1).U) {
        w_snpresp(i) := true.B

        when ((resp & CHICohStates.PassDirty) =/= 0.U) {
          // SD state is not supported on RNs because we adopt MESI coherency rather than MOESI for now.
          // Therefore when other peer-RNs are snooped to Shared state and pass dirty to HN, HN is reponsible
          // for either caching dirty copy in local cache of HN, or copying back the dirty data into SN.
          s_aw := false.B
          s_w := false.B
          w_b := false.B
        }
      }

      when ((resp & CHICohStates.PassDirty) =/= 0.U) {
        snpGotDirty := true.B
      }
    }

    when (chosenIdx === i.U) {
      when (rn(i).tx.rsp.fire && rn(i).tx.rsp.bits.opcode === CompAck) {
        w_compack := true.B
      }

      when (rn(i).tx.dat.fire && rn(i).tx.dat.bits.opcode === CopyBackWrData) {
        releaseBeatCnt := releaseBeatCnt + 1.U
        val beatIdx = rn(i).tx.dat.bits.dataID >> 1
        val data = rn(i).tx.dat.bits.data
        releaseBuf(beatIdx) := data
        cbwrdataValid := (rn(i).tx.dat.bits.resp & PassDirty) =/= I
        when (releaseBeatCnt === (beatSize - 1).U) {
          w_wrdata := true.B
        }
      }

      when (rn(i).tx.dat.fire && rn(i).tx.dat.bits.opcode === NonCopyBackWrData) {
        releaseBuf(0) := rn(i).tx.dat.bits.data
        be := rn(i).tx.dat.bits.be
        w_wrdata := true.B
      }
    }
  }

  r.ready := !w_r
  b.ready := !w_b
  for (i <- 0 until numRNs) {
    rn(i).tx.rsp.ready := Mux(chosenIdx === i.U, !w_compack, !w_snpresp(i))
    rn(i).tx.dat.ready := Mux(chosenIdx === i.U, !w_wrdata, !w_snpresp(i))
  }
}

class ReceiverLinkMonitor(implicit p: Parameters) extends LLCModule with HasCHIMsgParameters {
  val io = IO(new Bundle() {
    val in = Flipped(new PortIO)
    val out = new DecoupledPortIO
  })

  val txState = RegInit(LinkStates.STOP)
  val rxState = RegInit(LinkStates.STOP)

  Seq(txState, rxState).zip(MixedVecInit(Seq(io.in.tx, io.in.rx))).foreach { case (state, link) =>
    state := MuxLookup(Cat(link.linkactivereq, link.linkactiveack), LinkStates.STOP)(Seq(
      Cat(true.B, false.B) -> LinkStates.ACTIVATE,
      Cat(true.B, true.B) -> LinkStates.RUN,
      Cat(false.B, true.B) -> LinkStates.DEACTIVATE,
      Cat(false.B, false.B) -> LinkStates.STOP
    ))
  }

  /* IO assignment */
  val txreqDeact, txrspDeact, txdatDeact = Wire(Bool())
  val txDeact = txreqDeact && txrspDeact && txdatDeact
  LCredit2Decoupled(io.in.tx.req, io.out.tx.req, LinkState(txState), txreqDeact, Some("rxreq"))
  LCredit2Decoupled(io.in.tx.rsp, io.out.tx.rsp, LinkState(txState), txrspDeact, Some("rxrsp"))
  LCredit2Decoupled(io.in.tx.dat, io.out.tx.dat, LinkState(txState), txdatDeact, Some("rxdat"))
  Decoupled2LCredit(io.out.rx.snp, io.in.rx.snp, LinkState(rxState), Some("txsnp"))
  Decoupled2LCredit(io.out.rx.rsp, io.in.rx.rsp, LinkState(rxState), Some("txrsp"))
  Decoupled2LCredit(io.out.rx.dat, io.in.rx.dat, LinkState(rxState), Some("txdat"))

  io.in.rxsactive := true.B
  io.in.rx.linkactivereq := !reset.asBool
  io.in.tx.linkactiveack := RegNext(io.in.tx.linkactivereq) || !txDeact

  io.in.syscoack := RegNext(io.in.syscoreq)

  dontTouch(io.in)
}