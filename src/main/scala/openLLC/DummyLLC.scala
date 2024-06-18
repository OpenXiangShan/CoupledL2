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
import coupledL2.tl2chi.CHIOpcode.REQOpcodes._
import coupledL2.tl2chi.CHIOpcode.SNPOpcodes._
import coupledL2.tl2chi.CHIOpcode.RSPOpcodes._
import coupledL2.tl2chi.CHIOpcode.DATOpcodes._
import utility._

class DummyLLC(numRNs: Int = 1)(implicit p: Parameters) extends LazyModule with HasOpenLLCParameters {
  val axi4node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "LLC",
      id = IdRange(0, 16)
    ))
  )))

  lazy val module = new DummyLLCImp(numRNs)(this)
}

class DummyLLCImp(numRNs: Int)(wrapper: DummyLLC) extends LazyModuleImp(wrapper) with HasOpenLLCParameters {
  val io = IO(new Bundle {
    val rn = Vec(numRNs, Flipped(new PortIO))
  })

  println(s"====== DummyLLC ======")

  val (bus, edge) = wrapper.axi4node.out.head
  val aw = bus.aw
  val w = bus.w
  val b = bus.b
  val ar = bus.ar
  val r = bus.r

  val s_ar, w_r, s_aw, s_w, w_b = RegInit(true.B)
  val s_snp, w_snpresp = RegInit(VecInit(Seq.fill(numRNs)(true.B)))
  val s_comp, s_compdata, w_compack, w_cbwrdata = RegInit(true.B)
  val noSchedule = s_ar && s_aw && s_w && s_snp.asUInt.andR && s_comp && s_compdata
  val noWait = w_r && w_b && w_snpresp.asUInt.andR && w_compack && w_cbwrdata

  // LinkMonitor: transform between LCredit handshake and Decouple handshake
  val rn = Wire(Vec(numRNs, new DecoupledPortIO))
  for (i <- 0 until numRNs) {
    val linkMonitor = Module(new ReceiverLinkMonitor)
    linkMonitor.io.in <> io.rn(i)
    rn(i) <> linkMonitor.io.out
  }

  val releaseBuf = Reg(Vec(beatSize, UInt((beatBytes * 8).W)))
  val refillBuf = Reg(Vec(beatSize, UInt((beatBytes * 8).W)))
  val wBeatCnt = RegInit(0.U(log2Ceil(beatSize).W))
  val rBeatCnt = RegInit(0.U(log2Ceil(beatSize).W))
  val snpBeatCnt = RegInit(VecInit(Seq.fill(numRNs)(0.U(log2Ceil(beatSize).W))))
  val releaseBeatCnt = RegInit(0.U(log2Ceil(beatSize).W))
  val refillBeatCnt = RegInit(0.U(log2Ceil(beatSize).W))

  val snpGotData = RegInit(false.B)
  val snpGotDirty = RegInit(false.B)

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
    assert(isSnoopableRead || isDataless || isWriteBackFull || isEvict, "Unsupported opcode")

    when (isSnoopableRead) {
      s_ar := false.B
      w_r := false.B
    }
    
    when (isWriteBackFull) {
      s_aw := false.B
      s_w := false.B
      w_b := false.B
    }

    when (isSnoopableRead || isDataless) {
      s_snp := chosenOH
      w_snpresp := chosenOH
    }

    when (isSnoopableRead) {
      s_compdata := false.B
    }

    when (isDataless || isWriteBackFull || isEvict) {
      s_comp := false.B
    }

    when (isSnoopableRead || isDataless) {
      w_compack := false.B
    }

    when (isWriteBackFull) {
      w_cbwrdata := false.B
    }
  }

  // 3. Send out s_* tasks
  ar.valid := !s_ar
  aw.valid := !s_aw
  w.valid := !s_w && w_cbwrdata

  for (i <- 0 until numRNs) {
    rn(i).rx.snp.valid := !s_snp(i)
    rn(i).rx.rsp.valid := !s_comp && w_snpresp.asUInt.andR && chosenIdx === i.U
    rn(i).rx.dat.valid := !s_compdata && w_snpresp.asUInt.andR && w_r && chosenIdx === i.U
  }

  ar.bits.id := 0.U
  ar.bits.addr := req.addr
  ar.bits.len := (beatSize - 1).U
  ar.bits.size := log2Ceil(beatBytes).U
  ar.bits.burst := BURST_INCR
  ar.bits.lock := 0.U
  ar.bits.cache := CACHE_MODIFIABLE
  ar.bits.prot := 0.U
  ar.bits.qos := 0.U

  aw.bits.id := 0.U
  aw.bits.addr := req.addr
  aw.bits.len := (beatSize - 1).U
  aw.bits.size := log2Ceil(beatBytes).U
  aw.bits.burst := BURST_INCR
  aw.bits.lock := 0.U
  aw.bits.cache := CACHE_MODIFIABLE
  aw.bits.prot := 0.U
  aw.bits.qos := 0.U

  w.bits.data := releaseBuf(wBeatCnt)
  w.bits.strb := Fill(beatBytes, true.B)
  w.bits.last := wBeatCnt === (beatSize - 1).U

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
    rsp.opcode := ParallelLookUp(req.opcode, Seq(
      MakeUnique -> Comp,
      Evict -> Comp,
      WriteBackFull -> CompDBIDResp
    ))
    rsp.srcID := req.tgtID
    rsp.dbID := 0.U
    rsp.resp := ParallelLookUp(req.opcode, Seq(
      MakeUnique -> CHICohStates.UC,
      Evict -> CHICohStates.I,
      WriteBackFull -> CHICohStates.I
    ))

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
      CHICohStates.SC
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
    when (rn(i).rx.rsp.fire) { s_comp := true.B }
    when (rn(i).rx.dat.fire) {
      refillBeatCnt := refillBeatCnt + 1.U
      when (refillBeatCnt === (beatSize - 1).U) { s_compdata := true.B }
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

        when (resp === CHICohStates.SC_PD) {
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
        when (releaseBeatCnt === (beatSize - 1).U) {
          w_cbwrdata := true.B
        }
      }
    }
  }

  r.ready := !w_r
  b.ready := !w_b
  for (i <- 0 until numRNs) {
    rn(i).tx.rsp.ready := Mux(chosenIdx === i.U, !w_compack, !w_snpresp(i))
    rn(i).tx.dat.ready := Mux(chosenIdx === i.U, !w_cbwrdata, !w_snpresp(i))
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

  dontTouch(io.in)
}