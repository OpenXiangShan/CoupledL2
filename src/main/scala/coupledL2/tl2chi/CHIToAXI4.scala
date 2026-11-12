// Copyright (c) 2026. Licensed under Mulan PSL v2.
package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import coupledL2._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import utility.RRArbiterInit

/** A private, single-core memory endpoint. Each MSHR owns one transaction slot.
  * The write queue records the AW arbitration order and drives W in that order.
  * Slots remain occupied through CompAck, or through both WLAST and B.
  */
class CHIToAXI4(axiParams: AXI4BundleParameters)(implicit p: Parameters)
  extends TL2CHIL2Module with HasCHIOpcodes {
  require(mshrsAll == 16 && DATA_WIDTH == 256 && axiParams.dataBits == 256)
  require(axiParams.idBits >= 4, "AXI IDs must cover all endpoint slots")
  val io = IO(new Bundle {
    val chi = Flipped(new DecoupledPortIO)
    val axi = new AXI4Bundle(axiParams)
  })
  val idle :: sendAR :: waitR :: sendComp :: waitAck :: waitData :: writeReady :: writeBusy :: Nil = Enum(8)
  val state = RegInit(VecInit(Seq.fill(16)(idle)))
  val req = Reg(Vec(16, new CHIREQ))
  val data = Reg(Vec(16, Vec(2, UInt(256.W))))
  val mask = Reg(Vec(16, Vec(2, UInt(32.W))))
  val dataValid = RegInit(VecInit(Seq.fill(16)(0.U(2.W))))
  val wDone = RegInit(VecInit(Seq.fill(16)(false.B)))
  val bDone = RegInit(VecInit(Seq.fill(16)(false.B)))
  val rBeat = RegInit(VecInit(Seq.fill(16)(false.B)))
  val ackSeen = RegInit(VecInit(Seq.fill(16)(false.B)))
  // Request TxnIDs include the slice ID in their high bits. DBIDs and AXI
  // IDs name our private slots; preserve the complete request TxnID for replies.
  val freeSlots = VecInit(state.map(_ === idle)).asUInt
  // CompDBIDResp only releases the L2's CHI-side wait; the write may still
  // be waiting for data, AW/W, or B here. AXI does not order AR against AW,
  // so retain the older same-line transactions as dependencies until their
  // slots retire. Accept blocked requests into slots instead of blocking
  // TXREQ's queue head, which could prevent CopyBackWrData from being made.
  val dependencies = RegInit(VecInit(Seq.fill(16)(0.U(16.W))))
  for (i <- 0 until 16) {
    dependencies(i) := dependencies(i) & ~freeSlots
  }
  val idx = PriorityEncoder(freeSlots)
  val op = io.chi.tx.req.bits.opcode
  def isRead(x: UInt): Bool = x === ReadUnique || x === ReadNotSharedDirty
  def isWrite(x: UInt): Bool = x === WriteBackFull || x === WriteCleanFull || x === WriteEvictFull
  io.chi.tx.req.ready := freeSlots.orR
  when(io.chi.tx.req.fire) {
    assert(isRead(op) || isWrite(op) || op === MakeUnique || op === Evict ||
      op === WriteEvictOrEvict || op === CleanShared || op === CleanInvalid || op === MakeInvalid,
      "unsupported CHI transaction in no-LLC endpoint")
    req(idx) := io.chi.tx.req.bits
    dependencies(idx) := VecInit((0 until 16).map(i =>
      state(i) =/= idle && req(i).addr(ADDR_WIDTH - 1, offsetBits) === io.chi.tx.req.bits.addr(ADDR_WIDTH - 1, offsetBits)
    )).asUInt
    state(idx) := Mux(isRead(op), sendAR, sendComp)
    dataValid(idx) := 0.U
    rBeat(idx) := false.B
    wDone(idx) := false.B
    bDone(idx) := false.B
    ackSeen(idx) := false.B
  }
  io.chi.rx.snp.valid := false.B
  io.chi.rx.snp.bits := DontCare
  val ackID = io.chi.tx.rsp.bits.txnID(3, 0)
  io.chi.tx.rsp.ready := true.B
  when(io.chi.tx.rsp.fire) {
    assert(io.chi.tx.rsp.bits.txnID < 16.U)
    assert(io.chi.tx.rsp.bits.opcode === CompAck && !ackSeen(ackID) && req(ackID).expCompAck &&
      (state(ackID) === waitAck || state(ackID) === waitR && rBeat(ackID)),
      "unexpected CompAck or CompAck before the first CompData")
    ackSeen(ackID) := true.B
  }

  // Return completion responses. Write data returns our slot DBID as TxnID.
  val rsp = Module(new RRArbiterInit(new CHIRSP, 16))
  io.chi.rx.rsp <> rsp.io.out
  for(i <- 0 until 16) {
    // Always request write data promptly; only memory access/completion
    // needs to wait for predecessors, not collecting the write payload.
    rsp.io.in(i).valid := state(i) === sendComp && (isWrite(req(i).opcode) || !dependencies(i).orR)
    rsp.io.in(i).bits := 0.U.asTypeOf(new CHIRSP)
    val out = rsp.io.in(i).bits
    out.opcode := Mux(isWrite(req(i).opcode), CompDBIDResp, Comp)
    out.txnID := req(i).txnID
    out.dbID := i.U
    out.tgtID := req(i).srcID
    out.resp := Mux(req(i).opcode === MakeUnique, CHICohStates.UC, CHICohStates.I)
    out.traceTag := req(i).traceTag
    when(rsp.io.in(i).fire) {
      state(i) := Mux(isWrite(req(i).opcode), waitData,
        Mux(req(i).expCompAck, waitAck, idle))
    }
  }
  val datID = io.chi.tx.dat.bits.txnID(3, 0)
  val datBeat = io.chi.tx.dat.bits.dataID(1)
  io.chi.tx.dat.ready := state(datID) === waitData
  when(io.chi.tx.dat.fire) {
    assert(io.chi.tx.dat.bits.txnID < 16.U)
    assert(io.chi.tx.dat.bits.opcode === CopyBackWrData)
    assert(io.chi.tx.dat.bits.dataID === 0.U || io.chi.tx.dat.bits.dataID === 2.U)
    assert(!dataValid(datID)(datBeat))
    data(datID)(datBeat) := io.chi.tx.dat.bits.data
    mask(datID)(datBeat) := io.chi.tx.dat.bits.be
    // Keep this mask two bits wide. Dynamic shifting a two-bit literal
    // produces three bits; andR on that value can never become true.
    val newValid = dataValid(datID) | UIntToOH(datBeat, 2)
    require(newValid.getWidth == 2)
    dataValid(datID) := newValid
    when(newValid.andR) { state(datID) := writeReady }
  }

  val arArb = Module(new RRArbiterInit(new AXI4BundleAR(axiParams), 16))
  // AXI addresses must remain stable while VALID is stalled.
  io.axi.ar <> Queue.irrevocable(arArb.io.out, 1)
  val awArb = Module(new RRArbiterInit(new AXI4BundleAW(axiParams), 16))
  val writeOrder = Module(new Queue(UInt(4.W), 16))
  val awOut = Queue.irrevocable(awArb.io.out, 1)
  io.axi.aw.valid := awOut.valid && writeOrder.io.enq.ready
  io.axi.aw.bits := awOut.bits
  awOut.ready := io.axi.aw.ready && writeOrder.io.enq.ready
  writeOrder.io.enq.valid := io.axi.aw.fire
  writeOrder.io.enq.bits := io.axi.aw.bits.id
  for(i <- 0 until 16) {
    val ar = arArb.io.in(i)
    ar.valid := state(i) === sendAR && !dependencies(i).orR
    ar.bits := 0.U.asTypeOf(ar.bits)
    ar.bits.id := i.U
    ar.bits.addr := req(i).addr
    ar.bits.len := 1.U
    ar.bits.size := 5.U
    ar.bits.burst := AXI4Parameters.BURST_INCR
    when(ar.fire) { state(i) := waitR }
    val aw = awArb.io.in(i)
    aw.valid := state(i) === writeReady && !dependencies(i).orR
    aw.bits := 0.U.asTypeOf(aw.bits)
    aw.bits.id := i.U
    aw.bits.addr := req(i).addr
    aw.bits.len := 1.U
    aw.bits.size := 5.U
    aw.bits.burst := AXI4Parameters.BURST_INCR
    when(aw.fire) { state(i) := writeBusy }
  }
  // No independent W arbiter: its owner is always the head of the AW queue.
  val writeID = writeOrder.io.deq.bits
  val writeBeat = RegInit(false.B)
  io.axi.w.valid := writeOrder.io.deq.valid
  io.axi.w.bits := 0.U.asTypeOf(io.axi.w.bits)
  io.axi.w.bits.data := data(writeID)(writeBeat)
  io.axi.w.bits.strb := mask(writeID)(writeBeat)
  io.axi.w.bits.last := writeBeat
  writeOrder.io.deq.ready := io.axi.w.fire && writeBeat
  when(io.axi.w.fire) {
    writeBeat := !writeBeat
    when(writeBeat) { wDone(writeID) := true.B }
  }
  io.axi.b.ready := true.B
  when(io.axi.b.fire) {
    assert(io.axi.b.bits.id < 16.U)
    assert(state(io.axi.b.bits.id) === writeBusy)
    assert(io.axi.b.bits.resp === AXI4Parameters.RESP_OKAY, "DDR write failed")
    bDone(io.axi.b.bits.id) := true.B
  }
  for(i <- 0 until 16) {
    when(state(i) === writeBusy && wDone(i) && bDone(i)) { state(i) := idle }
    // Issue C+ permits CompAck after the first data beat. It must not free
    // the slot until the complete AXI read has also been forwarded.
    when(state(i) === waitAck && (!req(i).expCompAck || ackSeen(i))) { state(i) := idle }
  }

  // Forward the AXI read stream with a CHI completion header on each beat.
  val readID = io.axi.r.bits.id
  io.chi.rx.dat.valid := io.axi.r.valid
  io.axi.r.ready := io.chi.rx.dat.ready
  io.chi.rx.dat.bits := 0.U.asTypeOf(new CHIDAT)
  val rd = io.chi.rx.dat.bits
  rd.opcode := CompData
  rd.txnID := req(readID).txnID
  rd.dbID := readID
  rd.tgtID := req(readID).srcID
  rd.resp := CHICohStates.UC
  rd.traceTag := req(readID).traceTag
  rd.data := io.axi.r.bits.data
  rd.be := Fill(32, 1.U(1.W))
  rd.dataID := Mux(rBeat(readID), 2.U, 0.U)
  rd.respErr := Mux(io.axi.r.bits.resp === AXI4Parameters.RESP_OKAY, 0.U, RespErrEncodings.NDERR)
  rd.dataCheck.foreach { check =>
    require(dataCheckMethod == 1, "no-LLC endpoint supports odd parity data check")
    check := VecInit((0 until 32).map(i => !io.axi.r.bits.data(8*i+7, 8*i).xorR)).asUInt
  }
  when(io.axi.r.fire) {
    assert(readID < 16.U)
    assert(state(readID) === waitR)
    assert(io.axi.r.bits.last === rBeat(readID))
    rBeat(readID) := true.B
    when(io.axi.r.bits.last) { state(readID) := waitAck }
  }
}
