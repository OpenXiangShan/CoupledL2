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

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import coupledL2.{TaskWithData, TaskBundle, DSBlock, DSBeat}

class TXDATBlockBundle(implicit p: Parameters) extends TXBlockBundle {
  val blockSinkBReqEntrance = Bool()

  override def apply() = 0.U.asTypeOf(this)
}

class TXDAT(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new TaskWithData()))
    val out = DecoupledIO(new CHIDAT())

    val pipeStatusVec = Flipped(Vec(5, ValidIO(new PipeStatusWithCHI)))
    val toReqArb = Output(new TXDATBlockBundle)
  })

  assert(!io.in.valid || io.in.bits.task.toTXDAT, "txChannel is wrong for TXDAT")
  assert(!io.in.valid || io.in.ready, "TXDAT should never be full")
  require(chiOpt.isDefined)
  require(beatBytes * 8 == DATA_WIDTH)

  // TODO: an mshrsAll-entry queue is too much, evaluate for a proper size later
  // Use customized SRAM: dual_port, max 256bits:
  val queue = Module(new Queue(new TaskBundle(), entries = mshrsAll, flow = true))
  val queueData0 = Module(new Queue(new DSBeat(), entries = mshrsAll, flow = true))
  val queueData1 = Module(new Queue(new DSBeat(), entries = mshrsAll, flow = true))
  queue.io.enq.valid := io.in.valid
  queue.io.enq.bits := io.in.bits.task
  io.in.ready := queue.io.enq.ready
  val enqData = io.in.bits.data.asTypeOf(Vec(beatSize, new DSBeat))
  queueData0.io.enq.valid := io.in.valid
  queueData0.io.enq.bits := enqData(0)
  queueData1.io.enq.valid := io.in.valid
  queueData1.io.enq.bits := enqData(1)

  // Back pressure logic from TXDAT
  val queueCnt = queue.io.count
  // TODO: this may be imprecise, review this later
  val pipeStatus_s1_s5 = io.pipeStatusVec
  val pipeStatus_s1_s2 = pipeStatus_s1_s5.take(2)
  val pipeStatus_s2 = pipeStatus_s1_s2.tail
  val pipeStatus_s3_s5 = pipeStatus_s1_s5.drop(2)
  // inflightCnt equals the number of reqs on s2~s5 that may flow into TXDAT soon, plus queueCnt.
  // The calculation of inflightCnt might be imprecise and leads to false positive back pressue.
  val inflightCnt = PopCount(Cat(pipeStatus_s3_s5.map(s => s.valid && s.bits.toTXDAT && (s.bits.fromB || s.bits.mshrTask)))) +
    PopCount(Cat(pipeStatus_s2.map(s => s.valid && Mux(s.bits.mshrTask, s.bits.toTXDAT, s.bits.fromB)))) +
    queueCnt

  assert(inflightCnt <= mshrsAll.U, "in-flight overflow at TXDAT")

  val noSpaceForSinkBReq = inflightCnt >= mshrsAll.U
  val noSpaceForMSHRReq = inflightCnt >= (mshrsAll-2).U

  io.toReqArb.blockSinkBReqEntrance := noSpaceForSinkBReq
  io.toReqArb.blockMSHRReqEntrance := noSpaceForMSHRReq

  val beatValids = RegInit(VecInit(Seq.fill(beatSize)(false.B)))
  val taskValid = beatValids.asUInt.orR
  val taskR = RegInit(0.U.asTypeOf(new TaskWithData))

  val dequeueReady = !taskValid // TODO: this may introduce bubble?
  queue.io.deq.ready := dequeueReady
  queueData0.io.deq.ready := dequeueReady
  queueData1.io.deq.ready := dequeueReady
  when (queue.io.deq.fire) {
    beatValids.foreach(_ := true.B)
    taskR.task := queue.io.deq.bits
    taskR.data := Cat(queueData1.io.deq.bits.data, queueData0.io.deq.bits.data).asTypeOf(new DSBlock)
  }

  val data = taskR.data.data
  val beatsOH = beatValids.asUInt
  val (beat, next_beatsOH) = getBeat(data, beatsOH)

  io.out.valid := taskValid
  io.out.bits := toCHIDATBundle(taskR.task, beat, beatsOH)
  io.out.bits.respErr := Mux(taskR.task.corrupt, Mux(taskR.task.denied, RespErrEncodings.NDERR, RespErrEncodings.DERR), RespErrEncodings.OK)

  when (io.out.fire) {
    beatValids := VecInit(next_beatsOH.asBools)
  }

  def getBeat(data: UInt, beatsOH: UInt): (UInt, UInt) = {
    // get one beat from data according to beatsOH
    require(data.getWidth == (blockBytes * 8))
    require(beatsOH.getWidth == beatSize)
    // next beat
    val next_beat = ParallelPriorityMux(beatsOH, data.asTypeOf(Vec(beatSize, UInt((beatBytes * 8).W))))
    val selOH = PriorityEncoderOH(beatsOH)
    // remaining beats that haven't been sent out
    val next_beatsOH = beatsOH & ~selOH
    (next_beat, next_beatsOH)
  }

  def toCHIDATBundle(task: TaskBundle, beat: UInt, beatsOH: UInt): CHIDAT = {
    val dat = WireInit(0.U.asTypeOf(new CHIDAT()))

    // width parameters and width check
    require(beat.getWidth == dat.data.getWidth)
    val beatOffsetWidth = log2Up(beatBytes)
    val chunkOffsetWidth = log2Up(16) // DataID is assigned with the granularity of a 16-byte chunk

    dat.tgtID := task.tgtID.get
    dat.srcID := task.srcID.get
    dat.txnID := task.txnID.get
    dat.homeNID := task.homeNID.get
    dat.dbID := task.dbID.get
    dat.opcode := task.chiOpcode.get
    dat.ccID := 0.U // TODO: consider critical chunk id
    // The DataID field value must be set to Addr[5:4] because the DataID field represents Addr[5:4] of the lowest
    // addressed byte within the packet.
    // dat.dataID := ParallelPriorityMux(beatsOH.asBools.zipWithIndex.map(x => (x._1, (x._2 << beatOffsetWidth).U(5, 4))))
    dat.dataID := ParallelPriorityMux(
      beatsOH,
      List.tabulate(beatSize)(i => (i << (beatOffsetWidth - chunkOffsetWidth)).U)
    )
    dat.be := Fill(BE_WIDTH, 1.U(1.W))
    dat.data := beat
    dat.resp := task.resp.get
    dat.fwdState := task.fwdState.get
    dat.traceTag := task.traceTag.get
    dat.dataCheck := Cat((0 until DATACHECK_WIDTH).map(i => (beat(64 * (i + 1) - 1, 64 * i).xorR ^ false.B).asUInt))
    dat.poision := Fill(POISON_WIDTH, task.corrupt)

    dat
  }

}