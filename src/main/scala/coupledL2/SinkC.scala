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

package coupledL2

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import chipsalliance.rocketchip.config.Parameters
import coupledL2.utils.XSPerfAccumulate
import utility.MemReqSource

class PipeBufferRead(implicit p: Parameters) extends L2Bundle {
  val bufIdx = UInt(bufIdxBits.W)
}

class PipeBufferResp(implicit p: Parameters) extends L2Bundle {
  val data = Vec(beatSize, UInt((beatBytes * 8).W))
}

// SinkC receives upwards Release or ProbeAck:
// (1) For Release/ReleaseData, send it to RequestArb directly
// (2) For ProbeAck/ProbeAckData, wakeup w_probeack in MSHR
//     For ProbeAckData, save data into ReleaseBuffer
class SinkC(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val c = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    val task = DecoupledIO(new TaskBundle) // Release/ReleaseData
    val resp = Output(new RespBundle)
    val releaseBufWrite = Flipped(new MSHRBufWrite)
    val bufRead = Input(ValidIO(new PipeBufferRead))
    val bufResp = Output(new PipeBufferResp)
    val refillBufWrite = Flipped(new MSHRBufWrite)
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
  })
  
  val (first, last, _, beat) = edgeIn.count(io.c)
  val isRelease = io.c.bits.opcode(1)
  val hasData = io.c.bits.opcode(0)

  // dataBuf entry is valid when Release has data
  // taskBuf entry is valid when ReqArb is not ready to receive C tasks
  val dataBuf = Reg(Vec(bufBlocks, Vec(beatSize, UInt((beatBytes * 8).W))))
  val beatValids = RegInit(VecInit(Seq.fill(bufBlocks)(VecInit(Seq.fill(beatSize)(false.B)))))
  val dataValids = VecInit(beatValids.map(_.asUInt.orR)).asUInt
  val taskBuf = Reg(Vec(bufBlocks, new TaskBundle))
  val taskValids = RegInit(VecInit(Seq.fill(bufBlocks)(false.B)))
  val taskArb = Module(new RRArbiter(new TaskBundle, bufBlocks))
  val bufValids = taskValids.asUInt | dataValids

  val full = bufValids.andR
  val noSpace = full && hasData
  val nextPtr = PriorityEncoder(~bufValids)
  val nextPtrReg = RegEnable(nextPtr, 0.U.asTypeOf(nextPtr), io.c.fire() && isRelease && first && hasData)

  def toTaskBundle(c: TLBundleC): TaskBundle = {
    val task = Wire(new TaskBundle)
    task.channel := "b100".U
    task.tag := parseAddress(c.address)._1
    task.set := parseAddress(c.address)._2
    task.off := parseAddress(c.address)._3
    task.alias.foreach(_ := 0.U)
    task.vaddr.foreach(_ := 0.U)
    task.opcode := c.opcode
    task.param := c.param
    task.size := c.size
    task.sourceId := c.source
    task.bufIdx := 0.U(bufIdxBits.W)
    task.needProbeAckData := false.B
    task.mshrTask := false.B
    task.mshrId := 0.U(mshrBits.W)
    task.aliasTask.foreach(_ := false.B)
    task.useProbeData := false.B
    task.fromL2pft.foreach(_ := false.B)
    task.needHint.foreach(_ := false.B)
    task.dirty := false.B
    task.way := 0.U(wayBits.W)
    task.meta := 0.U.asTypeOf(new MetaEntry)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dsWen := false.B
    task.wayMask := Fill(cacheParams.ways, "b1".U)
    task.reqSource := MemReqSource.NoWhere.id.U // Ignore
    task.replTask := false.B
    task
  }

  when (io.c.fire() && isRelease) {
    when (hasData) {
      when (first) {
        dataBuf(nextPtr)(beat) := io.c.bits.data
        beatValids(nextPtr)(beat) := true.B
      }.otherwise {
        assert(last)
        dataBuf(nextPtrReg)(beat) := io.c.bits.data
        beatValids(nextPtrReg)(beat) := true.B
      }
    }
  }

  when (io.c.fire() && isRelease && last && (!io.task.ready || taskArb.io.out.valid)) {
    when (hasData) {
      taskValids(nextPtrReg) := true.B
      taskBuf(nextPtrReg) := toTaskBundle(io.c.bits)
      taskBuf(nextPtrReg).bufIdx := nextPtrReg
    }.otherwise {
      taskValids(nextPtr) := true.B
      taskBuf(nextPtr) := toTaskBundle(io.c.bits)
      taskBuf(nextPtr).bufIdx := nextPtr
    }
  }

  taskArb.io.out.ready := io.task.ready
  taskArb.io.in.zipWithIndex.foreach {
    case (in, i) =>
      in.valid := taskValids(i)
      in.bits := taskBuf(i)
      when (in.fire()) {
        taskValids(i) := false.B
      }
  }

  when (io.bufRead.valid) {
    beatValids(io.bufRead.bits.bufIdx).foreach(_ := false.B)
  }

  val cValid = io.c.valid && isRelease && last
  io.task.valid := cValid || taskArb.io.out.valid
  io.task.bits := Mux(taskArb.io.out.valid, taskArb.io.out.bits, toTaskBundle(io.c.bits))
  io.task.bits.bufIdx := Mux(taskArb.io.out.valid, taskArb.io.out.bits.bufIdx, nextPtrReg)

  io.resp.valid := io.c.valid && (first || last) && !isRelease
  io.resp.mshrId := 0.U // DontCare
  io.resp.tag := parseAddress(io.c.bits.address)._1
  io.resp.set := parseAddress(io.c.bits.address)._2
  io.resp.respInfo.opcode := io.c.bits.opcode
  io.resp.respInfo.param := io.c.bits.param
  io.resp.respInfo.last := last
  io.resp.respInfo.dirty := io.c.bits.opcode(0)
  io.resp.respInfo.isHit := io.c.bits.opcode(0)

  io.releaseBufWrite.valid := io.c.valid && io.c.bits.opcode === ProbeAckData
  io.releaseBufWrite.beat_sel := UIntToOH(beat)
  io.releaseBufWrite.data.data := Fill(beatSize, io.c.bits.data)
  io.releaseBufWrite.id := 0.U(mshrBits.W) // id is given by MSHRCtl by comparing address to the MSHRs

  // C-Release writing new data to refillBuffer, for repl-Release to write to DS
  val newdataMask = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === io.task.bits.set && s.bits.reqTag === io.task.bits.tag && s.bits.releaseNotSent
  )).asUInt

  // we must wait until 2nd beat written into databuf(idx) before we can read it
  // So we use RegNext
  // //Or we can use Cat(databuf(idx)(0), io.c.bits.data)

  // since what we are trying to prevent is that C-Release comes first and MSHR-Release comes later
  // we can make sure this refillBufWrite can be read by MSHR-Release
  // TODO: this is rarely triggered, consider just blocking?
  io.refillBufWrite.valid := RegNext(io.task.fire && io.task.bits.opcode === ReleaseData && newdataMask.orR, false.B)
  io.refillBufWrite.beat_sel := Fill(beatSize, 1.U(1.W))
  io.refillBufWrite.id := RegNext(OHToUInt(newdataMask))
  io.refillBufWrite.data.data := dataBuf(RegNext(io.task.bits.bufIdx)).asUInt

  io.c.ready := !isRelease || !first || !full || !hasData && io.task.ready && !taskArb.io.out.valid

  io.bufResp.data := dataBuf(io.bufRead.bits.bufIdx)

  // Performance counters
  val stall = io.c.valid && isRelease && !io.c.ready
  XSPerfAccumulate(cacheParams, "sinkC_c_stall", stall)
  XSPerfAccumulate(cacheParams, "sinkC_c_stall_for_noSpace", stall && hasData && first && full)
  XSPerfAccumulate(cacheParams, "sinkC_toReqArb_stall", io.task.valid && !io.task.ready)
  XSPerfAccumulate(cacheParams, "sinkC_buf_full", full)
}
