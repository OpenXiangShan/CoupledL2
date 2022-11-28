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
    val toReqArb = DecoupledIO(new TaskBundle) // Release/ReleaseData
    val resp = Output(new RespBundle)
    val releaseBufWrite = Flipped(new MSHRBufWrite)
    val bufRead = Input(ValidIO(new PipeBufferRead))
    val bufResp = Output(new PipeBufferResp)
  })
  
  val (first, last, _, beat) = edgeIn.count(io.c)
  val isRelease = io.c.bits.opcode(1)
  val hasData = io.c.bits.opcode(0)

  // io.toReqArb <> io.c
  // io.toReqArb.valid := io.c.valid && isRelease
  val buf = Reg(Vec(bufBlocks, Vec(beatSize, UInt((beatBytes * 8).W))))
  val beatValids = RegInit(VecInit(Seq.fill(bufBlocks)(VecInit(Seq.fill(beatSize)(false.B)))))
  val bufValids = VecInit(beatValids.map(_.asUInt.orR)).asUInt
  val full = bufValids.andR
  val noSpace = full && hasData
  val nextPtr = PriorityEncoder(~bufValids)
  val nextPtrReg = RegEnable(nextPtr, io.c.fire() && first && isRelease && hasData)

  def toTaskBundle(c: TLBundleC): TaskBundle = {
    val task = Wire(new TaskBundle)
    task := DontCare
    task.channel := "b100".U
    task.tag := parseAddress(c.address)._1
    task.set := parseAddress(c.address)._2
    task.off := parseAddress(c.address)._3
    task.alias := 0.U // TODO
    task.opcode := c.opcode
    task.param := c.param
    task.sourceId := c.source
    task.mshrTask := false.B
    task
  }

  when (io.c.fire() && hasData) {
    when (first) {
      buf(nextPtr)(beat) := io.c.bits.data
      beatValids(nextPtr)(beat) := true.B
    }.otherwise {
      assert(last) // there should be only 2 beats
      buf(nextPtrReg)(beat) := io.c.bits.data
      beatValids(nextPtrReg)(beat) := true.B
    }
  }

  when (io.bufRead.valid) {
    beatValids(io.bufRead.bits.bufIdx).foreach(_ := false.B)
  }

  io.toReqArb.valid := io.c.valid && isRelease && last
  io.toReqArb.bits := toTaskBundle(io.c.bits)
  io.toReqArb.bits.bufIdx := nextPtrReg

  io.resp.valid := io.c.valid && (first || last) && !isRelease
  io.resp.mshrId := 0.U // DontCare
  io.resp.tag := parseAddress(io.c.bits.address)._1
  io.resp.set := parseAddress(io.c.bits.address)._2
  io.resp.respInfo.opcode := io.c.bits.opcode
  io.resp.respInfo.param := io.c.bits.param
  io.resp.respInfo.last := last

  io.releaseBufWrite.valid := io.c.valid && io.c.bits.opcode === ProbeAckData
  io.releaseBufWrite.beat_sel := UIntToOH(beat)
  io.releaseBufWrite.data.data := io.c.bits.data
  io.releaseBufWrite.id := DontCare // id is given by MSHRCtl by comparing address to the MSHRs

  io.c.ready := !first || !noSpace && !(isRelease && !io.toReqArb.ready)

  io.bufResp.data := buf(io.bufRead.bits.bufIdx)
}