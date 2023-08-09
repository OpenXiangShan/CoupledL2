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

class grantAckQEntry(implicit p: Parameters) extends L2Bundle {
  val source = UInt(sourceIdBits.W)
  val sink = UInt(outerSinkBits.W)
}

// Communicate with L3
// Receive Grant/GrantData/ReleaseAck from d and
// Send GrantAck through e
class RefillUnit(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val sinkD = Flipped(DecoupledIO(new TLBundleD(edgeOut.bundle)))
    val sourceE = DecoupledIO(new TLBundleE(edgeOut.bundle))
    val refillBufWrite = Flipped(new MSHRBufWrite)
    val resp = Output(new RespBundle)
  })

  val (first, last, _, beat) = edgeOut.count(io.sinkD)
  val hasData = io.sinkD.bits.opcode(0)
  val isGrant = io.sinkD.bits.opcode === Grant || io.sinkD.bits.opcode === GrantData

  val grantAckQ = Module(new Queue(new grantAckQEntry, entries=mshrsAll, pipe=false, flow=false))

  grantAckQ.io.enq.valid := isGrant && io.sinkD.valid && first
  grantAckQ.io.enq.bits.source := io.sinkD.bits.source
  grantAckQ.io.enq.bits.sink := io.sinkD.bits.sink

  grantAckQ.io.deq.ready := io.sourceE.ready
  io.sourceE.bits.sink := grantAckQ.io.deq.bits.sink
  io.sourceE.valid := grantAckQ.io.deq.valid

  io.refillBufWrite.valid := io.sinkD.valid && hasData
  io.refillBufWrite.beat_sel := UIntToOH(beat)
  io.refillBufWrite.data.data := Fill(beatSize, io.sinkD.bits.data)
  io.refillBufWrite.id := io.sinkD.bits.source

  io.resp.valid := (first || last) && io.sinkD.valid
  io.resp.mshrId := io.sinkD.bits.source
  io.resp.set := 0.U(setBits.W)
  io.resp.tag := 0.U(tagBits.W)
  io.resp.respInfo.opcode := io.sinkD.bits.opcode
  io.resp.respInfo.param := io.sinkD.bits.param
  io.resp.respInfo.last := last
  io.resp.respInfo.dirty := io.sinkD.bits.echo.lift(DirtyKey).getOrElse(false.B)
  io.resp.respInfo.isHit := io.sinkD.bits.user.lift(IsHitKey).getOrElse(true.B)
  dontTouch(io.resp.respInfo.isHit)

  io.sinkD.ready := true.B

  // count refillData all zero
  // (assume beat0 and beat1 of the same block always come continuously, no intersection)
  val zero = RegInit(true.B)
  when (io.refillBufWrite.valid) {
    when (beat === beatSize.U) {
      zero := true.B // init as true
    } .otherwise {
      zero := zero & io.sinkD.bits.data === 0.U // if beat not 0.U, clear 'zero'
    }
  }
  XSPerfAccumulate(cacheParams, "sinkD_from_L3_zero", io.refillBufWrite.valid && beat === beatSize.U && zero && io.sinkD.bits.data === 0.U)
  XSPerfAccumulate(cacheParams, "sinkD_from_L3_all",  io.refillBufWrite.valid && beat === beatSize.U)
}
