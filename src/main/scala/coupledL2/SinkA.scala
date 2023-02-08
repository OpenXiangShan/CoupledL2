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

package coupledL2

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._

class SinkA(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val toReqArb = DecoupledIO(new TaskBundle)
    val pbRead = Flipped(DecoupledIO(new PutBufferRead))
    val pbResp = ValidIO(new PutBufferEntry)
  })
  val putBuffer = Reg(Vec(mshrsAll, Vec(beatSize, new PutBufferEntry)))
  val beatValids = RegInit(VecInit(Seq.fill(mshrsAll)(VecInit(Seq.fill(beatSize)(false.B)))))
  val valids = VecInit(beatValids.map(_.asUInt.orR())).asUInt
  
  val (first, last, done, count) = edgeIn.count(io.a)
  val hasData = edgeIn.hasData(io.a.bits)
  val full = valids.andR()
  val noSpace = full && hasData
  val insertIdx = PriorityEncoder(~valids)
  val insertIdxReg = RegEnable(insertIdx, io.a.fire() && first)

  when (io.a.fire() && hasData) {
    when (first) {
      putBuffer(insertIdx)(count).data.data := io.a.bits.data
      putBuffer(insertIdx)(count).mask := io.a.bits.mask
      beatValids(insertIdx)(count) := true.B
    }.otherwise {
      putBuffer(insertIdxReg)(count).data.data := io.a.bits.data
      putBuffer(insertIdxReg)(count).mask := io.a.bits.mask
      beatValids(insertIdxReg)(count) := true.B
    }
  }

  // val rIdx = io.pbRead.bits.idx
  // val res = putBuffer(rIdx)
  when (io.pbRead.fire()) {
    beatValids(io.pbRead.bits.idx)(io.pbRead.bits.count) := false.B
  }

  io.a.ready := !first || io.toReqArb.ready && !noSpace

  def fromTLAtoTaskBundle(a: TLBundleA): TaskBundle = {
    val task = Wire(new TaskBundle)
    task := DontCare
    task.channel := "b001".U
    task.tag := parseAddress(a.address)._1
    task.set := parseAddress(a.address)._2
    task.off := parseAddress(a.address)._3
    task.alias := a.user.lift(AliasKey).getOrElse(0.U)
    task.opcode := a.opcode
    task.param := a.param
    task.size := a.size
    task.sourceId := a.source
    task.mshrTask := false.B
    task.pbIdx := insertIdx
    task
  }
  io.toReqArb.valid := io.a.valid && first && !noSpace
  io.toReqArb.bits := fromTLAtoTaskBundle(io.a.bits)

  io.pbRead.ready := beatValids(io.pbRead.bits.idx)(io.pbRead.bits.count)
  assert(!io.pbRead.valid || io.pbRead.ready)

  io.pbResp.valid := RegNext(io.pbRead.fire())
  io.pbResp.bits := RegEnable(putBuffer(io.pbRead.bits.idx)(io.pbRead.bits.count), io.pbRead.fire())
}