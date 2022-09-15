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
import coupledL2.utils._
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config.Parameters

class RequestArb(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val sinkA = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val sinkC = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))

    val dirRead_s1 = ValidIO(new DirRead())  // To directory, read meta/tag
    val metaWrite_s1 = ValidIO(new MetaWrite())

    val taskToPipe_s2 = ValidIO(new TaskBundle())

    val mshrFull = Input(Bool())
  })

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  val valids = RegInit(0.U(8.W))  // 7 stages

  // Channel interaction
  io.sinkA.ready := !io.mshrFull && resetFinish && !io.sinkC.valid  // SinkC prior to SinkA
  io.sinkC.ready := !io.mshrFull && resetFinish

  // Task generation and pipelining
  val task_s1 = Wire(Valid(new TaskBundle()))
  task_s1 := DontCare
  task_s1.valid := (io.sinkC.valid || io.sinkA.valid) && resetFinish && !io.mshrFull
  task_s1.bits.addr := Mux(io.sinkC.valid, io.sinkC.bits.address, io.sinkA.bits.address)
  task_s1.bits.sourceId := Mux(io.sinkC.valid, io.sinkC.bits.source, io.sinkA.bits.source)
  task_s1.bits.opcode := Mux(io.sinkC.valid, io.sinkC.bits.opcode, io.sinkA.bits.opcode)
  task_s1.bits.param := Mux(io.sinkC.valid, io.sinkC.bits.param, io.sinkA.bits.param)
  task_s1.bits.channel := Mux(io.sinkC.valid, "b100".U, "b001".U)
  task_s1.bits.mshrId := 0.U  // TODO: handle MSHR request
  task_s1.bits.alias := 0.U  // TODO: handle anti-alias
  val data_s1 = io.sinkC.bits.data
  val (tag_s1, set_s1, offset_s1) = parseAddress(task_s1.bits.addr)

  val task_s2 = RegEnable(task_s1, task_s1.valid)
  task_s2.valid := task_s1.valid
  io.taskToPipe_s2 := task_s2

  // Manual initialize meta before reading
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  val metaInit = Wire(new MetaEntry())
  val metaWrite = Wire(new MetaEntry())
  metaInit := DontCare
  metaInit.state := MetaData.INVALID
  metaWrite := DontCare  // TODO: consider normal metaWrite

  // Meta read request
  io.dirRead_s1.valid := task_s1.valid
  io.dirRead_s1.bits.set := set_s1
  io.dirRead_s1.bits.tag := tag_s1
  io.dirRead_s1.bits.source := task_s1.bits.sourceId
  io.dirRead_s1.bits.replacerInfo.opcode := task_s1.bits.opcode
  io.dirRead_s1.bits.replacerInfo.channel := task_s1.bits.channel
  io.dirRead_s1.bits.idOH := 0.U  // TODO: use idOH to identity whether it is a fresh request or mshr request

  // Meta write request
  io.metaWrite_s1.valid := !resetFinish  // TODO: consider normal metaWrite
  io.metaWrite_s1.bits.set := resetIdx
  io.metaWrite_s1.bits.wayOH := Fill(cacheParams.ways, true.B)
  io.metaWrite_s1.bits.wmeta := Mux(resetFinish, metaInit, metaWrite)

  dontTouch(io)
}
