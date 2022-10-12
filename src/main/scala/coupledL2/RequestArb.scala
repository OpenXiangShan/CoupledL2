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
import coupledL2.TaskInfo._
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config.Parameters

class RequestArb(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    /* receive incoming tasks */
    val sinkA = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val sinkC = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    val mshrTask = Flipped(DecoupledIO(new SourceDReq))
    val mshrTaskID = Input(UInt(log2Ceil(mshrsAll).W))

    /* read/write directory */
    val dirRead_s1 = ValidIO(new DirRead())  // To directory, read meta/tag
    val metaWrite_s1 = ValidIO(new MetaWrite())

    /* send task to mainpipe */
    val taskToPipe_s2 = ValidIO(new TaskBundle())

    /* send wdata to data storage */
    val wdataToDS_s2 = Output(new DSBlock())

    /* send mshrBuf read request */
    val mshrBufRead = Flipped(new MSHRBufRead)

    /* mshr full, from MSHRCtrl */
    val mshrFull = Input(Bool())
  })

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  // val valids = RegInit(0.U(8.W))  // 7 stages

  /* ======== Stage 0 ======== */
  io.mshrTask.ready := true.B  // TODO: when to block mshrTask?
  val mshr_task_s0 = Wire(Valid(new TaskBundle()))
  mshr_task_s0 := DontCare
  mshr_task_s0.valid := io.mshrTask.valid
  mshr_task_s0.bits.set := io.mshrTask.bits.set
  mshr_task_s0.bits.tag := io.mshrTask.bits.tag
  mshr_task_s0.bits.off := io.mshrTask.bits.off
  mshr_task_s0.bits.sourceId := io.mshrTask.bits.source
  mshr_task_s0.bits.opcode := io.mshrTask.bits.opcode
  mshr_task_s0.bits.param := io.mshrTask.bits.param
  mshr_task_s0.bits.channel := 0.U
  mshr_task_s0.bits.alias := 0.U  // TODO: handle anti-alias
  mshr_task_s0.bits.mshrOpType := OP_REFILL.U
  mshr_task_s0.bits.mshrId := io.mshrTaskID

  /* ======== Stage 1 ======== */
  /* Task generation and pipelining */
  val l1_task_s1 = Wire(Valid(new TaskBundle()))
  l1_task_s1 := DontCare
  l1_task_s1.valid := (io.sinkC.valid || io.sinkA.valid) && resetFinish && !io.mshrFull
  val (l1_tag_s1, l1_set_s1, l1_offs_s1) = parseAddress(Mux(io.sinkC.valid, io.sinkC.bits.address, io.sinkA.bits.address))
  l1_task_s1.bits.set := l1_set_s1
  l1_task_s1.bits.tag := l1_tag_s1
  l1_task_s1.bits.off := l1_offs_s1
  l1_task_s1.bits.sourceId := Mux(io.sinkC.valid, io.sinkC.bits.source, io.sinkA.bits.source)
  l1_task_s1.bits.opcode := Mux(io.sinkC.valid, io.sinkC.bits.opcode, io.sinkA.bits.opcode)
  l1_task_s1.bits.param := Mux(io.sinkC.valid, io.sinkC.bits.param, io.sinkA.bits.param)
  l1_task_s1.bits.channel := Mux(io.sinkC.valid, "b100".U, "b001".U)
  l1_task_s1.bits.alias := 0.U  // TODO: handle anti-alias
  l1_task_s1.bits.mshrId := 0.U  // TODO: handle MSHR request

  val mshr_task_s1 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  when(mshr_task_s0.valid) {
    mshr_task_s1 := mshr_task_s0
  }.otherwise {
    mshr_task_s1.valid := false.B
  }

  val task_s1 = Mux(mshr_task_s1.valid, mshr_task_s1, l1_task_s1)
  val releaseData = io.sinkC.bits.data

  /* Meta read request */
  io.dirRead_s1.valid := task_s1.valid
  io.dirRead_s1.bits.set := task_s1.bits.set
  io.dirRead_s1.bits.tag := task_s1.bits.tag
  io.dirRead_s1.bits.source := task_s1.bits.sourceId
  io.dirRead_s1.bits.replacerInfo.opcode := task_s1.bits.opcode
  io.dirRead_s1.bits.replacerInfo.channel := task_s1.bits.channel
  io.dirRead_s1.bits.idOH := 0.U  // TODO: use idOH to identity whether it is a fresh request or mshr request

  /* Meta write request */
  val metaInit = Wire(new MetaEntry())
  val metaWrite = Wire(new MetaEntry())
  metaInit := DontCare
  metaInit.state := MetaData.INVALID
  metaWrite := DontCare  // TODO: consider normal metaWrite
  // Manual initialize meta before reading
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  io.metaWrite_s1.valid := !resetFinish  // TODO: consider normal metaWrite
  io.metaWrite_s1.bits.set := resetIdx
  io.metaWrite_s1.bits.wayOH := Fill(cacheParams.ways, true.B)
  io.metaWrite_s1.bits.wmeta := Mux(resetFinish, metaInit, metaWrite)

  /* ========  Stage 2 ======== */
  val task_s2 = RegInit(0.U.asTypeOf(task_s1))
  task_s2.valid := task_s1.valid
  when(task_s1.valid) { task_s2.bits := task_s1.bits }
  
  io.taskToPipe_s2 := task_s2
  io.wdataToDS_s2.data := Cat(RegNext(releaseData), releaseData) // TODO: the first beat is higher bits?
  // TODO: we need to assert L1 sends two beats continuously
  // TODO: we do not need `when(io.sinkC.valid)` for wdata. Valid is asserted by wen signal in mainpipe

  val mbRead_valid_m2 = task_s2.valid && task_s2.bits.mshrOpType === OP_REFILL.U
  val mbRead_valid_m3 = RegNext(mbRead_valid_m2, false.B)
  val mbRead_id_m2 = task_s2.bits.mshrId
  val mbRead_id_m3 = RegEnable(mbRead_id_m2, mbRead_valid_m2)
  io.mshrBufRead.valid := mbRead_valid_m2 || mbRead_valid_m3
  io.mshrBufRead.id := Mux(mbRead_valid_m2, mbRead_id_m2, mbRead_id_m3)
  io.mshrBufRead.beat := Mux(mbRead_valid_m2, 0.U, 1.U)  // TODO: remove hardcode here
  require(beatSize == 2)

    /* Channel interaction */
  io.sinkA.ready := !io.mshrFull && resetFinish && !io.sinkC.valid && !mshr_task_s1.valid // SinkC prior to SinkA
  io.sinkC.ready := !io.mshrFull && resetFinish && !mshr_task_s1.valid

  dontTouch(io)
}
