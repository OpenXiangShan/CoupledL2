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
import freechips.rocketchip.tilelink.TLMessages._
import chipsalliance.rocketchip.config.Parameters

class RequestArb(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    /* receive incoming tasks */
    val sinkA = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val sinkB = Flipped(DecoupledIO(new TLBundleB(edgeIn.bundle)))
    val sinkC = Flipped(DecoupledIO(new TaskBundle))
    val mshrTask = Flipped(DecoupledIO(new SourceDReq))
    val mshrTaskID = Input(UInt(log2Ceil(mshrsAll).W))

    /* read/write directory */
    val dirRead_s1 = ValidIO(new DirRead())  // To directory, read meta/tag
    // val metaWrite_s1 = ValidIO(new MetaWrite())

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
  // mshr_task_s0.bits.mshrOpType := OP_REFILL.U
  mshr_task_s0.bits.mshrTask := true.B
  mshr_task_s0.bits.mshrId := io.mshrTaskID

  /* ======== Stage 1 ======== */
  /* Task generation and pipelining */
  def fromTLAtoTaskBundle(a: TLBundleA): TaskBundle = {
    val task = Wire(new TaskBundle)
    task := DontCare
    task.channel := "b001".U
    task.tag := parseAddress(a.address)._1
    task.set := parseAddress(a.address)._2
    task.off := parseAddress(a.address)._3
    task.alias := 0.U // TODO
    task.opcode := a.opcode
    task.param := a.param
    task.sourceId := a.source
    task.mshrTask := false.B
    task
  }

  def fromTLBtoTaskBundle(b: TLBundleB): TaskBundle = {
    val task = Wire(new TaskBundle)
    task := DontCare
    task.channel := "b010".U
    task.tag := parseAddress(b.address)._1
    task.set := parseAddress(b.address)._2
    task.off := parseAddress(b.address)._3
    task.alias := 0.U // TODO
    task.opcode := b.opcode
    task.param := b.param
    task.mshrTask := false.B
    task
  }

  val sinkA = fromTLAtoTaskBundle(io.sinkA.bits)
  val sinkB = fromTLBtoTaskBundle(io.sinkB.bits)
  val sinkC = io.sinkC.bits
  val sinkValids = VecInit(Seq(io.sinkC, io.sinkB, io.sinkA).map(_.valid)).asUInt
  val l1_task_s1 = Wire(Valid(new TaskBundle()))
  l1_task_s1.valid := sinkValids.orR && resetFinish && !io.mshrFull
  l1_task_s1.bits := Mux1H(sinkValids, Seq(sinkC, sinkB, sinkA))

  val mshr_task_s1 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  when(mshr_task_s0.valid) {
    mshr_task_s1 := mshr_task_s0
  }.otherwise {
    mshr_task_s1.valid := false.B
  }

  val task_s1 = Mux(mshr_task_s1.valid, mshr_task_s1, l1_task_s1)

  /* Meta read request */
  io.dirRead_s1.valid := l1_task_s1.valid
  io.dirRead_s1.bits.set := task_s1.bits.set
  io.dirRead_s1.bits.tag := task_s1.bits.tag
  io.dirRead_s1.bits.source := task_s1.bits.sourceId
  io.dirRead_s1.bits.replacerInfo.opcode := task_s1.bits.opcode
  io.dirRead_s1.bits.replacerInfo.channel := task_s1.bits.channel

  /* block reqs when reset */
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }

  /* ========  Stage 2 ======== */
  val task_s2 = RegInit(0.U.asTypeOf(task_s1))
  task_s2.valid := task_s1.valid
  when(task_s1.valid) { task_s2.bits := task_s1.bits }
  
  io.taskToPipe_s2 := task_s2
  // io.wdataToDS_s2.data := Cat(RegNext(releaseData), releaseData) // TODO: the first beat is higher bits?
  // TODO: we need to assert L1 sends two beats continuously
  // TODO: we do not need `when(io.sinkC.valid)` for wdata. Valid is asserted by wen signal in mainpipe

  val mbRead_valid_m2 = task_s2.valid && task_s2.bits.mshrTask && task_s2.bits.opcode === GrantData
  val mbRead_valid_m3 = RegNext(mbRead_valid_m2, false.B)
  val mbRead_id_m2 = task_s2.bits.mshrId
  val mbRead_id_m3 = RegEnable(mbRead_id_m2, mbRead_valid_m2)
  io.mshrBufRead.valid := mbRead_valid_m2 || mbRead_valid_m3
  io.mshrBufRead.id := Mux(mbRead_valid_m2, mbRead_id_m2, mbRead_id_m3)
  // io.mshrBufRead.beat := Mux(mbRead_valid_m2, 0.U, 1.U)  // TODO: remove hardcode here
  require(beatSize == 2)

    /* Channel interaction */
  io.sinkA.ready := !io.mshrFull && resetFinish && !io.sinkB.valid && !io.sinkC.valid && !mshr_task_s1.valid // SinkC prior to SinkA & SinkB
  io.sinkB.ready := !io.mshrFull && resetFinish && !io.sinkC.valid && !mshr_task_s1.valid
  io.sinkC.ready := !io.mshrFull && resetFinish && !mshr_task_s1.valid

  dontTouch(io)
}
