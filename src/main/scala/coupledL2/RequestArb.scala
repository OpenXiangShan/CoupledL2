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
    val sinkB = Flipped(DecoupledIO(new TLBundleB(edgeOut.bundle)))
    val sinkC = Flipped(DecoupledIO(new TaskBundle)) // sinkC is TaskBundle
    val mshrTask = Flipped(DecoupledIO(new TaskBundle))

    /* read/write directory */
    val dirRead_s1 = DecoupledIO(new DirRead())  // To directory, read meta/tag
    // val metaWrite_s1 = ValidIO(new MetaWrite())

    /* send task to mainpipe */
    val taskToPipe_s2 = ValidIO(new TaskBundle())

    /* send mshrBuf read request */
    val refillBufRead_s2 = Flipped(new MSHRBufRead)
    val releaseBufRead_s2 = Flipped(new MSHRBufRead)

    /* mshr full, from MSHRCtrl */
    // val mshrFull = Input(Bool())

    /* status of s1*/
    val status_s1 = Output(new Bundle() {
      val sets = Vec(3, UInt(setBits.W)) // set of sinkC, sinkB, sinkA req in s1
      // val channel = UInt(3.W)
    })

    /* handle set conflict and nestB */
    val fromMSHRCtl = Input(new Bundle() {
      val blockA_s1 = Bool()
      val blockB_s1 = Bool()
      val blockC_s1 = Bool()
    })
    val fromMainPipe = Input(new Bundle() {
      val blockA_s1 = Bool()
      val blockB_s1 = Bool()
      val blockC_s1 = Bool()
    })
  })

  /* ======== Reset ======== */
  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  /* block reqs when reset */
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }
  // val valids = RegInit(0.U(8.W))  // 7 stages

  /* ======== Stage 0 ======== */
  io.mshrTask.ready := true.B  // TODO: when to block mshrTask?
  val mshr_task_s0 = Wire(Valid(new TaskBundle()))
  mshr_task_s0.valid := io.mshrTask.valid
  mshr_task_s0.bits := io.mshrTask.bits

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

  /* latch mshr_task from s0 to s1 */
  val mshr_task_s1 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  mshr_task_s1.valid := mshr_task_s0.valid
  when(mshr_task_s0.valid) {
    mshr_task_s1.bits := mshr_task_s0.bits
  }

  /* Channel interaction from s1 */
  val A_task = fromTLAtoTaskBundle(io.sinkA.bits)
  val B_task = fromTLBtoTaskBundle(io.sinkB.bits)
  val C_task = io.sinkC.bits
  val sinkValids = VecInit(Seq(
    io.sinkC.valid && !io.fromMSHRCtl.blockC_s1 && !io.fromMainPipe.blockC_s1,
    io.sinkB.valid && !io.fromMSHRCtl.blockB_s1 && !io.fromMainPipe.blockB_s1,
    io.sinkA.valid && !io.fromMSHRCtl.blockA_s1 && !io.fromMainPipe.blockA_s1
  )).asUInt
  val chnl_task_s1 = Wire(Valid(new TaskBundle()))
  chnl_task_s1.valid := io.dirRead_s1.ready && sinkValids.orR && resetFinish
  chnl_task_s1.bits := ParallelPriorityMux(sinkValids, Seq(C_task, B_task, A_task))

  io.status_s1.sets := VecInit(Seq(C_task.set, B_task.set, A_task.set))

  io.sinkA.ready := !io.fromMSHRCtl.blockA_s1 && !io.fromMainPipe.blockA_s1 && io.dirRead_s1.ready && resetFinish && !io.sinkB.valid && !io.sinkC.valid && !mshr_task_s1.valid // SinkC prior to SinkA & SinkB
  io.sinkB.ready := !io.fromMSHRCtl.blockB_s1 && !io.fromMainPipe.blockB_s1 && io.dirRead_s1.ready && resetFinish && !io.sinkC.valid && !mshr_task_s1.valid
  io.sinkC.ready := !io.fromMSHRCtl.blockC_s1 && !io.fromMainPipe.blockC_s1 && io.dirRead_s1.ready && resetFinish && !mshr_task_s1.valid

  val task_s1 = Mux(mshr_task_s1.valid, mshr_task_s1, chnl_task_s1)

  /* Meta read request */
  // ^ only sinkA/B/C tasks need to read directory
  io.dirRead_s1.valid := chnl_task_s1.valid && !mshr_task_s1.valid
  io.dirRead_s1.bits.set := task_s1.bits.set
  io.dirRead_s1.bits.tag := task_s1.bits.tag
  io.dirRead_s1.bits.source := task_s1.bits.sourceId
  io.dirRead_s1.bits.replacerInfo.opcode := task_s1.bits.opcode
  io.dirRead_s1.bits.replacerInfo.channel := task_s1.bits.channel

  /* ========  Stage 2 ======== */
  val task_s2 = RegInit(0.U.asTypeOf(task_s1))
  task_s2.valid := task_s1.valid
  when(task_s1.valid) { task_s2.bits := task_s1.bits }
  
  io.taskToPipe_s2 := task_s2

  val mshrTask_s2 = task_s2.valid && task_s2.bits.mshrTask
  // For GrantData, read refillBuffer
  io.refillBufRead_s2.valid := mshrTask_s2 && task_s2.bits.fromA && task_s2.bits.opcode === GrantData
  io.refillBufRead_s2.id := task_s2.bits.mshrId
  // For ReleaseData or ProbeAckData, read releaseBuffer
  // channel is used to differentiate GrantData and ProbeAckData
  io.releaseBufRead_s2.valid := mshrTask_s2 && (task_s2.bits.opcode === ReleaseData ||
    task_s2.bits.fromB && task_s2.bits.opcode === ProbeAckData)
  io.releaseBufRead_s2.id := task_s2.bits.mshrId
  assert(!io.refillBufRead_s2.valid || io.refillBufRead_s2.ready)
  assert(!io.releaseBufRead_s2.valid || io.releaseBufRead_s2.ready)
  require(beatSize == 2)

  dontTouch(io)
}
