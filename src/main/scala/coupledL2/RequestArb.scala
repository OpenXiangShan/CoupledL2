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
import utility._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2tl._
import coupledL2.tl2chi._

class RequestArb(implicit p: Parameters) extends L2Module
  with HasCHIOpcodes {

  val io = IO(new Bundle() {
    /* receive incoming tasks */
    val sinkA    = Flipped(DecoupledIO(new TaskBundle))
    val ATag     = Input(UInt(tagBits.W)) // !TODO: very dirty, consider optimize structure
    val ASet     = Input(UInt(setBits.W)) // To pass A entrance status to MP for blockA-info of ReqBuf
    val s1Entrance = ValidIO(new L2Bundle {
      val set = UInt(setBits.W)
    })

    val sinkB    = Flipped(DecoupledIO(new TaskBundle))
    val sinkC    = Flipped(DecoupledIO(new TaskBundle))
    val mshrTask = Flipped(DecoupledIO(new TaskBundle))

    /* read/write directory */
    val dirRead_s1 = DecoupledIO(new DirRead())  // To directory, read meta/tag

    /* send task to mainpipe */
    val taskToPipe_s2 = ValidIO(new TaskBundle())
    /* send s1 task info to mainpipe to help hint */
    val taskInfo_s1 = ValidIO(new TaskBundle())

    /* send mshrBuf read request */
    val refillBufRead_s2 = ValidIO(new MSHRBufRead)
    val releaseBufRead_s2 = ValidIO(new MSHRBufRead)

    /* status of each pipeline stage */
    val status_s1 = Output(new PipeEntranceStatus) // set & tag of entrance status
    val status_vec = Vec(2, ValidIO(new PipeStatus))
    val status_vec_toTX = if (enableCHI) Some(Vec(2, ValidIO(new PipeStatusWithCHI))) else None

    /* handle set conflict, capacity conflict */
    val fromMSHRCtl = Input(new BlockInfo())
    val fromMainPipe = Input(new BlockInfo())
    val fromGrantBuffer = Input(new Bundle() {
      val blockSinkReqEntrance = new BlockInfo()
      val blockMSHRReqEntrance = Bool()
    })
    val fromSourceC = if (!enableCHI) Some(Input(new SourceCBlockBundle)) else None
    val fromTXDAT = if (enableCHI) Some(Input(new TXDATBlockBundle)) else None
    val fromTXRSP = if (enableCHI) Some(Input(new TXRSPBlockBundle)) else None
    val fromTXREQ = if (enableCHI) Some(Input(new TXBlockBundle)) else None

    /* MSHR Status */
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo())))
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

  val s0_fire   = Wire(Bool())
  val s1_fire   = Wire(Bool())
  val s1_cango  = Wire(Bool())
  val s2_ready  = Wire(Bool())
  val mshr_task_s1 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))

  val s1_needs_replRead = mshr_task_s1.valid && mshr_task_s1.bits.fromA && mshr_task_s1.bits.replTask && (
    mshr_task_s1.bits.opcode(2, 1) === Grant(2, 1) ||
    mshr_task_s1.bits.opcode === AccessAckData ||
    mshr_task_s1.bits.opcode === HintAck && mshr_task_s1.bits.dsWen
  )

  /* ======== Stage 0 ======== */
  // if mshr_task_s1 is replRead, it might stall and wait for dirRead.ready, so we block new mshrTask from entering
  // TODO: will cause msTask path vacant for one-cycle after replRead, since not use Flow so as to avoid ready propagation
  io.mshrTask.ready := !io.fromGrantBuffer.blockMSHRReqEntrance && !s1_needs_replRead && !(mshr_task_s1.valid && !s2_ready) &&
    (if (io.fromSourceC.isDefined) !io.fromSourceC.get.blockMSHRReqEntrance else true.B) &&
    (if (io.fromTXDAT.isDefined) !io.fromTXDAT.get.blockMSHRReqEntrance else true.B) &&
    (if (io.fromTXRSP.isDefined) !io.fromTXRSP.get.blockMSHRReqEntrance else true.B) &&
    (if (io.fromTXREQ.isDefined) !io.fromTXREQ.get.blockMSHRReqEntrance else true.B)
  
    s0_fire := io.mshrTask.valid && io.mshrTask.ready

  /* ======== Stage 1 ======== */
  /* latch mshr_task from s0 to s1 */
  val mshr_replRead_stall = mshr_task_s1.valid && s1_needs_replRead && (!io.dirRead_s1.ready || io.fromMainPipe.blockG_s1)
  mshr_task_s1.valid := mshr_task_s1.valid && !s1_fire || s0_fire

  when (s0_fire) {
    mshr_task_s1.bits := io.mshrTask.bits
  }


  /* Channel interaction from s1 */
  val A_task = io.sinkA.bits
  val B_task = io.sinkB.bits
  val C_task = io.sinkC.bits
  val block_A = io.fromMSHRCtl.blockA_s1 || io.fromMainPipe.blockA_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockA_s1
  val block_B = io.fromMSHRCtl.blockB_s1 || io.fromMainPipe.blockB_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1 ||
    (if (io.fromSourceC.isDefined) io.fromSourceC.get.blockSinkBReqEntrance else false.B) ||
    (if (io.fromTXDAT.isDefined) io.fromTXDAT.get.blockSinkBReqEntrance else false.B) ||
    (if (io.fromTXRSP.isDefined) io.fromTXRSP.get.blockSinkBReqEntrance else false.B)
  val block_C = io.fromMSHRCtl.blockC_s1 || io.fromMainPipe.blockC_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockC_s1

//  val noFreeWay = Wire(Bool())

  val sinkValids = VecInit(Seq(
    io.sinkC.valid && !block_C,
    io.sinkB.valid && !block_B,
    io.sinkA.valid && !block_A
  )).asUInt

  // TODO: A Hint is allowed to enter if !s2_ready for mcp2_stall

  val sink_ready_basic = io.dirRead_s1.ready && resetFinish && !mshr_task_s1.valid && s2_ready

  io.sinkA.ready := sink_ready_basic && !block_A && !sinkValids(1) && !sinkValids(0) // SinkC prior to SinkA & SinkB
  io.sinkB.ready := sink_ready_basic && !block_B && !sinkValids(0) // SinkB prior to SinkA
  io.sinkC.ready := sink_ready_basic && !block_C

  val chnl_task_s1 = Wire(Valid(new TaskBundle()))
  chnl_task_s1.valid := io.dirRead_s1.ready && sinkValids.orR && resetFinish
  chnl_task_s1.bits := ParallelPriorityMux(sinkValids, Seq(C_task, B_task, A_task))

  // mshr_task_s1 is s1_[reg]
  // task_s1 is [wire] to s2_reg
  val task_s1 = Mux(mshr_task_s1.valid, mshr_task_s1, chnl_task_s1)
  val s1_to_s2_valid = task_s1.valid && !mshr_replRead_stall

  s1_cango  := task_s1.valid && !mshr_replRead_stall
  s1_fire   := s1_cango && s2_ready

  io.taskInfo_s1.valid := s1_fire
  io.taskInfo_s1.bits := task_s1.bits

  /* Meta read request */
  // ^ only sinkA/B/C tasks need to read directory
  io.dirRead_s1.valid := s2_ready && (chnl_task_s1.valid && !mshr_task_s1.valid || s1_needs_replRead && !io.fromMainPipe.blockG_s1)
  io.dirRead_s1.bits.set := task_s1.bits.set
  io.dirRead_s1.bits.tag := task_s1.bits.tag
  // invalid way which causes mshr_retry
  // TODO: random waymask can be used to avoid multi-way conflict
  io.dirRead_s1.bits.wayMask := Mux(mshr_task_s1.valid && mshr_task_s1.bits.mshrRetry, (~(1.U(cacheParams.ways.W) << mshr_task_s1.bits.way)), Fill(cacheParams.ways, "b1".U))
  io.dirRead_s1.bits.replacerInfo.opcode := task_s1.bits.opcode
  io.dirRead_s1.bits.replacerInfo.channel := task_s1.bits.channel
  io.dirRead_s1.bits.replacerInfo.reqSource := task_s1.bits.reqSource
  io.dirRead_s1.bits.replacerInfo.refill_prefetch := s1_needs_replRead && (mshr_task_s1.bits.opcode === HintAck && mshr_task_s1.bits.dsWen)
  io.dirRead_s1.bits.refill := s1_needs_replRead
  io.dirRead_s1.bits.mshrId := task_s1.bits.mshrId

  // block same-set A req
  io.s1Entrance.valid := mshr_task_s1.valid && s2_ready && mshr_task_s1.bits.metaWen || io.sinkC.fire || io.sinkB.fire
  io.s1Entrance.bits.set  := Mux(
    mshr_task_s1.valid && mshr_task_s1.bits.metaWen,
    mshr_task_s1.bits.set,
    Mux(io.sinkC.fire, C_task.set, B_task.set)
  )

  /* ========  Stage 2 ======== */
  val s1_AHint_fire = io.sinkA.fire && io.sinkA.bits.opcode === Hint
  // any req except AHint might access DS, and continuous DS accesses are prohibited
  val ds_mcp2_stall = RegNext(s1_fire && !s1_AHint_fire)

  s2_ready  := !ds_mcp2_stall

  val task_s2 = RegInit(0.U.asTypeOf(task_s1))
  task_s2.valid := s1_fire
  when(s1_fire) { task_s2.bits := task_s1.bits }

/*  val sameSet_s2 = task_s2.valid && task_s2.bits.fromA && !task_s2.bits.mshrTask && task_s2.bits.set === A_task.set
  val sameSet_s3 = RegNext(task_s2.valid && task_s2.bits.fromA && !task_s2.bits.mshrTask) &&
    RegEnable(task_s2.bits.set, task_s2.valid) === A_task.set
  val sameSetCnt = PopCount(VecInit(io.msInfo.map(s => s.valid && s.bits.set === A_task.set && s.bits.fromA) :+
    sameSet_s2 :+ sameSet_s3).asUInt)
  noFreeWay := sameSetCnt >= cacheParams.ways.U
 */
  io.taskToPipe_s2 := task_s2

  // MSHR task
  val mshrTask_s2 = task_s2.valid && task_s2.bits.mshrTask
  val mshrTask_s2_a_upwards = task_s2.bits.fromA &&
    (task_s2.bits.opcode === GrantData || task_s2.bits.opcode === Grant && task_s2.bits.dsWen ||
      task_s2.bits.opcode === AccessAckData || task_s2.bits.opcode === HintAck && task_s2.bits.dsWen)
  // For GrantData, read refillBuffer
  // Caution: GrantData-alias may read DataStorage or ReleaseBuf instead
  // Release-replTask also read refillBuf and then write to DS
  val releaseRefillData = task_s2.bits.replTask && (if (enableCHI) {
    task_s2.bits.toTXREQ && (
      task_s2.bits.chiOpcode.get === WriteBackFull ||
      task_s2.bits.chiOpcode.get === Evict
    )
  } else {
    task_s2.bits.opcode(2, 1) === Release(2, 1)
  })
  io.refillBufRead_s2.valid := mshrTask_s2 && (
    task_s2.bits.fromB && task_s2.bits.opcode(2, 1) === ProbeAck(2, 1) && task_s2.bits.replTask || // ???
    releaseRefillData ||
    mshrTask_s2_a_upwards && !task_s2.bits.useProbeData)
  io.refillBufRead_s2.bits.id := task_s2.bits.mshrId

  // ReleaseData and ProbeAckData read releaseBuffer
  // channel is used to differentiate GrantData and ProbeAckData
  val snoopNeedData = if (enableCHI) {
    task_s2.bits.fromB && task_s2.bits.toTXDAT && isSnpRespDataX(task_s2.bits.chiOpcode.get)
  } else {
    task_s2.bits.fromB && task_s2.bits.opcode === ProbeAckData
  }
  val releaseNeedData = if (enableCHI) {
    task_s2.bits.toTXDAT && task_s2.bits.chiOpcode.get === CopyBackWrData
  } else task_s2.bits.opcode === ReleaseData
  val dctNeedData = if (enableCHI) {
    task_s2.bits.toTXDAT && task_s2.bits.chiOpcode.get === CompData
  } else false.B
  val snpHitReleaseNeedData = if (enableCHI) {
    !mshrTask_s2 && task_s2.bits.fromB && task_s2.bits.snpHitReleaseWithData
  } else false.B
  io.releaseBufRead_s2.valid := Mux(
    mshrTask_s2,
    releaseNeedData ||
      snoopNeedData ||
      dctNeedData ||
      mshrTask_s2_a_upwards && task_s2.bits.useProbeData,
    task_s2.valid && snpHitReleaseNeedData
  )
  io.releaseBufRead_s2.bits.id := Mux(
    task_s2.bits.snpHitRelease,
    task_s2.bits.snpHitReleaseIdx,
    task_s2.bits.mshrId
  )

  require(beatSize == 2)

  /* status of each pipeline stage */
  io.status_s1.sets := VecInit(Seq(C_task.set, B_task.set, io.ASet, mshr_task_s1.bits.set))
  io.status_s1.tags := VecInit(Seq(C_task.tag, B_task.tag, io.ATag, mshr_task_s1.bits.tag))
 // io.status_s1.isKeyword := VecInit(Seq(C_task.isKeyword, B_task.isKeyword, io.isKeyword, mshr_task_s1.bits.isKeyword))

  require(io.status_vec.size == 2)
  io.status_vec.zip(Seq(task_s1, task_s2)).foreach {
    case (status, task) =>
      status.valid := task.valid
      status.bits.channel := task.bits.channel
  }

  if (enableCHI) {
    require(io.status_vec_toTX.get.size == 2)
    io.status_vec_toTX.get.zip(Seq(task_s1, task_s2)).foreach {
      case (status, task) =>
        status.valid := task.valid
        status.bits.channel := task.bits.channel
        status.bits.txChannel := task.bits.txChannel
        status.bits.mshrTask := task.bits.mshrTask
    }
  }

  dontTouch(io)

  // Performance counters
  XSPerfAccumulate("mshr_req", s0_fire)
  XSPerfAccumulate("mshr_req_stall", io.mshrTask.valid && !io.mshrTask.ready)

  XSPerfAccumulate("sinkA_req", io.sinkA.fire)
  XSPerfAccumulate("sinkB_req", io.sinkB.fire)
  XSPerfAccumulate("sinkC_req", io.sinkC.fire)

  XSPerfAccumulate("sinkA_stall", io.sinkA.valid && !io.sinkA.ready)
  XSPerfAccumulate("sinkB_stall", io.sinkB.valid && !io.sinkB.ready)
  XSPerfAccumulate("sinkC_stall", io.sinkC.valid && !io.sinkC.ready)

  XSPerfAccumulate("sinkA_stall_by_mshr", io.sinkA.valid && io.fromMSHRCtl.blockA_s1)
  XSPerfAccumulate("sinkB_stall_by_mshr", io.sinkB.valid && io.fromMSHRCtl.blockB_s1)

  XSPerfAccumulate("sinkA_stall_by_mainpipe", io.sinkA.valid && io.fromMainPipe.blockA_s1)
  XSPerfAccumulate("sinkB_stall_by_mainpipe", io.sinkB.valid && io.fromMainPipe.blockB_s1)
  XSPerfAccumulate("sinkC_stall_by_mainpipe", io.sinkC.valid && io.fromMainPipe.blockC_s1)

  XSPerfAccumulate("sinkA_stall_by_grantbuf", io.sinkA.valid && io.fromGrantBuffer.blockSinkReqEntrance.blockA_s1)
  XSPerfAccumulate("sinkB_stall_by_grantbuf", io.sinkB.valid && io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1)
  XSPerfAccumulate("sinkC_stall_by_grantbuf", io.sinkC.valid && io.fromGrantBuffer.blockSinkReqEntrance.blockC_s1)

  XSPerfAccumulate("sinkA_stall_by_dir", io.sinkA.valid && !block_A && !io.dirRead_s1.ready)
  XSPerfAccumulate("sinkB_stall_by_dir", io.sinkB.valid && !block_B && !io.dirRead_s1.ready)
  XSPerfAccumulate("sinkC_stall_by_dir", io.sinkC.valid && !block_C && !io.dirRead_s1.ready)

  XSPerfAccumulate("sinkA_stall_by_sinkB", io.sinkA.valid && sink_ready_basic && !block_A && sinkValids(1) && !sinkValids(0))
  XSPerfAccumulate("sinkA_stall_by_sinkC", io.sinkA.valid && sink_ready_basic && !block_A && sinkValids(0))
  XSPerfAccumulate("sinkB_stall_by_sinkC", io.sinkB.valid && sink_ready_basic && !block_B && sinkValids(0))

  XSPerfAccumulate("sinkA_stall_by_mshrTask", io.sinkA.valid && mshr_task_s1.valid)
  XSPerfAccumulate("sinkB_stall_by_mshrTask", io.sinkB.valid && mshr_task_s1.valid)
  XSPerfAccumulate("sinkC_stall_by_mshrTask", io.sinkC.valid && mshr_task_s1.valid)
}
