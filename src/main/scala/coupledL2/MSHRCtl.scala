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
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._

class MSHRSelector(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val idle = Input(Vec(mshrsAll, Bool()))
    val out = ValidIO(UInt(mshrsAll.W))
  })
  io.out.valid := ParallelOR(io.idle)
  io.out.bits := ParallelPriorityMux(io.idle.zipWithIndex.map {
    case (b, i) => (b, (1 << i).U)
  })
}

class MSHRCtl(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    /* interact with req arb */
    val fromReqArb = Input(new Bundle() {
      val status_s1 = new Bundle() {
        val sets = Vec(3, UInt(setBits.W))
        // val channel = UInt(3.W)
      }
    })
    val toReqArb = Output(new Bundle() {
      val blockA_s1 = Bool()
      val blockB_s1 = Bool()
      val blockC_s1 = Bool()
    })
    /* interact with mainpipe */
    val fromMainPipe = new Bundle() {
      val mshr_alloc_s3 = Flipped(ValidIO(new MSHRRequest))
    }
    val toMainPipe = new Bundle() {
      val mshr_alloc_ptr = Output(UInt(mshrBits.W))
    }

    /* to request arbiter */
    // val mshrFull = Output(Bool())
    val mshrTask = DecoupledIO(new TaskBundle())

    /* send reqs */
    val sourceA = DecoupledIO(new TLBundleA(edgeOut.bundle))
    val sourceB = DecoupledIO(new TLBundleA(edgeIn.bundle))

    /* receive resps */
    val resps = Input(new Bundle() {
      val sinkC = new RespBundle
      val sinkD = new RespBundle
      val sinkE = new RespBundle
    })
    
    val releaseBufWriteId = Output(UInt(mshrBits.W))

    /* nested writeback */
    val nestedwb = Input(new NestedWriteback)
    val nestedwbDataId = Output(ValidIO(UInt(mshrBits.W)))
  })

  val mshrs = Seq.fill(mshrsAll) { Module(new MSHR()) }

  val mshrValids = VecInit(mshrs.map(m => m.io.status.valid))
  val mshrFull = PopCount(Cat(mshrs.map(_.io.status.valid))) >= (mshrsAll-2).U
  val a_mshrFull = PopCount(Cat(mshrs.map(_.io.status.valid))) >= (mshrsAll-3).U // the last idle mshr should not be allocated for channel A req
  val mshrSelector = Module(new MSHRSelector())
  mshrSelector.io.idle := mshrs.map(m => !m.io.status.valid)
  val selectedMSHROH = mshrSelector.io.out.bits

  mshrs.zipWithIndex.foreach {
    case (m, i) =>
      m.io.id := i.U
      m.io.alloc.valid := selectedMSHROH(i) && io.fromMainPipe.mshr_alloc_s3.valid
      m.io.alloc.bits := io.fromMainPipe.mshr_alloc_s3.bits

      m.io.resps.sink_c.valid := m.io.status.valid && io.resps.sinkC.valid && io.resps.sinkC.set === m.io.status.bits.set // ! TODO: MSHRs are blocked by slot instead of by set
      m.io.resps.sink_c.bits := io.resps.sinkC.respInfo
      m.io.resps.sink_d.valid := m.io.status.valid && io.resps.sinkD.valid && io.resps.sinkD.mshrId === i.U
      m.io.resps.sink_d.bits := io.resps.sinkD.respInfo
      m.io.resps.sink_e.valid := m.io.status.valid && io.resps.sinkE.valid && io.resps.sinkE.mshrId === i.U
      m.io.resps.sink_e.bits := io.resps.sinkE.respInfo
      
      m.io.nestedwb := io.nestedwb
  }

  io.toMainPipe.mshr_alloc_ptr := OHToUInt(selectedMSHROH)
  // io.mshrFull := mshrFull
  val setMatchVec_a = mshrs.map(m => m.io.status.valid && m.io.status.bits.set === io.fromReqArb.status_s1.sets(2))
  val setMatchVec_b = mshrs.map(m => m.io.status.valid && m.io.status.bits.set === io.fromReqArb.status_s1.sets(1))
  val setConflictVec_b = (setMatchVec_b zip mshrs.map(_.io.status.bits.nestB)).map(x => x._1 && !x._2)
  io.toReqArb.blockC_s1 := false.B
  io.toReqArb.blockB_s1 := mshrFull || Cat(setConflictVec_b).orR
  io.toReqArb.blockA_s1 := a_mshrFull || Cat(setMatchVec_a).orR

  /* Acquire downwards */
  val acquireUnit = Module(new AcquireUnit())
  fastArb(mshrs.map(_.io.tasks.source_a), acquireUnit.io.task, Some("source_a"))
  io.sourceA <> acquireUnit.io.sourceA

  /* Probe upwards */
  val sourceB = Module(new SourceB())
  fastArb(mshrs.map(_.io.tasks.source_b), sourceB.io.task, Some("source_b"))
  io.sourceB <> sourceB.io.sourceB

  /* Arbitrate MSHR task to RequestArbiter */
  fastArb(mshrs.map(_.io.tasks.mainpipe), io.mshrTask, Some("mshr_task"))

  io.releaseBufWriteId := ParallelPriorityMux(mshrs.zipWithIndex.map {
    case (mshr, i) => (mshr.io.status.valid && mshr.io.status.bits.set === io.resps.sinkC.set, i.U)
  })

  io.nestedwbDataId.valid := Cat(mshrs.map(_.io.nestedwbData)).orR
  io.nestedwbDataId.bits := ParallelPriorityMux(mshrs.zipWithIndex.map {
    case (mshr, i) => (mshr.io.nestedwbData, i.U)
  })

  dontTouch(io.sourceA)
}
