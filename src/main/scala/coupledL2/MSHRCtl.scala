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
    /* interact with mainpipe */
    val fromMainPipe = new Bundle() {
      val mshr_alloc_s3 = Flipped(ValidIO(new MSHRRequest))
    }
    val toMainPipe = new Bundle() {
      val mshr_alloc_ptr = Output(UInt(mshrBits.W))
    }

    /* to request arbiter */
    val mshrFull = Output(Bool())
    val mshrTask = DecoupledIO(new TaskBundle())

    /* send reqs */
    val sourceA = DecoupledIO(new TLBundleA(edgeOut.bundle))

    /* receive resps */
    val resps = Input(new Bundle() {
      val sinkC = new RespBundle
      val sinkD = new RespBundle
      val sinkE = new RespBundle
    })
    
    val releaseBufWriteId = Output(UInt(mshrBits.W))
  })

  val mshrs = Seq.fill(mshrsAll) { Module(new MSHR()) }

  val mshrValids = VecInit(mshrs.map(m => m.io.status.valid))
  val mshrFull = PopCount(Cat(mshrs.map(_.io.status.valid))) >= (mshrsAll-2).U
  val mshrSelector = Module(new MSHRSelector())
  mshrSelector.io.idle := mshrs.map(m => !m.io.status.valid)
  val selectedMSHROH = mshrSelector.io.out.bits

  mshrs.zipWithIndex.foreach {
    case (m, i) =>
      m.io.id := i.U
      m.io.alloc.valid := selectedMSHROH(i) && io.fromMainPipe.mshr_alloc_s3.valid
      m.io.alloc.bits := io.fromMainPipe.mshr_alloc_s3.bits

      m.io.resps.sink_c.valid := io.resps.sinkC.valid && io.resps.sinkC.set === m.io.status.bits.set // ! TODO: MSHRs are blocked by slot instead of by set
      m.io.resps.sink_c.bits := io.resps.sinkC.respInfo
      m.io.resps.sink_d.valid := io.resps.sinkD.valid && io.resps.sinkD.mshrId === i.U
      m.io.resps.sink_d.bits := io.resps.sinkD.respInfo
      m.io.resps.sink_e.valid := io.resps.sinkE.valid && io.resps.sinkE.mshrId === i.U
      m.io.resps.sink_e.bits := io.resps.sinkE.respInfo
      
  }

  io.toMainPipe.mshr_alloc_ptr := OHToUInt(selectedMSHROH)
  io.mshrFull := mshrFull

  /* Acquire downwards */
  val acquireUnit = Module(new AcquireUnit())
  val oaArb = Module(new FastArbiter(new SourceAReq, mshrsAll))
  mshrs.zipWithIndex.foreach{
    case (m, i) =>
      oaArb.io.in(i) <> m.io.tasks.source_a
  }
  acquireUnit.io.task <> oaArb.io.out
  io.sourceA <> acquireUnit.io.sourceA

  /* Arbitrate MSHR task to RequestArbiter */
  val mshrTaskArb = Module(new FastArbiter(chiselTypeOf(io.mshrTask.bits), mshrsAll))
  mshrs.zipWithIndex.foreach{
    case (m, i) =>
      mshrTaskArb.io.in(i) <> m.io.tasks.mainpipe
  }
  io.mshrTask <> mshrTaskArb.io.out

  io.releaseBufWriteId := ParallelPriorityMux(mshrs.zipWithIndex.map {
    case (mshr, i) => (mshr.io.status.valid && mshr.io.status.bits.set === io.resps.sinkC.set, i.U)
  })

  dontTouch(io.sourceA)
}
