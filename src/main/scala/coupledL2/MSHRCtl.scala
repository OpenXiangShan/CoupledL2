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
    val sourceA = DecoupledIO(new TLBundleA(edgeIn.bundle))
    val fromMainPipe = new Bundle() {
      val need_acquire_s3 = Input(Bool())
      val infoA_s3 = Input(new Bundle() {
        val addr = UInt(addressBits.W)
        val opcode = UInt(3.W)
        val param = UInt(3.W)
        val source = UInt(sourceIdBits.W)
      })
      val mshr_alloc_s3 = Flipped(ValidIO(new Bundle() {
        val addr = UInt(addressBits.W)
        val way = UInt(wayBits.W)
      }))
    }
    val toMainPipe = new Bundle() {
      val mshr_alloc_ptr = Output(UInt(mshrBits.W))
    }
    val mshrFull = Output(Bool())
  })

  val mshrs = Seq.fill(mshrsAll) { Module(new MSHR()) }

  val mshrValids = VecInit(mshrs.map(m => m.io.status.valid))
  val mshrFull = PopCount(Cat(mshrs.map(_.io.status.valid))) >= (mshrsAll-2).U
  val mshrSelector = Module(new MSHRSelector())
  mshrSelector.io.idle := mshrs.map(m => !m.io.status.valid)
  val selectedMSHROH = mshrSelector.io.out.bits

  val alloc = Vec(mshrsAll, ValidIO(new MSHRRequest))
  mshrs.zipWithIndex.foreach {
    case (mshr, i) =>
      mshr.io.alloc.valid := selectedMSHROH(i) && io.fromMainPipe.mshr_alloc_s3.valid
      mshr.io.alloc.bits.addr := io.fromMainPipe.mshr_alloc_s3.bits.addr
      mshr.io.alloc.bits.way := io.fromMainPipe.mshr_alloc_s3.bits.way
  }

  io.toMainPipe.mshr_alloc_ptr := OHToUInt(selectedMSHROH)
  io.mshrFull := mshrFull

  val infoA_s3 = io.fromMainPipe.infoA_s3
  io.sourceA.valid := io.fromMainPipe.need_acquire_s3
  io.sourceA.bits.opcode := infoA_s3.opcode
  io.sourceA.bits.param := infoA_s3.param
  io.sourceA.bits.size := offsetBits.U
  io.sourceA.bits.source := infoA_s3.source
  io.sourceA.bits.address := infoA_s3.addr
  io.sourceA.bits.mask := Fill(edgeOut.manager.beatBytes, 1.U(1.W))
  io.sourceA.bits.corrupt := false.B
  io.sourceA.bits.data := DontCare

  dontTouch(io.sourceA)
}
