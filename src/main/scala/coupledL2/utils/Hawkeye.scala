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

// See LICENSE.SiFive for license details.

package coupledL2.utils

import utility.SRAMTemplate
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.HasCoupledL2Parameters

trait HasHawkeyeParams extends HasCoupledL2Parameters {
  val historyEntries  = 2*cacheParams.ways
  val occVecBits     = 3
  val predictEntries = 8*1024
  val predictBits    = 3
  val hashPCBits     = 13

  val sets           = cacheParams.sets
  val ways           = cacheParams.ways
}

abstract class HawkeyeBundle(implicit val p: Parameters) extends Bundle with HasHawkeyeParams
abstract class HawkeyeModule(implicit val p: Parameters) extends Module with HasHawkeyeParams

class HawkeyePredReq(implicit p: Parameters) extends HawkeyeBundle {
  val valid = Input(Bool())
  val pc = Input(UInt(hashPCBits.W))
  val cachefriendly = Output(Bool())
}

class HawkeyeTrain(implicit p: Parameters) extends HawkeyeBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val pc  = UInt(hashPCBits.W)
  val isAcquire = Bool()
}

class Hawkeye(implicit p: Parameters) extends HawkeyeModule {
  val io = IO(new Bundle() {
    val predict = new HawkeyePredReq()
    val train   = Flipped(DecoupledIO(new HawkeyeTrain))
    val resetFinish = Input(Bool())
    val resetIdx    = Input(UInt(setBits.W))
  })

  val historyTable      = Module(new SRAMTemplate(UInt(fullTagBits.W), set = sets, way = historyEntries, singlePort = true, shouldReset = true))
  val occupancyVecTable = Module(new SRAMTemplate(UInt(occVecBits.W), set = sets, way = historyEntries, singlePort = true, shouldReset = true))
  val predictorTable    = Module(new SRAMTemplate(UInt(predictBits.W), set = predictEntries, 1, singlePort = true, shouldReset = true))
  
  /* Read Hawkeye Predictor for cache-averse/friendly classify */
  // TODO: temp read and write sram_table both at s3, read at s2?
  val predictCounter = predictorTable.io.r(io.predict.valid, io.predict.pc).resp.data(0)
  io.predict.cachefriendly := predictCounter(2)===1.U
  
  /* OPTgen to compute opt-hit/miss and update Hawkeye Tables */
  val histRead = Wire(Vec(historyEntries, UInt(fullTagBits.W)))
  val occVecRead = Wire(Vec(historyEntries, UInt(occVecBits.W)))
  val predictRead = Wire(UInt(predictBits.W))
  histRead := historyTable.io.r(io.train.valid, io.train.bits.set).resp.data
  occVecRead := occupancyVecTable.io.r(io.train.valid, io.train.bits.set).resp.data
  predictRead := predictorTable.io.r(io.train.valid, io.train.bits.pc).resp.data(0)
  io.train.ready := true.B

  // search in histTable whether the datablock was reused before
  val reuseVec = Wire(Vec(historyEntries,Bool()))
  reuseVec.zipWithIndex.map { case (e, i) =>
    e := (histRead(i) === io.train.bits.tag)
  }
  val lastAccess = PriorityEncoder(reuseVec)
  // search in occVec whether aging_counter reach 7(the oldest value)
  val ageVec = Wire(Vec(historyEntries,Bool()))
  ageVec.zipWithIndex.map { case (e, i) =>
    e := (occVecRead(i) === 7.U)
  }
  val agedIdx = PriorityEncoder(ageVec)
  // OPT hits/miss
  val OPT_hit = Mux((lastAccess === historyEntries.U), false.B, (agedIdx > lastAccess))
  
  // update History, enqueue new req_tag
  val histWrite = Wire(Vec(historyEntries, UInt(fullTagBits.W)))
  histWrite(0) := io.train.bits.tag
  for (i <- 1 until historyEntries) {
    histWrite(i) := histRead(i-1)
  }
  // update OccVec, new_req occ=0, upd other occ by OPT_hit
  val occVecWrite = Wire(Vec(historyEntries, UInt(occVecBits.W)))
  occVecWrite(0) := 0.U
  for (i <- 1 until historyEntries) {
    occVecWrite(i) := Mux(OPT_hit, 
                          Mux(i.U > (lastAccess+1.U), occVecRead(i-1), occVecRead(i-1) + 1.U),
                          occVecRead(i-1)
                      )
  }
  // update Predictor, only acquire, OPT_hit++, miss--
  val predictWrite = Mux(OPT_hit && (predictRead < 7.U), predictRead + 1.U,
                        Mux(!OPT_hit && (predictRead > 0.U), predictRead - 1.U, predictRead)
                    )

  historyTable.io.w(
    !io.resetFinish || io.train.valid,
    Mux(io.resetFinish, histWrite, VecInit(Seq.fill(historyEntries)(0.U(fullTagBits.W)))),
    Mux(io.resetFinish, io.train.bits.set, io.resetIdx),
    Fill(historyEntries, 1.U)
  )
  occupancyVecTable.io.w(
    !io.resetFinish || io.train.valid,
    Mux(io.resetFinish, occVecWrite, VecInit(Seq.fill(historyEntries)(0.U(occVecBits.W)))),
    Mux(io.resetFinish, io.train.bits.set, io.resetIdx),
    Fill(historyEntries, 1.U)
  )
  predictorTable.io.w(
    !io.resetFinish || (io.train.valid && io.train.bits.isAcquire),
    Mux(io.resetFinish, predictWrite, 4.U),
    Mux(io.resetFinish, io.train.bits.pc, io.resetIdx),
    1.U
  )
}