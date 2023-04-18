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
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.utils._

class CustomL1HintIOBundle(implicit p: Parameters) extends L2Bundle {
  // input information
  val s1 = Flipped(ValidIO(new TaskBundle()))
  val s2 = Flipped(ValidIO(new TaskBundle()))
  val s3 = new L2Bundle {
      val task      = Flipped(ValidIO(new TaskBundle()))
      val d         = Input(Bool())
      val need_mshr = Input(Bool())
  }
  val s4 = new L2Bundle {
      val task                  = Flipped(ValidIO(new TaskBundle()))
      val d                     = Input(Bool())
      val need_write_releaseBuf = Input(Bool())
      val need_write_refillBuf  = Input(Bool())
  }
  val s5 = new L2Bundle {
      val task = Flipped(ValidIO(new TaskBundle()))
      val d    = Input(Bool())
  }
  val globalCounter   = Input(UInt(log2Ceil(mshrsAll).W))
  val grantBufferHint = Flipped(ValidIO(new L2ToL1Hint()))

  // output hint
  val l1Hint = ValidIO(new L2ToL1Hint())
}


// grantData hint interface
// use this interface to give a hint to l1 before actually sending a GrantData
class CustomL1Hint(implicit p: Parameters) extends L2Module {
  val io = IO(new CustomL1HintIOBundle)

  val task_s1 = io.s1
  val task_s2 = io.s2
  val task_s3 = io.s3.task
  val task_s4 = io.s4.task
  val task_s5 = io.s5.task

  val d_s3 = io.s3.d
  val d_s4 = io.s4.d
  val d_s5 = io.s5.d

  val globalCounter   = io.globalCounter
  val grantBufferHint = io.grantBufferHint

  val mshr_req_s3  = task_s3.bits.mshrTask
  val need_mshr_s3 = io.s3.need_mshr

  val need_write_releaseBuf_s4 = io.s4.need_write_releaseBuf
  val need_write_refillBuf_s4  = io.s4.need_write_refillBuf

  //  req_grantbuffer_next_cycle_s4: this **hit** req will request grantBuffer in S5
  val req_grantbuffer_next_cycle_s4 = !need_write_releaseBuf_s4 && !need_write_refillBuf_s4

  val s3_l2_hit_grant_data = task_s3.valid && !mshr_req_s3 && !need_mshr_s3 && task_s3.bits.fromA && task_s3.bits.opcode === AcquireBlock && !task_s3.bits.fromL2pft.getOrElse(false.B)
  val s4_l2_hit_grant_data = task_s4.valid && req_grantbuffer_next_cycle_s4 && task_s4.bits.opcode === GrantData && task_s4.bits.fromA && !task_s4.bits.mshrTask && !task_s4.bits.fromL2pft.getOrElse(false.B)

  val hint_s1, hint_s2, hint_s3, hint_s4, hint_s5 = Wire(io.l1Hint.cloneType)
  
  // S1 hint
  //    * l1 acquire and l2 miss situation, **no hit situation**
  val s1_l2_miss_refill_grant_data    = task_s1.valid && task_s1.bits.fromA && task_s1.bits.opcode === GrantData
  val s1_l2_miss_refill_counter_match = Wire(Bool())

  // TODO: generalization, for now, only fit hintCycleAhead == 3
  s1_l2_miss_refill_counter_match := PopCount(Seq(d_s3, d_s4, d_s5, s3_l2_hit_grant_data, s4_l2_hit_grant_data, task_s2.valid && task_s2.bits.fromA)) === 0.U && globalCounter <= 2.U

  hint_s1.valid         := s1_l2_miss_refill_grant_data && s1_l2_miss_refill_counter_match
  hint_s1.bits.sourceId := task_s1.bits.sourceId

  // S2 hint
  //    * l1 acquire and l2 miss situation, **no hit situation**
  val s2_l2_miss_refill_grant_data    = task_s2.valid && task_s2.bits.fromA && task_s2.bits.opcode === GrantData && task_s2.bits.mshrTask
  val s2_l2_miss_refill_counter_match = Wire(Bool())

  // TODO: generalization, for now, only fit hintCycleAhead == 2
  //  s2_l2_miss_refill_counter_match := PopCount(Seq(d_s3, d_s4, d_s5, s3_l2_hit_grant_data, s4_l2_hit_grant_data)) === 0.U && globalCounter === 0.U

  s2_l2_miss_refill_counter_match := MuxLookup(Cat(d_s3, d_s4, d_s5), false.B, Seq(
    Cat(true.B, true.B, true.B)    -> ((globalCounter + 4.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U),
    Cat(true.B, true.B, false.B)   -> ((globalCounter + 3.U + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U),
    Cat(true.B, false.B, true.B)   -> Mux(s4_l2_hit_grant_data, (globalCounter + 4.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                                                (globalCounter + 3.U + task_s5.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U),
    Cat(false.B, true.B, true.B)   -> Mux(s3_l2_hit_grant_data, (globalCounter + 4.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                                                (globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U),
    Cat(true.B, false.B, false.B)  -> Mux(s4_l2_hit_grant_data, (globalCounter + 3.U + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                                                (globalCounter + 2.U + task_s3.bits.opcode(0)) === hintCycleAhead.U),
    Cat(false.B, true.B, false.B)  -> ((globalCounter + 2.U + task_s4.bits.opcode(0)) === hintCycleAhead.U),
    Cat(false.B, false.B, true.B)  -> Mux(s4_l2_hit_grant_data,
                                          Mux(s3_l2_hit_grant_data, (globalCounter + 4.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                                (globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U),
                                                    (globalCounter + 2.U + task_s5.bits.opcode(0)) === hintCycleAhead.U),
    Cat(false.B, false.B, false.B) -> Mux(s4_l2_hit_grant_data,
                                          Mux(s3_l2_hit_grant_data, (globalCounter + 4.U + task_s4.bits.opcode(0) + task_s3.bits.opcode(0)) === hintCycleAhead.U,
                                                (globalCounter + 3.U + task_s4.bits.opcode(0)) === hintCycleAhead.U),
                                                    (globalCounter + 2.U) === hintCycleAhead.U)
  ))

  hint_s2.valid         := s2_l2_miss_refill_grant_data && s2_l2_miss_refill_counter_match
  hint_s2.bits.sourceId := task_s2.bits.sourceId

  // S3 hint
  //    * l1 acquire and l2 hit situation
  val s3_l2_hit_counter_match = Wire(Bool())
  when(d_s5 && d_s4) {
    s3_l2_hit_counter_match := (globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U
  }.elsewhen(d_s4) {
    s3_l2_hit_counter_match := (globalCounter + 3.U + task_s4.bits.opcode(0)) === hintCycleAhead.U
  }.elsewhen(d_s5) {
    // NOTE: if s4 is a hit grantData, it will not request grantBuffer in s4, but in s5
    when(s4_l2_hit_grant_data) {
      s3_l2_hit_counter_match := (globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U
    }.otherwise {
      s3_l2_hit_counter_match := (globalCounter + 3.U + task_s5.bits.opcode(0)) === hintCycleAhead.U
    }
  }.otherwise {
    when(s4_l2_hit_grant_data) {
      s3_l2_hit_counter_match := (globalCounter + 3.U + task_s4.bits.opcode(0)) === hintCycleAhead.U
    }.otherwise {
      s3_l2_hit_counter_match :=  globalCounter + 3.U === hintCycleAhead.U
    }
  }
  val validHint_s3 = s3_l2_hit_grant_data && s3_l2_hit_counter_match

  // S3 hint
  //    * l1 acquire and l2 miss situation
  val s3_l2_miss_refill_grant_data    = d_s3 && mshr_req_s3 && task_s3.bits.fromA && task_s3.bits.opcode === GrantData && !task_s3.bits.fromL2pft.getOrElse(false.B)
  val s3_l2_miss_refill_counter_match = Wire(Bool())
  when(d_s5 && d_s4) {
    s3_l2_miss_refill_counter_match := (globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U
  }.elsewhen(d_s4) {
    s3_l2_miss_refill_counter_match := (globalCounter + 2.U + task_s4.bits.opcode(0)) === hintCycleAhead.U
  }.elsewhen(d_s5) {
    when(s4_l2_hit_grant_data) {
      s3_l2_miss_refill_counter_match := (globalCounter + 3.U + task_s5.bits.opcode(0) + task_s4.bits.opcode(0)) === hintCycleAhead.U
    }.otherwise {
      s3_l2_miss_refill_counter_match := (globalCounter + 2.U + task_s5.bits.opcode(0)) === hintCycleAhead.U
    }
  }.otherwise {
    s3_l2_miss_refill_counter_match :=  globalCounter + 1.U === hintCycleAhead.U
  }
  val validHintMiss_s3 = s3_l2_miss_refill_grant_data && s3_l2_miss_refill_counter_match

  hint_s3.valid         := validHint_s3 || validHintMiss_s3
  hint_s3.bits.sourceId := task_s3.bits.sourceId

  // S4 hint
  //    * l1 acquire and l2 hit situation
  val s4_l2_hit_counter_match = Mux(d_s5 && task_s5.bits.opcode(0), (globalCounter + 3.U) === hintCycleAhead.U,
                                      (globalCounter + 2.U) === hintCycleAhead.U )
  val validHint_s4            = s4_l2_hit_grant_data && s4_l2_hit_counter_match
  // S4 hint
  //    * l1 acquire and l2 miss situation
  val s4_l2_miss_refill_grant_data    = d_s4 && task_s4.bits.opcode === GrantData && task_s4.bits.fromA && task_s4.bits.mshrTask && !task_s4.bits.fromL2pft.getOrElse(false.B)
  val s4_l2_miss_refill_counter_match = Mux(d_s5 && task_s5.bits.opcode(0), (globalCounter + 3.U) === hintCycleAhead.U, 
                                            Mux(d_s5 && !task_s5.bits.opcode(0), (globalCounter + 2.U) === hintCycleAhead.U, 
                                                (globalCounter + 1.U) === hintCycleAhead.U ))
  val validHintMiss_s4 = s4_l2_miss_refill_grant_data && s4_l2_miss_refill_counter_match

  hint_s4.valid         := validHint_s4 || validHintMiss_s4
  hint_s4.bits.sourceId := task_s4.bits.sourceId

  // S5 hint
  //    * l1 acquire and l2 hit situation
  val validHint_s5 = d_s5 && task_s5.bits.opcode === GrantData && task_s5.bits.fromA && !task_s5.bits.mshrTask && ((globalCounter + 1.U) === hintCycleAhead.U) && !task_s5.bits.fromL2pft.getOrElse(false.B)
  // S5 hint
  //    * l1 acquire and l2 miss situation
  val validHintMiss_s5 = d_s5 && task_s5.bits.opcode === GrantData && task_s5.bits.fromA && task_s5.bits.mshrTask && ((globalCounter + 1.U) === hintCycleAhead.U) && !task_s5.bits.fromL2pft.getOrElse(false.B)

  hint_s5.valid         := validHint_s5 || validHintMiss_s5
  hint_s5.bits.sourceId := task_s5.bits.sourceId

  val hint_valid    = Seq(grantBufferHint.valid,         hint_s1.valid,         hint_s2.valid,         hint_s3.valid,         hint_s4.valid,         hint_s5.valid)
  val hint_sourceId = Seq(grantBufferHint.bits.sourceId, hint_s1.bits.sourceId, hint_s2.bits.sourceId, hint_s3.bits.sourceId, hint_s4.bits.sourceId, hint_s5.bits.sourceId)

  io.l1Hint.valid         := VecInit(hint_valid).asUInt.orR
  io.l1Hint.bits.sourceId := ParallelMux(hint_valid zip hint_sourceId)
  // TODO: open this assert when hint is really correct for all situations
  // assert(PopCount(VecInit(hint_valid)) <= 1.U)

  XSPerfAccumulate(cacheParams, "hint_grantBufferHint_valid", grantBufferHint.valid)
  XSPerfAccumulate(cacheParams, "hint_s1_valid", hint_s1.valid)
  XSPerfAccumulate(cacheParams, "hint_s2_valid", hint_s2.valid)
  XSPerfAccumulate(cacheParams, "hint_s3_valid", hint_s3.valid)
  XSPerfAccumulate(cacheParams, "hint_s4_valid", hint_s4.valid)
  XSPerfAccumulate(cacheParams, "hint_s5_valid", hint_s5.valid)

}