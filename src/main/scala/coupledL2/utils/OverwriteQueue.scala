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

package coupledL2.utils
import chisel3._
import chisel3.util._

/*************************************************************************************
 * This file defines an Queue by Regs.
 * It supports the following configurable features:
 *
 * - hasFlow: When true and the queue is empty, new enqueued data is directly passed to the
 *   output (bypassed), reducing latency. When the queue has entries, it behaves normally.
 * 
 * The queue uses a circular buffer implementation with head and tail pointers, and a
 * maybe_full flag to distinguish between empty and full states when pointers are equal.
 * 
 * Note: The number of entries must be a power of 2 to simplify pointer arithmetic.
 */
class OverwriteQueue[T <: Data](
  gen: T, 
  entries: Int, //Must be a power of 2
  hasFlow: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
  })
  def wrapInc(ptr: UInt): UInt = Mux(ptr === (entries - 1).U, 0.U, ptr + 1.U)
  require(entries > 1, "Queue must have positive entries")
  require((entries & (entries - 1)) == 0, "entries must be a power of 2")

  val queue = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val maybe_full = RegInit(false.B)
  val ptrBits = log2Up(entries)  // pointer width: ensure at least 1 bit to avoid zero-width UInts
  val headPtr = RegInit(0.U(ptrBits.W))
  val tailPtr = RegInit(0.U(ptrBits.W))
  val full = headPtr === tailPtr && maybe_full
  val empty = headPtr === tailPtr && !maybe_full
  
  // Decide enq ready depending on hasOverWrite (elaboration-time constant)
  io.enq.ready := true.B 

  // Compute deq.valid according to modes (use Scala if for param-time branching)
  val deq_valid = if (hasFlow) {
    !empty || io.enq.valid // hasFlow mode: Data passes through directly when the queue is empty
  } else {
    !empty
  }

  // Dequeue fire when deq is valid and consumer ready
  // Note:It does not necessarily come from the data already stored in the Queue.
  val do_deq = deq_valid && io.deq.ready 

  // Enqueue fire (data presented at input)
  // Note:Data is not necessarily stored inside the Queue.
  val do_enq = io.enq.fire

  val flowHappens = if (hasFlow) empty && do_enq && io.deq.ready else false.B

  val overwriteHappens =  full &&  do_enq && !do_deq 
  
  when(overwriteHappens) {
    maybe_full := true.B // Overwrite: both pointers advance together, queue stays full
  } .elsewhen(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  // Enqueue logic
  when (do_enq && !flowHappens){
    queue(tailPtr) := io.enq.bits
  }

  when(do_enq ) {
    tailPtr := wrapInc(tailPtr)
  }

  // Dequeue logic: advance headPtr on dequeue OR overwrite
  when(do_deq || overwriteHappens) {
    headPtr := wrapInc(headPtr)
  }

  // Drive deq outputs
  io.deq.valid := deq_valid 
  io.deq.bits := Mux(flowHappens, io.enq.bits, queue(headPtr)) 

  //add assert
  assert(!(overwriteHappens && !full), "Queue_Regs: overwriteHappens implies full")

  when (flowHappens) {
    assert(empty, "Queue_Regs: flowHappens but queue not empty")
    assert(io.deq.ready, "Queue_Regs: flowHappens but deq not ready")
  }
  assert(!(io.deq.fire && !io.deq.valid), "Queue_Regs: deq.fire but deq.valid false")
}