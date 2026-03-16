package coupledL2.utils
import chisel3._
import chisel3.util._

/*************************************************************************************
 * This file defines an OverwriteQueue, a hardware queue that can overwrite old data when full.
 * It supports the following configurable features:
 * 
 * - hasOverWrite: When true and the queue is full, new enqueued data overwrites the oldest
 *   entry by advancing both head and tail pointers simultaneously. When false, the queue
 *   behaves like a standard queue and blocks enqueue when full.
 * 
 * - hasFlow: When true and the queue is empty, new enqueued data is directly passed to the
 *   output (bypassed), reducing latency. When the queue has entries, it behaves normally.
 * 
 * - hasFlush: When true, provides a flush signal that resets the queue to empty state.
 * 
 * The queue uses a circular buffer implementation with head and tail pointers, and a
 * maybe_full flag to distinguish between empty and full states when pointers are equal.
 * 
 * Note: The number of entries must be a power of 2 to simplify pointer arithmetic.
 */
class Queue_Regs[T <: Data](
  gen: T, 
  entries: Int, //Must be a power of 2
  hasFlush: Boolean = false, 
  hasOverWrite: Boolean = true,
  hasFlow: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
    val flush = if (hasFlush) Some(Input(Bool())) else None
    val count = Output(UInt(log2Up(entries + 1).W)) // Indicates how many entries have been saved in the current clock cycle
  })
  
  require(entries > 0, "Queue must have positive entries")
  require((entries & (entries - 1)) == 0, "entries must be a power of 2")

  val queue = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val maybe_full = RegInit(false.B)
  val headCounter = Counter(entries)
  val headPtr = headCounter.value
  val tailCounter =  Counter(entries)
  val tailPtr = tailCounter.value
  val empty = headPtr === tailPtr && !maybe_full
  val full = headPtr === tailPtr && maybe_full
  val flushEn = io.flush.getOrElse(false.B) && hasFlush.B

  // Decide enq ready depending on hasOverWrite (elaboration-time constant)
  if (hasOverWrite) {
    io.enq.ready := !flushEn
  } else {
    io.enq.ready := !full && !flushEn
  }

  // Compute deq.valid according to modes (use Scala if for param-time branching)
  val deq_valid = if (hasFlow) {
    // hasFlow mode: Data passes through directly when the queue is empty
    !empty || io.enq.valid
  } else {
    !empty
  }

  // Dequeue fire when deq is valid and consumer ready
  val do_deq = deq_valid && io.deq.ready && !flushEn

  // Enqueue fire (data presented at input)
  val do_enq = io.enq.valid && io.enq.ready && !flushEn

  // hasFlow bypass: data flows directly to output without entering queue
  val flowBypass = hasFlow.B && empty && do_enq && io.deq.ready

  // Overwrite condition: queue is full, overwrite enabled, and enqueue happens
  // Note: full and empty are mutually exclusive, so no need to check !flowBypass
  val overwriteHappens = full && hasOverWrite.B && do_enq && !do_deq

  when(flushEn) {  
    maybe_full := false.B 
  } .elsewhen(overwriteHappens) {
    // Overwrite: both pointers advance together, queue stays full
    maybe_full := true.B
  } .elsewhen(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  // Enqueue logic
  when(flushEn) {
    tailCounter.reset()  
  } .elsewhen(do_enq) {
    queue(tailPtr) := io.enq.bits
    tailCounter.inc()
  }

  // Dequeue logic: advance headPtr on dequeue OR overwrite
  when(flushEn) {
    headCounter.reset()  
  } .elsewhen(do_deq || overwriteHappens) {
    headCounter.inc()
  }

  // Drive deq outputs
  io.deq.valid := deq_valid && !flushEn
  io.deq.bits := Mux(flowBypass, io.enq.bits, queue(headPtr))
 
  val ptr_diff = tailPtr - headPtr
  io.count := Mux(maybe_full && (headPtr === tailPtr), entries.U, ptr_diff)
}