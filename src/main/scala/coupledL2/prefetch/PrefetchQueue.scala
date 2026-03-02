package coupledL2.prefetch

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.{Field, Parameters}

class PrefetchQueue[T <: Data](
  size: Int,
  enableFilter: Boolean,
  enableFlow: Boolean,
  gen: T,
  addrOf: T => UInt
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen.cloneType))
    val deq = DecoupledIO(gen.cloneType)
  })
  /*  Here we implement a queue that
   *  1. is pipelined  2. flows
   *  3. always has the latest reqs, which means the queue is always ready for enq and deserting the eldest ones
   */
  // assert(!enableFilter || size <= 20) // timing limited
  val queue = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(gen))))
  val valids = RegInit(VecInit(Seq.fill(size)(false.B)))
  val idxWidth = log2Up(size)
  val head = RegInit(0.U(idxWidth.W))
  val tail = RegInit(0.U(idxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last

  val matchVec = VecInit((0 until size).map(i => valids(i) && addrOf(queue(i)) === addrOf(io.enq.bits)))
  val enqFilterHit = enableFilter.B && io.enq.valid && matchVec.asUInt.orR

  val deqFireFromQueue = !empty && io.deq.ready
  val deqFireFromEnq = empty && io.deq.ready && io.enq.valid && enableFlow.B
  val enqFireToQueue = io.enq.valid && !enqFilterHit && !deqFireFromEnq

  when(deqFireFromQueue) {
    valids(head) := false.B
    head := head + 1.U
  }

  when(enqFireToQueue) {
    queue(tail) := io.enq.bits
    valids(tail) := true.B
    tail := tail + 1.U
    when(full && !deqFireFromQueue) {
      head := head + 1.U
    }
  }

  io.enq.ready := true.B
  io.deq.valid := !empty || (io.enq.valid && enableFlow.B)
  io.deq.bits := Mux(empty && enableFlow.B, io.enq.bits, queue(head))

  // The reqs that are discarded = enq - deq
  XSPerfAccumulate("prefetch_queue_enq", io.enq.fire)
  XSPerfAccumulate("prefetch_queue_enq_filtered", enqFilterHit)
  XSPerfAccumulate("prefetch_queue_enq_replace_old", enqFireToQueue && full && !deqFireFromQueue)
  XSPerfAccumulate("prefetch_queue_deq", io.deq.fire)
  XSPerfAccumulate("prefetch_queue_empty", empty)
  XSPerfAccumulate("prefetch_queue_full", full)
  XSPerfHistogram("prefetch_queue_entry", PopCount(valids.asUInt), true.B, 0, size, 1)
}