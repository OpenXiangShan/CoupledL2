package coupledL2.utils
import chisel3._
import chisel3.util._


class OverwriteQueue[T <: Data](
  gen: T, 
  entries: Int, //Must be a power of 2
  foreverFlow : Boolean = false,
  flow: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
    val count = Output(UInt(log2Up(entries + 1).W)) // Indicates how many entries have been saved in the current clock cycle
  })
  
  
  require(entries > 0, "Queue must have positive entries")
  require((entries & (entries - 1)) == 0, "entries must be a power of 2")

  
  val queue = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val idxWidth = log2Up(entries)
  val head = RegInit(0.U(idxWidth.W))  
  val tail = RegInit(0.U(idxWidth.W))  
  val empty = head === tail && !valids.last //head eq tail,exit entry is empty,then now all is empty
  val full = head === tail && valids.last
  
  // Decide enq ready depending on foreverFlow (elaboration-time constant)
  if (foreverFlow) {
    io.enq.ready := io.deq.ready
  } else {
    io.enq.ready := true.B
  }

  // Compute deq.valid according to modes (use Scala if for param-time branching)
  val deq_valid = if (foreverFlow) {
    io.enq.valid
  } else if (flow) {
    // Flow mode: Data passes through directly when the queue is empty
    !empty || io.enq.valid
  } else {
    !empty
  }

  // Dequeue fire when deq is valid and consumer ready
  val do_deq = deq_valid && io.deq.ready

  // Enqueue fire should follow the real handshake (enq.valid && enq.ready)
  val do_enq = io.enq.valid && io.enq.ready

  // Enqueue logic (only on actual fire)
  when(do_enq) {
    queue(tail) := io.enq.bits
    // valid bit: if flow enabled, we only mark stored entry when we actually store (no bypass)
    valids(tail) := Mux(flow.B, !empty || !io.deq.ready, true.B)
    tail := tail + (!empty || !io.deq.ready).asUInt

    // When the queue is full and consumer not ready, new data displaces old data
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
  }

  // Dequeue logic (only on actual fire)
  when(do_deq) {
    // If dequeue fires and the element was stored, clear its valid bit and advance head
    when(!foreverFlow.B) {
      // In non-foreverFlow modes we only advance head (and clear valid) if the
      // current head slot actually contains a stored entry. In flow/bypass
      // scenarios the slot may be unused (valids(head) == false) so we must not
      // advance head in that case.
      when(valids(head)) {
        valids(head) := false.B
        head := head + 1.U
      }
    }
  }

  // Drive deq outputs
  io.deq.valid := deq_valid
  if (foreverFlow) {
    io.deq.bits := io.enq.bits
  } else if (flow) {
    // only read enq.bits when empty and enq.valid to avoid exposing DontCare
    io.deq.bits := Mux(empty && io.enq.valid, io.enq.bits, queue(head))
  } else {
    io.deq.bits := queue(head)
  }
  
  
 
  val ptr_diff = tail - head
  io.count := Mux(
    empty, 
    0.U,
    Mux(full, entries.U, Mux(tail > head, ptr_diff, entries.U - head + tail))
  )
}