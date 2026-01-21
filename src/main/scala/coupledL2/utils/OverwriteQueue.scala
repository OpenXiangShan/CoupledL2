package coupledL2.utils
import chisel3._
import chisel3.util._


class OverwriteQueue[T <: Data](
  gen: T, 
  entries: Int,
  pipe: Boolean = false,
  flow: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
    val count = Output(UInt(log2Up(entries + 1).W))  
  })
  
  
  require(entries > 0, "Queue must have positive entries")
  
  val queue = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val idxWidth = log2Up(entries)
  val head = RegInit(0.U(idxWidth.W))  
  val tail = RegInit(0.U(idxWidth.W))  
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last
  
  // Dequeue logic
  val do_deq = !empty && io.deq.ready
  when(do_deq) {
    valids(head) := false.B
    head := head + 1.U
  }
  
  // Enqueue logic
  val do_enq = io.enq.valid
  when(do_enq) {
    queue(tail) := io.enq.bits
    valids(tail) := !empty || !io.deq.ready 
    tail := tail + (!empty || !io.deq.ready).asUInt
    
    // When the queue is full, new data displaces old data, and the head moves forward.
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
  }
  
 
  io.enq.ready := true.B  
  
  if (flow) {
   // Flow mode: Data passes through directly when the queue is empty
    io.deq.valid := !empty || io.enq.valid
    io.deq.bits := Mux(empty, io.enq.bits, queue(head))
  } else {
    io.deq.valid := !empty
    io.deq.bits := queue(head)
  }
  
 
  val ptr_diff = tail - head
  io.count := Mux(
    empty, 
    0.U,
    Mux(full, entries.U, Mux(tail > head, ptr_diff, entries.U - head + tail))
  )
}