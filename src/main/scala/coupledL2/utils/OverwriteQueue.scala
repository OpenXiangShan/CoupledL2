package coupledL2.utils
import chisel3._
import chisel3.util._

/** 通用队列,支持覆盖旧数据
  * @param gen 队列存储的数据类型
  * @param entries 队列深度
  * @param pipe 是否流水线模式
  * @param flow 是否直通模式
  * 
  * 特性:
  * 1. 总是 ready 接收新数据
  * 2. 队列满时,新数据会挤掉最旧的数据
  * 3. 支持 flow (空队列时数据直通)
  */
class OverwriteQueue[T <: Data](
  gen: T, 
  entries: Int,
  pipe: Boolean = false,
  flow: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
    val count = Output(UInt(log2Up(entries + 1).W))  // 当前队列中元素数量
  })
  
  /*  实现一个队列:
   *  1. 总是 ready,可以持续接收新数据
   *  2. 满时自动丢弃最旧的数据
   *  3. 支持 flow: 空队列时数据可以直通
   */
  require(entries > 0, "Queue must have positive entries")
  
  val queue = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val idxWidth = log2Up(entries)
  val head = RegInit(0.U(idxWidth.W))  // 出队指针
  val tail = RegInit(0.U(idxWidth.W))  // 入队指针
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last
  
  // 出队逻辑
  val do_deq = !empty && io.deq.ready
  when(do_deq) {
    valids(head) := false.B
    head := head + 1.U
  }
  
  // 入队逻辑
  val do_enq = io.enq.valid
  when(do_enq) {
    queue(tail) := io.enq.bits
    valids(tail) := !empty || !io.deq.ready  // 如果空且deq ready,则flow直通,不存储
    tail := tail + (!empty || !io.deq.ready).asUInt
    
    // 队列满时,新数据挤占旧数据,head 前移
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
  }
  
  // 接口赋值
  io.enq.ready := true.B  // 总是 ready
  
  if (flow) {
    // Flow 模式: 空队列时数据直通
    io.deq.valid := !empty || io.enq.valid
    io.deq.bits := Mux(empty, io.enq.bits, queue(head))
  } else {
    io.deq.valid := !empty
    io.deq.bits := queue(head)
  }
  
  // 计算队列中元素数量
  val ptr_diff = tail - head
  io.count := Mux(
    empty, 
    0.U,
    Mux(full, entries.U, Mux(tail > head, ptr_diff, entries.U - head + tail))
  )
}