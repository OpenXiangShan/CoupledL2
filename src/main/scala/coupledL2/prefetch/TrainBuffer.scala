package coupledL2.prefetch

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import utility.mbist.MbistPipeline
import coupledL2._

// TODO: 模版化，参数化，参考 prefetchQueue
class TrainBuffer(size: Int, name: String)(implicit p: Parameters) extends PrefetchModule {
  private val enqLen = banks

  val io = IO(new Bundle() {
    val enable = Input(Bool())
    val flush = Input(Bool())
    val trainInVec = Flipped(Vec(enqLen, DecoupledIO(new PrefetchTrain())))
    val trainOut = DecoupledIO(new PrefetchTrain())
  })

  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => size ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = Reg(Vec(size, new PrefetchTrain))
  val valids = RegInit(VecInit(Seq.fill(size){ (false.B) }))

  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new Ptr))

  val deqPtr = WireInit(deqPtrExt.value)

  require(size >= enqLen)

  io.trainInVec.map(_.ready := true.B)

  val reqs_l = io.trainInVec.map(_.bits)
  val reqs_vl = io.trainInVec.map(_.valid)
  val needAlloc = Wire(Vec(enqLen, Bool()))
  val canAlloc = Wire(Vec(enqLen, Bool()))

  val entry_match = Wire(Vec(enqLen, Bool()))
  val prev_enq_match = Wire(Vec(enqLen, Bool()))
  for(i <- (0 until enqLen)) {
    val req = reqs_l(i)
    val req_v = reqs_vl(i)
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)
    entry_match(i) := Cat(entries.zip(valids).map {
      case(e, v) => v && block_hash_tag(e.addr) === block_hash_tag(req.addr)
    }).orR
    if(i == 0){
      prev_enq_match(i) := false.B
    } else {
      prev_enq_match(i) := Cat(reqs_l.zip(reqs_vl).take(i).map {
        case(pre, pre_v) => pre_v && block_hash_tag(pre.addr) === block_hash_tag(req.addr)
      }).orR
    }

    needAlloc(i) := req_v && !entry_match(i) && !prev_enq_match(i)
    canAlloc(i) := needAlloc(i) && allocPtr >= deqPtrExt && io.enable

    when(canAlloc(i)) {
      valids(allocPtr.value) := true.B
      entries(allocPtr.value) := req
    }
  }
  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach{case x => when(canAlloc.asUInt.orR) {x := x + allocNum} }

  // deq
  io.trainOut.valid := false.B
  io.trainOut.bits := DontCare
  valids.zip(entries).zipWithIndex.foreach {
    case((valid, entry), i) => {
      when(deqPtr === i.U) {
        io.trainOut.valid := valid && io.enable
        io.trainOut.bits := entry
      }
    }
  }

  when(io.trainOut.fire) {
    valids(deqPtr) := false.B
    deqPtrExt := deqPtrExt + 1.U
  }

  when(RegNext(io.flush)) {
    valids.foreach {case valid => valid := false.B}
    (0 until enqLen).map {case i => enqPtrExt(i) := i.U.asTypeOf(new Ptr)}
    deqPtrExt := 0.U.asTypeOf(new Ptr)
  }

  XSPerfAccumulate(s"${name}_train_filter_full", PopCount(valids) === size.U)
  XSPerfAccumulate(s"${name}_train_filter_half", PopCount(valids) >= (size / 2).U)
  XSPerfAccumulate(s"${name}_train_filter_empty", PopCount(valids) === 0.U)

  val raw_enq_pattern = Cat(reqs_vl)
  val filtered_enq_pattern = Cat(needAlloc)
  val actual_enq_pattern = Cat(canAlloc)
  XSPerfAccumulate(s"${name}_train_filter_enq", allocNum)
  XSPerfAccumulate(s"${name}_train_filter_enq_multi", allocNum > 0.U)
  XSPerfAccumulate(s"${name}_train_filter_enq_filter", PopCount(entry_match) + PopCount(prev_enq_match))
  XSPerfAccumulate(s"${name}_train_filter_deq", io.trainOut.fire)
  for(i <- 0 until (1 << enqLen)) {
    XSPerfAccumulate(s"${name}_train_filter_raw_enq_pattern_${toBinary(i)}", raw_enq_pattern === i.U)
    XSPerfAccumulate(s"${name}_train_filter_filtered_enq_pattern_${toBinary(i)}", filtered_enq_pattern === i.U)
    XSPerfAccumulate(s"${name}_train_filter_actual_enq_pattern_${toBinary(i)}", actual_enq_pattern === i.U)
  }
}