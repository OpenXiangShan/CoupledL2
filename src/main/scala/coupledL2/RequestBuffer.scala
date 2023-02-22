package coupledL2

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import chisel3._
import chisel3.util._
import coupledL2.utils._
import utility.FastArbiter

class RequestBuffer(flow: Boolean = true, entries: Int = 16)(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new TaskBundle))
    val out = DecoupledIO(new TaskBundle)
    val mshr_status = Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus)))
  })

  val buffer = Mem(entries, new TaskBundle)
  val valids = RegInit(VecInit(Seq.fill(entries){ false.B }))
  // which mshr the entry is waiting for
  val wait_table = Reg(Vec(entries, UInt(mshrsAll.W)))
  /*
      buffer_dep_mask[i][j] => entry i should wait entry j
      this is used to make sure that same set requests will be sent
      to MSHR in order
   */
  val buffer_dep_mask = Reg(Vec(entries, Vec(entries, Bool())))
  val rdys = RegInit(VecInit(Seq.fill(entries){ false.B }))

  val issue_arb = Module(new FastArbiter(new TaskBundle, entries))
  for (i <- 0 until entries){
    issue_arb.io.in(i).valid := valids(i) && rdys(i)
    issue_arb.io.in(i).bits := buffer(i)
    when(issue_arb.io.in(i).fire()){
      valids(i) := false.B
    }
  }
  val output_pipe = Queue(issue_arb.io.out, entries = 1, pipe = true, flow = false)
  output_pipe.ready := io.out.ready

  val full = Cat(valids).andR()
  val no_ready_entry = !output_pipe.valid
  io.out.bits := Mux(no_ready_entry && flow.B, io.in.bits, output_pipe.bits)
  // TODO: flow new request even buffer is full
  io.out.valid := (flow.B && no_ready_entry && io.in.valid && !full) | output_pipe.valid

  io.in.ready := !full

  val in_set = io.in.bits.set

  def set_conflict(set_a: UInt, set_b: UInt): Bool = {
    set_a(setBits - 1, 0) === set_b(setBits - 1, 0)
  }
  val conflict_mask = (0 until mshrsAll) map { i =>
    val s = io.mshr_status(i)
    val s_conflict = s.valid && set_conflict(s.bits.set, in_set) && !s.bits.will_free
    s_conflict
  }
  val conflict = Cat(conflict_mask).orR()
  // filter out duplicated prefetch requests
  val dup_mask = (0 until entries) map { i =>
    valids(i) && (Cat(buffer(i).tag, buffer(i).set) === Cat(io.in.bits.tag, io.in.bits.set))
  }
  val isPrefetch = io.in.bits.fromA && io.in.bits.opcode === Hint
  val dup = io.in.valid && isPrefetch && Cat(dup_mask).orR()
  val req_deps = (0 until entries) map { i =>
    valids(i) && set_conflict(buffer(i).set, in_set)
  }
  val insert_idx = PriorityEncoder(~valids.asUInt())
  val alloc = !full && io.in.valid && !(flow.B && no_ready_entry && io.out.ready) && !dup
  when(alloc){
    buffer(insert_idx) := io.in.bits
    valids(insert_idx) := true.B
    wait_table(insert_idx) := VecInit(conflict_mask).asUInt()
    buffer_dep_mask(insert_idx) := VecInit(req_deps)
    assert(PopCount(conflict_mask) <= 2.U)
    rdys(insert_idx) := !conflict && !Cat(req_deps).orR()
  }

  val free_mask = VecInit(io.mshr_status.map(s => s.valid && s.bits.will_free)).asUInt()
  for (i <- 0 until entries){
    when(valids(i)){
      val wait_next = WireInit(wait_table(i))
      val dep_mask_next = WireInit(buffer_dep_mask(i))
      wait_next := wait_table(i).asUInt() & (~free_mask.asUInt()).asUInt()
      when(issue_arb.io.out.fire()){
        dep_mask_next(issue_arb.io.chosen) := false.B
      }
      wait_table(i) := wait_next
      rdys(i) := !wait_next.orR() && !Cat(dep_mask_next).orR()
    }
    when(issue_arb.io.out.fire()){
      buffer_dep_mask(i)(issue_arb.io.chosen) := false.B
    }
  }

  XSPerfAccumulate(cacheParams, "req_buffer_merge", dup && !full)
  if(flow){
    XSPerfAccumulate(cacheParams, "req_buffer_flow", no_ready_entry && io.in.fire())
  }
  XSPerfAccumulate(cacheParams, "req_buffer_alloc", alloc)
  XSPerfAccumulate(cacheParams, "req_buffer_full", full)
  for(i <- 0 until entries){
    val update = PopCount(valids) === i.U
    XSPerfAccumulate(cacheParams, s"req_buffer_util_$i", update)
  }
  XSPerfAccumulate(cacheParams, "recv_prefetch", io.in.fire() && isPrefetch)
  XSPerfAccumulate(cacheParams, "recv_normal", io.in.fire() && !isPrefetch)
}
