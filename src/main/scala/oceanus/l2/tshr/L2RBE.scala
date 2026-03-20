package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import oceanus.l2.tshr.L2RBE.PathVPipeBlock

object L2RBE {

  class PathVPipeBlock extends Bundle {
    val EVT = Bool()
    val SNP = Bool()
    val REQ = Bool()
  }
}

class L2RBE[T <: Bundle](
  gen: T,
  pipe: Boolean = false,
  flow: Boolean = false,
  direct: Boolean = false
) extends Module {

  val io = IO(new Bundle {

    val blockFromVPipe = Input(new PathVPipeBlock)
        
    val directoryReadDone = Input(Bool())
    val directoryReadNeed = Input(Bool())

    val in = Flipped(Decoupled(gen.cloneType))
    val out = Valid(gen.cloneType)
  })

  // blocking conditions
  val blockByVPipe = io.blockFromVPipe.asUInt.orR
  val blockByDirectory = !io.directoryReadDone && io.directoryReadNeed

  val block = blockByVPipe || blockByDirectory

  // auxiliary
  val _is_stalling = Wire(Bool())

  //
  if (!direct) {

    val queue = Module(new Queue(gen.cloneType, entries = 1, pipe = pipe, flow = flow))

    queue.io.deq.ready := !block

    // flit input connections
    queue.io.enq <> io.in

    // flit output connections
    io.out.bits := queue.io.deq.bits
    io.out.valid := queue.io.deq.fire

    //
    _is_stalling := queue.io.deq.valid && !queue.io.deq.ready

  } else {

    io.out.bits := io.in.bits
    io.out.valid := io.in.valid && !block

    io.in.ready := !block

    //
    _is_stalling := io.in.valid && !io.in.ready
    
  }

  // performance counters
  val perf_stallCycleCnt = RegInit(0.U(32.W))
  val perf_stallCycleCnt_byDirectory = RegInit(0.U(32.W))
  val perf_stallCycleCnt_byEVT = RegInit(0.U(32.W))
  val perf_stallCycleCnt_bySNP = RegInit(0.U(32.W))
  val perf_stallCycleCnt_byREQ = RegInit(0.U(32.W))
  when (io.out.fire) {
    perf_stallCycleCnt := 0.U
    perf_stallCycleCnt_byDirectory := 0.U
    perf_stallCycleCnt_byEVT := 0.U
    perf_stallCycleCnt_bySNP := 0.U
    perf_stallCycleCnt_byREQ := 0.U
  }.elsewhen (_is_stalling) {
    perf_stallCycleCnt := perf_stallCycleCnt + 1.U
    when (blockByDirectory) { perf_stallCycleCnt_byDirectory := perf_stallCycleCnt_byDirectory + 1.U }
    when (io.blockFromVPipe.EVT) { perf_stallCycleCnt_byEVT := perf_stallCycleCnt_byEVT + 1.U }
    when (io.blockFromVPipe.SNP) { perf_stallCycleCnt_bySNP := perf_stallCycleCnt_bySNP + 1.U }
    when (io.blockFromVPipe.REQ) { perf_stallCycleCnt_byREQ := perf_stallCycleCnt_byREQ + 1.U }
  }

  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_total", _is_stalling)
  XSPerfHistogram(s"L2RBE_${gen.className}_stallCycleCnt", perf_stallCycleCnt, io.out.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram(s"L2RBE_${gen.className}_stallCycleCnt", perf_stallCycleCnt, io.out.fire, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byDirectory_total", _is_stalling && blockByDirectory)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byEVT_total", _is_stalling && io.blockFromVPipe.EVT)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_bySNP_total", _is_stalling && io.blockFromVPipe.SNP)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byREQ_total", _is_stalling && io.blockFromVPipe.REQ)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byDirectory", perf_stallCycleCnt_byDirectory, io.out.fire, 0, 40, 2, right_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byDirectory", perf_stallCycleCnt_byDirectory, io.out.fire, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byEVT", perf_stallCycleCnt_byEVT, io.out.fire, 0, 40, 2, right_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byEVT", perf_stallCycleCnt_byEVT, io.out.fire, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_bySNP", perf_stallCycleCnt_bySNP, io.out.fire, 0, 40, 2, right_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_bySNP", perf_stallCycleCnt_bySNP, io.out.fire, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byREQ", perf_stallCycleCnt_byREQ, io.out.fire, 0, 40, 2, right_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_stallCycleCnt_byREQ", perf_stallCycleCnt_byREQ, io.out.fire, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2RBE_${gen.className}_issueCnt", io.out.fire)
}
