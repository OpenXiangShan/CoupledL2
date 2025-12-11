package coupledL2.wpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{RegNextN, XSPerfAccumulate}
import coupledL2._

// TODO: remove the latency later
class WPUWrapper(wpuParam: WPUParameters)(implicit p:Parameters) extends L2Module {
  val l2wpuInDCache = false
  val wpu = AlgoWPUMap(wpuParam)
  val in = IO(Flipped(new Bundle {
    val read = DecoupledIO(new WPURead)
    val update = ValidIO(new WPUUpdate)
  }))
  val out = IO(new Bundle {
    val res = new WPUResult()
  })

  out.res := wpu.out.res

  if (l2wpuInDCache) {
    wpu.in <> in
  } else {
    wpu.in.read.valid := in.read.valid
    wpu.in.read.bits := in.read.bits
    in.read.ready := true.B

    val updQueue = Module(new Queue(new WPUUpdate, 4, pipe = true, flow = true))
    updQueue.io.enq.valid := in.update.valid
    updQueue.io.enq.bits := in.update.bits
    updQueue.io.deq.ready := !in.read.valid
    wpu.in.upd.valid := updQueue.io.deq.valid
    wpu.in.upd.bits := updQueue.io.deq.bits
    XSPerfAccumulate("updQueue_overflow", !updQueue.io.enq.ready && in.update.valid)
  }

  // Perf
  val s1_valid = in.read.valid
  val s3_valid = in.update.valid
  val s3_upd = in.update.bits
  val pred_valid = s3_valid & !s3_upd.isEvict & !s3_upd.isReplace
  val predhit_succ = pred_valid & s3_upd.actualHit & s3_upd.predHit & s3_upd.actualWay === s3_upd.predWay
  val predmiss_succ = pred_valid & !s3_upd.actualHit & !s3_upd.predHit
  val pred_succ = predhit_succ | predmiss_succ
  val pred_unmatch = pred_valid & s3_upd.actualHit & s3_upd.predHit & s3_upd.actualWay =/= s3_upd.predWay
  val pred_miss_but_hit = pred_valid & !s3_upd.predHit & s3_upd.actualHit
  val pred_hit_but_miss = pred_valid & s3_upd.predHit & !s3_upd.actualHit
  val repl = s3_valid & s3_upd.isReplace
  val evict = s3_valid & s3_upd.isEvict

  XSPerfAccumulate(s"WPU_pred_times", pred_valid)
  XSPerfAccumulate(s"WPU_pred_succ", pred_succ)
  XSPerfAccumulate(s"WPU_predhit_succ", predhit_succ)
  XSPerfAccumulate(s"WPU_predmiss_succ", predmiss_succ)
  XSPerfAccumulate(s"WPU_unmatch", pred_unmatch)
  XSPerfAccumulate(s"WPU_pmiss_ahit", pred_miss_but_hit)
  XSPerfAccumulate(s"WPU_phit_amiss", pred_hit_but_miss)
  XSPerfAccumulate(s"WPU_repl", repl)
  XSPerfAccumulate(s"WPU_evict", evict)
}
