package coupledL2.wpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{RegNextN, XSPerfAccumulate}
import coupledL2._

// TODO: remove the latency later
class WPUWrapper(wpuParam: WPUParameters, updLatency: Int)(implicit p:Parameters) extends L2Module {
  val wpu = AlgoWPUMap(wpuParam)
  val in = IO(Flipped(new Bundle {
    val read = ValidIO(new WPURead())
    val update = ValidIO(new WPUUpdate())
  }))
  val out = IO(new Bundle {
    val res = ValidIO(new WPUResult())
  })

  wpu.in.read.valid := in.read.valid
  wpu.in.read.bits := in.read.bits
  out.res.bits := wpu.out.res.bits
  out.res.valid := in.read.fire

  val upd, upd_tmp = Reg(Valid(new WPUUpdate))
  upd := in.update
  val upd_ready = !in.read.valid

  when (!upd_ready || upd_ready && upd_tmp.valid) {
    upd_tmp := upd
  }

  wpu.in.upd.valid := upd.valid || upd_tmp.valid
  wpu.in.upd.bits := Mux(upd_tmp.valid, upd_tmp.bits, upd.bits)
  assert(!(upd.valid && upd_tmp.valid && !upd_ready))
  // Perf
  val algo_name = wpuParam.algoName

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

  MyPerf(s"WPU_pred_times", pred_valid)
  MyPerf(s"WPU_pred_succ", pred_succ)
  MyPerf(s"WPU_predhit_succ", predhit_succ)
  MyPerf(s"WPU_predmiss_succ", predmiss_succ)
  MyPerf(s"WPU_unmatch", pred_unmatch)
  MyPerf(s"WPU_pmiss_ahit", pred_miss_but_hit)
  MyPerf(s"WPU_phit_amiss", pred_hit_but_miss)
  MyPerf(s"WPU_repl", repl)
  MyPerf(s"WPU_evict", evict)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_pred_times", pred_valid)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_pred_succ", pred_succ)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_predhit_succ", predhit_succ)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_predmiss_succ", predmiss_succ)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_unmatch", pred_unmatch)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_pmiss_ahit", pred_miss_but_hit)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_phit_amiss", pred_hit_but_miss)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_repl", repl)
//  XSPerfAccumulate(s"${algo_name}_${updLatency}WPU_evict", evict)
//
//  DebugAccumulate(s3_valid, "s3_valid")
//  DebugAccumulate(pred_valid, "pred_valid")
//  DebugAccumulate(pred_succ, "pred_succ")
//  DebugAccumulate(predhit_succ, "predhit_succ")
//  DebugAccumulate(predmiss_succ, "predmiss_succ")
//  DebugAccumulate(pred_unmatch, "pred_unmatch")
//  DebugAccumulate(pred_miss_but_hit, "pred_miss_but_hit")
//  DebugAccumulate(pred_hit_but_miss, "pred_hit_but_miss")
//  DebugAccumulate(repl, "repl")
//  DebugAccumulate(evict, "evict")
}
