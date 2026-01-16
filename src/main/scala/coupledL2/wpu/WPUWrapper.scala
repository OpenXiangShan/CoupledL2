package coupledL2.wpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{RegNextN, XSPerfAccumulate}
import coupledL2._
import freechips.rocketchip.util.{BundleField, BundleFieldBase, BundleKeyBase, ControlKey}
import chisel3.experimental.requireIsChiselType
import chisel3.experimental.dataview._
import utility.GTimer

class WPUWrapper(implicit p:Parameters) extends L2Module {
  val wpu = AlgoWPUMap(wpuParam)
  val in = IO(Flipped(new Bundle {
    val read = ValidIO(UInt(fullAddressBits.W))
    val update = ValidIO(new WPUUpdate)
  }))
  val out = IO(new Bundle {
    val res = Valid(new WPUResult())
  })

  val cancelPred = Wire(Bool())
  out.res.valid := RegNext(in.read.valid)
  out.res.bits.viewAsSupertype(new WPUResultInsideWPU) := wpu.out.res
  out.res.bits.canceld := RegEnable(cancelPred, in.read.valid)
  out.res.bits.set.foreach(_ := RegNext(wpu.in.read.bits.set))
  out.res.bits.tag.foreach(_ := RegNext(wpu.in.read.bits.tag))
  out.res.bits.timeCnt.foreach(_ := RegNext(GTimer()))

  wpu.in.read.valid := in.read.valid && !cancelPred
  wpu.in.read.bits.parseAddr(in.read.bits)

  class UpdQueue[T <: Data](val gen: T, val entries: Int) extends Module {
    val io = IO(new Bundle {
      val enq = Flipped(EnqIO(gen))
      val deq = Flipped(DeqIO(gen))
      val data = Vec(entries, Valid(gen))
      val full = Bool()
    })
    val ram = Reg(Vec(entries, gen))
    val valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
    val enq_ptr, deq_ptr = Counter(entries)
    val maybe_full = RegInit(false.B)
    val ptr_match = enq_ptr.value === deq_ptr.value
    val empty = ptr_match && !maybe_full
    val full = ptr_match && maybe_full
    val do_enq = WireDefault(io.enq.fire)
    val do_deq = WireDefault(io.deq.fire)

    when(do_enq) {
      ram(enq_ptr.value) := io.enq.bits
      valids(enq_ptr.value) := true.B
      enq_ptr.inc()
    }
    when(do_deq) {
      valids(deq_ptr.value) := false.B
      deq_ptr.inc()
    }
    when(do_enq =/= do_deq) {
      maybe_full := do_enq
    }

    io.deq.valid := !empty
    io.enq.ready := io.deq.ready || !full

    io.deq.bits := ram(deq_ptr.value)

    (ram zip valids zip io.data).foreach { case ((r, valid), data) =>
      data.valid := valid
      data.bits := r
    }
    io.full := full
  }

  val updQueue = Module(new UpdQueue(new WPUUpdate, if (wpuParam.debug) 16 else 2))
  updQueue.io.enq.valid := in.update.valid
  updQueue.io.enq.bits := in.update.bits
  updQueue.io.deq.ready := wpu.in.upd.ready
  wpu.in.upd.valid := updQueue.io.deq.valid
  wpu.in.upd.bits := updQueue.io.deq.bits
  cancelPred := updQueue.io.data.map(x => x.valid && x.bits.set === wpu.in.read.bits.set).reduce(_ || _) ||
    updQueue.io.full

  assert(updQueue.io.enq.ready || !in.update.valid)
  XSPerfAccumulate("updQueue_overflow", !updQueue.io.enq.ready && in.update.valid)

  // Perf
  val pred_bits = wpu.in.upd.bits
  val pred_valid = wpu.in.upd.fire && !wpu.in.upd.bits.canceld.getOrElse(false.B) && !pred_bits.isEvict & !pred_bits.isReplace
  val pred_canceld = wpu.in.upd.fire && wpu.in.upd.bits.canceld.getOrElse(false.B) && !pred_bits.isEvict & !pred_bits.isReplace
  val predhit_succ = pred_valid & pred_bits.actualHit & pred_bits.predHit & pred_bits.actualWay === pred_bits.predWay
  val predmiss_succ = pred_valid & !pred_bits.actualHit & !pred_bits.predHit
  val pred_succ = predhit_succ | predmiss_succ
  val pred_unmatch = pred_valid & pred_bits.actualHit & pred_bits.predHit & pred_bits.actualWay =/= pred_bits.predWay
  val pred_miss_but_hit = pred_valid & !pred_bits.predHit & pred_bits.actualHit
  val pred_hit_but_miss = pred_valid & pred_bits.predHit & !pred_bits.actualHit
  val pred_fail = pred_unmatch | pred_miss_but_hit | pred_hit_but_miss
  val repl = wpu.in.upd.fire & pred_bits.isReplace
  val evict = wpu.in.upd.fire & pred_bits.isEvict

  XSPerfAccumulate(s"WPU_pred_times", pred_valid)
  XSPerfAccumulate(s"WPU_pred_canceld", pred_canceld)
  XSPerfAccumulate(s"WPU_pred_succ", pred_succ)
  XSPerfAccumulate(s"WPU_pred_fail", pred_fail)
  XSPerfAccumulate(s"WPU_predhit_succ", predhit_succ)
  XSPerfAccumulate(s"WPU_predmiss_succ", predmiss_succ)
  XSPerfAccumulate(s"WPU_unmatch", pred_unmatch)
  XSPerfAccumulate(s"WPU_pmiss_ahit", pred_miss_but_hit)
  XSPerfAccumulate(s"WPU_phit_amiss", pred_hit_but_miss)
  XSPerfAccumulate(s"WPU_repl", repl)
  XSPerfAccumulate(s"WPU_evict", evict)

  if (wpuParam.debug) {
    val updTimeVec = RegInit(VecInit(Seq.fill(cacheParams.sets)((0.U(64.W)))))
    val updTimeFlag = if (wpuParam.updOnLookup) { wpu.in.upd.fire } else
      { wpu.in.upd.fire && (wpu.in.upd.bits.isEvict || wpu.in.upd.bits.isReplace) }
    when (updTimeFlag) {
      updTimeVec(wpu.in.upd.bits.set) := GTimer()
    }
    val pred_fail_cause = pred_fail && pred_bits.timeCnt.get <= updTimeVec(pred_bits.set)
    XSPerfAccumulate("WPU_pred_fail_caused_by_late_upd", pred_fail_cause)
    // assert(Mux(pred_valid, pred_succ | pred_fail && pred_fail_cause, true.B))
  }
}
