package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.l2._
import oceanus.l2.L2TSHR._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.SeqBoolBitwiseOps
import freechips.rocketchip.util.SeqToAugmentedSeq
import oceanus.compactchi.CCHIOpcode._


class L2TSHRTarget extends Bundle {
  val EVT = Bool()
  val SNP = Bool()
  val REQ = Bool()
}

object L2TSHRTarget {
  def empty = {
    val t = Wire(new L2TSHRTarget)
    t.EVT := false.B
    t.SNP := false.B
    t.REQ := false.B
    t
  }
  def asEVT = {
    val t = WireInit(empty)
    t.EVT := true.B
    t
  }
  def asSNP = {
    val t = WireInit(empty)
    t.SNP := true.B
    t
  }
  def asREQ = {
    val t = WireInit(empty)
    t.REQ := true.B
    t
  }
}

case class L2TSHRAllocTarget(val name: String, val priority: Int) // Allocation Target

object L2TSHRAllocTarget {
  val EVT = L2TSHRAllocTarget("EVT", 2)
  val SNP = L2TSHRAllocTarget("SNP", 1)
  val REQ = L2TSHRAllocTarget("REQ", 0)
}

case class L2TSHRResvTarget(val name: String) // Reservation Target

object L2TSHRResvTarget {
  val L1EVT = L2TSHRResvTarget("L1EVT")
  val L2EVT = L2TSHRResvTarget("L2EVT")
  val L3SNP = L2TSHRResvTarget("L3SNP")
}

class L2TSHRAllocConfig(val cluster: Seq[Seq[L2TSHRAllocTarget]], val resv: Seq[(Int, L2TSHRResvTarget)])(implicit val p: Parameters) extends HasL2Params {
  // cluster: allocation clusters, each entry is a sequence of allocation targets that belong to the same cluster
  // resv: reservation configuration, each entry is a tuple of (target index, reservation target)
  require(cluster.nonEmpty, "Cluster config should not be empty")
  cluster.foreach(c => require(c.nonEmpty, s"Each cluster should have at least one allocation target, current: ${cluster}"))
  require(cluster.flatten.size == cluster.flatten.toSet.size, s"Allocation Targets should be unique, current: ${cluster}")
  require(cluster.flatten.size == 3, s"All Allocation Targets should be covered, current: ${cluster}")
  require(resv.size == resv.map(_._1).toSet.size, s"Reservation Targets should be unique, current: ${resv}")
  resv.map(_._1).foreach(i => require(i < paramL2.mshrSize, s"Reservation target index out of bounds (mshrSize = ${paramL2.mshrSize}), current: ${resv}"))
}

object L2TSHRAlloc {

  class PathFromTSHR(implicit val p: Parameters) extends Bundle with HasL2Params {
    val paddr = UInt(paramL2.physicalAddrWidth.W)
    val busy = new L2TSHRTarget
  }

  class PathToTSHR(implicit val p: Parameters) extends Bundle with HasL2Params {
    val paddr = UInt(paramL2.physicalAddrWidth.W)
    val alloc = new L2TSHRTarget
    val reuse = new L2TSHRTarget
  }

  class PathFromTSHRCtrl(implicit val p: Parameters) extends Bundle with HasL2Params {
    val RXEVT = Flipped(Decoupled(new FlitEVT))
    val RXSNP = Flipped(Decoupled(new CHIBundleSNP))
    val RXREQ = Flipped(Decoupled(new FlitREQ))
  }
}

class L2TSHRAlloc(val config: L2TSHRAllocConfig)(implicit val p: Parameters) extends Module with HasL2Params {

  // TODO: move this into common utilities
  def arb[T <: Bundle](in: Seq[DecoupledIO[T]], out: DecoupledIO[T], name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) { a <> req }
    out <> arb.io.out
  }

  val clusterCount = config.cluster.size
  val clusterName = config.cluster.zipWithIndex.map { case (cluster, idx) => s"cluster_${idx}_${cluster.map(_.name).mkString("_")}" }

  val io = IO(new Bundle {

    val fromTSHR = Flipped(Vec(paramL2.mshrSize, Valid(new L2TSHRAlloc.PathFromTSHR)))
    val toTSHR = Output(Vec(paramL2.mshrSize, new L2TSHRAlloc.PathToTSHR))

    val fromTSHRCtrl = new L2TSHRAlloc.PathFromTSHRCtrl
  })

  class ClusterBundle extends Bundle {
    val paddr = UInt((paramL2.physicalAddrWidth).W)
    val opcode = UInt(Seq(io.fromTSHRCtrl.RXEVT.bits.Opcode.getWidth, io.fromTSHRCtrl.RXSNP.bits.Opcode.get.getWidth, io.fromTSHRCtrl.RXREQ.bits.Opcode.getWidth).max.W)
    val target = new L2TSHRTarget
  }

  val preclusterRXEVT = Wire(Decoupled(new ClusterBundle))
  preclusterRXEVT.valid := io.fromTSHRCtrl.RXEVT.valid
  preclusterRXEVT.bits.paddr := io.fromTSHRCtrl.RXEVT.bits.Addr
  preclusterRXEVT.bits.opcode := io.fromTSHRCtrl.RXEVT.bits.Opcode
  preclusterRXEVT.bits.target := L2TSHRTarget.asEVT
  io.fromTSHRCtrl.RXEVT.ready := preclusterRXEVT.ready

  val preclusterRXSNP = Wire(Decoupled(new ClusterBundle))
  preclusterRXSNP.valid := io.fromTSHRCtrl.RXSNP.valid
  preclusterRXSNP.bits.paddr := io.fromTSHRCtrl.RXSNP.bits.Addr.get << 3
  preclusterRXSNP.bits.opcode := io.fromTSHRCtrl.RXSNP.bits.Opcode.get
  preclusterRXSNP.bits.target := L2TSHRTarget.asSNP
  io.fromTSHRCtrl.RXSNP.ready := preclusterRXSNP.ready

  val preclusterRXREQ = Wire(Decoupled(new ClusterBundle))
  preclusterRXREQ.valid := io.fromTSHRCtrl.RXREQ.valid
  preclusterRXREQ.bits.paddr := io.fromTSHRCtrl.RXREQ.bits.Addr
  preclusterRXREQ.bits.opcode := io.fromTSHRCtrl.RXREQ.bits.Opcode
  preclusterRXREQ.bits.target := L2TSHRTarget.asREQ
  io.fromTSHRCtrl.RXREQ.ready := preclusterRXREQ.ready

  val postcluster = Wire(Vec(clusterCount, Decoupled(new ClusterBundle)))

  config.cluster.zipWithIndex.map { case (cluster, idx) => {

    val clustered = cluster.sortWith(_.priority > _.priority).map { _ match {
      case L2TSHRAllocTarget.EVT => preclusterRXEVT
      case L2TSHRAllocTarget.SNP => preclusterRXSNP
      case L2TSHRAllocTarget.REQ => preclusterRXREQ
      case _ => { 
        require(false, "Illegal Allocation Target in cluster config")
        preclusterRXEVT // unused
      }
    }}

    if (cluster.size > 1)
      arb(clustered, postcluster(idx), Some(s"${clusterName(idx)}"))
    else if (cluster.size == 1)
      postcluster(idx) <> clustered.head
    else {
      postcluster(idx).valid := false.B
      postcluster(idx).bits := 0.U.asTypeOf(postcluster(idx).bits)
    }
  }}

  val postcluster_isL1EVT = postcluster.map(p => p.bits.target.EVT)
  val postcluster_isL2EVT = postcluster.map(p => p.bits.target.REQ && p.bits.opcode === EvictBack.U)
  val postcluster_isL3SNP = postcluster.map(p => p.bits.target.SNP)

  val postcluster_paddr_match_mat = Wire(Vec(clusterCount, Vec(clusterCount, Bool())))
  (0 until clusterCount).foreach (c1Idx => 
    (0 until clusterCount).foreach (c2Idx =>
      postcluster_paddr_match_mat(c1Idx)(c2Idx) := {
        if (c1Idx > c2Idx) postcluster(c1Idx).bits.paddr === postcluster(c2Idx).bits.paddr
        else if (c1Idx < c2Idx) postcluster_paddr_match_mat(c2Idx)(c1Idx)
        else true.B
  }))

  val paddr_hit_vec = postcluster.map(p => io.fromTSHR.map(t => t.valid && p.valid && (t.bits.paddr >> 6) === (p.bits.paddr >> 6)))
  val paddr_hit_any = paddr_hit_vec.map(ParallelOR(_))

  paddr_hit_vec.zipWithIndex.foreach { case (hit, idx) =>
    assert(PopCount(hit) <= 1.U, s"Multiple TSHRs hit the same post-cluster PA at ${clusterName(idx)}")
  }

  val alloc_vec = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))
  val reuse_vec = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))

  val can_alloc_vec = (0 until clusterCount).map(cIdx => io.fromTSHR.zipWithIndex.map { case (t, tIdx) => 
    !t.valid && (config.resv.find(_._1 == tIdx) match {
      case Some((_, L2TSHRResvTarget.L1EVT)) => postcluster_isL1EVT(cIdx)
      case Some((_, L2TSHRResvTarget.L2EVT)) => postcluster_isL2EVT(cIdx)
      case Some((_, L2TSHRResvTarget.L3SNP)) => postcluster_isL3SNP(cIdx)
      case Some(_) => {
        require(false, "Illegal Allocation Reservation in cluster config")
        false.B // unused
      }
      case None => true.B
    }) && /* conflict mask from other clusters */ !{
      if (cIdx == 0)
        false.B
      else
        ParallelOR((0 until cIdx).map { c2Idx =>
          alloc_vec(c2Idx)(tIdx) && !postcluster_paddr_match_mat(cIdx)(c2Idx) })
    }
  })

  val alloc_vec_mask = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))
  alloc_vec_mask.zipWithIndex.foreach { case (mask, cIdx) => 
    mask.head := false.B
    mask.zipWithIndex.tail.foreach { case (m, tIdx) => m := mask(tIdx - 1) || can_alloc_vec(cIdx)(tIdx - 1) }
  }

  val can_reuse_vec = (0 until clusterCount).map(cIdx => io.fromTSHR.zipWithIndex.map { case (t, tIdx) => 
    paddr_hit_vec(cIdx)(tIdx) && (
      !t.bits.busy.EVT && postcluster(cIdx).bits.target.EVT ||
      !t.bits.busy.SNP && postcluster(cIdx).bits.target.SNP ||
      !t.bits.busy.REQ && postcluster(cIdx).bits.target.REQ)
  })

  val can_reuse_any = can_reuse_vec.map(ParallelOR(_))

  val reuse_vec_mask = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))
  reuse_vec_mask.zipWithIndex.foreach { case (mask, cIdx) =>
    mask.head := false.B
    mask.zipWithIndex.tail.foreach { case (m, tIdx) => m := mask(tIdx - 1) || can_reuse_vec(cIdx)(tIdx - 1) }
  }

  alloc_vec.zipWithIndex.foreach { case (alloc_vec, cIdx) =>
    alloc_vec.zipWithIndex.foreach { case (alloc, tIdx) =>
      alloc := can_alloc_vec(cIdx)(tIdx) && !alloc_vec_mask(cIdx)(tIdx) && !paddr_hit_any(cIdx) && postcluster(cIdx).valid }
    assert(PopCount(alloc_vec) <= 1.U, s"Multiple TSHR allocation on ${clusterName(cIdx)}")
  }

  reuse_vec.zipWithIndex.foreach { case (reuse_vec, cIdx) =>
    reuse_vec.zipWithIndex.foreach { case (reuse, tIdx) => 
      reuse := can_reuse_vec(cIdx)(tIdx) && !reuse_vec_mask(cIdx)(tIdx) }
    assert(PopCount(reuse_vec) <= 1.U, s"Multiple TSHR reuse on ${clusterName(cIdx)}")
  }

  val alloc_any = alloc_vec.map(_.asUInt.orR)
  val reuse_any = reuse_vec.map(_.asUInt.orR)

  alloc_any.zip(reuse_any).zipWithIndex.foreach { case ((a, r), cIdx) =>
    assert(!a || !r, s"Both allocating and reusing TSHRs on ${clusterName(cIdx)}")
  }

  postcluster.zipWithIndex.foreach { case (p, cIdx) => p.ready := alloc_any(cIdx) || reuse_any(cIdx) }

  config.cluster.zipWithIndex.foreach { case (cluster, cIdx) => {
    cluster.foreach { _ match {
      case L2TSHRAllocTarget.EVT => {
        io.toTSHR.map(_.alloc).zip(alloc_vec(cIdx)).foreach(t => t._1.EVT := t._2 && postcluster(cIdx).bits.target.EVT)
        io.toTSHR.map(_.reuse).zip(reuse_vec(cIdx)).foreach(t => t._1.EVT := t._2 && postcluster(cIdx).bits.target.EVT)
      }
      case L2TSHRAllocTarget.SNP => {
        io.toTSHR.map(_.alloc).zip(alloc_vec(cIdx)).foreach(t => t._1.SNP := t._2 && postcluster(cIdx).bits.target.SNP)
        io.toTSHR.map(_.reuse).zip(reuse_vec(cIdx)).foreach(t => t._1.SNP := t._2 && postcluster(cIdx).bits.target.SNP)
      }
      case L2TSHRAllocTarget.REQ => {
        io.toTSHR.map(_.alloc).zip(alloc_vec(cIdx)).foreach(t => t._1.REQ := t._2 && postcluster(cIdx).bits.target.REQ)
        io.toTSHR.map(_.reuse).zip(reuse_vec(cIdx)).foreach(t => t._1.REQ := t._2 && postcluster(cIdx).bits.target.REQ)
      }
      case _ => {}
  }}}}

  assert((PopCount(io.toTSHR.map(_.alloc).asUInt) + PopCount(io.toTSHR.map(_.reuse).asUInt)) <= clusterCount.U, 
    "Allocated and reused TSHRs more than cluster count")

  assert(PopCount(io.toTSHR.map(_.alloc.EVT).asUInt) <= 1.U, "Multiple TSHR allocated for EVT within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.alloc.SNP).asUInt) <= 1.U, "Multiple TSHR allocated for SNP within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.alloc.REQ).asUInt) <= 1.U, "Multiple TSHR allocated for REQ within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.reuse.EVT).asUInt) <= 1.U, "Multiple TSHR reused for EVT within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.reuse.SNP).asUInt) <= 1.U, "Multiple TSHR reused for SNP within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.reuse.REQ).asUInt) <= 1.U, "Multiple TSHR reused for REQ within 1 cycle")

  io.toTSHR.zipWithIndex.foreach { case (t, tIdx) =>
    t.paddr := ParallelMux(alloc_vec.map(_(tIdx)).zip(reuse_vec.map(_(tIdx))).zipWithIndex.map { 
      case ((alloc, reuse), cIdx) => {
        (alloc, postcluster(cIdx).bits.paddr, cIdx) /*'paddr' output valid only on allocation*/ 
    }}.map { case (en, paddr, cIdx) => {
      assert(!en || paddr === t.paddr, s"Disrupted PA output to TSHR ${tIdx} on ${clusterName(cIdx)}")
      (en, paddr)
    }}
  )}

  
  // performance counters
  val perf_stallCycleCnt_EVT = RegInit(0.U(32.W))
  val perf_stallCycleCnt_SNP = RegInit(0.U(32.W))
  val perf_stallCycleCnt_REQ = RegInit(0.U(32.W))
  when (io.fromTSHRCtrl.RXEVT.fire) {
    perf_stallCycleCnt_EVT := 0.U
  }.elsewhen (io.fromTSHRCtrl.RXEVT.valid && !io.fromTSHRCtrl.RXEVT.ready) {
    perf_stallCycleCnt_EVT := perf_stallCycleCnt_EVT + 1.U
  }
  when (io.fromTSHRCtrl.RXSNP.fire) {
    perf_stallCycleCnt_SNP := 0.U
  }.elsewhen (io.fromTSHRCtrl.RXSNP.valid && !io.fromTSHRCtrl.RXSNP.ready) {
    perf_stallCycleCnt_SNP := perf_stallCycleCnt_SNP + 1.U
  }
  when (io.fromTSHRCtrl.RXREQ.fire) {
    perf_stallCycleCnt_REQ := 0.U
  }.elsewhen (io.fromTSHRCtrl.RXREQ.valid && !io.fromTSHRCtrl.RXREQ.ready) {
    perf_stallCycleCnt_REQ := perf_stallCycleCnt_REQ + 1.U
  }

  XSPerfAccumulate("L2TSHRAlloc_alloc_EVT", io.toTSHR.map(_.alloc.EVT).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_alloc_SNP", io.toTSHR.map(_.alloc.SNP).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_alloc_REQ", io.toTSHR.map(_.alloc.REQ).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_reuse_EVT", io.toTSHR.map(_.reuse.EVT).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_reuse_SNP", io.toTSHR.map(_.reuse.SNP).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_reuse_REQ", io.toTSHR.map(_.reuse.REQ).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_stallCycleCnt_EVT_total", io.fromTSHRCtrl.RXEVT.valid && !io.fromTSHRCtrl.RXEVT.ready)
  XSPerfAccumulate("L2TSHRAlloc_stallCycleCnt_SNP_total", io.fromTSHRCtrl.RXSNP.valid && !io.fromTSHRCtrl.RXSNP.ready)
  XSPerfAccumulate("L2TSHRAlloc_stallCycleCnt_REQ_total", io.fromTSHRCtrl.RXREQ.valid && !io.fromTSHRCtrl.RXREQ.ready)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_EVT", perf_stallCycleCnt_EVT, io.fromTSHRCtrl.RXEVT.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_EVT", perf_stallCycleCnt_EVT, io.fromTSHRCtrl.RXEVT.fire, 40, 800, 40, left_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_SNP", perf_stallCycleCnt_SNP, io.fromTSHRCtrl.RXSNP.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_SNP", perf_stallCycleCnt_SNP, io.fromTSHRCtrl.RXSNP.fire, 40, 800, 40, left_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_REQ", perf_stallCycleCnt_REQ, io.fromTSHRCtrl.RXREQ.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_REQ", perf_stallCycleCnt_REQ, io.fromTSHRCtrl.RXREQ.fire, 40, 800, 40, left_strict = true)
}


class L2TSHRCtrl(implicit val p: Parameters) extends Module with HasL2Params {

  val tshrs = Seq.tabulate(paramL2.mshrSize)(i => Module(new L2TSHR(i)))

}
