package coupledL2.utils
import chisel3._
import chisel3.util._
import utility.XSPerfAccumulate
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
import svsim.CommonCompilationSettings.Timescale.Unit.s
import utility.LockingRRArbiterInit
import utility.FastArbiter
import utility.XSPerfHistogram

object ArbPerf {
    def apply(valids: Seq[Bool], readys: Seq[Bool], outRdy: Bool, name: String)(implicit p: Parameters): Unit = {
        require(valids.size == readys.size)
        XSPerfHistogram(s"${name}_myconflict", PopCount(valids), valids.asUInt.orR, 1, valids.size + 1)
        val n = valids.size
        val a = valids.zip(readys).zipWithIndex.map { case ((v, r), i) =>
            val w = RegInit(0.U(64.W))
            when (v && !r && outRdy) { w := w + 1.U }
            when (v && r) { w := 0.U }
            0.until(n*2).map(x => ((x.U === w && v && r).asUInt, x)) :+ (((w >= (n*2).U && v && r).asUInt, n*2))
        }.reduce { (left, right) =>
            left.zip(right).map { case (a, b) => (a._1 + b._1, a._2) }
        }.foreach { case (cnt, time) =>
            XSPerfAccumulate(s"${name}_mywait_${time}", cnt)
        }
    }

    def apply[T <: Data](arb: LockingRRArbiterInit[T], name: String)(implicit p: Parameters): Unit = {
        apply(arb.io.in.map(_.valid), arb.io.in.map(_.ready), arb.io.out.ready, name)
    }

    def apply[T <: Data](arb: FastArbiter[T], name: String)(implicit p: Parameters): Unit = {
        apply(arb.io.in.map(_.valid), arb.io.in.map(_.ready), arb.io.out.ready, name)
    }
}