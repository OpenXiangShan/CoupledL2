package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem.L2
import utility.ParallelOR

class L2TSHRAllocTSHRInfo(implicit val p: Parameters) extends Bundle with HasL2Params {
    val V = Bool()
    val PA = UInt(paramL2.physicalAddrWidth.W)
    val sleep = Bool()
    val head = Bool()
    val tail = Bool()
}

class L2TSHRAllocTSHRUpdate(implicit val p: Parameters) extends Bundle with HasL2Params {
    val V = Valid(Bool())
    val PA = Valid(UInt(paramL2.physicalAddrWidth.W))
    val sleep = Valid(Bool())
    val head = Valid(Bool())
    val tail = Valid(Bool())
    val SCR = Valid(UInt(log2Up(paramL2.mshrSize + 1).W))
}

class L2TSHRAllocInput(implicit val p: Parameters) extends Bundle with HasL2Params {
    val PA = UInt(paramL2.physicalAddrWidth.W)
    val nextFree = Valid(UInt(log2Up(paramL2.mshrSize + 1).W))
}

class L2TSHRAlloc(implicit val p: Parameters) extends Module with HasL2Params {

    val io = IO(new Bundle {
        val fromTSHR = Input(Vec(paramL2.mshrSize, new L2TSHRAllocTSHRInfo()))
        val toTSHR = Output(Vec(paramL2.mshrSize, new L2TSHRAllocTSHRUpdate()))
        val in = Decoupled(Input(new L2TSHRAllocInput()))
    })

    //
    val s1_nextFree = Wire(Vec(paramL2.mshrSize, Bool()))
    s1_nextFree.zipWithIndex.foreach{ case (f, i) => f := io.in.bits.nextFree.valid && (io.in.bits.nextFree.bits === i.U) }

    //
    val s1_PA_hit = io.fromTSHR.map(t => io.in.fire && t.V && t.PA === io.in.bits.PA)

    //
    val s1_PA_hit_any = ParallelOR(s1_PA_hit)

    val s1_PA_hit_head = io.fromTSHR.zip(s1_PA_hit).map { case (t, hit) => t.head && hit }
    val s1_PA_hit_tail = io.fromTSHR.zip(s1_PA_hit).map { case (t, hit) => t.tail && hit }

    // Set 'V' of new TSHR
    val s1_set_V = io.in.fire
    io.toTSHR.map(_.V.valid).zip(s1_nextFree).map { case (en, nextFree) => en := s1_set_V && nextFree }
    io.toTSHR.foreach(_.V.bits := true.B)

    // Set 'sleep' of new TSHR on PA hit
    val s1_set_sleep = s1_PA_hit_any
    io.toTSHR.map(_.sleep.valid).zip(s1_nextFree).map { case (en, nextFree) => en := s1_set_sleep && nextFree }
    io.toTSHR.foreach(_.sleep.bits := true.B)

    // Set 'head' of new TSHR on PA miss
    val s1_set_head = !s1_PA_hit_any
    io.toTSHR.map(_.head.valid).zip(s1_nextFree).map { case (en, nextFree) => en := s1_set_head && nextFree }
    io.toTSHR.foreach(_.head.bits := true.B)

    // Set 'tail' of new TSHR
    val s1_set_tail = io.in.fire
    io.toTSHR.map(_.tail.valid).zip(s1_nextFree).map { case (en, nextFree) => en := s1_set_tail && nextFree }
    io.toTSHR.foreach(_.tail.bits := true.B)

    // Set 'SCR' of previous tail TSHR on PA hit
    val s1_set_SCR = s1_PA_hit_tail
    io.toTSHR.map(_.SCR.valid).zip(s1_set_SCR).map { case (en, set) => en := set }
    io.toTSHR.foreach(_.SCR.bits := io.in.bits.nextFree.bits)

    // Clear 'tail' of previous tail TSHR on PA hit
    val s1_clear_tail = s1_PA_hit_tail
    io.toTSHR.map(_.tail.valid).zip(s1_clear_tail).map { case (en, clr) => en := clr }
    io.toTSHR.foreach(_.tail.bits := false.B)

    //
    io.in.ready := io.in.bits.nextFree.valid

    // TODO
}
