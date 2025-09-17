package coupledL2

import coupledL2.utils._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import org.chipsalliance.cde.config._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import chisel3.simulator.ChiselSim
import chisel3.simulator.HasSimulator
import svsim.verilator.Backend.CompilationSettings.TraceStyle
import svsim.verilator.Backend.CompilationSettings.TraceKind

object TestSplittedSRAM extends App with ChiselSim {
  val dataw = 24
  val nsets = 32
  val nways = 18
  val ntests = 16
  val dumpVcd = false
//  println(getVerilogString(new SplittedSRAM(UInt(dataw.W), sets = nsets, ways = nways,
//    setSplit = 2, waySplit = 3, dataSplit = 4, shouldReset = false, singlePort = true)))
  implicit val sim: chisel3.simulator.HasSimulator = if (dumpVcd)
    HasSimulator.simulators.verilator(
      verilatorSettings = svsim.verilator.Backend.CompilationSettings(
        traceStyle = Some(TraceStyle(TraceKind.Vcd)),
        outputSplit = Some(30000),
        outputSplitCFuncs = Some(30000),
        disabledWarnings = Seq("STMTDLY", "WIDTH")
      )
    )
  else
    HasSimulator.default

  simulate(
    new SplittedSRAM(UInt(dataw.W), set = nsets, way = nways, setSplit = 2, waySplit = 3, dataSplit = 4,
      shouldReset = false, singlePort = true))
  { s =>
    val randomWaymask = new Random(12)
    val randomData = new Random(34)
    val randomSet = new Random(56)
    var correctData = ArrayBuffer.fill(nsets)(ArrayBuffer.fill(nways)(0))
    s.clock.step(2)

    for (i <- 0 until ntests) {
      val data = (0 until nways).map(_ => randomData.nextInt(1 << dataw))
      val waymask = randomWaymask.nextInt(1 << nways)
      val set = randomSet.nextInt(nsets)
      s.io.w.req.valid.poke(true.B)

      s.io.w.req.bits.setIdx.poke(set.U)
      s.io.w.req.bits.waymask.map(_.poke(waymask.U))
      (0 until nways).map {
        w =>
          s.io.w.req.bits.data(w).poke(data(w).U)
          correctData(set)(w) = if ((waymask & (1 << w)) != 0) data(w) else correctData(set)(w)
      }

      s.clock.step(1)
      s.io.w.req.valid.poke(false.B)

      s.clock.step(1)
      s.io.r.req.valid.poke(true.B)
      s.io.r.req.bits.setIdx.poke(set.U)

      s.clock.step(1)
      s.io.r.req.valid.poke(false.B)

      (0 until nways).map {
        w => s.io.r.resp.data(w).expect(correctData(set)(w).U)
      }
      s.clock.step(1)

      println(s"passed $i")
    }
  }
}

/*
# To build a signle test Module
mill:
	mill -i CoupledL2.test.runMain coupledL2.TestSplittedSRAM
*/