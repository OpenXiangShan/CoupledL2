package coupledL2

import freechips.rocketchip.diplomacy.LazyModule

import coupledL2.utils._

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.RawTester.test
import chisel3.experimental._
import chisel3.testers._

import org.chipsalliance.cde.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import scala.collection.mutable.ArrayBuffer
import chiseltest.WriteVcdAnnotation
import chiseltest.WriteFstAnnotation

object TestTPSRAM extends App {

  println(getVerilogString(new SplittedSRAM(UInt(24.W), sets = 32, ways = 18,
    setSplit = 2, waySplit = 3, dataSplit = 4, shouldReset = false, singlePort = true)))

  test(
    new SplittedSRAM(UInt(24.W), sets = 32, ways = 18, setSplit = 2, waySplit = 3, dataSplit = 4, shouldReset = false, singlePort = true),
    Seq(WriteVcdAnnotation))
  { s =>

    for (i <- 0 until 16) {
      s.io.w.req.valid.poke(true.B)

      s.io.w.req.bits.setIdx.poke(0.U)
      s.io.w.req.bits.waymask.map(_.poke((1 << i).U))
      val data = i ^ 2 + 2 * i + 7
      s.io.w.req.bits.data(i).poke(data.U)

      s.clock.step(1)
      s.io.w.req.valid.poke(false.B)

      s.clock.step(1)
      s.io.r.req.valid.poke(true.B)
      s.io.r.req.bits.setIdx.poke(0.U)

      s.clock.step(1)
      s.io.r.req.valid.poke(false.B)

      s.io.r.resp.data(i).expect(data.U)
      s.clock.step(1)

      println(s"passed $i")
    }
  }
}

/*
# To build a signle test Module
mill:
	mill -i CoupledL2.test.runMain coupledL2.TestTPSRAM
*/