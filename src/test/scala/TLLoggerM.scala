package coupledL2

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import huancun._
import coupledL2.prefetch._
import coupledL2.tl2tl._
import utility._

// one TLLoggerM for each MClient
class TLLoggerM(name: String, enable: Boolean)(implicit p: Parameters) extends Module {
  val edgeIn = p(EdgeOutKey) // edgeIn for TLLoggerM is edgeOut for TLClient-Matrix

  val io = IO(new Bundle {
    val a = Flipped(ValidIO(new TLBundleA(edgeIn.bundle)))
    val mdata_out = Flipped(ValidIO(new MatrixDataBundle()))
  })

  if (enable) {
    val table = ChiselDB.createTable("TLLog", new TLLog, basicDB = true)
    val m = io.mdata_out

    val numClients = edgeIn.client.endSourceId
    // Get -> MdataOut
    val a_d_addrs = Reg(Vec(numClients, UInt(edgeIn.bundle.addressBits.W)))
    when(io.a.valid) {
      a_d_addrs(io.a.bits.source) := io.a.bits.address
    }

    val log = WireInit(0.U.asTypeOf(new TLLog))
    log.channel := 5.U // a-e: 0-4
    log.opcode := 8.U  // whatever
    log.param := 0.U   // whatever
    log.source := m.bits.sourceId
    log.sink := 0.U    // whatever
    log.address := a_d_addrs(m.bits.sourceId)
    log.data := m.bits.data.asUInt(511, 256).asTypeOf(log.data) //IVYTODO:! logdata is 256b, mdata is 512b
    log.user := 0.U    // whatever
    log.echo := 0.U    // whatever

    table.log(log, m.valid, name, clock, reset)
  }
}

//class TLLoggerM(name: String, enable: Boolean)(implicit p: Parameters) extends LazyModule {
//  val node = TLAdapterNode()
//
//  // Ensure LazyModuleImp is instantiated correctly
//  lazy val module = new LazyModuleImp(this) {
//    val mdata_out = IO(Vec(l2banks, Flipped(DecoupledIO(new MatrixDataBundle()))))
//
//    if (enable) {
//      val table = ChiselDB.createTable("TLLog", new TLLog, basicDB = true)
//
//      for (((in, edgeIn), m) <- node.in.zip(mdata_out)) {
//        val numClients = edgeIn.client.endSourceId
//        val a_d_addrs = Reg(Vec(numClients, UInt(edgeIn.bundle.addressBits.W)))
//        when(in.a.fire) {
//          a_d_addrs(in.a.bits.source) := in.a.bits.address
//        }
//
//        val log = WireInit(0.U.asTypeOf(new TLLog))
//        log.channel := 5.U
//        log.opcode := 8.U
//        log.param := 0.U
//        log.source := m.bits.sourceId
//        log.sink := 0.U
//        log.address := a_d_addrs(m.bits.sourceId)
//        log.data := m.bits.data.asUInt(511, 256).asTypeOf(log.data)
//        log.user := 0.U
//        log.echo := 0.U
//
//        table.log(log, m.fire, name, clock, reset)
//      }
//    }
//  }
//}
