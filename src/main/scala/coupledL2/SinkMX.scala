package coupledL2

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.utils._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLPermissions._

// IN: a, c
// OUT: a, c
// -----------------------
// | IN | OPCODE  | OUT |
// =======================
// | A  | Acquire | A   |
// | A  | Get     | A   |
// | A  | Put     | C   |
// | C  | Release | C   |  higher priv
// -----------------------
class SinkMX(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // in
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val c = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    // out
    val out_a = DecoupledIO(new TLBundleA(edgeIn.bundle))
    val out_c = DecoupledIO(new TLBundleC(edgeIn.bundle))
  })

  val matrix_key = io.a.bits.user.lift(MatrixKey).getOrElse(0.U)
  val matrix_req = matrix_key(0)
  val modify_flag = matrix_key(1)

  def isMatrixPut(a: TLBundleA): Bool = {
    (a.opcode === PutFullData || a.opcode === PutPartialData) && matrix_req
  }

  def isMatrixGet(a: TLBundleA): Bool = {
    a.opcode === Get && matrix_req
  }

  val out_a = WireInit(io.a)
  val out_c = WireInit(io.c)

  val a = io.a.bits
  val c = io.c.bits

  // Handle MatrixGet
  when(isMatrixGet(a) && io.a.valid) {
    out_a.bits.opcode := io.a.bits.opcode//Get Or AcquireBlock
    // although TileLink requires Get's param fixed 0 (NtoB),
    // here we specifically design Get with NtoT, which also triggers needT
    out_a.bits.param := Mux(modify_flag, NtoT, NtoB)
  }

  // Handle MatrixPut
  when(isMatrixPut(a) && io.a.valid) {
    when(!io.c.valid) {
      // If c is not valid, convert the request to ReleaseData
      out_c.bits.opcode := ReleaseData
      out_c.bits.param := TtoN // Set appropriate parameters for ReleaseData
      out_c.bits.data := a.data // Use data from the A channel
      out_c.bits.address := a.address // Use address from the A channel
      out_c.bits.size := a.size
      out_c.bits.source := a.source
      out_c.bits.corrupt := a.corrupt
    //   out_c.bits.user(VaddrKey) := a.address
      out_c.valid := true.B
      out_c.bits.user.lift(MatrixKey).foreach(_ := "b01".U)
      out_a.valid := false.B
    //   io.a.ready := false.B // Hold the A channel until the C channel is processed
    }
  }

  // Connect output signals
  io.out_a <> out_a
  io.out_c <> out_c

  // Handle ready signals
  // io.a.ready := out_a.ready
  io.c.ready := out_c.ready

  // TODO: it is not recommended to use input valid to drive input ready,
  // might cause longer path, unfriendly to timing
  io.a.ready := Mux(isMatrixPut(a), io.out_c.ready && !io.c.valid, io.out_a.ready)

}