package coupledL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._

// Route Matrix PutFullData from A to C so SinkC/MainPipe can reuse the existing data path.
class SinkMX(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val c = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    val out_a = DecoupledIO(new TLBundleA(edgeIn.bundle))
    val out_c = DecoupledIO(new TLBundleC(edgeIn.bundle))
  })

  io.out_a <> io.a
  io.out_c <> io.c

  val matrixKey = io.a.bits.user.lift(MatrixKey).getOrElse(0.U)
  val isMatrixReq = MatrixInfo.isMatrix(matrixKey)
  val isMatrixPutFull = io.a.bits.opcode === PutFullData && isMatrixReq
  val isMatrixPutPartial = io.a.bits.opcode === PutPartialData && isMatrixReq

  assert(!(io.a.valid && isMatrixPutPartial), "Matrix PutPartialData is not supported")

  when (io.a.valid && isMatrixPutFull && !io.c.valid) {
    io.out_c.valid := true.B
    io.out_c.bits.opcode := ReleaseData
    io.out_c.bits.param := TtoN
    io.out_c.bits.size := io.a.bits.size
    io.out_c.bits.source := io.a.bits.source
    io.out_c.bits.address := io.a.bits.address
    io.out_c.bits.data := io.a.bits.data
    io.out_c.bits.corrupt := io.a.bits.corrupt
    io.out_c.bits.user.lift(MatrixKey).foreach(_ := matrixKey)
    io.out_c.bits.user.lift(AmeChannelKey).foreach(_ := io.a.bits.user.lift(AmeChannelKey).getOrElse(0.U))
    io.out_c.bits.user.lift(AmeIndexKey).foreach(_ := io.a.bits.user.lift(AmeIndexKey).getOrElse(0.U))

    io.out_a.valid := false.B
    io.a.ready := io.out_c.ready
    io.c.ready := false.B
  }

  when (io.a.valid && isMatrixPutFull && io.c.valid) {
    io.out_a.valid := false.B
    io.out_c.valid := io.c.valid
    io.out_c.bits := io.c.bits
    io.a.ready := false.B
    io.c.ready := io.out_c.ready
  }
}
