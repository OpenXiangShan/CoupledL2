package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import oceanus.l2.L2Common.fastArb

object L2PCreditPool {

  class Entry extends Bundle {
    val pCrdType = UInt(4.W)
    val srcId = UInt(12.W) // TODO: parameterize with CHI node id width
  }
}

class L2PCreditPool(val sliceNum: Int)(implicit val p: Parameters) extends Module with HasL2Params {

  val entryCount = sliceNum * paramL2.mshrSize

  class EmptyBundle extends Bundle

  // TODO: maybe we need a more timing-friendly implementation in future

  val io = IO(new Bundle {
    val pCrdGrant = Flipped(Valid(new L2PCreditPool.Entry))
    val mshrQuery = Vec(sliceNum, Vec(paramL2.mshrSize, Valid(new L2PCreditPool.Entry)))
    val mshrGrant = Output(Vec(sliceNum, Vec(paramL2.mshrSize, Bool())))
  })

  val queue = Module(new Queue(new L2PCreditPool.Entry, entryCount))

  queue.io.enq.valid := io.pCrdGrant.valid
  queue.io.enq.bits := io.pCrdGrant.bits

  val mshrPCrdHitVec = io.mshrQuery.flatten.map { case m => {
    m.valid && queue.io.deq.valid && m.bits.srcId === queue.io.deq.bits.srcId && m.bits.pCrdType === queue.io.deq.bits.pCrdType 
  }}

  val mshrPCrdArbIn = mshrPCrdHitVec.zip(io.mshrGrant.flatten).map { case (hit, grant) => {
    val p = Wire(Decoupled(new EmptyBundle))
    p.valid := hit
    grant := p.ready
    p
  }}

  val mshrPCrdArbOut = {
    val p = Wire(Decoupled(new EmptyBundle))
    p.ready := true.B
    queue.io.deq.ready := p.valid
    p
  }

  fastArb(mshrPCrdArbIn, mshrPCrdArbOut, Some("PCreditPool"))

  assert(!(queue.io.enq.valid && !queue.io.enq.ready), "P-Credit Pool overflow")
}
