package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class RefillAllocReq(implicit val p: Parameters) extends Bundle with HasL2Params {
  val mshrId = UInt(log2Ceil(nMSHR).W)
  val paddr = UInt(paramL2.physicalAddrWidth.W)
  val alias = UInt(2.W)
  val way = UInt(4.W)
  val isDirty = Bool()
  val meta = new L2Directory.Meta
}

class EvictAllocReq(implicit val p: Parameters) extends Bundle with HasL2Params {
  val mshrId = UInt(log2Ceil(nMSHR).W)
  val refillMshrId = UInt(log2Ceil(nMSHR).W)
  val paddr = UInt(paramL2.physicalAddrWidth.W)
  val way = UInt(4.W)
  val meta = new L2Directory.Meta
}

class EvictAllocGnt(implicit val p: Parameters) extends Bundle with HasL2Params {
  val refillMshrId = UInt(log2Ceil(nMSHR).W)
  val evictMshrId = UInt(log2Ceil(nMSHR).W)
}

class EvictDatReadComp(implicit val p: Parameters) extends Bundle with HasL2Params {
  val evictMshrId = UInt(log2Ceil(nMSHR).W)
  val refillMshrId = UInt(log2Ceil(nMSHR).W)
}

class RefillModule(implicit val p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle {
    val refillReq = Flipped(Decoupled(new RefillAllocReq))
    val evictReq = Decoupled(new EvictAllocReq)
    val evictGnt = Flipped(Valid(new EvictAllocGnt))
    val evictDatOut = Flipped(Valid(new EvictDatReadComp))
    val victimDatSafeOut = Output(Bool())
    val mshrId = Input(UInt(log2Ceil(nMSHR).W))
  })

  io.refillReq.ready := true.B
  io.evictReq.valid := false.B
  io.evictReq.bits := 0.U.asTypeOf(new EvictAllocReq)
  io.victimDatSafeOut := RegNext(io.refillReq.fire, false.B) || (io.refillReq.fire && !io.refillReq.bits.isDirty)
}
