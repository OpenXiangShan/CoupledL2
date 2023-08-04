package coupledL2.debug

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam, StringParam}
import coupledL2._
import utility._

trait HasCLikeTypes {
  // c++ firendly data types
  def uint8_t = UInt(8.W)
  def uint32_t = UInt(32.W)
  def uint64_t = UInt(64.W)
}

class DirectoryInfo extends Bundle with HasCLikeTypes {
  val paddr = uint64_t
  val set_idx = uint64_t
  val way = uint8_t
  val hit = uint8_t
  val req_type = uint8_t
}

class DirLogWriter(prefix: String) extends Module {
  val io = IO(Flipped(ValidIO(new DirectoryInfo)))
  DirectoryLogger.table.log(io, prefix, this.clock, this.reset)
}

object DirectoryLogger {
  val table = ChiselDB.createTable("DirLog", new DirectoryInfo, basicDB = true)
  def apply[T <: Data](
    prefix: String
  )(paddr:     UInt,
    set:       UInt,
    way:       UInt,
    hit:       Bool,
    req_type:  UInt,
    wen:       Bool
  )(
    implicit p: Parameters
  ) = {
    //if(p(L2ParamKey).enableDebug){
      val dirLogger = Module(new DirLogWriter(prefix))
      dirLogger.io.bits.paddr := paddr
      dirLogger.io.bits.set_idx := set
      dirLogger.io.bits.way := way
      dirLogger.io.bits.hit := hit
      dirLogger.io.bits.req_type := req_type
      dirLogger.io.valid := wen
    //}
  }
}
