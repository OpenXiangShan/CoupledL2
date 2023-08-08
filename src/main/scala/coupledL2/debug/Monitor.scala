package coupledL2.debug

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2._
import utility._

class MainpipeMoni(implicit p: Parameters) extends L2Bundle {
  val task_s2 = ValidIO(new TaskBundle())
  val task_s3 = ValidIO(new TaskBundle())
  val task_s4 = ValidIO(new TaskBundle())
  val task_s5 = ValidIO(new TaskBundle())
  val dirResult_s3 = new DirResult
}

class Monitor(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val fromMainPipe = Input(new MainpipeMoni())
  })

  val mp = io.fromMainPipe

  // assertion for set blocking
  assert(!(mp.task_s2.bits.set === mp.task_s3.bits.set &&
    mp.task_s2.valid && !mp.task_s2.bits.mshrTask && mp.task_s2.bits.fromA &&
    mp.task_s3.valid && !mp.task_s3.bits.mshrTask && mp.task_s3.bits.fromA))

  assert(!(mp.task_s2.bits.set === mp.task_s4.bits.set &&
    mp.task_s2.valid && !mp.task_s2.bits.mshrTask && mp.task_s2.bits.fromA &&
    mp.task_s4.valid && !mp.task_s4.bits.mshrTask && mp.task_s4.bits.fromA))

}
