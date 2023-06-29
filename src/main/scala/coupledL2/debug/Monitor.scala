package coupledL2.debug

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import coupledL2._
import coupledL2.MetaData._
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

  val mp            = io.fromMainPipe
  val s3_valid      = mp.task_s3.valid
  val req_s3        = mp.task_s3.bits
  val mshr_req_s3   = req_s3.mshrTask
  val dirResult_s3  = mp.dirResult_s3
  val meta_s3       = mp.dirResult_s3.meta

  /* ======== MainPipe Assertions ======== */
  assert(!(s3_valid && req_s3.fromC && !dirResult_s3.hit),
    "C Release should always hit, Tag %x Set %x",
    req_s3.tag, req_s3.set)

  assert(RegNext(!(s3_valid && !mshr_req_s3 && dirResult_s3.hit &&
    meta_s3.state === TRUNK && !meta_s3.clients.orR)),
    "Trunk should have some client hit")


  // assertion for set blocking
  // make sure we don't send two reqs continuously with the same set
  assert(!(mp.task_s2.bits.set === mp.task_s3.bits.set &&
    mp.task_s2.valid && !mp.task_s2.bits.mshrTask && mp.task_s2.bits.fromA &&
    mp.task_s3.valid && !mp.task_s3.bits.mshrTask && mp.task_s3.bits.fromA),
    "s2 and s3 task same set, failed in blocking")

  assert(!(mp.task_s2.bits.set === mp.task_s4.bits.set &&
    mp.task_s2.valid && !mp.task_s2.bits.mshrTask && mp.task_s2.bits.fromA &&
    mp.task_s4.valid && !mp.task_s4.bits.mshrTask && mp.task_s4.bits.fromA))



}
