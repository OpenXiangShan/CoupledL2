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
  val allocMSHR_s3 = ValidIO(UInt(mshrBits.W))
  val metaW_s3 = ValidIO(new MetaWrite)
}

class CPL2S3Info(implicit p: Parameters) extends L2Bundle {
  val mshrTask = Bool()

  val channel = UInt(3.W)
  val opcode = UInt(3.W)
  val tag = UInt(tagBits.W)
  val sset = UInt(setBits.W) // set is C++ common word

  val dirHit = Bool()
  val dirWay = UInt(wayBits.W)
  val allocValid = Bool()
  val allocPtr = UInt(mshrBits.W)
  val mshrId = UInt(mshrBits.W)

  val metaWvalid = Bool()
  val metaWway = UInt(wayBits.W)
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

  /* ======== ChiselDB ======== */
//  assert(cacheParams.hartIds.length == 1, "private L2 should have one and only one hardId")

  val hartId = if(cacheParams.hartIds.length == 1) cacheParams.hartIds.head else 0
  val table = ChiselDB.createTable(s"L2MP", new CPL2S3Info, basicDB = true)
  val s3Info = Wire(new CPL2S3Info)
  s3Info.mshrTask := req_s3.mshrTask
  s3Info.channel := req_s3.channel
  s3Info.opcode := req_s3.opcode
  s3Info.tag := req_s3.tag
  s3Info.sset := req_s3.set
  s3Info.dirHit := dirResult_s3.hit
  s3Info.dirWay := dirResult_s3.way
  s3Info.allocValid := mp.allocMSHR_s3.valid
  s3Info.allocPtr := mp.allocMSHR_s3.bits
  s3Info.mshrId := req_s3.mshrId
  s3Info.metaWvalid := mp.metaW_s3.valid
  s3Info.metaWway := OHToUInt(mp.metaW_s3.bits.wayOH)

  table.log(s3Info, s3_valid, s"L2${hartId}_${p(SliceIdKey)}", clock, reset)
}
