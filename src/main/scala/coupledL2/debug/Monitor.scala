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
//  val nestedWBValid = Input(Bool())
  })

  val mp            = io.fromMainPipe
  val s2_valid      = mp.task_s2.valid
  val req_s2        = mp.task_s2.bits
  val s3_valid      = mp.task_s3.valid
  val req_s3        = mp.task_s3.bits
  val mshr_req_s3   = req_s3.mshrTask
  val dirResult_s3  = mp.dirResult_s3
  val meta_s3       = mp.dirResult_s3.meta

  /* ======== MainPipe Assertions ======== */
  // ! Release w/o data will not trigger nestedWBValid, either
  // ! consider using mshrs.map(_.io.nestedwb_match) and passes to Monitor, if necessary
//  val c_notHit = s3_valid && req_s3.fromC && !dirResult_s3.hit
//  val c_noNested = !io.nestedWBValid
//  assert(RegNext(!(c_notHit && c_noNested)),
//    "C Release should always hit or have some MSHR meta nested, Tag %x Set %x",
//    req_s3.tag, req_s3.set)

  assert(RegNext(!(s3_valid && !mshr_req_s3 && dirResult_s3.hit &&
    meta_s3.state === TRUNK && !meta_s3.clients.orR)),
    "Trunk should have some client hit")

  assert(RegNext(!(s3_valid && req_s3.fromC && dirResult_s3.hit &&
    !meta_s3.clients.orR)),
    "Invalid Client should not send Release")

  // assertion for set blocking
  // A channel task @s1 never have same-set task @s2/s3
  // to ensure that meta written can be read by chnTask
//  assert(RegNext(!(mp.task_s2.bits.set === mp.task_s3.bits.set &&
//    s2_valid && !req_s2.mshrTask && s3_valid)),
//    "chnTask-s2 and s3 same set, failed in blocking")
//
//  assert(RegNext(!(mp.task_s2.bits.set === RegNext(mp.task_s3.bits.set) &&
//    s2_valid && !req_s2.mshrTask && RegNext(s3_valid))),
//    "chosen-chnTask-s1 and s3 task same set, failed in blocking")

//   TODO: whether mshrGrant also need such blocking, since it reads dir as well


  /* ======== ChiselDB ======== */
//  assert(cacheParams.hartIds.length == 1, "private L2 should have one and only one hardId")
  if (!cacheParams.FPGAPlatform) {
    val hartId = if (cacheParams.hartIds.length == 1) cacheParams.hartIds.head else 0
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
}
