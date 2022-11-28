/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package coupledL2

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import TaskInfo._

abstract class L2Module(implicit val p: Parameters) extends MultiIOModule with HasCoupledL2Parameters
abstract class L2Bundle(implicit val p: Parameters) extends Bundle with HasCoupledL2Parameters

class ReplacerInfo(implicit p: Parameters) extends L2Bundle {
  val channel = UInt(3.W)
  val opcode = UInt(3.W)
}

trait HasChannelBits { this: Bundle =>
  val channel = UInt(3.W)
  def fromA = channel(0).asBool
  def fromB = channel(1).asBool
  def fromC = channel(2).asBool
}

// We generate a Task for every TL request
// this is the info that flows in Mainpipe
class TaskBundle(implicit p: Parameters) extends L2Bundle with HasChannelBits {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val off = UInt(offsetBits.W)
  val alias = UInt(aliasBits.W)           // color bits in cache-alias issue
  val owner = UInt(ownerBits.W)           // who owns this block
  val opcode = UInt(3.W)                  // type of the task operation
  val param = UInt(3.W)
  val sourceId = UInt(sourceIdBits.W)     // tilelink sourceID
  val id = UInt(idBits.W)                 // identity of the task
  val bufIdx = UInt(bufIdxBits.W)         // idx of SinkC buffer

  // val mshrOpType = UInt(mshrOpTypeBits.W) // type of the MSHR task operation
  // MSHR may send Release(Data) or Grant(Data) or ProbeAck(Data) through Main Pipe
  val mshrTask = Bool()                   // is task from mshr
  val mshrId = UInt(mshrBits.W)           // mshr entry index (used only in mshr-task)

  // if this is an mshr task and it needs to write dir
  val way = UInt(wayBits.W)
  val meta = new MetaEntry()
  val metaWen = Bool()
  val tagWen = Bool()

  def hasData = opcode(0)
}

class MSHRStatus(implicit p: Parameters) extends L2Bundle with HasChannelBits {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val way = UInt(wayBits.W)
  val off = UInt(offsetBits.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(sourceIdBits.W)
}

// MSHR Task that MainPipe sends to MSHRCtl
class MSHRRequest(implicit p: Parameters) extends L2Bundle {
  val dirResult = new DirResult()
  val state = new FSMState()
  val task = new TaskBundle()
}

class RespInfoBundle(implicit p: Parameters) extends L2Bundle {
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val last = Bool() // last beat
}

class RespBundle(implicit p: Parameters) extends L2Bundle {
  val valid = Bool()
  val mshrId = UInt(mshrBits.W)
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val respInfo = new RespInfoBundle
}

class FSMState(implicit p: Parameters) extends L2Bundle {
  // schedule
  val s_acquire = Bool()  // acquire downwards
  val s_rprobe = Bool()   // probe upwards, caused by replace
  val s_pprobe = Bool()   // probe upwards, casued by probe
  val s_release = Bool()  // release downwards
  val s_probeack = Bool() // respond probeack downwards
  val s_refill = Bool()   // respond grant upwards
  // val s_grantack = Bool() // respond grantack downwards
  val s_writeback = Bool()// writeback tag/dir

  // wait
  val w_rprobeackfirst = Bool()
  val w_rprobeacklast = Bool()
  val w_pprobeackfirst = Bool()
  val w_pprobeacklast = Bool()
  val w_pprobeack = Bool()
  val w_grantfirst = Bool()
  val w_grantlast = Bool()
  val w_grant = Bool()
  val w_releaseack = Bool()
  val w_grantack = Bool()
}

class SourceAReq(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val off = UInt(offsetBits.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(mshrBits.W)
}