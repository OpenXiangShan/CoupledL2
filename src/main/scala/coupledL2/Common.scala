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

class TaskBundle(implicit p: Parameters) extends L2Bundle with HasChannelBits {
  val valid = Bool()                      // valid
  val addr = UInt(addressBits.W)          // task address
  val alias = UInt(aliasBits.W)           // color bits in cache-alias issue
  val owner = UInt(ownerBits.W)           // who owns this block
  val opcode = UInt(3.W)                  // type of the task operation
  val param = UInt(3.W)
  val sourceId = UInt(sourceIdBits.W)     // tilelink sourceID
  val id = UInt(idBits.W)                 // identity of the task

  val mshrOpType = UInt(mshrOpTypeBits.W) // type of the MSHR task operation
  val mshrId = UInt(mshrBits.W)           // mshr entry index (used only in mshr-task)
}

class MSHRStatus(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val way = UInt(wayBits.W)
  val off = UInt(offsetBits.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(sourceIdBits.W)
}

class MSHRRequest(implicit p: Parameters) extends L2Bundle {
  val addr = UInt(addressBits.W)
  val way = UInt(wayBits.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(sourceIdBits.W)
  val dirResult = new DirResult()
  val state = new FSMState()
}

class RefillUnitResp(implicit p: Parameters) extends L2Bundle {
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val last = Bool() // last beat
}

class FSMState(implicit p: Parameters) extends L2Bundle {
  val s_acquire = Bool()
  val s_rprobe = Bool()
  val s_pprobe = Bool()
  val s_release = Bool()
  val s_probeack = Bool()
  val s_execute = Bool()
  val s_grantack = Bool()
  val s_writeback = Bool()

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

class SourceDReq(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val off = UInt(offsetBits.W)
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val source = UInt(mshrBits.W)
}