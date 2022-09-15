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

class TaskBundle(implicit p: Parameters) extends L2Bundle {
  val valid = Bool()                      // valid
  val addr = UInt(addressBits.W)          // task address
  val alias = UInt(aliasBits.W)           // color bits in cache-alias issue
  val owner = UInt(ownerBits.W)           // who owns this block
  val opcode = UInt(3.W)                  // type of the task operation
  val channel = UInt(3.W)
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
}

class MSHRRequest(implicit p: Parameters) extends L2Bundle {
  val addr = UInt(addressBits.W)
  val way = UInt(wayBits.W)
}
