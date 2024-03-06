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

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLPermissions._
import utility.MemReqSource
import coupledL2.{HasChannelBits, DirResult}

class MSHRStatus(implicit p: Parameters) extends TL2CHIL2Bundle with HasChannelBits {
  // TODO
}

class FSMState(implicit p: Parameters) extends TL2CHIL2Bundle {
  // TODO
}

trait HasCHITaskFields extends HasCHIMsgParameters { Bundle =>
  val tgtID = UInt(TGTID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val dbID = UInt(DBID_WIDTH.W)
  val opcode = UInt(OPCODE_WIDTH.W)
  val pCrdType = UInt(PCRDTYPE_WIDTH.W)
}
class TaskBundle(implicit p: Parameters) extends coupledL2.TaskBundle
  with HasCHITaskFields {
  override val opcode = UInt(OPCODE_WIDTH.W)
}

class MSHRRequest(implicit p: Parameters) extends TL2CHIL2Bundle {
  val dirResult = new DirResult()
  val state = new FSMState()
  val task = new TaskBundle()
}