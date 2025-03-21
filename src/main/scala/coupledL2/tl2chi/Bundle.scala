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
import utility.chi._
import utility.MemReqSource
import coupledL2.{HasTLChannelBits, DirResult, PipeStatus}

object CHIChannel {
  def TXREQ = "b001".U
  def TXRSP = "b010".U
  def TXDAT = "b100".U
}

trait HasCHIChannelBits { this: Bundle =>
  val txChannel = UInt(3.W)
  def toTXREQ = txChannel(0).asBool
  def toTXRSP = txChannel(1).asBool
  def toTXDAT = txChannel(2).asBool
}

class PipeStatusWithCHI(implicit p: Parameters) extends PipeStatus
  with HasCHIChannelBits {
  val mshrTask = Bool()
}

class MSHRStatus(implicit p: Parameters) extends TL2CHIL2Bundle
  with HasTLChannelBits 
  with HasCHIChannelBits {
  // TODO
  val set         = UInt(setBits.W)
  val reqTag      = UInt(tagBits.W)
  val metaTag     = UInt(tagBits.W)
  val needsRepl = Bool()
  val w_c_resp = Bool()
  val w_d_resp = Bool()
  val will_free = Bool()

//  val way = UInt(wayBits.W)
//  val off = UInt(offsetBits.W)
//  val opcode = UInt(3.W)
//  val param = UInt(3.W)
//  val size = UInt(msgSizeBits.W)
//  val source = UInt(sourceIdBits.W)
//  val alias = aliasBitsOpt.map(_ => UInt(aliasBitsOpt.get.W))
//  val aliasTask = aliasBitsOpt.map(_ => Bool())
//  val needProbeAckData = Bool() // only for B reqs
//  val fromL2pft = prefetchOpt.map(_ => Bool())
//  val needHint = prefetchOpt.map(_ => Bool())

  // for TopDown usage
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
  val is_miss = Bool()
  val is_prefetch = Bool()

}

class PCrdQueryBundle(implicit p: Parameters) extends TL2CHIL2Bundle with HasCHIOpcodes {
  val query = Output(ValidIO(new Bundle() {
    val pCrdType = UInt(PCRDTYPE_WIDTH.W)
    val srcID = UInt(SRCID_WIDTH.W)
  }))
  val grant = Input(Bool())
}