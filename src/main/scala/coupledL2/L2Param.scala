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
import chisel3.util.log2Ceil
import freechips.rocketchip.diplomacy.BufferParams
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.Field

// General parameter key of CoupledL2
case object L2ParamKey extends Field[L2Param](L2Param())

// Indicate whether this block is dirty or not (only used in handle Release/ReleaseData)
// Now it only works for non-inclusive cache (ignored in inclusive cache)
case object DirtyKey extends ControlKey[Bool](name = "blockisdirty")

case class DirtyField() extends BundleField(DirtyKey) {
  override def data: Bool = Output(Bool())
  override def default(x: Bool): Unit = {
    x := true.B
  }
}

case class L2Param (
  name: String = "L2",
  ways: Int = 4,
  sets: Int = 128,
  blockBytes: Int = 64,
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),

  // Client
  echoField: Seq[BundleFieldBase] = Nil,
  reqField: Seq[BundleFieldBase] = Nil, 
  respKey: Seq[BundleKeyBase] = Nil,
  // Manager
  reqKey: Seq[BundleKeyBase] = Nil,
  respField: Seq[BundleFieldBase] = Nil,

  innerBuf: TLBufferParams = TLBufferParams(),
  outerBuf: TLBufferParams = TLBufferParams(
    a = BufferParams.default,
    b = BufferParams.default,
    c = BufferParams.default,
    d = BufferParams.default,
    e = BufferParams.default
  )
) {
}

case object EdgeInKey extends Field[TLEdgeIn]

case object EdgeOutKey extends Field[TLEdgeOut]
