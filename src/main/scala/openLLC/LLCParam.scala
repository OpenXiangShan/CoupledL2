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

package openLLC

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.AXI4EdgeParameters
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.{Field, Parameters}

case class OpenLLCParam
(
  name: String = "L2",
  ways: Int = 8,
  sets: Int = 256,
  blockBytes: Int = 64,
  beatBytes: Int = 32,
  mshrs: Int = 16,

  // Network layer SAM
  sam: Seq[(AddressSet, Int)] = Seq(AddressSet.everything -> 0)
) {

}

case object OpenLLCParamKey extends Field[OpenLLCParam](OpenLLCParam())

case object EdgeOutKey extends Field[AXI4EdgeParameters]

// common parameters used inside LLC
trait HasOpenLLCParameters {
  val p: Parameters
  def cacheParams = p(OpenLLCParamKey)

  def blockBytes = cacheParams.blockBytes
  def beatBytes = cacheParams.beatBytes
  def beatSize = blockBytes / beatBytes

  def wayBits = log2Ceil(cacheParams.ways)
  def setBits = log2Ceil(cacheParams.sets)
  def offsetBits = log2Ceil(blockBytes)
  def beatBits = offsetBits - log2Ceil(beatBytes)

  def edgeOut = p(EdgeOutKey)
  def fullAddressBits = edgeOut.bundle.addrBits
  def tagBits = fullAddressBits - setBits - offsetBits

  def mshrs = cacheParams.mshrs
  def mshrBits = log2Up(mshrs)  // TODO: check this



  def sam = cacheParams.sam

  def parseAddress(x: UInt): (UInt, UInt, UInt) = {
    val offset = x  // TODO: check address mapping
    val set = offset >> offsetBits
    val tag = set >> setBits
    (tag(tagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
  }

}