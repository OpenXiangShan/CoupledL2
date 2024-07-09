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
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.{Field, Parameters}
import coupledL2.L2Param
import huancun.CacheParameters

case class OpenLLCParam
(
  name: String = "L3",
  ways: Int = 8,
  sets: Int = 256,
  blockBytes: Int = 64,
  beatBytes: Int = 32,
  mshrs: Int = 16,
  fullAddressBits: Int = 16,
  replacement: String = "plru",
  clientCaches: Seq[L2Param] = Nil,

  // Performance analysis
  enablePerf: Boolean = true,

  // Network layer SAM
  sam: Seq[(AddressSet, Int)] = Seq(AddressSet.everything -> 0)
) {
  def toCacheParams: CacheParameters = CacheParameters(
    name = name,
    sets = sets,
    ways = ways,
    blockGranularity = log2Ceil(sets),
    blockBytes = blockBytes
  )
}

case object OpenLLCParamKey extends Field[OpenLLCParam](OpenLLCParam())


// common parameters used inside LLC
trait HasOpenLLCParameters {
  val p: Parameters
  def cacheParams = p(OpenLLCParamKey)

  def blockBytes = cacheParams.blockBytes
  def beatBytes = cacheParams.beatBytes
  def beatSize = blockBytes / beatBytes
  def blocks = cacheParams.ways * cacheParams.sets

  def wayBits = log2Ceil(cacheParams.ways)
  def setBits = log2Ceil(cacheParams.sets)
  def blockBits = log2Ceil(blocks)
  def offsetBits = log2Ceil(blockBytes)
  def beatBits = offsetBits - log2Ceil(beatBytes)

  def fullAddressBits = cacheParams.fullAddressBits
  def tagBits = fullAddressBits - setBits - offsetBits

  def mshrs = cacheParams.mshrs
  def mshrBits = log2Up(mshrs)  // TODO: check this

  def clientBits = cacheParams.clientCaches.size

  def sam = cacheParams.sam

  def parseAddress(x: UInt): (UInt, UInt, UInt) = {
    val offset = x  // TODO: check address mapping
    val set = offset >> offsetBits
    val tag = set >> setBits
    (tag(tagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
  }

  def sizeBytesToStr(sizeBytes: Double): String = sizeBytes match {
    case _ if sizeBytes >= 1024 * 1024 => (sizeBytes / 1024 / 1024) + "MB"
    case _ if sizeBytes >= 1024        => (sizeBytes / 1024) + "KB"
    case _                             => "B"
  }

}