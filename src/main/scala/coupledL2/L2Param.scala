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
import org.chipsalliance.cde.config.Field
import huancun.{AliasKey, CacheParameters, IsHitKey, PrefetchKey}
import coupledL2.prefetch._
import utility.{MemReqSource, ReqSourceKey}

case object EnableCHI extends Field[Boolean](false)

// L1 Cache Params, used for TestTop generation
case class L1Param
(
  name: String = "L1D",
  sets: Int = 32,
  ways: Int = 8,
  blockBytes: Int = 64,
  aliasBitsOpt: Option[Int] = None,
  vaddrBitsOpt: Option[Int] = None,
  isKeywordBitsOpt : Option[Boolean] = None
) {
  val capacity = sets * ways * blockBytes
  val setBits = log2Ceil(sets)
  val offsetBits = log2Ceil(blockBytes)
  val needResolveAlias = aliasBitsOpt.nonEmpty
}

// Pass virtual address of upper level cache
case object VaddrKey extends ControlKey[UInt]("vaddr")
case class VaddrField(width: Int) extends BundleField[UInt](VaddrKey, Output(UInt(width.W)), _ := 0.U(width.W))

// Pass load_miss_acquire_keyword of upper level cache (L1)
case object IsKeywordKey extends ControlKey[Bool]("isKeyword")
case class IsKeywordField() extends BundleField[Bool](IsKeywordKey, Output(Bool()), _ := false.B)

// Parameters shared by both tilelink-to-tilelink L2 and tilelink-to-chi L2
trait HasL2BaseParameters {
  def name: String
  def ways: Int
  def sets: Int

  val blockBytes: Int = 64
  val pageBytes: Int = 4096
  val channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32)

  def clientCaches: Seq[L1Param]
  def replacement: String
  def mshrs: Int

  // Client
  def echoField: Seq[BundleFieldBase]
  def reqField: Seq[BundleFieldBase]
  def respKey: Seq[BundleKeyBase]
  // Manager
  def reqKey: Seq[BundleKeyBase]
  def respField: Seq[BundleFieldBase]

  val innerBuf: TLBufferParams = TLBufferParams()

  def hartIds: Seq[Int]
  // Prefetch
  def prefetch: Option[PrefetchParameters]
  // Performance analysis
  def enablePerf: Boolean
  // RollingDB
  def enableRollingDB: Boolean
  // Monitor
  def enableMonitor: Boolean
  // TopDown
  def elaboratedTopDown: Boolean
  // env
  def FPGAPlatform: Boolean

  def toCacheParams: CacheParameters = CacheParameters(
    name = name,
    sets = sets,
    ways = ways,
    blockGranularity = log2Ceil(sets),
    blockBytes = blockBytes
  )
}

case object EdgeInKey extends Field[TLEdgeIn]

case object EdgeOutKey extends Field[TLEdgeOut]

case object BankBitsKey extends Field[Int]

case object L2NBanksKey extends Field[Int]

case object SliceIdKey extends Field[Int]
