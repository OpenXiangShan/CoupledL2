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
import utility.{MemReqSource, ReqSourceKey, ReqSourceField}

// General parameter key of CoupledL2
case object L2ParamKey extends Field[L2Param](L2Param())

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

case class L2Param
(
  name: String = "L2",
  ways: Int = 4,
  sets: Int = 128,
  blockBytes: Int = 64,
  pageBytes: Int = 4096,
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  clientCaches: Seq[L1Param] = Nil,
  replacement: String = "plru",
  mshrs: Int = 16,
  releaseData: Int = 3,
  /* 0 for dirty alone
   * 1 for dirty and accessed
   * 2 for all except prefetch & !accessed
   * 3 for all
   */

  // Client (these are set in Configs.scala in XiangShan)
  echoField: Seq[BundleFieldBase] = Nil,
  reqField: Seq[BundleFieldBase] = Nil,
  respKey: Seq[BundleKeyBase] = Seq(IsHitKey),
  // Manager
  reqKey: Seq[BundleKeyBase] = Seq(AliasKey, VaddrKey, PrefetchKey, ReqSourceKey),
  respField: Seq[BundleFieldBase] = Nil,

  innerBuf: TLBufferParams = TLBufferParams(),
  outerBuf: TLBufferParams = TLBufferParams(
    a = BufferParams.default,
    b = BufferParams.default,
    c = BufferParams.none,
    d = BufferParams.default,
    e = BufferParams.default
  ),

  innerBufPipe: TLBufferParams = TLBufferParams(
    a = BufferParams.pipe,
    b = BufferParams.default,
    c = BufferParams.pipe,
    d = BufferParams.default,
    e = BufferParams.pipe
  ),

  hartIds: Seq[Int] = Seq[Int](),
  // Prefetch
  prefetch: Option[PrefetchParameters] = None,
  // Performance analysis
  enablePerf: Boolean = true,
  // Monitor
  enableMonitor: Boolean = true,
  // TopDown
  elaboratedTopDown: Boolean = true,
  // env
  FPGAPlatform: Boolean = false
) {
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

case object SliceIdKey extends Field[Int]
