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
import freechips.rocketchip.diplomacy.{AddressSet, BufferParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Field
import huancun.{AliasKey, CacheParameters, IsHitKey, PrefetchKey}
import coupledL2.prefetch._
import utility.{Code, MemReqSource, ReqSourceKey}

case object EnableCHI extends Field[Boolean](false)
case object EnableL2ClockGate extends Field[Boolean](true)

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

// Pass way in [ L2 -> D -> L1 ] and [ L1 -> C -> L2 ]
case object WayKey extends ControlKey[UInt]("way")
case class WayField() extends BundleField[UInt](WayKey, Output(UInt(4.W)), _ := 0.U(4.W))

// Pass PMA and uncached memory attribute from PBMT to MMIOBridge
case object MemBackTypeMM extends ControlKey[Bool]("memBackType_MM")
case class MemBackTypeMMField() extends BundleField[Bool](MemBackTypeMM, Output(Bool()), _ := false.B)

case object MemPageTypeNC extends ControlKey[Bool]("memPageType_NC")
case class MemPageTypeNCField() extends BundleField[Bool](MemPageTypeNC, Output(Bool()), _ := false.B)

// Pass virtual address of upper level cache
case object VaddrKey extends ControlKey[UInt]("vaddr")
case class VaddrField(width: Int) extends BundleField[UInt](VaddrKey, Output(UInt(width.W)), _ := 0.U(width.W))

// Pass load_miss_acquire_keyword of upper level cache (L1)
case object IsKeywordKey extends ControlKey[Bool]("isKeyword")
case class IsKeywordField() extends BundleField[Bool](IsKeywordKey, Output(Bool()), _ := false.B)

case class L2Param(
  name: String = "L2",
  ways: Int = 4,
  sets: Int = 128,
  blockBytes: Int = 64,
  pageBytes: Int = 4096,
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  clientCaches: Seq[L1Param] = Nil,
  replacement: String = "drrip",
  mshrs: Int = 16,
  releaseData: Int = 3,
  /* 0 for dirty alone
   * 1 for dirty and accessed
   * 2 for all except prefetch & !accessed
   * 3 for all
   */
  mmioBridgeSize: Int = 8,

  // Client
  echoField: Seq[BundleFieldBase] = Nil,
  reqField: Seq[BundleFieldBase] = Nil,
  respKey: Seq[BundleKeyBase] = Seq(IsHitKey, WayKey),
  // Manager
  reqKey: Seq[BundleKeyBase] = Seq(AliasKey, VaddrKey, PrefetchKey, ReqSourceKey, WayKey),
  respField: Seq[BundleFieldBase] = Seq(WayField()),

  innerBuf: TLBufferParams = TLBufferParams(),
  outerBuf: TLBufferParams = TLBufferParams(
    a = BufferParams.default,
    b = BufferParams.default,
    c = BufferParams.none,
    d = BufferParams.default,
    e = BufferParams.default
  ),

  hartId: Int = 0,
  // Prefetch
  prefetch: Seq[PrefetchParameters] = Nil,
  // L2 Flush
  enableL2Flush: Boolean = false,
  // Performance analysis
  enablePerf: Boolean = true,
  // RollingDB
  enableRollingDB: Boolean = true,
  // Monitor
  enableMonitor: Boolean = true,
  // TLLog
  enableTLLog: Boolean = true,
  // CHILog
  enableCHILog: Boolean = true,
  // TopDown
  elaboratedTopDown: Boolean = true,
  // env
  FPGAPlatform: Boolean = false,
  // ECC
  tagECC: Option[String] = None,
  dataECC: Option[String] = None,
  enableTagECC: Boolean = false,
  enableDataECC: Boolean = false,
  // DataCheck
  dataCheck: Option[String] = Some("oddparity"),
  enablePoison: Boolean = true,

  // Network layer SAM
  sam: Seq[(AddressSet, Int)] = Seq(AddressSet.everything -> 0),

  // Enable sram test support
  hasMbist: Boolean = false,
  hasSramCtl: Boolean = false,

  // both EnablePrivateClint and PrivateClintRange are from soc parameters.
  PrivateClintRange: Option[AddressSet] = None
) {
  require(ways <= 16, "L1 way record for L2: only support up to 16 ways")

  def toCacheParams: CacheParameters = CacheParameters(
    name = name,
    sets = sets,
    ways = ways,
    blockGranularity = log2Ceil(sets),
    blockBytes = blockBytes
  )

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)

  def hasDFT: Boolean = hasMbist || hasSramCtl
}

case object L2ParamKey extends Field[L2Param](L2Param())

case object EdgeInKey extends Field[TLEdgeIn]

case object EdgeOutKey extends Field[TLEdgeOut]

case object BankBitsKey extends Field[Int]

case object L2NBanksKey extends Field[Int]

case object SliceIdKey extends Field[Int]
