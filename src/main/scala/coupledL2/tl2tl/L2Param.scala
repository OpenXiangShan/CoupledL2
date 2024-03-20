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

package coupledL2.tl2tl

import chisel3._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Field
import coupledL2._
import coupledL2.prefetch.PrefetchParameters
import huancun.{AliasKey, CacheParameters, IsHitKey, PrefetchKey}
import utility.ReqSourceKey

// Parameters particular to original tileLink-to-tilelink CoupledL2
trait HasTLL2Parameters {
  def releaseData: Int
  /* 0 for dirty alone
   * 1 for dirty and accessed
   * 2 for all except prefetch & !accessed
   * 3 for all
   */
  val outerBuf: TLBufferParams = TLBufferParams(
    a = BufferParams.default,
    b = BufferParams.default,
    c = BufferParams.none,
    d = BufferParams.default,
    e = BufferParams.default
  )
}

case class L2Param
(
  name: String = "L2",
  ways: Int = 4,
  sets: Int = 128,
  clientCaches: Seq[L1Param] = Nil,
  replacement: String = "plru",
  mshrs: Int = 16,

  // Client (these are set in Configs.scala in XiangShan)
  echoField: Seq[BundleFieldBase] = Nil,
  reqField: Seq[BundleFieldBase] = Nil,
  respKey: Seq[BundleKeyBase] = Seq(IsHitKey),
  // Manager
  reqKey: Seq[BundleKeyBase] = Seq(AliasKey, VaddrKey, PrefetchKey, ReqSourceKey),
  respField: Seq[BundleFieldBase] = Nil,

  hartIds: Seq[Int] = Seq[Int](),
  prefetch: Option[PrefetchParameters] = None,
  enablePerf: Boolean = true,
  enableRollingDB: Boolean = true,
  enableMonitor: Boolean = true,
  elaboratedTopDown: Boolean = true,
  FPGAPlatform: Boolean = false,

  // Parameters particular to original tileLink-to-tilelink CoupledL2
  releaseData: Int = 3
) extends HasL2BaseParameters with HasTLL2Parameters

// General parameter key of CoupledL2
case object L2ParamKey extends Field[L2Param](L2Param())