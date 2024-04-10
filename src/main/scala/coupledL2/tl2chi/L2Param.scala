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
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Field
import coupledL2._
import coupledL2.prefetch.PrefetchParameters
import huancun.{AliasKey, CacheParameters, IsHitKey, PrefetchKey}
import utility.ReqSourceKey

// Parameters particular to original tileLink-to-chi CoupledL2
trait HasCHIL2Parameters {
  val sam: Seq[(AddressSet, Int)]
}

case class L2Param(
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

  // Parameters particular to original tileLink-to-chi CoupledL2
  sam: Seq[(AddressSet, Int)] = Seq(AddressSet.everything -> 33)
) extends HasL2BaseParameters with HasCHIL2Parameters

// General parameter key of CoupledL2
case object L2ParamKey extends Field[L2Param](L2Param())