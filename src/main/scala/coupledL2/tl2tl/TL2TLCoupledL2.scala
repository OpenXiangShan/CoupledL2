/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
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
import chisel3.util._
import coupledL2._
import coupledL2.prefetch._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import huancun.TPmetaReq
import huancun.TPmetaResp
import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import scala.math.max
import utility.FastArbiter
import utility.ParallelPriorityMux
import utility.Pipeline
import utility.RegNextN

class TL2TLCoupledL2(implicit p: Parameters) extends CoupledL2Base {

  class CoupledL2Imp(wrapper: LazyModule) extends BaseCoupledL2Imp(wrapper)

  lazy val module = new CoupledL2Imp(this)
}
