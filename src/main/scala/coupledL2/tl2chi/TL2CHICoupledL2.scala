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

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import utility.{FastArbiter, Pipeline}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import scala.math.max
import coupledL2._
import coupledL2.prefetch._
import coupledL2.utils.XSPerfAccumulate
import scala.languageFeature.implicitConversions
import java.util.ResourceBundle

trait HasTL2CHICoupledL2Parameteres extends HasCoupledL2Parameters {
  val tl2chiParams: HasCHIL2Parameters = p(L2ParamKey)

  // TODO
}

abstract class TL2CHIL2Bundle(implicit val p: Parameters) extends Bundle
  with HasTL2CHICoupledL2Parameteres
  with HasCHIMsgParameters
abstract class TL2CHIL2Module(implicit val p: Parameters) extends Module
  with HasTL2CHICoupledL2Parameteres
  with HasCHIMsgParameters

class TL2CHICoupledL2(implicit p: Parameters) extends CoupledL2Base
  with HasTL2CHICoupledL2Parameteres {
  
  // TODO

  class CoupledL2Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    // TODO
  }

  lazy val module = new CoupledL2Imp(this)
}