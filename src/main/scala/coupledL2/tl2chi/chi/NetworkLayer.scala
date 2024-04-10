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
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import utility.ParallelPriorityMux

/**
  * System Address Map
  * 
  * Each Requester, that is, each RN and HN in the system, must have a System Address Map (SAM)
  * to determine the target ID of a request.
  */
class SAM(sam: Seq[(AddressSet, Int)]) {
  def check(x: UInt): Bool = Cat(sam.map(_._1.contains(x))).orR

  // def lookup(x: BigInt): Int = ParallelPriorityMux(sam.map(m => (m._1.contains(x), m._2)))
  def lookup(x: UInt): UInt = {
    assert(check(x))
    ParallelPriorityMux(sam.map(m => (m._1.contains(x), m._2.U)))
  }
}

object SAM {
  def apply(sam: Seq[(AddressSet, Int)]) = new SAM(sam)
  def apply(sam: (AddressSet, Int)) = new SAM(Seq(sam))
}