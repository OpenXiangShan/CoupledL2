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
import chisel3.util._
import coupledL2.utils._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._

// wbq receive reqs from MainPipe unconditionally, and send them out through channel C
// NOTICE: channel C may be unable to receive
// NOTICE: two beats of the same address should not be interrupted by another address
class WritebackQueue(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val req = Flipped(Decoupled(new TLBundleC(edgeOut.bundle)))
    val c = Decoupled(new TLBundleC(edgeOut.bundle))
  })

  // TODO

}