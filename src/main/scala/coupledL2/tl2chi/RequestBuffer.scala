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

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import chisel3._
import chisel3.util._
import coupledL2._
import coupledL2.utils._
import utility._

class RequestBuffer(flow: Boolean = true, entries: Int = 4)(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new TaskBundle()))
    val out = DecoupledIO(new TaskBundle())
    val hasLatePF = Output(Bool())
  })

  io <> DontCare
}