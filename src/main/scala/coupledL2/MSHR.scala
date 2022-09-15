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
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import chipsalliance.rocketchip.config.Parameters

class MSHR(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val status = ValidIO(new MSHRStatus)
    val alloc = Flipped(ValidIO(new MSHRRequest))
  })

  val (alloc_tag, alloc_set, alloc_offset) = parseAddress(io.alloc.bits.addr)
  val status_reg = RegInit(0.U.asTypeOf(Valid(new MSHRStatus())))
  when(io.alloc.valid) {
    status_reg.valid := true.B
    status_reg.bits.tag := alloc_tag
    status_reg.bits.set := alloc_set
    status_reg.bits.way := io.alloc.bits.way
  }

  io.status.valid := status_reg.valid
  io.status.bits <> status_reg.bits

}
