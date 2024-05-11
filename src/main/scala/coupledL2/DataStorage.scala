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
import coupledL2.utils.SRAMTemplate
import utility.RegNextN
import org.chipsalliance.cde.config.Parameters

class DSRequest(implicit p: Parameters) extends L2Bundle {
  val way = UInt(wayBits.W)
  val set = UInt(setBits.W)
  val wen = Bool()
}

// mask not used
class DSBeat(implicit p: Parameters) extends L2Bundle {
  val data = UInt((beatBytes * 8).W)
}

class DSBlock(implicit p: Parameters) extends L2Bundle {
  val data = UInt((blockBytes * 8).W)
}

class DataStorage(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // there is only 1 read or write request in the same cycle,
    // so only 1 req port is necessary
    val req = Flipped(ValidIO(new DSRequest))
    val rdata = Output(new DSBlock)
    val wdata = Input(new DSBlock)
  })

  val array = Module(new SRAMTemplate(
    gen = new DSBlock,
    set = blocks,
    way = 1,
    singlePort = true,
    holdRead = true
  ))

  val arrayIdx = Cat(io.req.bits.way, io.req.bits.set)
  val wen = io.req.valid && io.req.bits.wen
  val ren = io.req.valid && !io.req.bits.wen
  array.io.w.apply(wen, io.wdata, arrayIdx, 1.U)
  array.io.r.apply(ren, arrayIdx)

  // for timing, we set this as multicycle path
  // s3 read, s4 pass and s5 to destination
  io.rdata := array.io.r.resp.data(0)

  assert(!io.req.valid || !RegNext(io.req.valid, false.B),
    "Continuous SRAM req prohibited under MCP2!")
}
