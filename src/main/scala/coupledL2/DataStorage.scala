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
import coupledL2.utils.{HoldUnless, SplittedSRAM}
import utility.{ClockGate, SRAMTemplate}
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

case object HasDataEccParam

class DataStorage(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // en is the actual r/w valid from mainpipe (last for one cycle)
    // en is used to generate gated_clock for SRAM
    val en = Input(Bool())

    // ECC error
    val error = Output(Bool())

    // 1. there is only 1 read or write request in the same cycle,
    // so only 1 req port is necessary
    // 2. according to the requirement of MCP2, [req.valid, req.bits, wdata]
    // must hold for 2 cycles (unchanged at en and RegNext(en))
    val req = Flipped(ValidIO(new DSRequest))
    val rdata = Output(new DSBlock)
    val wdata = Input(new DSBlock)
  })

  def get_ecc_from_encData(encData: UInt) = {
    require(encData.getWidth == encDataBits)
    encData(encDataBits - 1, blockBytes * 8)
  }

  val dataECCParam = if(enableDataECC) Some(HasDataEccParam) else None

  // read data is set MultiCycle Path 2
  val array = Module(new SplittedSRAM(
    gen = new DSBlock,
    set = blocks,
    way = 1,
    dataSplit = 4,
    singlePort = true,
    readMCP2 = true
  ))

  val eccArray = dataECCParam.map {
    case _ =>
      val array = Module(new SRAMTemplate(
        gen = UInt(eccDataBits.W),
        set = blocks,
        way = 1,
        singlePort = true,
      ))
      array
  }

  val masked_clock = ClockGate(false.B, io.en, clock)
  array.clock := masked_clock

  val arrayIdx = Cat(io.req.bits.way, io.req.bits.set)
  val wen = io.req.valid && io.req.bits.wen
  val ren = io.req.valid && !io.req.bits.wen

  // make sure SRAM input signals will not change during the two cycles
  // TODO: This check is done elsewhere
  array.io.w.apply(wen, io.wdata, arrayIdx, 1.U)
  array.io.r.apply(ren, arrayIdx)

  // ECC R/W
  val eccRead = Wire(UInt(eccDataBits.W))
  eccArray match {
    case Some(ecc) =>
      eccRead := ecc.io.r(ren, arrayIdx).resp.data(0)
    case None =>
      eccRead := 0.U.asTypeOf(eccRead)
  }
  eccArray match {
    case Some(ecc) =>
      ecc.io.w(
        wen,
        get_ecc_from_encData(cacheParams.tagCode.encode(io.wdata.data)),
        arrayIdx,
        1.U
      )
    case None =>
  }

  val eccData = Cat(eccRead, array.io.r.resp.data(0).data)
  val error = cacheParams.dataCode.decode(eccData).error

  // for timing, we set this as multicycle path
  // s3 read, s4 pass and s5 to destination
  io.rdata := array.io.r.resp.data(0)
  io.error := error

  assert(!io.en || !RegNext(io.en, false.B),
    "Continuous SRAM req prohibited under MCP2!")

  assert(!(RegNext(io.en) && (io.req.asUInt =/= RegNext(io.req.asUInt))),
    s"DataStorage req fails to hold for 2 cycles!")

  assert(!(RegNext(io.en && io.req.bits.wen) && (io.wdata.asUInt =/= RegNext(io.wdata.asUInt))),
    s"DataStorage wdata fails to hold for 2 cycles!")
}
