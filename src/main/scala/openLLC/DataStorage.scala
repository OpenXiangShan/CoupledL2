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

package openLLC

import chisel3._
import chisel3.util._
import coupledL2.utils.SRAMTemplate
import org.chipsalliance.cde.config.Parameters

class DSRequest(implicit p: Parameters) extends LLCBundle {
  val way = UInt(wayBits.W)
  val set = UInt(setBits.W)
}

class DSBeat(implicit p: Parameters) extends LLCBundle {
  val data = UInt((beatBytes * 8).W)
}

class DSBlock(implicit p: Parameters) extends LLCBundle {
  val data = Vec(beatSize, new DSBeat())
}

class WBEntry(implicit p: Parameters) extends LLCBundle {
  val blockIdx = UInt(blockBits.W)
  val data = new DSBlock
}

class DataStorage(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /**
      * Support read and write request in the same cycle.
      * When reading and writing the same address,
      * the data before writing is returned
      */
    val read  = Flipped(ValidIO(new DSRequest()))
    val write = Flipped(ValidIO(new DSRequest()))
    val rdata = Output(new DSBlock())
    val wdata = Input(new DSBlock())
  })

  val array = Module(new SRAMTemplate(
    gen = new DSBlock,
    set = blocks,
    way = 1,
    singlePort = false
  ))

  val ren = io.read.valid
  val wen = io.write.valid
  val readIdx = Cat(io.read.bits.way, io.read.bits.set)
  val writeIdx = Cat(io.write.bits.way, io.write.bits.set)

  val writeBuffer = RegInit(0.U.asTypeOf(new WBEntry()))

  /* WriteBuffer update logic */
  /**
    * New write requests are not written directly to SRAM,
    * but are written to the buffer first.
    */
  when (wen) {
    writeBuffer.blockIdx := writeIdx
    writeBuffer.data := io.wdata
  }

  /* SRAM write logic */
  // SRAM is written when the data block of the buffer is replaced
  val writeHit = writeIdx === writeBuffer.blockIdx
  val writeBack = !writeHit && wen
  array.io.w.apply(writeBack, writeBuffer.data, writeIdx, 1.U)

  /* Read request response */
  val readHit = readIdx === writeBuffer.blockIdx
  val readBuffer = readHit && ren
  array.io.r.apply(!readBuffer, readIdx)
  val rdata_s1 = Mux(
    RegNext(readBuffer, false.B), 
    RegEnable(writeBuffer.data, 0.U.asTypeOf(new DSBlock), readBuffer),
    array.io.r.resp.data(0)
  )
  val rdata_s2 = RegEnable(rdata_s1, 0.U.asTypeOf(new DSBlock), RegNext(ren, false.B))
  io.rdata := rdata_s2

}