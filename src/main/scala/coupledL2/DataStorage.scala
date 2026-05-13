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
import coupledL2.utils.GatedSplittedSRAM
import org.chipsalliance.cde.config.Parameters
import utility.mbist.MbistPipeline

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
  val data = UInt(blockBits.W)
}

class DSECCBankBlock(implicit p: Parameters) extends L2Bundle {
  val data = if (enableDataECC) {
    UInt((encDataPadBits + encBankBits * dataBankSplit).W)
  } else {
    UInt((dataBankBits * dataBankSplit).W)
  }
}


class DataStorage(implicit p: Parameters) extends L2Module {
  require(dataSetSplit >= 1, s"DataStorage set bank split ($dataSetSplit) must be at least 1")
  require(isPow2(dataSetSplit), s"DataStorage set bank split ($dataSetSplit) must be a power of two")
  require(cacheParams.sets % dataSetSplit == 0,
    s"DataStorage set bank split ($dataSetSplit) must divide sets (${cacheParams.sets})")
  require(blocks % dataSetSplit == 0,
    s"DataStorage set bank split ($dataSetSplit) must divide blocks ($blocks)")

  val io = IO(new Bundle() {
    // en is the actual r/w valid from mainpipe (last for one cycle)
    // en is used to generate gated_clock for SRAM
    val en = Input(Bool())

    // ECC error
    val error = Output(Bool())

    // 1. there is only 1 read or write request in the same cycle,
    // so only 1 req port is necessary
    // 2. MainPipe only sends a one-cycle request pulse. DataStorage
    // keeps the target bank's req/wdata stable for the extra MCP2 cycle.
    val req = Flipped(ValidIO(new DSRequest))
    val rdata = Output(new DSBlock)
    val wdata = Input(new DSBlock)
  })

  private val bankedBlocks = blocks / dataSetSplit
  private val currentReqFire = io.en && io.req.valid
  private val currentReqBank = get_data_bank(io.req.bits.set)
  private val currentReqBankOH = UIntToOH(currentReqBank, dataSetSplit)
  private val readBankOH_s3 = Wire(UInt(dataSetSplit.W))
  readBankOH_s3 := Mux(
    currentReqFire && !io.req.bits.wen,
    currentReqBankOH,
    0.U(dataSetSplit.W)
  )
  private val readBankOH_s4 = RegInit(0.U(dataSetSplit.W))
  private val readBankOH_s5 = RegInit(0.U(dataSetSplit.W))
  readBankOH_s4 := readBankOH_s3
  readBankOH_s5 := readBankOH_s4

  // read data is set MultiCycle Path 2
  val arrays = Seq.fill(dataSetSplit)(Module(new GatedSplittedSRAM(
    gen = new DSECCBankBlock,
    set = bankedBlocks,
    way = 1,
    dataSplit = dataSRAMSplit,
    singlePort = true,
    readMCP2 = true,
    hasMbist = p(L2ParamKey).hasMbist,
    hasSramCtl = p(L2ParamKey).hasSramCtl,
    extraHold = true,
    withClockGate = true
  )))
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, "L2DataStorage", p(L2ParamKey).hasMbist)

  val arrayWriteData = if (enableDataECC) {
    Cat(0.U(encDataPadBits.W), Cat(VecInit(Seq.tabulate(dataBankSplit)(i =>
      io.wdata.data(dataBankBits * (i + 1) - 1, dataBankBits * i))).map(data => cacheParams.dataCode.encode(data))))
  } else {
    io.wdata.data
  }

  val bankReqHold = RegInit(VecInit(Seq.fill(dataSetSplit)(false.B)))
  val bankReqReg = RegInit(VecInit(Seq.fill(dataSetSplit)(0.U.asTypeOf(new DSRequest))))
  val bankWdataReg = RegInit(VecInit(Seq.fill(dataSetSplit)(0.U(arrayWriteData.getWidth.W))))
  val bankReads = Wire(Vec(dataSetSplit, new DSECCBankBlock))
  val bankErrors = Wire(Vec(dataSetSplit, Bool()))

  private def bankArrayIdx(req: DSRequest): UInt = Cat(req.way, req.set) >> dataSetBankBits

  arrays.zipWithIndex.foreach { case (array, i) =>
    val bankAccept = currentReqFire && currentReqBankOH(i)
    val bankReqValid = bankAccept || bankReqHold(i)
    val bankReq = Wire(new DSRequest)
    val bankWdata = Wire(UInt(arrayWriteData.getWidth.W))
    val bankWrite = Wire(new DSECCBankBlock)

    bankReq := Mux(bankAccept, io.req.bits, bankReqReg(i))
    bankWdata := Mux(bankAccept, arrayWriteData, bankWdataReg(i))
    bankWrite.data := bankWdata

    when (bankAccept) {
      bankReqReg(i) := io.req.bits
      bankWdataReg(i) := arrayWriteData
    }
    bankReqHold(i) := bankAccept

    array.io_en := bankAccept
    array.io.w.apply(bankReqValid && bankReq.wen, bankWrite, bankArrayIdx(bankReq), 1.U)
    array.io.r.apply(bankReqValid && !bankReq.wen, bankArrayIdx(bankReq))

    bankReads(i) := array.io.r.resp.data(0)
    val bankError = if (enableDataECC) {
      VecInit(Seq.tabulate(dataBankSplit)(j =>
        bankReads(i).data(encBankBits * (j + 1) - 1, encBankBits * j)
      )).map(data => cacheParams.dataCode.decode(data).error).reduce(_ | _)
    } else {
      false.B
    }
    bankErrors(i) := bankError

    assert(!(bankAccept && bankReqHold(i)),
      s"Continuous SRAM req prohibited under MCP2 for data bank $i!")
    assert(!(bankReqHold(i) && (bankReq.asUInt =/= bankReqReg(i).asUInt)),
      s"DataStorage req fails to hold for 2 cycles in data bank $i!")
    assert(!(bankReqHold(i) && bankReq.wen && (bankWdata.asUInt =/= bankWdataReg(i).asUInt)),
      s"DataStorage wdata fails to hold for 2 cycles in data bank $i!")
  }

  val arrayRead = WireInit(0.U.asTypeOf(new DSECCBankBlock))
  when (readBankOH_s5.orR) {
    arrayRead := Mux1H(readBankOH_s5.asBools, bankReads)
  }
  val dataRead = Wire(new DSBlock)
  val bankDataRead = if (enableDataECC) {
    Cat(VecInit(Seq.tabulate(dataBankSplit)(i => arrayRead.data(encBankBits * (i + 1) - 1, encBankBits * i)(dataBankBits - 1, 0))))
  } else {
    arrayRead.data
  }
  dataRead.data := bankDataRead

  val error = if (enableDataECC) {
    readBankOH_s5.orR && Mux1H(readBankOH_s5.asBools, bankErrors)
  } else {
    false.B
  }

  // for timing, we set this as multicycle path
  // s3 read, s4 pass and s5 to destination
  io.rdata := dataRead
  io.error := error

  assert(io.en === io.req.valid,
    "DataStorage expects MainPipe to issue a single-cycle request pulse on both io.en and io.req.valid")
}
