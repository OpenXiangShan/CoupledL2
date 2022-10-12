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
import freechips.rocketchip.util.leftOR
import chipsalliance.rocketchip.config.Parameters
import coupledL2.utils._

class Slice()(implicit p: Parameters) extends L2Module with DontCareInnerLogic {
  val io = IO(new Bundle {
    val in = Flipped(TLBundle(edgeIn.bundle))
    val out = TLBundle(edgeOut.bundle)
  })

  val reqArb = Module(new RequestArb())
  val mainPipe = Module(new MainPipe())
  val mshrCtl = Module(new MSHRCtl())
  val directory = Module(new Directory())
  val dataStorage = Module(new DataStorage())
  val refillUnit = Module(new RefillUnit())
  val mshrBuf = Module(new MSHRBuffer())

  reqArb.io.dirRead_s1 <> directory.io.read
  reqArb.io.metaWrite_s1 <> directory.io.metaWReq
  reqArb.io.taskToPipe_s2 <> mainPipe.io.taskFromArb_s2
  reqArb.io.mshrFull <> mshrCtl.io.mshrFull
  reqArb.io.wdataToDS_s2 <> dataStorage.io.wdata_s2
  reqArb.io.mshrTask <> mshrCtl.io.mshrTask
  reqArb.io.mshrTaskID <> mshrCtl.io.mshrTaskID
  reqArb.io.mshrBufRead <> mshrBuf.io.r

  directory.io.resp <> mainPipe.io.dirResp_s3
  dataStorage.io.wen_s3 <> mainPipe.io.wdata_en_s3
  
  mainPipe.io.toMSHRCtl <> mshrCtl.io.fromMainPipe
  mainPipe.io.fromMSHRCtl <> mshrCtl.io.toMainPipe

  refillUnit.io.mshrBufWrite <> mshrBuf.io.w
  refillUnit.io.resp <> mshrCtl.io.refillUnitResp

  /* input & output signals */
  val inBuf = cacheParams.innerBuf
  val outBuf = cacheParams.outerBuf
  reqArb.io.sinkA <> inBuf.a(io.in.a)
  reqArb.io.sinkC <> inBuf.c(io.in.c)
  io.out.a <> outBuf.a(mshrCtl.io.sourceA)
  refillUnit.io.sinkD <> outBuf.d(io.out.d)
  io.out.e <> outBuf.e(refillUnit.io.sourceE)

  directory.io.tagWReq <> DontCare
  dataStorage.io <> DontCare
  dontTouch(io.in)
  dontTouch(io.out)
}
