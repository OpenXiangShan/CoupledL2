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
  val sinkC = Module(new SinkC) // or ReleaseUnit?
  val refillBuf = Module(new MSHRBuffer())
  val releaseBuf = Module(new MSHRBuffer(wPorts = releaseBufWPorts))
  val wbq = Module(new WritebackQueue)

  val prbq = Module(new ProbeQueue())
  prbq.io <> DontCare // @XiaBin TODO

  reqArb.io.sinkC <> sinkC.io.toReqArb
  reqArb.io.dirRead_s1 <> directory.io.read
  reqArb.io.taskToPipe_s2 <> mainPipe.io.taskFromArb_s2
  reqArb.io.mshrFull <> mshrCtl.io.mshrFull
  reqArb.io.mshrTask <> mshrCtl.io.mshrTask
  reqArb.io.refillBufRead_s2 <> refillBuf.io.r
  reqArb.io.releaseBufRead_s2 <> releaseBuf.io.r

  mshrCtl.io.resps.sinkC := sinkC.io.resp
  mshrCtl.io.resps.sinkD := refillUnit.io.resp

  directory.io.resp <> mainPipe.io.dirResp_s3
  directory.io.metaWReq <> mainPipe.io.metaWReq
  directory.io.tagWReq <> mainPipe.io.tagWReq

  dataStorage.io.req <> mainPipe.io.toDS.req_s3
  dataStorage.io.wdata := mainPipe.io.toDS.wdata_s3
  
  mainPipe.io.toMSHRCtl <> mshrCtl.io.fromMainPipe
  mainPipe.io.fromMSHRCtl <> mshrCtl.io.toMainPipe
  mainPipe.io.bufRead <> sinkC.io.bufRead
  mainPipe.io.bufResp <> sinkC.io.bufResp
  mainPipe.io.toDS.rdata_s5 := dataStorage.io.rdata
  mainPipe.io.toSourceC <> wbq.io.req
  mainPipe.io.refillBufResp_s3.valid := RegNext(refillBuf.io.r.valid && refillBuf.io.r.ready)
  mainPipe.io.refillBufResp_s3.bits := refillBuf.io.r.data
  mainPipe.io.releaseBufResp_s3.valid := RegNext(releaseBuf.io.r.valid && releaseBuf.io.r.ready)
  mainPipe.io.releaseBufResp_s3.bits := releaseBuf.io.r.data

  releaseBuf.io.w.last <> sinkC.io.releaseBufWrite
  releaseBuf.io.w.last.id := mshrCtl.io.releaseBufWriteId
  VecInit(releaseBuf.io.w.init) <> mainPipe.io.releaseBufWrite

  refillUnit.io.refillBufWrite <> refillBuf.io.w.last

  /* input & output signals */
  val inBuf = cacheParams.innerBuf
  val outBuf = cacheParams.outerBuf
  
  /* connect upward channels */
  reqArb.io.sinkA <> inBuf.a(io.in.a)
  sinkC.io.c <> inBuf.c(io.in.c)
  inBuf.d(io.in.d) <> mainPipe.io.toSourceD

  /* connect downward channels */
  io.out.a <> outBuf.a(mshrCtl.io.sourceA)
  io.out.c <> outBuf.c(wbq.io.c)
  refillUnit.io.sinkD <> outBuf.d(io.out.d)
  io.out.e <> outBuf.e(refillUnit.io.sourceE)

  dontTouch(io.in)
  dontTouch(io.out)
}
