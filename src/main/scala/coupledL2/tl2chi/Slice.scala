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
import freechips.rocketchip.tilelink._
import javax.xml.transform.OutputKeys
import org.chipsalliance.cde.config.Parameters
import coupledL2.{L2ToL1Hint, DirResult, Directory, DataStorage, MSHRBuffer, MSHRBufWrite, SinkA, SinkC, GrantBuffer, RequestBuffer}
import coupledL2.prefetch.PrefetchIO

class Slice()(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val in = Flipped(TLBundle(edgeIn.bundle))
    val out = new PortIO
    val sliceId = Input(UInt(bankBits.W))
    val l1Hint = Decoupled(new L2ToL1Hint())
    val prefetch = prefetchOpt.map(_ => Flipped(new PrefetchIO))
    val msStatus = topDownOpt.map(_ => Vec(mshrsAll, ValidIO(new MSHRStatus)))
    val dirResult = topDownOpt.map(_ => ValidIO(new DirResult))
    val latePF = topDownOpt.map(_ => Output(Bool()))
  })

  /* Upwards TileLink-related modules */
  val sinkA = Module(new SinkA)
  val sinkC = Module(new SinkC)
  val grantBuf = Module(new GrantBuffer)

  /* Downwards CHI-related modules */
  val txreq = Module(new TXREQ())
  val txdat = Module(new TXDAT())
  val txrsp = Module(new TXRSP())
  val rxsnp = Module(new RXSNP())
  val rxdat = Module(new RXDAT())
  val rxrsp = Module(new RXRSP())

  /* Data path and control path */
  val directory = Module(new Directory())
  val dataStorage = Module(new DataStorage())
  val refillBuf = Module(new MSHRBuffer(wPorts = 2))
  val releaseBuf = Module(new MSHRBuffer(wPorts = 3))

  val reqArb = Module(new RequestArb())
  val mainPipe = Module(new MainPipe())
  val reqBuf = Module(new RequestBuffer())
  val mshrCtl = Module(new MSHRCtl())

  sinkC.io.msInfo := mshrCtl.io.msInfo

  grantBuf.io.d_task <> mainPipe.io.toSourceD
  grantBuf.io.fromReqArb.status_s1 := reqArb.io.status_s1
  grantBuf.io.pipeStatusVec := reqArb.io.status_vec ++ mainPipe.io.status_vec_toD

  txreq.io.in <> mshrCtl.io.toTXREQ
  txdat.io.in <> mainPipe.io.toTXDAT
  txrsp.io.in <> mainPipe.io.toTXRSP

  directory.io.read <> reqArb.io.dirRead_s1
  directory.io.metaWReq := mainPipe.io.metaWReq
  directory.io.tagWReq := mainPipe.io.tagWReq
  directory.io.msInfo := mshrCtl.io.msInfo

  dataStorage.io.req := mainPipe.io.toDS.req_s3
  dataStorage.io.wdata := mainPipe.io.toDS.wdata_s3

  reqArb.io.sinkA <> reqBuf.io.out
  reqArb.io.rxsnp <> rxsnp.io.in
  reqArb.io.sinkC <> sinkC.io.task
  reqArb.io.mshrTask <> mshrCtl.io.mshrTask
  reqArb.io.fromMSHRCtl := mshrCtl.io.toReqArb
  reqArb.io.fromMainPipe := mainPipe.io.toReqArb
  reqArb.io.fromGrantBuffer := grantBuf.io.toReqArb

  reqBuf.io.in <> sinkA.io.task
  reqBuf.io.mshrInfo := mshrCtl.io.msInfo
  reqBuf.io.mainPipeBlock := mainPipe.io.toReqBuf
  reqBuf.io.s1Entrance := reqArb.io.s1Entrance

  mainPipe.io.taskFromArb_s2 := reqArb.io.taskToPipe_s2
  mainPipe.io.taskInfo_s1 := reqArb.io.taskInfo_s1
  mainPipe.io.fromReqArb.status_s1 := reqArb.io.status_s1
  mainPipe.io.bufResp := sinkC.io.bufResp
  mainPipe.io.dirResp_s3 := directory.io.resp.bits
  mainPipe.io.replResp := directory.io.replResp
  mainPipe.io.fromMSHRCtl <> mshrCtl.io.toMainPipe
  mainPipe.io.bufResp := sinkC.io.bufResp
  mainPipe.io.refillBufResp_s3.valid := RegNext(refillBuf.io.r.valid, false.B)
  mainPipe.io.refillBufResp_s3.bits := refillBuf.io.resp.data
  mainPipe.io.releaseBufResp_s3.valid := RegNext(releaseBuf.io.r.valid, false.B)
  mainPipe.io.releaseBufResp_s3.bits := releaseBuf.io.resp.data
  mainPipe.io.toDS.rdata_s5 := dataStorage.io.rdata
  mainPipe.io.grantBufferHint := grantBuf.io.l1Hint
  mainPipe.io.globalCounter := grantBuf.io.globalCounter

  mshrCtl.io.fromReqArb.status_s1 := reqArb.io.status_s1
  mshrCtl.io.fromMainPipe <> mainPipe.io.toMSHRCtl
  mshrCtl.io.fromMainPipe.mshr_alloc_s3 := mainPipe.io.toMSHRCtl.mshr_alloc_s3
  mshrCtl.io.grantStatus := grantBuf.io.grantStatus
  mshrCtl.io.resps.sinkC := sinkC.io.resp
  mshrCtl.io.resps.rxrsp := rxrsp.io.in
  mshrCtl.io.resps.rxdat := rxdat.io.in
  mshrCtl.io.nestedwb := mainPipe.io.nestedwb
  mshrCtl.io.replResp := directory.io.replResp

  /* Read and write release buffer */
  releaseBuf.io.r := reqArb.io.releaseBufRead_s2
  val nestedWriteReleaseBuf,
    sinkCWriteReleaseBuf,
    mpWriteReleaseBuf = Wire(Valid(new MSHRBufWrite()))
  nestedWriteReleaseBuf.valid := mshrCtl.io.nestedwbDataId.valid
  nestedWriteReleaseBuf.bits.data := mainPipe.io.nestedwbData
  nestedWriteReleaseBuf.bits.id := mshrCtl.io.nestedwbDataId.bits
  sinkCWriteReleaseBuf match { case x =>
    x := sinkC.io.releaseBufWrite
    x.bits.id := mshrCtl.io.releaseBufWriteId
  }
  mpWriteReleaseBuf := mainPipe.io.releaseBufWrite
  releaseBuf.io.w <> VecInit(Seq(
    nestedWriteReleaseBuf,
    sinkCWriteReleaseBuf,
    mpWriteReleaseBuf
  ))

  /* Read and write refill buffer */
  refillBuf.io.r := reqArb.io.refillBufRead_s2
  refillBuf.io.w <> VecInit(Seq(rxdat.io.refillBufWrite, sinkC.io.refillBufWrite))

  io.prefetch.foreach { p =>
    p.train <> mainPipe.io.prefetchTrain.get
    sinkA.io.prefetchReq.get <> p.req
    p.resp <> grantBuf.io.prefetchResp.get
  }

  /* IO Connection */
  io.l1Hint.valid := mainPipe.io.l1Hint.valid
  io.l1Hint.bits := mainPipe.io.l1Hint.bits
  topDownOpt.foreach (
    _ => {
      io.msStatus.get := mshrCtl.io.msStatus.get
      io.dirResult.get.valid := directory.io.resp.valid && !directory.io.replResp.valid // exclude MSHR-Grant read-dir
      io.dirResult.get.bits := directory.io.resp.bits
      io.latePF.get := reqBuf.io.hasLatePF
    }
  )

  /* Connect upwards channels */
  val inBuf = cacheParams.innerBuf
  // val outBuf = tl2tlParams.outerBuf
  sinkA.io.a <> inBuf.a(io.in.a)
  io.in.b <> inBuf.b(mshrCtl.io.toSourceB)
  sinkC.io.c <> inBuf.c(io.in.c)
  io.in.d <> inBuf.d(grantBuf.io.d)
  grantBuf.io.e <> inBuf.e(io.in.e)

  /* Connect downwards channels */
  io.out.tx.req <> txreq.io.out
  io.out.tx.dat <> txdat.io.out
  io.out.tx.rsp <> txrsp.io.out
  rxsnp.io.out <> io.out.rx.snp
  rxdat.io.out <> io.out.rx.dat
  rxrsp.io.out <> io.out.rx.rsp
}