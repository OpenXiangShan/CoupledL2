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
import org.chipsalliance.cde.config.Parameters
import coupledL2._
import coupledL2.prefetch.PrefetchIO

class OuterBundle(implicit p: Parameters) extends DecoupledPortIO with BaseOuterBundle

class Slice()(implicit p: Parameters) extends BaseSlice[OuterBundle]
  with HasCoupledL2Parameters
  with HasCHIMsgParameters {

  val io = IO(new BaseSliceIO[OuterBundle] {
    override val out: OuterBundle = new OuterBundle
  })
  val io_pCrd = IO(Vec(mshrsAll, new PCrdQueryBundle))
  val io_msStatus = topDownOpt.map(_ => IO(Vec(mshrsAll, ValidIO(new MSHRStatus))))

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

  val status_vec_toTX = reqArb.io.status_vec_toTX.get ++ mainPipe.io.status_vec_toTX
  txreq.io.pipeReq <> mainPipe.io.toTXREQ
  txreq.io.mshrReq <> mshrCtl.io.toTXREQ
  txreq.io.pipeStatusVec := status_vec_toTX
  txreq.io.sliceId := io.sliceId

  txdat.io.in <> mainPipe.io.toTXDAT
  txdat.io.pipeStatusVec := status_vec_toTX

  txrsp.io.pipeRsp <> mainPipe.io.toTXRSP
  txrsp.io.mshrRsp <> mshrCtl.io.toTXRSP
  txrsp.io.pipeStatusVec := status_vec_toTX

  rxsnp.io.msInfo := mshrCtl.io.msInfo

  directory.io.read <> reqArb.io.dirRead_s1
  directory.io.metaWReq := mainPipe.io.metaWReq
  directory.io.tagWReq := mainPipe.io.tagWReq
  directory.io.msInfo := mshrCtl.io.msInfo

  dataStorage.io.en := mainPipe.io.toDS.en_s3
  dataStorage.io.req := mainPipe.io.toDS.req_s3
  dataStorage.io.wdata := mainPipe.io.toDS.wdata_s3

  reqArb.io.ATag := reqBuf.io.ATag
  reqArb.io.ASet := reqBuf.io.ASet
  reqArb.io.sinkA <> reqBuf.io.out
  reqArb.io.sinkB <> rxsnp.io.task
  reqArb.io.sinkC <> sinkC.io.task
  reqArb.io.mshrTask <> mshrCtl.io.mshrTask
  reqArb.io.fromMSHRCtl := mshrCtl.io.toReqArb
  reqArb.io.fromMainPipe := mainPipe.io.toReqArb
  reqArb.io.fromGrantBuffer := grantBuf.io.toReqArb
  reqArb.io.fromTXDAT.foreach(_ := txdat.io.toReqArb)
  reqArb.io.fromTXRSP.foreach(_ := txrsp.io.toReqArb)
  reqArb.io.fromTXREQ.foreach(_ := txreq.io.toReqArb)
  reqArb.io.msInfo := mshrCtl.io.msInfo

  reqBuf.io.in <> sinkA.io.task
  reqBuf.io.mshrInfo := mshrCtl.io.msInfo
  reqBuf.io.mainPipeBlock := mainPipe.io.toReqBuf
  reqBuf.io.s1Entrance := reqArb.io.s1Entrance
  reqBuf.io.taskFromArb_s2 := reqArb.io.taskToPipe_s2

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
  // mainPipe.io.grantBufferHint := grantBuf.io.l1Hint
  // mainPipe.io.globalCounter := grantBuf.io.globalCounter

  mshrCtl.io.fromReqArb.status_s1 := reqArb.io.status_s1
  mshrCtl.io.fromMainPipe <> mainPipe.io.toMSHRCtl
  mshrCtl.io.fromMainPipe.mshr_alloc_s3 := mainPipe.io.toMSHRCtl.mshr_alloc_s3
  mshrCtl.io.grantStatus := grantBuf.io.grantStatus
  mshrCtl.io.resps.sinkC := sinkC.io.resp
  mshrCtl.io.resps.rxrsp := rxrsp.io.in
  mshrCtl.io.resps.rxdat := rxdat.io.in
  mshrCtl.io.nestedwb := mainPipe.io.nestedwb
  mshrCtl.io.replResp := directory.io.replResp
  mshrCtl.io.aMergeTask := reqBuf.io.aMergeTask
  // TODO: This is ugly
  mshrCtl.io.pipeStatusVec(0) := (reqArb.io.status_vec)(1) // s2 status
  mshrCtl.io.pipeStatusVec(1) := mainPipe.io.status_vec_toD(0) // s3 status

  /* Read and write release buffer */
  releaseBuf.io.r := reqArb.io.releaseBufRead_s2
  val nestedWriteReleaseBuf,
    sinkCWriteReleaseBuf,
    mpWriteReleaseBuf = Wire(Valid(new MSHRBufWrite()))
  nestedWriteReleaseBuf.valid := mshrCtl.io.nestedwbDataId.valid
  nestedWriteReleaseBuf.bits.data := mainPipe.io.nestedwbData
  nestedWriteReleaseBuf.bits.id := mshrCtl.io.nestedwbDataId.bits
  nestedWriteReleaseBuf.bits.beatMask := Fill(beatSize, true.B)
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
    p.tlb_req.req.ready := true.B
    p.tlb_req.resp.valid := false.B
    p.tlb_req.resp.bits := DontCare
    p.tlb_req.pmp_resp := DontCare
    p.recv_addr := 0.U.asTypeOf(p.recv_addr)
  }

  /* IO Connection */
  io.l1Hint <> mainPipe.io.l1Hint
  topDownOpt.foreach (
    _ => {
      io_msStatus.get := mshrCtl.io.msStatus.get
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
  rxsnp.io.rxsnp <> io.out.rx.snp
  rxdat.io.out <> io.out.rx.dat
  rxrsp.io.out <> io.out.rx.rsp

  io_pCrd <> mshrCtl.io.pCrd

  /* ===== Hardware Performance Monitor ===== */
  val perfEvents = Seq(mshrCtl, mainPipe).flatMap(_.getPerfEvents)
  generatePerfEvent()
}
