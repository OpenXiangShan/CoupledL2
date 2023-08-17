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
import freechips.rocketchip.util.leftOR
import chipsalliance.rocketchip.config.Parameters
import coupledL2.utils._
import coupledL2.debug._
import coupledL2.prefetch.PrefetchIO
import utility.RegNextN

class Slice()(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle {
    val in = Flipped(TLBundle(edgeIn.bundle))
    val out = TLBundle(edgeOut.bundle)
    val sliceId = Input(UInt(bankBits.W))
    val l1Hint = Decoupled(new L2ToL1Hint())
    val prefetch = prefetchOpt.map(_ => Flipped(new PrefetchIO))
    val msStatus = topDownOpt.map(_ => Vec(mshrsAll, ValidIO(new MSHRStatus)))
    val dirResult = topDownOpt.map(_ => ValidIO(new DirResult))
    val latePF = topDownOpt.map(_ => Output(Bool()))
  })

  val reqArb = Module(new RequestArb())
  val a_reqBuf = Module(new RequestBuffer)
  val mainPipe = Module(new MainPipe())
  val mshrCtl = Module(new MSHRCtl())
  val directory = Module(new Directory())
  val dataStorage = Module(new DataStorage())
  val refillUnit = Module(new RefillUnit())
  val sinkA = Module(new SinkA)
  val sinkB = Module(new SinkB)
  val sinkC = Module(new SinkC)
  val sourceC = Module(new SourceC)
  val grantBuf = if (!useFIFOGrantBuffer) Module(new GrantBuffer) else Module(new GrantBufferFIFO)
  val refillBuf = Module(new MSHRBuffer(wPorts = 3))
  val releaseBuf = Module(new MSHRBuffer(wPorts = 3))

  val prbq = Module(new ProbeQueue())
  prbq.io <> DontCare // @XiaBin TODO

  a_reqBuf.io.in <> sinkA.io.task
  a_reqBuf.io.mshrInfo := mshrCtl.io.msInfo
  a_reqBuf.io.mainPipeBlock := mainPipe.io.toReqBuf
  a_reqBuf.io.s1Entrance := reqArb.io.s1Entrance
  sinkB.io.msInfo := mshrCtl.io.msInfo
  sinkC.io.msInfo := mshrCtl.io.msInfo

  reqArb.io.sinkA <> a_reqBuf.io.out
  reqArb.io.ATag := a_reqBuf.io.ATag
  reqArb.io.ASet := a_reqBuf.io.ASet

  reqArb.io.sinkB <> sinkB.io.task
  reqArb.io.sinkC <> sinkC.io.task
  reqArb.io.dirRead_s1 <> directory.io.read
  reqArb.io.taskToPipe_s2 <> mainPipe.io.taskFromArb_s2
  reqArb.io.mshrTask <> mshrCtl.io.mshrTask
  reqArb.io.refillBufRead_s2 <> refillBuf.io.r
  reqArb.io.releaseBufRead_s2 <> releaseBuf.io.r
  reqArb.io.fromMSHRCtl := mshrCtl.io.toReqArb
  reqArb.io.fromMainPipe := mainPipe.io.toReqArb
  reqArb.io.fromGrantBuffer := grantBuf.io.toReqArb

  mshrCtl.io.fromReqArb.status_s1 := reqArb.io.status_s1
  mshrCtl.io.resps.sinkC := sinkC.io.resp
  mshrCtl.io.resps.sinkD := refillUnit.io.resp
  mshrCtl.io.resps.sinkE := grantBuf.io.e_resp
  mshrCtl.io.resps.sourceC := sourceC.io.resp
  mshrCtl.io.nestedwb := mainPipe.io.nestedwb
  mshrCtl.io.bMergeTask := sinkB.io.bMergeTask
  mshrCtl.io.replResp <> directory.io.replResp
  mainPipe.io.replResp <> directory.io.replResp

  directory.io.resp <> mainPipe.io.dirResp_s3
  directory.io.metaWReq <> mainPipe.io.metaWReq
  directory.io.tagWReq <> mainPipe.io.tagWReq
  directory.io.msInfo <> mshrCtl.io.msInfo

  dataStorage.io.req <> mainPipe.io.toDS.req_s3
  dataStorage.io.wdata := mainPipe.io.toDS.wdata_s3
  
  mainPipe.io.toMSHRCtl <> mshrCtl.io.fromMainPipe
  mainPipe.io.fromMSHRCtl <> mshrCtl.io.toMainPipe
  mainPipe.io.bufRead <> sinkC.io.bufRead
  mainPipe.io.bufResp <> sinkC.io.bufResp
  mainPipe.io.toDS.rdata_s5 := dataStorage.io.rdata
  mainPipe.io.refillBufResp_s3.valid := RegNext(refillBuf.io.r.valid && refillBuf.io.r.ready, false.B)
  mainPipe.io.refillBufResp_s3.bits := refillBuf.io.r.data
  mainPipe.io.releaseBufResp_s3.valid := RegNext(releaseBuf.io.r.valid && releaseBuf.io.r.ready, false.B)
  mainPipe.io.releaseBufResp_s3.bits := releaseBuf.io.r.data
  mainPipe.io.fromReqArb.status_s1 := reqArb.io.status_s1
  mainPipe.io.grantBufferHint := grantBuf.io.l1Hint
  mainPipe.io.globalCounter := grantBuf.io.globalCounter
  mainPipe.io.taskInfo_s1 <> reqArb.io.taskInfo_s1

  // priority: nested-ReleaseData / probeAckData [NEW] > mainPipe DS rdata [OLD]
  // 0/1 might happen at the same cycle with 2
  releaseBuf.io.w(0).valid := mshrCtl.io.nestedwbDataId.valid
  releaseBuf.io.w(0).beat_sel := Fill(beatSize, 1.U(1.W))
  releaseBuf.io.w(0).data := mainPipe.io.nestedwbData
  releaseBuf.io.w(0).id := mshrCtl.io.nestedwbDataId.bits
  releaseBuf.io.w(1) <> sinkC.io.releaseBufWrite
  releaseBuf.io.w(1).id := mshrCtl.io.releaseBufWriteId
  releaseBuf.io.w(2) <> mainPipe.io.releaseBufWrite

  refillBuf.io.w(0) <> refillUnit.io.refillBufWrite
  refillBuf.io.w(1) <> sinkC.io.refillBufWrite
  refillBuf.io.w(2) <> mainPipe.io.refillBufWrite

  sourceC.io.in <> mainPipe.io.toSourceC
  
  io.l1Hint.valid := mainPipe.io.l1Hint.valid
  io.l1Hint.bits := mainPipe.io.l1Hint.bits
  mshrCtl.io.grantStatus := grantBuf.io.grantStatus

  grantBuf.io.d_task <> mainPipe.io.toSourceD
  grantBuf.io.fromReqArb.status_s1 := reqArb.io.status_s1
  grantBuf.io.pipeStatusVec := reqArb.io.status_vec ++ mainPipe.io.status_vec
  mshrCtl.io.pipeStatusVec(0) := reqArb.io.status_vec(1) // s2 status
  mshrCtl.io.pipeStatusVec(1) := mainPipe.io.status_vec(0) // s3 status

  io.prefetch.foreach {
    p =>
      p.train <> mainPipe.io.prefetchTrain.get
      sinkA.io.prefetchReq.get <> p.req
      p.resp <> grantBuf.io.prefetchResp.get
      p.recv_addr := 0.U.asTypeOf(ValidIO(UInt(64.W)))
  }

  /* input & output signals */
  val inBuf = cacheParams.innerBuf
  val outBuf = cacheParams.outerBuf
  
  /* connect upward channels */
  sinkA.io.a <> inBuf.a(io.in.a)
  io.in.b <> inBuf.b(mshrCtl.io.sourceB)
  sinkC.io.c <> inBuf.c(io.in.c)
  io.in.d <> inBuf.d(grantBuf.io.d)
  grantBuf.io.e <> inBuf.e(io.in.e)

  /* connect downward channels */
  io.out.a <> outBuf.a(mshrCtl.io.sourceA)
  sinkB.io.b <> outBuf.b(io.out.b)
  io.out.c <> outBuf.c(sourceC.io.out)
  refillUnit.io.sinkD <> outBuf.d(io.out.d)
  io.out.e <> outBuf.e(refillUnit.io.sourceE)

  dontTouch(io.in)
  dontTouch(io.out)

  topDownOpt.foreach (
    _ => {
      io.msStatus.get        := mshrCtl.io.msStatus.get
      io.dirResult.get.valid := RegNextN(directory.io.read.fire, 2, Some(false.B)) // manually generate dirResult.valid
      io.dirResult.get.bits  := directory.io.resp
      io.latePF.get          := a_reqBuf.io.hasLatePF
    }
  )

  if (cacheParams.enablePerf) {
    val a_begin_times = RegInit(VecInit(Seq.fill(sourceIdAll)(0.U(64.W))))
    val timer = RegInit(0.U(64.W))
    timer := timer + 1.U
    a_begin_times.zipWithIndex.foreach {
      case (r, i) =>
        when (sinkA.io.a.fire() && sinkA.io.a.bits.source === i.U) {
          r := timer
        }
    }
    val d_source = grantBuf.io.d.bits.source
    val delay = timer - a_begin_times(d_source)
    val (first, _, _, _) = edgeIn.count(grantBuf.io.d)
    val delay_sample = grantBuf.io.d.fire && grantBuf.io.d.bits.opcode =/= ReleaseAck && first
    XSPerfHistogram(cacheParams, "a_to_d_delay", delay, delay_sample, 0, 20, 1, true, true)
    XSPerfHistogram(cacheParams, "a_to_d_delay", delay, delay_sample, 20, 300, 10, true, true)
    XSPerfHistogram(cacheParams, "a_to_d_delay", delay, delay_sample, 300, 500, 20, true, true)
    XSPerfHistogram(cacheParams, "a_to_d_delay", delay, delay_sample, 500, 1000, 100, true, false)
  }

  if (cacheParams.enableMonitor) {
    val monitor = Module(new Monitor())
    monitor.io.fromMainPipe <> mainPipe.io.toMonitor
//  monitor.io.nestedWBValid := mshrCtl.io.nestedwbDataId.valid
  } else {
    mainPipe.io.toMonitor <> DontCare
  }
}
