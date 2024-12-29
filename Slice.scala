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
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2chi.{DecoupledPortIO, DecoupledNoSnpPortIO}

class Slice()(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val in = Flipped(new DecoupledPortIO)
    val out = new DecoupledNoSnpPortIO
    val snpMask = Output(Vec(numRNs, Bool()))
    val msStatus = topDownOpt.map(_ => Vec(mshrs.response, ValidIO(new ResponseInfo())))
  })

  val txUp = io.in.rx
  val rxUp = io.in.tx
  val txDown = io.out.tx
  val rxDown = io.out.rx

  /* UpStream CHI-related modules */
  val txrspUp = Module(new TXRSP())
  val txdatUp = Module(new TXDAT())
  val txsnpUp = Module(new TXSNP())

  val rxreqUp = Module(new RXREQ())
  val rxrspUp = Module(new RXRSP())
  val rxdatUp = Module(new RXDAT())

  /* DownStream CHI-related modules */
  val txreqDown = Module(new TXREQ())
  val txdatDown = Module(new TXDAT())

  val rxrspDown = Module(new RXRSP())
  val rxdatDown = Module(new RXDAT())

  /* Data path and control path */
  val directory = Module(new Directory())
  val dataStorage = Module(new DataStorage())

  val reqBuf = Module(new RequestBuffer())
  val reqArb = Module(new RequestArb())
  val mainPipe = Module(new MainPipe())
  val refillUnit = Module(new RefillUnit())
  val memUnit = Module(new MemUnit())
  val responseUnit = Module(new ResponseUnit())
  val snpUnit = Module(new SnoopUnit())
  
  /* Connect upwards channels */
  rxreqUp.io.req <> rxUp.req
  rxrspUp.io.in <> rxUp.rsp
  rxdatUp.io.in <> rxUp.dat
  txUp.dat <> txdatUp.io.dat
  txUp.rsp <> txrspUp.io.rsp
  txUp.snp <> txsnpUp.io.snp

  /* Connect downwards channels */
  txDown.req <> txreqDown.io.req
  txDown.dat <> txdatDown.io.dat
  rxrspDown.io.in <> rxDown.rsp
  rxdatDown.io.in <> rxDown.dat

  txsnpUp.io.task <> snpUnit.io.out
  txrspUp.io.task <> responseUnit.io.txrsp
  txdatUp.io.task <> responseUnit.io.txdat
  txreqDown.io.task <> memUnit.io.txreq
  txdatDown.io.task <> memUnit.io.txdat
  txreqDown.io.task <> memUnit.io.txreq

  reqBuf.io.in <> rxreqUp.io.task

  reqArb.io.busTask_s1 <> reqBuf.io.out
  reqArb.io.refillTask_s1 <> refillUnit.io.task
  reqArb.io.pipeInfo <> mainPipe.io.pipeInfo
  reqArb.io.refillInfo <> refillUnit.io.refillInfo
  reqArb.io.respInfo <> responseUnit.io.respInfo
  reqArb.io.snpInfo <> snpUnit.io.snpInfo
  reqArb.io.memInfo <> memUnit.io.memInfo

  mainPipe.io.taskFromArb_s2 <> reqArb.io.taskToPipe_s2
  mainPipe.io.dirResp_s3 <> directory.io.resp.bits
  mainPipe.io.refillBufResp_s4 := RegEnable(
    refillUnit.io.data,
    0.U.asTypeOf(new DSBlock()),
    RegNext(refillUnit.io.read.valid, false.B)
  )
  mainPipe.io.rdataFromDS_s6 <> dataStorage.io.rdata

  directory.io.read <> reqArb.io.dirRead_s1
  directory.io.write <> mainPipe.io.dirWReq_s3

  dataStorage.io.read <> mainPipe.io.toDS_s4.read
  dataStorage.io.write <> mainPipe.io.toDS_s4.write
  dataStorage.io.wdata <> mainPipe.io.toDS_s4.wdata

  refillUnit.io.read <> reqArb.io.refillBufRead_s2
  refillUnit.io.respData <> rxdatUp.io.out
  refillUnit.io.resp <> rxrspUp.io.out
  refillUnit.io.alloc <> mainPipe.io.refillReq_s4

  memUnit.io.fromMainPipe <> mainPipe.io.toMemUnit
  memUnit.io.urgentRead <> responseUnit.io.urgentRead
  memUnit.io.rnRxdat <> rxdatUp.io.out
  memUnit.io.rnRxrsp <> rxrspUp.io.out
  memUnit.io.snRxrsp <> rxrspDown.io.out
  memUnit.io.respInfo <> responseUnit.io.respInfo

  responseUnit.io.fromMainPipe <> mainPipe.io.toResponseUnit
  responseUnit.io.bypassData <> memUnit.io.bypassData
  responseUnit.io.rnRxdat <> rxdatUp.io.out
  responseUnit.io.rnRxrsp <> rxrspUp.io.out
  responseUnit.io.snRxdat <> rxdatDown.io.out
  responseUnit.io.snRxrsp <> rxrspDown.io.out

  snpUnit.io.in <> mainPipe.io.snoopTask_s4
  snpUnit.io.respInfo <> responseUnit.io.respInfo
  snpUnit.io.ack <> rxrspUp.io.out

  io.snpMask := txsnpUp.io.snpMask

  topDownOpt.foreach {_ =>
    io.msStatus.get.zip(responseUnit.io.respInfo).foreach {
      case (sink, src) =>
        sink := src
    }
  }
}
