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
import coupledL2.tl2chi.{DecoupledPortIO, PCrdInfo, DecoupledNoSnpPortIO}

class Slice()(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val in = Flipped(new DecoupledPortIO)
    val out = new DecoupledNoSnpPortIO

    val waitPCrdInfo = Output(Vec(mshrs, new PCrdInfo))
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
  
  rxreqUp.io.req <> rxUp.req

  rxrspUp.io.in <> rxUp.rsp

  rxdatUp.io.in <> rxUp.dat

  txsnpUp.io.task <> snpUnit.io.out

  txrspUp.io.task <> responseUnit.io.txrsp

  txdatUp.io.task <> responseUnit.io.txdat

  rxrspDown.io.in <> rxDown.rsp

  rxdatDown.io.in <> rxDown.dat

  txreqDown.io.task <> memUnit.io.txreq

  txdatDown.io.task <> memUnit.io.txdat

  txUp.dat <> txdatUp.io.dat
  txUp.rsp <> txrspUp.io.rsp
  txUp.snp <> txsnpUp.io.snp

  txDown.req <> txreqDown.io.req
  txDown.dat <> txdatDown.io.dat

  txreqDown.io.task := DontCare
  io.waitPCrdInfo := DontCare

  reqBuf.io.in <> rxreqUp.io.task

  reqArb.io.busTask_s1 <> reqBuf.io.out
  reqArb.io.refillTask_s1 <> refillUnit.io.task_out

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
  refillUnit.io.resp <> rxdatUp.io.out

  refillUnit.io.task_in <> mainPipe.io.refillTask_s4

  memUnit.io.fromMainPipe <> mainPipe.io.toMemUnit
  memUnit.io.urgentRead <> responseUnit.io.urgentRead
  memUnit.io.resp <> rxrspDown.io.out

  responseUnit.io.fromMainPipe <> mainPipe.io.toResponseUnit
  responseUnit.io.bypassData <> memUnit.io.bypassData
  responseUnit.io.memData <> rxdatDown.io.out
  responseUnit.io.snpData <> rxdatUp.io.out
  responseUnit.io.response <> rxrspUp.io.out

  snpUnit.io.in <> mainPipe.io.snoopTask_s4
  snpUnit.io.respInfo <> responseUnit.io.respInfo
  snpUnit.io.ack <> rxrspUp.io.out

  println(s"addrBits $fullAddressBits")

}
