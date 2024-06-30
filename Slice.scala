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
  val refillBuf = Module(new MSHRBuffer())

  val reqBuf = Module(new RequestBuffer())
  val reqArb = Module(new RequestArb())
  val mainPipe = Module(new MainPipe())
  val mshrCtl = Module(new MSHRCtl())
  val requestUnit = Module(new RequestUnit())
  val responseUnit = Module(new ResponseUnit())
  
  rxreqUp.io.req <> rxUp.req

  rxrspUp.io.in <> rxUp.rsp

  rxdatUp.io.dat <> rxUp.dat
  rxdatUp.io.task.ready := false.B

  txsnpUp.io.task <> mainPipe.io.toTXSNP.task_s4

  txrspUp.io.task <> responseUnit.io.taskToTXRSP

  txdatUp.io.task <> responseUnit.io.taskToTXDAT

  rxrspDown.io.in <> rxDown.rsp

  rxdatDown.io.dat <> rxDown.dat

  txreqDown.io.task <> requestUnit.io.taskToTXREQ

  txdatDown.io.task <> requestUnit.io.taskToTXDAT

  txUp.dat <> txdatUp.io.dat
  txUp.rsp <> txrspUp.io.rsp
  txUp.snp <> txsnpUp.io.snp

  txDown.req <> txreqDown.io.req
  txDown.dat <> txdatDown.io.dat

  rxreqUp.io.task := DontCare
  txreqDown.io.task := DontCare
  io.waitPCrdInfo := DontCare

  reqBuf.io.in <> rxreqUp.io.task

  reqArb.io.busTask_s1 <> reqBuf.io.out
  reqArb.io.mshrTask_s1 <> mshrCtl.io.mshrTask

  mainPipe.io.taskFromArb_s2 <> reqArb.io.taskToPipe_s2
  mainPipe.io.dirResp_s3 <> directory.io.resp.bits
  mainPipe.io.refillBufResp_s4 := RegEnable(
    refillBuf.io.resp,
    0.U.asTypeOf(new MSHRBufResp()),
    RegNext(refillBuf.io.r.valid, false.B)
  )
  mainPipe.io.rdataFromDS_s6 <> dataStorage.io.rdata

  directory.io := DontCare
  directory.io.read <> reqArb.io.dirRead_s1

  dataStorage.io.read <> mainPipe.io.toDS.read_s4
  dataStorage.io.write <> mainPipe.io.toDS.write_s4
  dataStorage.io.wdata <> mainPipe.io.toDS.wdata_s4

  refillBuf.io.r <> reqArb.io.refillBufRead_s2
  refillBuf.io.w <> rxdatUp.io.refillBufWrite

  mshrCtl.io.fromMainPipe <> mainPipe.io.toMSHRCtl

  requestUnit.io.fromMainPipe <> mainPipe.io.toRequestUnit
  requestUnit.io.rspFromRXRSP <> rxrspDown.io.out
  requestUnit.io.rdataFromDS_s6 <>dataStorage.io.rdata

  responseUnit.io.fromMainPipe <> mainPipe.io.toResponseUnit
  responseUnit.io.taskFromRXDAT <> rxdatDown.io.task
  responseUnit.io.rspFromRXRSP <> rxrspUp.io.out

  println(s"addrBits $fullAddressBits")

}
