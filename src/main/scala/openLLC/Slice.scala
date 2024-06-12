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
  val txrspUp = Module(new UpTXRSP())
  val txdatUp = Module(new UpTXDAT())
  val txsnpUp = Module(new UpTXSNP())

  val rxreqUp = Module(new UpRXREQ())
  val rxrspUp = Module(new UpRXRSP())
  val rxdatUp = Module(new UpRXDAT())

  /* DownStream CHI-related modules */
  val txreqDown = Module(new DownTXREQ())
  val txdatDown = Module(new DownTXDAT())

  val rxrspDown = Module(new DownRXRSP())
  val rxdatDown = Module(new DownRXDAT())

  /* Data path and control path */
  val mainPipe = Module(new MainPipe())

  txUp.dat <> txdatUp.io.dat
  txUp.rsp <> txrspUp.io.rsp
  txUp.snp <> txsnpUp.io.snp

  rxUp.req <> rxreqUp.io.req
  rxUp.rsp <> rxrspUp.io.rsp
  rxUp.dat <> rxdatUp.io.dat

  txDown.req <> txreqDown.io.req
  txDown.dat <> txdatDown.io.dat

  rxDown.rsp <> rxrspDown.io.rsp
  rxDown.dat <> rxdatDown.io.dat

  txsnpUp.io.task := DontCare
  rxreqUp.io.task := DontCare
  txreqDown.io.task := DontCare

  io.waitPCrdInfo := DontCare

  println(s"addrBits $fullAddressBits")

}
