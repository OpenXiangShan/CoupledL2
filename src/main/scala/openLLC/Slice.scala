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
import coupledL2.tl2chi.{DecoupledPortIO, PCrdInfo}

class Slice()(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val in = Flipped(new DecoupledPortIO)
    val out = AXI4Bundle(edgeOut.bundle)

    val waitPCrdInfo = Output(Vec(mshrs, new PCrdInfo))
  })

  val tx = io.in.rx
  val rx = io.in.tx

  /* UpStream CHI-related modules */
  val txrsp = Module(new TXRSP())
  val txdat = Module(new TXDAT())
  val txsnp = Module(new TXSNP())

  val rxreq = Module(new RXREQ())
  val rxrsp = Module(new RXRSP())
  val rxdat = Module(new RXDAT())

  /* Data path and control path */
  val mainPipe = Module(new MainPipe())

  tx.dat <> txdat.io.txdat
  tx.rsp <> txrsp.io.txrsp
  tx.snp <> txsnp.io.txsnp

  rx.req <> rxreq.io.rxreq
  rx.rsp <> rxrsp.io.rxrsp
  rx.dat <> rxdat.io.rxdat

  txsnp.io.task := DontCare
  rxreq.io.task := DontCare

  io.out := DontCare
  io.waitPCrdInfo := DontCare

  println(s"addrBits $fullAddressBits")

}
