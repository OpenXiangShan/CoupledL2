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
import freechips.rocketchip.amba.axi4.AXI4Parameters._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import coupledL2.tl2chi._
import cc.xiangshan.openncb._
import cc.xiangshan.openncb.axi._
import cc.xiangshan.openncb.chi._

class OpenNCB(implicit p: Parameters) extends LazyModule with HasOpenLLCParameters {
  val ncbParams = p(NCBParametersKey)
  val axi4node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "LLC",
      id = IdRange(0, ncbParams.outstandingDepth),
      aligned = true,
      maxFlight = Some(ncbParams.outstandingDepth)
    ))
  )))

  lazy val module = new OpenNCBImp(this)
}

class OpenNCBImp(wrapper: OpenNCB)(implicit p: Parameters) extends LazyModuleImp(wrapper)
  with HasOpenLLCParameters 
  with HasCHIOpcodes {

  val ncbParams = p(NCBParametersKey)

  val io = IO(new Bundle {
    val chi = Flipped(new NoSnpPortIO())
  })

  println(s"====== OpenNCB ======")
  println(s"CHI Issue Version: ${p(CHIIssue)}")
  println(s"CHI REQ Flit Width: ${io.chi.tx.req.flit.getWidth}")
  println(s"CHI RSP Flit Width: ${io.chi.rx.rsp.flit.getWidth}")
  println(s"CHI DAT Flit Width: ${io.chi.rx.dat.flit.getWidth}")
  println(s"CHI Port Width: ${io.chi.getWidth}")

  val chi = io.chi
  val (axi, edge) = wrapper.axi4node.out.head
  dontTouch(chi)

  val uNCB200 = Module(new NCB200()(new Config((_, _, _) => {
    case AXI4ParametersKey  => new AXI4Parameters(
      idWidth   = edge.bundle.idBits,
      addrWidth = edge.bundle.addrBits,
      dataWidth = edge.bundle.dataBits
    )
    case CHIParametersKey => new CHIParameters(
      issue = p(CHIIssue) match {
        case "B"    => EnumCHIIssue.B
        case "E.b"  => EnumCHIIssue.E
        case _      => throw new IllegalArgumentException(s"Unsupported CHI Issue: ${p(CHIIssue)}")
      },
      nodeIdWidth      = NODEID_WIDTH,
      reqAddrWidth     = ADDR_WIDTH,
      reqRsvdcWidth    = REQ_RSVDC_WIDTH,
      datRsvdcWidth    = DAT_RSVDC_WIDTH,
      dataWidth        = DATA_WIDTH,
      dataCheckPresent = true,
      poisonPresent    = true,
      mpamPresent      = true
    )
    case NCBParametersKey => ncbParams
  })))
  val chi2ncb = uNCB200.io.chi.asToRaw
  val ncb2axi = uNCB200.io.axi.asToRocketChip

  chi2ncb.txlinkactiveack := chi.rx.linkactiveack
  chi2ncb.rxlinkactivereq := chi.tx.linkactivereq
  chi2ncb.rxsactive := chi.txsactive

  Seq((chi2ncb.rxreq, chi.tx.req), (chi2ncb.rxdat, chi.tx.dat), (chi2ncb.txrsp, chi.rx.rsp),
    (chi2ncb.txdat, chi.rx.dat)).foreach { case (e, t) =>
      e.flitpend <> t.flitpend
      e.flitv <> t.flitv
      e.flit <> t.flit
      e.lcrdv <> t.lcrdv
    }

  chi.tx.linkactiveack := chi2ncb.rxlinkactiveack
  chi.rx.linkactivereq := chi2ncb.txlinkactivereq
  chi.rxsactive := chi2ncb.txsactive

  axi <> ncb2axi
}
