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
import coupledL2.{L2ToL1Hint, DirResult}
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

  // TODO
  io <> DontCare
}