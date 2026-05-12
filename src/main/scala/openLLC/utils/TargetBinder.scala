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
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import coupledL2.tl2chi._
import scala.collection.mutable.{Map}
import utility.{FastArbiter}

object TargetBinder {
  def route(in: PortIO, map: Map[AddressSet, Int])(implicit p: Parameters): Map[Int, PortIO] = {
    val sam = SAM(map.toSeq)
    val outSize = map.values.toSet.size
    val inner = Wire(new DecoupledPortIO())
    val outer = Wire(Vec(outSize, new DecoupledPortIO()))
    val out = Wire(Vec(outSize, new PortIO()))
    val linkMonitor = Module(new ReceiverLinkMonitor())
    val rxsnpArb = Module(new FastArbiter(new CHISNP(), outSize))
    val rxrspArb = Module(new FastArbiter(new CHIRSP(), outSize))
    val rxdatArb = Module(new FastArbiter(new CHIDAT(), outSize))
    val routerMap = Map[Int, PortIO]()

    inner <> linkMonitor.io.out
    inner.tx.req.bits.tgtID := sam.lookup(inner.tx.req.bits.addr)
    inner.tx.req.ready := Cat(outer.map(_.tx.req.fire)).orR
    inner.tx.rsp.ready := Cat(outer.map(_.tx.rsp.fire)).orR
    inner.tx.dat.ready := Cat(outer.map(_.tx.dat.fire)).orR
    inner.rx.snp.valid := rxsnpArb.io.out.valid
    inner.rx.rsp.valid := rxrspArb.io.out.valid
    inner.rx.dat.valid := rxdatArb.io.out.valid
    inner.rx.snp.bits := rxsnpArb.io.out.bits
    inner.rx.rsp.bits := rxrspArb.io.out.bits
    inner.rx.dat.bits := rxdatArb.io.out.bits
    rxsnpArb.io.out.ready := inner.rx.snp.ready
    rxrspArb.io.out.ready := inner.rx.rsp.ready
    rxdatArb.io.out.ready := inner.rx.dat.ready
    linkMonitor.io.in <> in

    for ((key, i) <- map.values.toSet.zipWithIndex) {
      val linkMonitor = Module(new TransmitterLinkMonitor())
      rxsnpArb.io.in(i).valid := outer(i).rx.snp.valid
      rxrspArb.io.in(i).valid := outer(i).rx.rsp.valid
      rxdatArb.io.in(i).valid := outer(i).rx.dat.valid
      rxsnpArb.io.in(i).bits := outer(i).rx.snp.bits
      rxrspArb.io.in(i).bits := outer(i).rx.rsp.bits
      rxdatArb.io.in(i).bits := outer(i).rx.dat.bits

      outer(i).tx.req.valid := inner.tx.req.valid && inner.tx.req.bits.tgtID === key.U
      outer(i).tx.rsp.valid := inner.tx.rsp.valid && inner.tx.rsp.bits.tgtID === key.U
      outer(i).tx.dat.valid := inner.tx.dat.valid && inner.tx.dat.bits.tgtID === key.U
      outer(i).rx.snp.ready := rxsnpArb.io.out.fire && rxsnpArb.io.chosen === i.U
      outer(i).rx.rsp.ready := rxrspArb.io.out.fire && rxrspArb.io.chosen === i.U
      outer(i).rx.dat.ready := rxdatArb.io.out.fire && rxdatArb.io.chosen === i.U
      outer(i).tx.req.bits := inner.tx.req.bits
      outer(i).tx.rsp.bits := inner.tx.rsp.bits
      outer(i).tx.dat.bits := inner.tx.dat.bits
      out(i) <> linkMonitor.io.out

      linkMonitor.io.in <> outer(i)

      routerMap += (key -> out(i))
    }
    routerMap
  }

  def bind(srcMap: Map[Int, PortIO], sinkMap: Map[Int, PortIO]): Unit = {
    for (key <- srcMap.keys) {
      if (sinkMap.contains(key)) {
        srcMap(key) <> sinkMap(key)
      }
    }
  }
}
