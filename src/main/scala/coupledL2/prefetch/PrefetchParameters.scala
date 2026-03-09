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

package coupledL2.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2._
import utility.MemReqSource

trait PrefetchParameters {
  val hasPrefetchBit:  Boolean
  val hasPrefetchSrc:  Boolean
  val inflightEntries: Int // max num of inflight prefetch reqs
}

trait HasPrefetchParameters extends HasCoupledL2Parameters {
  def inflightEntries = if(prefetchers.nonEmpty) prefetchers.map(_.inflightEntries).max else 0
  val peBits = 20
  val banks = 1 << bankBits
  val degreeBits = 2

  // I-POP style delay counting rules at prefetch refill return
  def estTcas = 30.24 // Column Access Strobe latency  16 cycles(CL  ) * 0.63 ns * 3 GHz = 30.24 cycle
  def estTrcd = 34.02 // Row to Column Delay           18 cycles(tRCD) * 0.63 ns * 3 GHz = 34.02 cycle
  def estTrp  = 34.02 // Row Precharge time            18 cycles(tRP ) * 0.63 ns * 3 GHz = 34.02 cycle
  def estRBMissRate = 0.5 // NOTE: this is a very rough estimation for now, because there is no way to get it.
  def estTcommand = 1 // Ref origin paper
  def estTdata = 1 // Ref origin paper
  def estTnoc = 0 // Ref origin paper: 0 cycle for single-core, 2 cycles for multi-core
  def estTbus = ((1 + 2 * estRBMissRate) * estTcommand + estTdata).toInt // Ref origin paper
  def estTbank = (estTcas + estRBMissRate * (estTrp + estTrcd)).toInt // Ref origin paper
}

abstract class PrefetchBundle(implicit val p: Parameters) extends Bundle with HasPrefetchParameters
abstract class PrefetchModule(implicit val p: Parameters) extends Module with HasPrefetchParameters

object PfSource extends Enumeration {
  val NoWhere = Value("NoWhere")
  val SMS     = Value("SMS")
  val BOP     = Value("BOP")
  val PBOP     = Value("PBOP")
  val Stream  = Value("Stream")
  val Stride  = Value("Stride")
  val TP      = Value("TP")
  val Berti   = Value("Berti")

  val PfSourceCount = Value("PfSourceCount")
  val pfSourceBits = log2Ceil(PfSourceCount.id)

  def fromMemReqSource(s: UInt): UInt = {
    val pfsrc = WireInit(NoWhere.id.U.asTypeOf(UInt(pfSourceBits.W)))
    switch(s) {
      is (MemReqSource.Prefetch2L2BOP.id.U) { pfsrc := BOP.id.U }
      is (MemReqSource.Prefetch2L2PBOP.id.U) { pfsrc := PBOP.id.U }
      is (MemReqSource.Prefetch2L2SMS.id.U) { pfsrc := SMS.id.U }
      is (MemReqSource.Prefetch2L2TP.id.U)  { pfsrc := TP.id.U  }
      is (MemReqSource.Prefetch2L2Stream.id.U) { pfsrc := Stream.id.U }
      is (MemReqSource.Prefetch2L2Stride.id.U) { pfsrc := Stride.id.U }
      is (MemReqSource.Prefetch2L2Berti.id.U) { pfsrc := Berti.id.U }
    }
    pfsrc
  }
}
