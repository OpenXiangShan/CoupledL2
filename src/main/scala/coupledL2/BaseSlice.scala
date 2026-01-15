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
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLBundle
import utility._
import coupledL2.prefetch.PrefetchIO

trait BaseOuterBundle

abstract class BaseSliceIO[T_OUT <: BaseOuterBundle](implicit p: Parameters) extends L2Bundle {
  val in = Flipped(TLBundle(edgeIn.bundle))
  val out: T_OUT
  val sliceId = Input(UInt(bankBits.W))
  val l1Hint = DecoupledIO(new L2ToL1Hint())
  val prefetch = prefetchOpt.map(_ => Flipped(new PrefetchIO))
  // val msStatus = topDownOpt.map(_ => Vec(mshrsAll, ValidIO(new MSHRStatus)))
  val dirResult = topDownOpt.map(_ => ValidIO(new DirResult))
  val latePF = topDownOpt.map(_ => Output(Bool()))
  val error = DecoupledIO(new L2CacheErrorInfo())
  val l2Miss = Output(Bool())
  val l2Flush = Option.when(cacheParams.enableL2Flush) (Input(Bool()))
  val l2FlushDone = Option.when(cacheParams.enableL2Flush) (Output(Bool()))
  val wpuRead = Option.when(enWPU) (Input(Valid(UInt(fullAddressBits.W))))
}

abstract class BaseSlice[T_OUT <: BaseOuterBundle](implicit p: Parameters) extends L2Module with HasPerfEvents {
  val io: BaseSliceIO[T_OUT]
}
