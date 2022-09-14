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
import coupledL2.utils._
import chipsalliance.rocketchip.config.Parameters

class DSRequest(implicit p: Parameters) extends L2Bundle {
  val way = UInt(wayBits.W)
  val set = UInt(setBits.W)
  val beat = UInt(beatBits.W)
  val wen = Bool()
}

class DSData(implicit p: Parameters) extends L2Bundle {
  val data = UInt((beatBytes * 8).W)
}

class DataStorage(implicit p: Parameters) extends L2Module with DontCareInnerLogic {
  val io = IO(new Bundle() {
    val rreq_s3 = Flipped(ValidIO(new DSRequest()))
    val rdata_s6 = Output(new DSData())
  })

}
