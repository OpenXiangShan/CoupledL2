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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

case class MyPrefetchParameters(
  // TODO
) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true
  override val inflightEntries: Int = 16 // TODO
  override def toString: String = s"My prefetch :)"
}

trait HasMyPrefetchParams extends HasPrefetcherHelper {
  def params = prefetchers.find {
    case p: MyPrefetchParameters => true
    case _ => false
  }.get.asInstanceOf[MyPrefetchParameters]
  // TODO
}

abstract class MyPrefetchModule(implicit val p: Parameters) extends Module with HasMyPrefetchParams

class MyPrefetch(implicit p: Parameters) extends MyPrefetchModule {
  val io = IO(new L2PrefetchIO())

  // TODO
  io <> DontCare
}
