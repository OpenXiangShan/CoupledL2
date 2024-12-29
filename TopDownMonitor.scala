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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{XSPerfAccumulate}

class TopDownMonitor()(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    val msStatus = Vec(banks, Vec(mshrs.response, Flipped(ValidIO(new ResponseInfo()))))
    val debugTopDown = new Bundle() {
      val robHeadPaddr = Vec(cacheParams.hartIds.length, Flipped(Valid(UInt(48.W))))
      val addrMatch = Vec(cacheParams.hartIds.length, Output(Bool()))
    }
  })

  /* ====== PART ONE ======
  * Check whether the Addr given by core is a Miss in Cache
  */
  for (((hartId, pAddr), addrMatch) <- cacheParams.hartIds zip io.debugTopDown.robHeadPaddr zip io.debugTopDown.addrMatch) {
    val addrMatchVec = io.msStatus.zipWithIndex.map {
      case(slice, i) =>
        slice.map {
          ms =>
            val msBlockAddr = if(bankBits == 0) Cat(ms.bits.tag, ms.bits.set)
                              else Cat(ms.bits.tag, ms.bits.set, i.U(bankBits - 1, 0))
            val pBlockAddr  = (pAddr.bits >> 6.U).asUInt

            val isMiss   = ms.valid && ms.bits.is_miss
            pAddr.valid && msBlockAddr === pBlockAddr && isMiss
        }
    }

    addrMatch := Cat(addrMatchVec.flatten).orR
    XSPerfAccumulate(s"${cacheParams.name}MissMatch_${hartId}", addrMatch)
  }
}

