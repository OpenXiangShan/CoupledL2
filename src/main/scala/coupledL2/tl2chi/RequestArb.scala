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
import utility._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import org.chipsalliance.cde.config.Parameters
import coupledL2._

class RequestArb(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val sinkA = Flipped(DecoupledIO(new TaskBundle()))
    val s1Entrance = ValidIO(new TL2CHIL2Bundle {
      val set = UInt(setBits.W)
    })
    val rxsnp = Flipped(DecoupledIO(new TaskBundle()))
    val sinkC = Flipped(DecoupledIO(new TaskBundle()))
    val mshrTask = Flipped(DecoupledIO(new TaskBundle()))

    /* read/write directory */
    val dirRead_s1 = DecoupledIO(new DirRead())  // To directory, read meta/tag

    /* send task to mainpipe */
    val taskToPipe_s2 = ValidIO(new TaskBundle())
    /* send s1 task info to mainpipe to help hint */
    val taskInfo_s1 = ValidIO(new TaskBundle())

    /* send mshrBuf read request */
    val refillBufRead_s2 = ValidIO(new MSHRBufRead)
    val releaseBufRead_s2 = ValidIO(new MSHRBufRead)

    /* status of each pipeline stage */
    val status_s1 = Output(new PipeEntranceStatus) // set & tag of entrance status
    val status_vec = Vec(2, ValidIO(new PipeStatus)) // whether this stage will flow into SourceD

    /* handle set conflict, capacity conflict */
    val fromMSHRCtl = Input(new BlockInfo())
    val fromMainPipe = Input(new BlockInfo())
    val fromGrantBuffer = Input(new Bundle() {
      val blockSinkReqEntrance = new BlockInfo()
      val blockMSHRReqEntrance = Bool()
    })
    // TODO
  })

  // TODO
  io <> DontCare
}