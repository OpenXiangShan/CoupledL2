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
import coupledL2.tl2chi._

class MainPipe(implicit p: Parameters) extends LLCModule {
  val io = IO(new Bundle() {
    /* receive incoming task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new Task()))

    /* get meta at stage 3 */
    val dirResp_s3 = Input(new DirResult())

    /* get RefillBuffer read result at stage 4 */
    val refillBufResp_s4 = Flipped(new MSHRBufResp())

    /* send allocation request to MSHRCtl at stage 4 */
    val toMSHRCtl = new Bundle() {
      val mshr_alloc_s4 = ValidIO(new MSHRRequest())
    }

    /* send Snoop request via upstream TXSNP channel */
    val toTXSNP = new Bundle() {
      val task_s4 = DecoupledIO(new Task())
    }

    /* send ReadNoSnp/WriteNoSnp task to RequestUnit */
    val toRequestUnit = new Bundle() {
      val task_s4 = DecoupledIO(new Task())
    }

    /* send CompDBIDResp/CompData task to ResponseUnit */
    val toResponseUnit = new Bundle() {
      val compDBIDResp_s4 = DecoupledIO(new Task())
      val compData_s6 = DecoupledIO(new TaskWithData())
    }

    /* interact with datastorage */
    val toDS = new Bundle() {
      val read_s4 = ValidIO(new DSRequest())
      val write_s4 = ValidIO(new DSRequest())
      val wdata_s4 = Output(new DSBlock())
    }
    val rdataFromDS_s6 = Input(new DSBlock())
  })

  io.toDS := DontCare
  io.toMSHRCtl := DontCare
  io.toTXSNP := DontCare
  io.toRequestUnit := DontCare
  io.toResponseUnit := DontCare
}
