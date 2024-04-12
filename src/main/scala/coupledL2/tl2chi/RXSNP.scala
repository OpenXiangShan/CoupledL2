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
import org.chipsalliance.cde.config.Parameters
import scala.collection.View.Fill
import coupledL2.{TaskBundle, MSHRInfo, MetaEntry, MergeTaskBundle}

class RXSNP(
  lCreditNum: Int = 4 // the number of L-Credits that a receiver can provide
)(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val rxsnp = Flipped(DecoupledIO(new CHISNP()))
    val task = DecoupledIO(new TaskBundle())
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo())))
  })

  val task = fromSnpToTaskBundle(io.rxsnp.bits)

  // unable to accept incoming B req because same-addr as some MSHR REQ
  val addrConflictMask = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.reqTag === task.tag && !s.bits.willFree && !s.bits.nestB
  )).asUInt
  val addrConflict = addrConflictMask.orR

  /*
   1. For TL unable to accept incoming B req because same-addr as some MSHR replaced block and cannot nest
   2. For CHI ignore this kind of block
   */
  val replaceConflictMask = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && s.bits.blockRefill && !s.bits.w_releaseack
  )).asUInt
  val replaceConflict = replaceConflictMask.orR

  val stall = addrConflict || replaceConflict
  io.task.valid := io.rxsnp.valid && !stall
  io.task.bits := task
  io.rxsnp.ready := io.task.ready && !stall

  val stallCnt = RegInit(0.U(64.W))
  when(io.rxsnp.fire) {
    stallCnt := 0.U
  }.elsewhen(io.rxsnp.valid && !io.rxsnp.ready) {
    stallCnt := stallCnt + 1.U
  }

  val STALL_CNT_MAX = 28000.U
  assert(stallCnt <= STALL_CNT_MAX, "stallCnt full! maybe there is a deadlock! addr => 0x%x req_opcode => %d txn_id => %d", io.rxsnp.bits.addr, io.rxsnp.bits.opcode, io.rxsnp.bits.txnID);

  assert(!(stall && io.rxsnp.fire))
  dontTouch(addrConflictMask)
  dontTouch(addrConflict)
  dontTouch(replaceConflictMask)
  dontTouch(replaceConflict)

  def fromSnpToTaskBundle(snp: CHISNP): TaskBundle = {
    val task = WireInit(0.U.asTypeOf(new TaskBundle))
    task.channel := "b010".U
    task.tag := parseAddress(snp.addr)._1
    task.set := parseAddress(snp.addr)._2
    task.off := parseAddress(snp.addr)._3
    task.alias.foreach(_ := 0.U)
    task.vaddr.foreach(_ := 0.U)
    task.isKeyword.foreach(_ := false.B)
    // task.opcode := snp.opcode
    task.param := 0.U
    task.size := log2Up(cacheParams.blockBytes).U
    task.sourceId := 0.U(sourceIdBits.W)
    task.bufIdx := 0.U(bufIdxBits.W)
    task.needProbeAckData := false.B
    task.mshrTask := false.B
    task.mshrId := 0.U(mshrBits.W)
    task.aliasTask.foreach(_ := false.B)
    task.useProbeData := false.B
    task.mshrRetry := false.B
    task.fromL2pft.foreach(_ := false.B)
    task.needHint.foreach(_ := false.B)
    task.dirty := false.B
    task.way := 0.U(wayBits.W)
    task.meta := 0.U.asTypeOf(new MetaEntry)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dsWen := false.B
    task.wayMask := Fill(cacheParams.ways, "b1".U)
    task.reqSource := MemReqSource.NoWhere.id.U
    task.replTask := false.B
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    // task.snpHitRelease := replaceConflict   //indicate read release buffer @s2 to get snoop data 
    // task.snpHitReleaseIdx := replaceConflictMask  //the index of ReleaseBuffer that conflict 
    task.tgtID.foreach(_ := 0.U) // TODO
    task.srcID.foreach(_ := snp.srcID)
    task.txnID.foreach(_ := snp.txnID)
    task.dbID.foreach(_ := 0.U)
    task.fwdNID.foreach(_ := snp.fwdNID)
    task.fwdTxnID.foreach(_ := snp.fwdTxnID)
    task.chiOpcode.foreach(_ := snp.opcode)
    task.pCrdType.foreach(_ := 0.U)
    task.retToSrc.foreach(_ := snp.retToSrc)
    task
  }

}
