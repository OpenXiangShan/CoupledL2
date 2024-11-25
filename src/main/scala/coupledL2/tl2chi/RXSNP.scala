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
import coupledL2.MetaData._

class RXSNP(
  lCreditNum: Int = 4 // the number of L-Credits that a receiver can provide
)(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val rxsnp = Flipped(DecoupledIO(new CHISNP()))
    val task = DecoupledIO(new TaskBundle())
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo())))
  })

  val rxsnp = Wire(io.rxsnp.cloneType)
  val queue = Module(new Queue(io.rxsnp.bits.cloneType, 2, flow = false))
  rxsnp <> queue.io.deq
  queue.io.enq <> io.rxsnp
  val task = Wire(new TaskBundle)

  /**
    * When should an MSHR with Acquire address of X block/nest an incoming snoop with address X?
    * 
    * 1. Before MSHR receives the first beat of CompData, snoop should be **nested** because snoop has higher priority
    *    than request according to CHI spec.
    * 2. After MSHR receives the first beat of CompData, and before L2 receives GrantAck from L1, snoop of X should be
    *    **blocked**, because a slave should not issue a Probe if there is a pending GrantAck on the block according
    *    to TileLink spec.
    * 3. Before MSHR sends out WriteBackFull/Evict to write refilled data into DS, snoop should be **blocked**, Because
    *    the snooped block is still in RefillBuffer rather than DS.
    * 4. After MSHR sends out WriteBackFull/Evict and write refilled data into DS, snoop should be **nested**, still
    *    because snoop has higher priority than request.
    */
  val reqBlockSnpMask = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.reqTag === task.tag &&
    (s.bits.w_grantfirst || s.bits.aliasTask.getOrElse(false.B) && !s.bits.w_rprobeacklast) &&
    (s.bits.blockRefill || s.bits.w_releaseack) && !s.bits.willFree
  )).asUInt
  val reqBlockSnp = reqBlockSnpMask.orR

  /**
    * When should an MSHR that is going to replace cacheline Y block/nest an incoming snoop with address Y?
    * 
    * 1. After MSHR decides which way to replace but before MSHR finished all the rProbes, the incoming snoop of Y
    *    should be **blocked**, because Once the Probe is issued the slave should not issue further Probes on the block
    *    until it receives a ProbeAck.
    * 2. After MSHR receives all the ProbeAcks of rProbe, the snoop of Y should be nested.
    * 3. In CMO transactions (reusing some replacing datapathes), before MSHR finishing all the rProbes and sending
    *    release tasks to MainPipe (DS write was done in release tasks on MainPipe), the incoming snoop of Y should 
    *    be **blocked**.
    */
  val cmoBlockSnpMask = VecInit(io.msInfo.map(s => 
    s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && s.bits.dirHit && isValid(s.bits.metaState) &&
    !s.bits.s_cmoresp && (!s.bits.s_release || !s.bits.w_rprobeacklast) &&
    !s.bits.willFree
  )).asUInt
  val cmoBlockSnp = cmoBlockSnpMask.orR
  val replaceBlockSnpMask = VecInit(io.msInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && !s.bits.dirHit && isValid(s.bits.metaState) &&
    s.bits.s_cmoresp && s.bits.w_replResp && (!s.bits.w_rprobeacklast || s.bits.w_releaseack || !RegNext(s.bits.w_replResp)) &&
    !s.bits.willFree
  )).asUInt
  val replaceBlockSnp = replaceBlockSnpMask.orR
  val replaceNestSnpMask = VecInit(io.msInfo.map(s =>
      s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && 
      (!s.bits.dirHit || (!s.bits.s_cmoresp && !s.bits.releaseToB)) && s.bits.metaState =/= INVALID &&
      RegNext(s.bits.w_replResp) && s.bits.w_rprobeacklast && !s.bits.w_releaseack
    )).asUInt
  val replaceDataMask = VecInit(io.msInfo.map(_.bits.replaceData)).asUInt

  task := fromSnpToTaskBundle(rxsnp.bits)

  val stall = reqBlockSnp || replaceBlockSnp || cmoBlockSnp // addrConflict || replaceConflict
  io.task.valid := rxsnp.valid && !stall
  io.task.bits := task
  rxsnp.ready := io.task.ready && !stall

  val stallCnt = RegInit(0.U(64.W))
  when(rxsnp.fire) {
    stallCnt := 0.U
  }.elsewhen(rxsnp.valid && !rxsnp.ready) {
    stallCnt := stallCnt + 1.U
  }

  val STALL_CNT_MAX = 28000.U
  assert(stallCnt <= STALL_CNT_MAX,
    "stallCnt full! maybe there is a deadlock! addr => 0x%x req_opcode => %d txn_id => %d",
    rxsnp.bits.addr, rxsnp.bits.opcode, rxsnp.bits.txnID)

  assert(!(stall && rxsnp.fire))

  def fromSnpToTaskBundle(snp: CHISNP): TaskBundle = {
    val task = WireInit(0.U.asTypeOf(new TaskBundle))
    task.channel := "b010".U
    // Addr in CHI SNP channel has 3 fewer bits than full address
    val snpFullAddr = Cat(snp.addr, 0.U(3.W))
    task.tag := parseAddress(snpFullAddr)._1
    task.set := parseAddress(snpFullAddr)._2
    task.off := parseAddress(snpFullAddr)._3
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
    task.snpHitRelease := replaceNestSnpMask.orR
    task.snpHitReleaseWithData := (replaceNestSnpMask & replaceDataMask).orR
    task.snpHitReleaseIdx := PriorityEncoder(replaceNestSnpMask)
    task.tgtID.foreach(_ := 0.U) // TODO
    task.srcID.foreach(_ := snp.srcID)
    task.txnID.foreach(_ := snp.txnID)
    task.dbID.foreach(_ := 0.U)
    task.fwdNID.foreach(_ := snp.fwdNID)
    task.fwdTxnID.foreach(_ := snp.fwdTxnID)
    task.chiOpcode.foreach(_ := snp.opcode)
    task.pCrdType.foreach(_ := 0.U)
    task.retToSrc.foreach(_ := snp.retToSrc)
    task.traceTag.foreach(_ := snp.traceTag)
    task
  }

}
