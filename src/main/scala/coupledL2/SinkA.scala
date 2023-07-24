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
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLHints._
import coupledL2.prefetch.PrefetchReq
import coupledL2.utils.XSPerfAccumulate

class SinkA(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val prefetchReq = prefetchOpt.map(_ => Flipped(DecoupledIO(new PrefetchReq)))
    val toReqArb = DecoupledIO(new TaskBundle)
    val pbRead = Flipped(DecoupledIO(new PutBufferRead))
    val pbResp = ValidIO(new PutBufferEntry)
  })
  val putBuffer = Reg(Vec(mshrsAll, Vec(beatSize, new PutBufferEntry)))
  val beatValids = RegInit(VecInit(Seq.fill(mshrsAll)(VecInit(Seq.fill(beatSize)(false.B)))))
  val valids = VecInit(beatValids.map(_.asUInt.orR())).asUInt
  
  val (first, last, done, count) = edgeIn.count(io.a)
  val hasData = edgeIn.hasData(io.a.bits)
  val full = valids.andR()
  val noSpace = full && hasData
  val insertIdx = PriorityEncoder(~valids)
  val insertIdxReg = RegEnable(insertIdx, 0.U.asTypeOf(insertIdx), io.a.fire() && first)

  when (io.a.fire() && hasData) {
    when (first) {
      putBuffer(insertIdx)(count).data.data := io.a.bits.data
      putBuffer(insertIdx)(count).mask := io.a.bits.mask
      beatValids(insertIdx)(count) := true.B
    }.otherwise {
      putBuffer(insertIdxReg)(count).data.data := io.a.bits.data
      putBuffer(insertIdxReg)(count).mask := io.a.bits.mask
      beatValids(insertIdxReg)(count) := true.B
    }
  }

  // val rIdx = io.pbRead.bits.idx
  // val res = putBuffer(rIdx)
  when (io.pbRead.fire()) {
    beatValids(io.pbRead.bits.idx)(io.pbRead.bits.count) := false.B
  }

  val commonReq = Wire(io.toReqArb.cloneType)
  val prefetchReq = prefetchOpt.map(_ => Wire(io.toReqArb.cloneType))

  io.a.ready := !first || commonReq.ready && !noSpace

  def fromTLAtoTaskBundle(a: TLBundleA): TaskBundle = {
    val task = Wire(new TaskBundle)
    task.channel := "b001".U
    task.tag := parseAddress(a.address)._1
    task.set := parseAddress(a.address)._2
    task.off := parseAddress(a.address)._3
    task.alias.foreach(_ := a.user.lift(AliasKey).getOrElse(0.U))
    task.opcode := a.opcode
    task.param := a.param
    task.size := a.size
    task.sourceId := a.source
    task.bufIdx := 0.U(bufIdxBits.W)
    task.needProbeAckData := false.B
    task.mshrTask := false.B
    task.mshrId := 0.U(mshrBits.W)
    task.aliasTask.foreach(_ := false.B)
    task.useProbeData := false.B
    task.pbIdx := insertIdx
    task.fromL2pft.foreach(_ := false.B)
    task.needHint.foreach(_ := a.user.lift(PrefetchKey).getOrElse(false.B))
    task.dirty := false.B
    task.way := 0.U(wayBits.W)
    task.meta := 0.U.asTypeOf(new MetaEntry)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dsWen := false.B
    task.wayMask := 0.U(cacheParams.ways.W)
    task.reqSource := a.user.lift(utility.ReqSourceKey).getOrElse(MemReqSource.NoWhere.id.U)
    task
  }
  def fromPrefetchReqtoTaskBundle(req: PrefetchReq): TaskBundle = {
    val task = Wire(new TaskBundle)
    val fullAddr = Cat(req.tag, req.set, 0.U(offsetBits.W))
    task.channel := "b001".U
    task.tag := parseAddress(fullAddr)._1
    task.set := parseAddress(fullAddr)._2
    task.off := 0.U
    task.alias.foreach(_ := 0.U)
    task.opcode := Hint
    task.param := Mux(req.needT, PREFETCH_WRITE, PREFETCH_READ)
    task.size := offsetBits.U
    task.sourceId := req.source
    task.bufIdx := 0.U(bufIdxBits.W)
    task.needProbeAckData := false.B
    task.mshrTask := false.B
    task.mshrId := 0.U(mshrBits.W)
    task.aliasTask.foreach(_ := false.B)
    task.useProbeData := false.B
    task.pbIdx := 0.U(mshrBits.W)
    task.fromL2pft.foreach(_ := req.isBOP)
    task.needHint.foreach(_ := false.B)
    task.dirty := false.B
    task.way := 0.U(wayBits.W)
    task.meta := 0.U.asTypeOf(new MetaEntry)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dsWen := false.B
    task.wayMask := 0.U(cacheParams.ways.W)
    task.reqSource := MemReqSource.L2Prefetch.id.U
    task
  }
  commonReq.valid := io.a.valid && first && !noSpace
  commonReq.bits := fromTLAtoTaskBundle(io.a.bits)
  if (prefetchOpt.nonEmpty) {
    prefetchReq.get.valid := io.prefetchReq.get.valid
    prefetchReq.get.bits := fromPrefetchReqtoTaskBundle(io.prefetchReq.get.bits)
    io.prefetchReq.get.ready := prefetchReq.get.ready
    fastArb(Seq(commonReq, prefetchReq.get), io.toReqArb)
  } else {
    io.toReqArb <> commonReq
  }

  io.pbRead.ready := beatValids(io.pbRead.bits.idx)(io.pbRead.bits.count)
  assert(!io.pbRead.valid || io.pbRead.ready)

  io.pbResp.valid := RegNext(io.pbRead.fire(), false.B)
  io.pbResp.bits := RegEnable(putBuffer(io.pbRead.bits.idx)(io.pbRead.bits.count), io.pbRead.fire())

  // Performance counters
  // num of reqs
  XSPerfAccumulate(cacheParams, "sinkA_req", io.toReqArb.fire())
  XSPerfAccumulate(cacheParams, "sinkA_acquire_req", io.a.fire() && io.a.bits.opcode(2, 1) === AcquireBlock(2, 1))
  XSPerfAccumulate(cacheParams, "sinkA_acquireblock_req", io.a.fire() && io.a.bits.opcode === AcquireBlock)
  XSPerfAccumulate(cacheParams, "sinkA_acquireperm_req", io.a.fire() && io.a.bits.opcode === AcquirePerm)
  XSPerfAccumulate(cacheParams, "sinkA_get_req", io.a.fire() && io.a.bits.opcode === Get)
  XSPerfAccumulate(cacheParams, "sinkA_put_req", io.toReqArb.fire() &&
    (io.toReqArb.bits.opcode === PutFullData || io.toReqArb.bits.opcode === PutPartialData))
  XSPerfAccumulate(cacheParams, "sinkA_put_beat", io.a.fire() &&
    (io.a.bits.opcode === PutFullData || io.a.bits.opcode === PutPartialData))
  prefetchOpt.foreach {
    _ =>
      XSPerfAccumulate(cacheParams, "sinkA_prefetch_req", io.prefetchReq.get.fire)
      XSPerfAccumulate(cacheParams, "sinkA_prefetch_from_l2", io.prefetchReq.get.bits.isBOP && io.prefetchReq.get.fire)
      XSPerfAccumulate(cacheParams, "sinkA_prefetch_from_l1", !io.prefetchReq.get.bits.isBOP && io.prefetchReq.get.fire)
  }

  // cycels stalled by mainpipe
  val stall = io.toReqArb.valid && !io.toReqArb.ready
  XSPerfAccumulate(cacheParams, "sinkA_stall_by_mainpipe", stall)
  XSPerfAccumulate(cacheParams, "sinkA_acquire_stall_by_mainpipe", stall &&
    (io.toReqArb.bits.opcode === AcquireBlock || io.toReqArb.bits.opcode === AcquirePerm))
  XSPerfAccumulate(cacheParams, "sinkA_get_stall_by_mainpipe", stall && io.toReqArb.bits.opcode === Get)
  XSPerfAccumulate(cacheParams, "sinkA_put_stall_by_mainpipe", stall &&
    (io.toReqArb.bits.opcode === PutFullData || io.toReqArb.bits.opcode === PutPartialData))
  prefetchOpt.foreach { _ => XSPerfAccumulate(cacheParams, "sinkA_prefetch_stall_by_mainpipe", stall && io.toReqArb.bits.opcode === Hint) }

  // cycles stalled for no space
  XSPerfAccumulate(cacheParams, "sinkA_put_stall_for_noSpace", io.a.valid && first && noSpace)
  XSPerfAccumulate(cacheParams, "putbuffer_full", full)
}