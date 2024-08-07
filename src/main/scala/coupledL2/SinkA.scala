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
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLHints._
import coupledL2.prefetch.PrefetchReq
import huancun.{AliasKey, PrefetchKey}
import utility.{MemReqSource, XSPerfAccumulate}

class SinkA(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val prefetchReq = prefetchOpt.map(_ => Flipped(DecoupledIO(new PrefetchReq)))
    val task = DecoupledIO(new TaskBundle)
  })
  assert(!(io.a.valid && io.a.bits.opcode(2, 1) === 0.U), "no Put")

  def fromTLAtoTaskBundle(a: TLBundleA): TaskBundle = {
    val task = Wire(new TaskBundle)
    task := 0.U.asTypeOf(new TaskBundle)
    task.channel := "b001".U
    task.txChannel := 0.U
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
    task.mshrRetry := false.B
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
    task.replTask := false.B
    task.vaddr.foreach(_ := a.user.lift(VaddrKey).getOrElse(0.U))
    //miss acquire keyword
    task.isKeyword.foreach(_ := a.echo.lift(IsKeywordKey).getOrElse(false.B))
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    task
  }
  def fromPrefetchReqtoTaskBundle(req: PrefetchReq): TaskBundle = {
    val task = Wire(new TaskBundle)
    val fullAddr = Cat(req.tag, req.set, 0.U(offsetBits.W))
    task := 0.U.asTypeOf(new TaskBundle)
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
    task.fromL2pft.foreach(_ := req.needAck)
    task.mshrRetry := false.B
    task.needHint.foreach(_ := false.B)
    task.dirty := false.B
    task.way := 0.U(wayBits.W)
    task.meta := 0.U.asTypeOf(new MetaEntry)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dsWen := false.B
    task.wayMask := 0.U(cacheParams.ways.W)
    task.reqSource := req.pfSource
    task.replTask := false.B
    task.vaddr.foreach(_ := req.vaddr.getOrElse(0.U))
    task.isKeyword.foreach(_ := false.B)
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    task
  }
  if (prefetchOpt.nonEmpty) {
    io.task.valid := io.a.valid || io.prefetchReq.get.valid
    io.task.bits := Mux(
      io.a.valid,
      fromTLAtoTaskBundle(io.a.bits),
      fromPrefetchReqtoTaskBundle(io.prefetchReq.get.bits
    ))

    io.a.ready := io.task.ready
    io.prefetchReq.get.ready := io.task.ready && !io.a.valid
  } else {
    io.task.valid := io.a.valid
    io.task.bits := fromTLAtoTaskBundle(io.a.bits)
    io.a.ready := io.task.ready
  }

  // Performance counters
  // num of reqs
  XSPerfAccumulate("sinkA_req", io.task.fire)
  XSPerfAccumulate("sinkA_acquire_req", io.a.fire && io.a.bits.opcode(2, 1) === AcquireBlock(2, 1))
  XSPerfAccumulate("sinkA_acquireblock_req", io.a.fire && io.a.bits.opcode === AcquireBlock)
  XSPerfAccumulate("sinkA_acquireperm_req", io.a.fire && io.a.bits.opcode === AcquirePerm)
  XSPerfAccumulate("sinkA_get_req", io.a.fire && io.a.bits.opcode === Get)
  prefetchOpt.foreach {
    _ =>
      XSPerfAccumulate("sinkA_prefetch_req", io.prefetchReq.get.fire)
      XSPerfAccumulate("sinkA_prefetch_from_l2", io.prefetchReq.get.bits.fromL2 && io.prefetchReq.get.fire)
      XSPerfAccumulate("sinkA_prefetch_from_l1", !io.prefetchReq.get.bits.fromL2 && io.prefetchReq.get.fire)
  }

  // cycels stalled by mainpipe
  val stall = io.task.valid && !io.task.ready
  XSPerfAccumulate("sinkA_stall_by_mainpipe", stall)
  XSPerfAccumulate("sinkA_acquire_stall_by_mainpipe", stall &&
    (io.task.bits.opcode === AcquireBlock || io.task.bits.opcode === AcquirePerm))
  XSPerfAccumulate("sinkA_get_stall_by_mainpipe", stall && io.task.bits.opcode === Get)
  XSPerfAccumulate("sinkA_put_stall_by_mainpipe", stall &&
    (io.task.bits.opcode === PutFullData || io.task.bits.opcode === PutPartialData))
  prefetchOpt.foreach { _ => XSPerfAccumulate("sinkA_prefetch_stall_by_mainpipe", stall && io.task.bits.opcode === Hint) }
}
