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
    val cmoAll = Option.when(cacheParams.enableL2Flush) (new IOCMOAll)
  })
  assert(!(io.a.valid && (io.a.bits.opcode === PutFullData ||
                          io.a.bits.opcode === PutPartialData)),
    "no Put");

  // flush L2 all control defines
  val set = Option.when(cacheParams.enableL2Flush)(RegInit(0.U(setBits.W))) 
  val way = Option.when(cacheParams.enableL2Flush)(RegInit(0.U(wayBits.W))) 
  val sIDLE :: sCMOREQ :: sWAITLINE :: sWAITMSHR :: sDONE :: Nil = Enum(5)
  val state = Option.when(cacheParams.enableL2Flush)(RegInit(sIDLE))
  val stateVal = state.getOrElse(sIDLE)
  val setVal = set.getOrElse(0.U)
  val wayVal = way.getOrElse(0.U)
  val cmoAllValid = stateVal === sCMOREQ
  val cmoAllBlock = stateVal === sCMOREQ || stateVal === sWAITLINE
  io.cmoAll.foreach { cmoAll => cmoAll.l2FlushDone := stateVal === sDONE }
  io.cmoAll.foreach { cmoAll => cmoAll.cmoAllBlock := cmoAllBlock }

  def fromTLAtoTaskBundle(a: TLBundleA): TaskBundle = {
    val task = Wire(new TaskBundle)
    task := 0.U.asTypeOf(new TaskBundle)
    task.channel := "b001".U
    task.txChannel := 0.U
    task.tag := parseAddress(a.address)._1
    task.set := Mux(cmoAllValid, setVal, parseAddress(a.address)._2)
    task.off := parseAddress(a.address)._3
    task.alias.foreach(_ := a.user.lift(AliasKey).getOrElse(0.U))
    task.opcode := Mux(cmoAllValid, CBOFlush, a.opcode)
    task.param := a.param
    task.size := a.size
    task.sourceId := a.source
    task.corrupt := a.corrupt
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
    task.way := Mux(cmoAllValid, wayVal, 0.U(wayBits.W))
    task.meta := 0.U.asTypeOf(new MetaEntry)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dsWen := false.B
    task.wayMask := 0.U(cacheParams.ways.W)
    task.reqSource := a.user.lift(utility.ReqSourceKey).getOrElse(MemReqSource.NoWhere.id.U)
    task.replTask := false.B
    task.vaddr.foreach(_ := a.user.lift(VaddrKey).getOrElse(0.U))
    task.pc.foreach(_ := a.user.lift(PCKey).getOrElse(0.U)) 
    //miss acquire keyword
    task.isKeyword.foreach(_ := a.echo.lift(IsKeywordKey).getOrElse(false.B))
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    task.cmoAll := cmoAllValid
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
    task.corrupt := false.B
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
    task.pc.foreach(_ := 0.U)  
    task.isKeyword.foreach(_ := false.B)
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    task
  }
  if (prefetchOpt.nonEmpty) {
    io.task.valid := io.a.valid && !cmoAllBlock || io.prefetchReq.get.valid || cmoAllValid
    io.task.bits := Mux(
      io.a.valid || cmoAllValid,
      fromTLAtoTaskBundle(io.a.bits),
      fromPrefetchReqtoTaskBundle(io.prefetchReq.get.bits
    ))
    io.a.ready := io.task.ready && !cmoAllBlock
    io.prefetchReq.get.ready := io.task.ready && !io.a.valid
  } else {
    io.task.valid := io.a.valid && !cmoAllBlock || cmoAllValid
    io.task.bits := fromTLAtoTaskBundle(io.a.bits) 
    io.a.ready := io.task.ready && !cmoAllBlock
  }

  /*
   Flush L2 All means search all L2$ VALID cacheLine and RELEASE to Downwords memory:
   -------------------------------------------------------------------------------------------------------
          Step by Step                                                   |    Interface 
   ----------------------------------------------------------------------|--------------------------------
   0. Core initiate flush L2$ All operation                              |  io.cmoAll.l2Flush 
   1. wait all mshrs done, block sinkA/C by ready until l2 flush done    |  io.cmoAll.cmoAllBlock 
   2. search all cacheline set with a loop (0 ~ numSets)                 |  io.task.set
   3. for each set, search all ways with a loop (0 ~ numWays)            |  io.task.way
   4. if cacheline is VALID, after cmo flush, Mainpipe send back resp    |  io.cmoAll.cmoLineDone
   5. if cacheline is INVALID, MainPipe drop it and send back resp       |  io.cmoAll.cmoLineDone
   6. after all slices is flushed, inform Core                           |  io.cmoAll.l2FlushDone 
   7. after all slices is flushed, exit coherency                        |  TL2CHICoupledL2.io_chi.syscoreq
   ---------------------------------------------------------------------------------------------------------*/
  val l2Flush = io.cmoAll.map(_.l2Flush).getOrElse(false.B)
  val mshrValid = io.cmoAll.map(_.mshrValid).getOrElse(false.B)
  val cmoLineDone = io.cmoAll.map(_.cmoLineDone).getOrElse(false.B)

  when (stateVal === sIDLE && l2Flush && !mshrValid) {
    state.foreach { _ := sCMOREQ }
  }
  when (stateVal === sCMOREQ && io.task.fire) {
    state.foreach { _ := sWAITLINE }
  }
  when (stateVal === sWAITLINE && cmoLineDone) {
    when (setVal === (cacheParams.sets-1).U && wayVal === (cacheParams.ways-1).U) { 
      state.foreach { _ := sDONE }
    }.otherwise {
      when (wayVal === (cacheParams.ways-1).U) {
        way.foreach { _ := 0.U }
        set.foreach { _ := setVal + 1.U }
      }.otherwise {
        way.foreach { _ := wayVal + 1.U }
      }
      when (mshrValid) {
        state.foreach { _ := sCMOREQ }
      }.otherwise {
        state.foreach { _ := sWAITMSHR }
      }
    }
  }
  when (stateVal === sWAITMSHR && !mshrValid) {
    state.foreach { _ := sCMOREQ }
  }

  // Performance counters
  // num of reqs
  XSPerfAccumulate("sinkA_req", io.task.fire)
  XSPerfAccumulate("sinkA_acquire_req", io.a.fire && (io.a.bits.opcode === AcquirePerm || io.a.bits.opcode === AcquireBlock))
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
