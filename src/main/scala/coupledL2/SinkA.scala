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
import huancun.{AliasKey, PrefetchKey,PutBufferPop,PutBufferBeatEntry}
import utility.{MemReqSource, XSPerfAccumulate, RRArbiterInit}
import freechips.rocketchip.tilelink.TLPermissions._

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

  def isMatrixGet(a: TLBundleA): Bool = {
    val en = a.opcode === Get  && (a.user.lift(MatrixKey).getOrElse(0.U) === 1.U)
    en
  }

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
    task.wayMask := Mux(a.opcode===PutFullData,
                        Fill(cacheParams.ways, "b1".U),
                        0.U(cacheParams.ways.W))
    task.reqSource := a.user.lift(utility.ReqSourceKey).getOrElse(MemReqSource.NoWhere.id.U)
    task.replTask := false.B
    task.vaddr.foreach(_ := a.user.lift(VaddrKey).getOrElse(0.U))
    //miss acquire keyword
    task.isKeyword.foreach(_ := a.echo.lift(IsKeywordKey).getOrElse(false.B))
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    task.cmoAll := cmoAllValid
    task
  }
  when (io.a.fire && isPutFullData) {
    when (hasData) {
      when (first) {
        //TODO Support Mask
        dataBuf(nextPtr)(beat) := io.a.bits.data
        beatValids(nextPtr)(beat) := true.B
        printf(p"Put First message received: address=${Hexadecimal(io.a.bits.address)}, data=${Hexadecimal(io.a.bits.data)}\n")
      }.otherwise {
        assert(last)
        dataBuf(nextPtrReg)(beat) := io.a.bits.data
        beatValids(nextPtrReg)(beat) := true.B
        printf(p"Put Last message received: address=${Hexadecimal(io.a.bits.address)}, data=${Hexadecimal(io.a.bits.data)}\n")
      }
    }
  }

  when (io.a.fire && isPutFullData && last) {
    when (hasData) {
      taskValids(nextPtrReg) := true.B
      taskBuf(nextPtrReg) := fromTLAtoTaskBundle(io.a.bits)
      taskBuf(nextPtrReg).bufIdx := nextPtrReg
    }.otherwise {
      taskValids(nextPtr) := true.B
      taskBuf(nextPtr) := fromTLAtoTaskBundle(io.a.bits)
      taskBuf(nextPtr).bufIdx := nextPtr
    }
  }

  taskArb.io.out.ready := io.task.ready
  taskArb.io.in.zipWithIndex.foreach {
    case (in, i) =>
      in.valid := taskValids(i)
      in.bits := taskBuf(i)
      when (in.fire) {
        taskValids(i) := false.B
      }
  }

  val aValid = io.a.valid && isPutFullData && last
  io.task.valid := taskArb.io.out.valid
  io.task.bits := taskArb.io.out.bits
  io.task.bits.bufIdx := taskArb.io.out.bits.bufIdx

  // io.resp.valid := io.a.valid && (first || last) && !isPutFullData
  // io.resp.mshrId := 0.U // DontCare
  // io.resp.tag := parseAddress(io.a.bits.address)._1
  // io.resp.set := parseAddress(io.a.bits.address)._2
  // io.resp.respInfo := 0.U.asTypeOf(io.resp.respInfo.cloneType)
  // io.resp.respInfo.opcode := io.a.bits.opcode
  // io.resp.respInfo.param := io.a.bits.param
  // io.resp.respInfo.last := last
  // io.resp.respInfo.dirty := io.a.bits.opcode(0)
  // io.resp.respInfo.isHit := io.a.bits.opcode(0)
  // io.resp.respInfo.corrupt := io.a.bits.corrupt

  // keep the first beat of ProbeAckData
  // val probeAckDataBuf = RegEnable(io.a.bits.data, 0.U((beatBytes * 8).W),
  //   io.a.valid && io.a.bits.opcode === ProbeAckData && first)

  // io.releaseBufWrite.valid := io.a.valid && io.a.bits.opcode === ProbeAckData && last
  // io.releaseBufWrite.bits.id := 0.U(mshrBits.W) // id is given by MSHRCtl by comparing address to the MSHRs
  // io.releaseBufWrite.bits.data.data := Cat(io.a.bits.data, probeAckDataBuf)
  // io.releaseBufWrite.bits.beatMask := Fill(beatSize, true.B)

  // // C-Release, with new data, comes before repl-Release writes old refill data back to DS
  // val newdataMask = VecInit(io.msInfo.map(s =>
  //   s.valid && s.bits.set === io.task.bits.set && s.bits.reqTag === io.task.bits.tag && s.bits.blockRefill
  // )).asUInt

  // we must wait until 2nd beat written into databuf(idx) before we can read it
  // So we use RegNext
  // //Or we can use Cat(databuf(idx)(0), io.a.bits.data)

  // since what we are trying to prevent is that C-Release comes first and MSHR-Release comes later
  // we can make sure this refillBufWrite can be read by MSHR-Release
  // TODO: this is rarely triggered, consider just blocking? but blocking may affect timing of SinkC-Directory
  // io.refillBufWrite.valid := RegNext(io.task.fire && io.task.bits.opcode === ReleaseData && newdataMask.orR, false.B)
  // io.refillBufWrite.bits.id := RegNext(OHToUInt(newdataMask))
  // io.refillBufWrite.bits.data.data := dataBuf(RegNext(io.task.bits.bufIdx)).asUInt
  // io.refillBufWrite.bits.beatMask := Fill(beatSize, true.B)

  // io.a.ready := !isPutFullData || !first || !full
  io.a.ready := Mux(first, !noSpace, true.B)

  // io.bufResp.data := RegNext(RegEnable(dataBuf(io.task.bits.bufIdx), io.task.fire))
  when(RegNext(io.task.fire)) {
    beatValids(RegNext(io.task.bits.bufIdx)).foreach(_ := false.B)
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
