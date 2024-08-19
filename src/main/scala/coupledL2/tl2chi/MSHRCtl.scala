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
import chisel3.util.random.LFSR
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.prefetch.PrefetchTrain
import coupledL2._

// PCrd info for MSHR Retry 
class PCrdInfo(implicit p: Parameters) extends TL2CHIL2Bundle
{
  val valid = Bool()
  val srcID = chiOpt.map(_ => UInt(SRCID_WIDTH.W))
  val pCrdType = chiOpt.map(_ => UInt(PCRDTYPE_WIDTH.W))
}

class MSHRCtl(implicit p: Parameters) extends TL2CHIL2Module with HasCHIOpcodes {
  val io = IO(new Bundle() {
    /* interact with req arb */
    val fromReqArb = Input(new Bundle() {
      val status_s1 = new PipeEntranceStatus()
    })
    val toReqArb = Output(new BlockInfo())

    /* interact with mainpipe */
    val fromMainPipe = new Bundle() {
      val mshr_alloc_s3 = Flipped(ValidIO(new MSHRRequest()))
    }
    val toMainPipe = new Bundle() {
      val mshr_alloc_ptr = Output(UInt(mshrBits.W))
    }

    /* to request arbiter */
    // val mshrFull = Output(Bool())
    val mshrTask = DecoupledIO(new TaskBundle())

    /* status of s2 and s3 */
    val pipeStatusVec = Flipped(Vec(2, ValidIO(new PipeStatus)))

    /* send reqs */
    val toTXREQ = DecoupledIO(new CHIREQ())
    val toTXRSP = DecoupledIO(new CHIRSP()) // TODO: unify with main pipe, which should be TaskBundle
    val toSourceB = DecoupledIO(new TLBundleB(edgeIn.bundle))

    /* to block sourceB from sending same-addr probe until GrantAck received */
    val grantStatus = Input(Vec(grantBufInflightSize, new GrantStatus()))

    /* receive resps */
    val resps = Input(new Bundle() {
      val sinkC = new RespBundle()  //probeAck from core
      val rxrsp = new RespBundle()  //releaseAck(CompDBID) from CHI
      val rxdat = new RespBundle()  //AcquireBlock(CompData) from CHI
    })

    val releaseBufWriteId = Output(UInt(mshrBits.W))

    /* nested writeback */
    val nestedwb = Input(new NestedWriteback)
    val nestedwbDataId = Output(ValidIO(UInt(mshrBits.W)))

    /* MSHR info to Sinks */
    val msInfo = Vec(mshrsAll, ValidIO(new MSHRInfo()))
    val aMergeTask = Flipped(ValidIO(new AMergeTask))

    /* refill read replacer result */
    val replResp = Flipped(ValidIO(new ReplacerResult))

    /* for TopDown Monitor */
    val msStatus = topDownOpt.map(_ => Vec(mshrsAll, ValidIO(new MSHRStatus)))

    /* to Slice Top for pCrd info.*/
    val waitPCrdInfo  = Output(Vec(mshrsAll, new PCrdInfo))
})

  /*MSHR allocation pointer gen -> to Mainpipe*/
  class MSHRSelector(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle() {
      val idle = Input(Vec(mshrsAll, Bool()))
      val out = ValidIO(UInt(mshrsAll.W))
    })
    io.out.valid := ParallelOR(io.idle)
    io.out.bits := ParallelPriorityMux(io.idle.zipWithIndex.map {
      case (b, i) => (b, (1 << i).U)
    })
  }

  val mshrs = Seq.fill(mshrsAll) { Module(new MSHR()) }
  val mshrValids = VecInit(mshrs.map(m => m.io.status.valid))
  val pipeReqCount = PopCount(Cat(io.pipeStatusVec.map(_.valid))) // TODO: consider add !mshrTask to optimize
  val mshrCount = PopCount(Cat(mshrs.map(_.io.status.valid)))
  val mshrFull = pipeReqCount + mshrCount >= mshrsAll.U
  val a_mshrFull = pipeReqCount + mshrCount >= (mshrsAll-1).U // the last idle mshr should not be allocated for channel A req
  val mshrSelector = Module(new MSHRSelector())
  val selectedMSHROH = mshrSelector.io.out.bits

  mshrSelector.io.idle := mshrs.map(m => !m.io.status.valid)
  io.toMainPipe.mshr_alloc_ptr := OHToUInt(selectedMSHROH)

  /*
   when PCrdGrant, give credit to one entry that:
   1. got RetryAck and not Reissued
   2. match srcID and PCrdType
   3. use Round-Robin arbiter if multi-entry match
   */
  val isPCrdGrant = io.resps.rxrsp.valid && (io.resps.rxrsp.respInfo.chiOpcode.get === PCrdGrant)
  val waitPCrdInfo  = Wire(Vec(mshrsAll, new PCrdInfo))
  val timeOutPri = VecInit(Seq.fill(16)(false.B))
  val timeOutSel = WireInit(false.B)
  val pCrdPri = VecInit(Seq.fill(16)(false.B))
  val pArb = Module(new RRArbiterInit(UInt(), mshrsAll))

  val matchPCrdGrant = VecInit(waitPCrdInfo.map(p =>
      isPCrdGrant && p.valid &&
      p.srcID.get === io.resps.rxrsp.respInfo.srcID.get &&
      p.pCrdType.get === io.resps.rxrsp.respInfo.pCrdType.get
  ))

  pArb.io.in.zipWithIndex.foreach {
    case (in, i) =>
      in.valid := matchPCrdGrant(i)
      in.bits := 0.U
  }
  pArb.io.out.ready := true.B

  val pCrdOH = VecInit(UIntToOH(pArb.io.chosen).asBools)
  val pCrdFixPri = VecInit(pCrdOH zip matchPCrdGrant map {case(a,b) => a && b})
//val pCrdFixPri = VecInit(PriorityEncoderOH(matchPCrdGrantReg)) //fix priority arbiter

  // timeout protect
  val counter = RegInit(VecInit(Seq.fill(mshrsAll)(0.U((log2Ceil(mshrsAll)+1).W))))

  for(i <- 0 until 16) {
    when(matchPCrdGrant(i)) {
      when(!timeOutSel && pCrdFixPri(i) || timeOutPri(i)) {
        counter(i):=0.U
      }.otherwise {
        counter(i):= counter(i) + 1.U
      }
    }
  }
  val timeOutOH = PriorityEncoderOH(counter.map(_>=12.U) zip matchPCrdGrant map {case(a,b) => a&&b})
  timeOutPri := VecInit(timeOutOH)

  timeOutSel := timeOutPri.reduce(_|_)
  pCrdPri := Mux(timeOutSel, timeOutPri, pCrdFixPri)

  dontTouch (timeOutPri)
  dontTouch (timeOutSel)
  dontTouch (pCrdOH)
  dontTouch (pCrdFixPri)
  dontTouch (pCrdPri)

  /* when PCrdGrant come before RetryAck, 16 entry CAM used to:
   1. save {srcID, PCrdType} 
   2. Broadcast to each MSHR for seaching when RetryAck   
   */
//  val pCamValids = RegInit(VecInit(Seq.fill(mshrsAll){ false.B }))
  val pCam  = RegInit(VecInit(Seq.fill(mshrsAll)(0.U.asTypeOf(new PCrdInfo))))
  val pCamPri = Wire(UInt(5.W))
  val pCamValids = Cat(pCam.map(_.valid))
  val enqIdx = PriorityEncoder(~pCamValids.asUInt)

//  when (isPCrdGrant && !pCrdIsWait.orR){
  when (isPCrdGrant){
    pCam(enqIdx).valid := true.B
    pCam(enqIdx).srcID.get := io.resps.rxrsp.respInfo.srcID.get
    pCam(enqIdx).pCrdType.get := io.resps.rxrsp.respInfo.pCrdType.get
  }

  pCamPri := 16.U  //out of range of mshrAll 

  //each entry zip pCam
  for (i <- 0 until mshrsAll) { //entry
    when (waitPCrdInfo(i).valid) {
      for (j <- 0 until mshrsAll) { //pCam
        when (pCam(j).valid &&
              waitPCrdInfo(i).srcID.get === pCam(j).srcID.get &&
              waitPCrdInfo(i).srcID.get === pCam(j).pCrdType.get) {
          pCam(j).valid := false.B
          pCamPri := i.U
        }
      }
    }
  }

  /* SinkC(release) search MSHR with PA */
  val resp_sinkC_match_vec = mshrs.map { mshr =>
    val status = mshr.io.status.bits
    val tag = Mux(status.needsRepl, status.metaTag, status.reqTag)
    mshr.io.status.valid && status.w_c_resp && io.resps.sinkC.set === status.set && io.resps.sinkC.tag === tag
  }

  /* Port connection of MSHR entry */
  mshrs.zipWithIndex.foreach {
    case (m, i) =>
      m.io.id := i.U
      m.io.alloc.valid := selectedMSHROH(i) && io.fromMainPipe.mshr_alloc_s3.valid
      m.io.alloc.bits := io.fromMainPipe.mshr_alloc_s3.bits
      m.io.alloc.bits.task.isKeyword.foreach(_:= io.fromMainPipe.mshr_alloc_s3.bits.task.isKeyword.getOrElse(false.B))

      m.io.resps.sinkC.valid := io.resps.sinkC.valid && resp_sinkC_match_vec(i)
      m.io.resps.sinkC.bits := io.resps.sinkC.respInfo

      m.io.resps.rxdat.valid := m.io.status.valid && io.resps.rxdat.valid && io.resps.rxdat.mshrId === i.U
      m.io.resps.rxdat.bits := io.resps.rxdat.respInfo

      m.io.resps.rxrsp.valid := (m.io.status.valid && io.resps.rxrsp.valid && !isPCrdGrant && io.resps.rxrsp.mshrId === i.U) || (isPCrdGrant && pCrdPri(i))
      m.io.resps.rxrsp.bits := io.resps.rxrsp.respInfo

      m.io.replResp.valid := io.replResp.valid && io.replResp.bits.mshrId === i.U
      m.io.replResp.bits := io.replResp.bits

      io.msInfo(i) := m.io.msInfo
      m.io.nestedwb := io.nestedwb
      m.io.aMergeTask.valid := io.aMergeTask.valid && io.aMergeTask.bits.id === i.U
      m.io.aMergeTask.bits := io.aMergeTask.bits.task

      waitPCrdInfo(i) := m.io.waitPCrdInfo 
      m.io.pCamPri := 0.U /*(pCamPri === i.U) && waitPCrdInfo(i).valid*/
  }
  /* Reserve 1 entry for SinkB */
  io.waitPCrdInfo <> waitPCrdInfo

  /* Reserve 1 entry for SinkB */
  io.toReqArb.blockC_s1 := false.B
  io.toReqArb.blockB_s1 := mshrFull   // conflict logic in SinkB
  io.toReqArb.blockA_s1 := a_mshrFull // conflict logic in ReqBuf
  io.toReqArb.blockG_s1 := false.B

   /* Acquire downwards to TXREQ*/
  fastArb(mshrs.map(_.io.tasks.txreq), io.toTXREQ, Some("txreq"))

  /* Response downwards to TXRSP*/
  fastArb(mshrs.map(_.io.tasks.txrsp), io.toTXRSP, Some("txrsp"))

  /* Probe upwards */
  val sourceB = Module(new SourceB())
  fastArb(mshrs.map(_.io.tasks.source_b), sourceB.io.task, Some("source_b"))
  sourceB.io.grantStatus := io.grantStatus
  io.toSourceB <> sourceB.io.sourceB

  /* Arbitrate MSHR task to RequestArbiter */
  fastArb(mshrs.map(_.io.tasks.mainpipe), io.mshrTask, Some("mshr_task"))

  /* releaseBuf link to MSHR id */ 
  io.releaseBufWriteId := ParallelPriorityMux(resp_sinkC_match_vec, (0 until mshrsAll).map(i => i.U))

  /* Nest writeback check */
  io.nestedwbDataId.valid := Cat(mshrs.map(_.io.nestedwbData)).orR
  io.nestedwbDataId.bits := ParallelPriorityMux(mshrs.zipWithIndex.map {
    case (mshr, i) => (mshr.io.nestedwbData, i.U)
  })
  assert(RegNext(PopCount(mshrs.map(_.io.nestedwbData)) <= 1.U), "should only be one nestedwbData")


  /* Status for topDown monitor */
  topDownOpt.foreach (_ =>
    io.msStatus.get.zip(mshrs).foreach {
      case (in, s) => in := s.io.status
    }
  )
  /* Performance counters */
/*  XSPerfAccumulate("capacity_conflict_to_sinkA", a_mshrFull)
  XSPerfAccumulate("capacity_conflict_to_sinkB", mshrFull)
  XSPerfHistogram("mshr_alloc", io.toMainPipe.mshr_alloc_ptr,
    enable = io.fromMainPipe.mshr_alloc_s3.valid,
    start = 0, stop = mshrsAll, step = 1)
  if (cacheParams.enablePerf) {
    val start = 0
    val stop = 100
    val step = 5
    val acquire_period = ParallelMux(mshrs.map { case m => m.io.resps.sink_d.valid -> m.acquire_period }) 
    val release_period = ParallelMux(mshrs.map { case m => m.io.resps.sink_d.valid -> m.release_period })
    val probe_period = ParallelMux(mshrs.map { case m => m.io.resps.sink_c.valid -> m.probe_period })
    val acquire_period_en = io.resps.rxdat.valid &&
      (io.resps.rxdat.respInfo.opcode === Grant || io.resps.rxdat.respInfo.opcode === GrantData)
    val release_period_en = io.resps.rxdat.valid && io.resps.rxdat.respInfo.opcode === ReleaseAck
    val probe_period_en = io.resps.sinkC.valid &&
      (io.resps.sinkC.respInfo.opcode === ProbeAck || io.resps.sinkC.respInfo.opcode === ProbeAckData)
    XSPerfHistogram("acquire_period", acquire_period, acquire_period_en, start, stop, step)
    XSPerfHistogram("release_period", release_period, release_period_en, start, stop, step)
    XSPerfHistogram("probe_period", probe_period, probe_period_en, start, stop, step)
 
    val timers = RegInit(VecInit(Seq.fill(mshrsAll)(0.U(64.W))))
    for (((timer, m), i) <- timers.zip(mshrs).zipWithIndex) {
      when (m.io.alloc.valid) {
        timer := 1.U
      }.otherwise {
        timer := timer + 1.U
      }
      val enable = m.io.status.valid && m.io.status.bits.will_free
      XSPerfHistogram("mshr_latency_" + Integer.toString(i, 10),
        timer, enable, 0, 300, 10)
      XSPerfMax("mshr_latency", timer, enable)
    }
  }*/
}

