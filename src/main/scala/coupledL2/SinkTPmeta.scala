package coupledL2

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLHints._
import coupledL2.prefetch.PrefetchReq
import huancun.{AliasKey, PrefetchKey}
import utility.MemReqSource

class SinkTPmeta(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val tpMetaReq = prefetchOpt.map(_ => Flipped(DecoupledIO(new TPmetaL2Req)))
    val task = DecoupledIO(new TaskBundle)
    // TODO: Mainpipe
    val tpMetaDataW = prefetchOpt.map(_ => DecoupledIO(new TPmetaData))
  })

  def fromTPmetaReqtoTaskBundle(req: TPmetaL2Req): TaskBundle = {
    val task = Wire(new TaskBundle)
    task.channel := "b1000".U
    task.txChannel := 0.U
    task.tag := req.l2ReqBundle.tag
    task.set := req.l2ReqBundle.set
    task.off := 0.U
    task.alias.foreach(_ := 0.U)
    task.opcode := 0.U //dontcare
    task.param := 0.U
    task.size := offsetBits.U
    task.sourceId := 0.U
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

    task.wayMask := 0.U(cacheParams.ways.W)
    task.reqSource := 0.U
    task.replTask := false.B
    task.vaddr.foreach(_ := 0.U)
    task.pc.foreach(_ := 0.U)
    //miss acquire keyword
    task.isKeyword.foreach(_ := false.B)
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    task.snpHitRelease := false.B
    task.snpHitReleaseWithData := false.B
    task.snpHitReleaseIdx := 0.U
    task.tpmeta := true.B
    task.tpmetaWen := req.wmode
    task.tpmetaWenRepl := false.B
    task.tpReplTag := req.l2ReqBundle.tag
    task.tpmetaReplTag := req.replTag
    task
  }

  if(prefetchOpt.nonEmpty && hasTPPrefetcher) {
    io.task.valid := io.tpMetaReq.get.valid
    io.task.bits := fromTPmetaReqtoTaskBundle(io.tpMetaReq.get.bits)
    io.tpMetaReq.get.ready := io.task.ready
  } else {
    io.task.valid := false.B
    io.task.bits := DontCare
  }

  val tpMetaDataDataQueue = Module(new Queue(new TPmetaData(), 4, pipe = false, flow = false))
  if(prefetchOpt.nonEmpty && hasTPPrefetcher) {
    tpMetaDataDataQueue.io.enq.valid := io.tpMetaReq.get.fire && io.tpMetaReq.get.bits.wmode
    tpMetaDataDataQueue.io.enq.bits.hartid := io.tpMetaReq.get.bits.rawData(511, 508)
    tpMetaDataDataQueue.io.enq.bits.rawData := io.tpMetaReq.get.bits.rawData

    tpMetaDataDataQueue.io.deq.ready := io.tpMetaDataW.get.fire

    io.tpMetaDataW.get.valid := tpMetaDataDataQueue.io.deq.valid
    io.tpMetaDataW.get.bits := tpMetaDataDataQueue.io.deq.bits
  } else {
    tpMetaDataDataQueue.io.enq.valid := false.B
    tpMetaDataDataQueue.io.enq.bits.hartid := DontCare
    tpMetaDataDataQueue.io.enq.bits.rawData := DontCare

    tpMetaDataDataQueue.io.deq.ready := false.B
  }


}