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
import freechips.rocketchip.tilelink.TLPermissions._
import utility.MemReqSource
import tl2chi.{HasCHIMsgParameters, HasCHIChannelBits, CHIREQ, MemAttr, OrderEncodings}

abstract class L2Module(implicit val p: Parameters) extends Module with HasCoupledL2Parameters
abstract class L2Bundle(implicit val p: Parameters) extends Bundle with HasCoupledL2Parameters

class ReplacerInfo(implicit p: Parameters) extends L2Bundle {
  val channel = UInt(4.W)
  val opcode = UInt(3.W)
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
  val refill_prefetch = Bool()
}

trait HasTLChannelBits { this: Bundle =>
  val channel = UInt(4.W)
  def fromA = channel(0).asBool
  def fromB = channel(1).asBool
  def fromC = channel(2).asBool
  def fromTP = channel(3).asBool
}

class MergeTaskBundle(implicit p: Parameters) extends L2Bundle {
  val off = UInt(offsetBits.W)
  val alias = aliasBitsOpt.map(_ => UInt(aliasBitsOpt.get.W)) // color bits in cache-alias issue
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W)) // vaddr passed by client cache, for prefetcher train
  val isKeyword = isKeywordBitsOpt.map(_ => Bool())
  val opcode = UInt(3.W) // type of the task operation
  val param = UInt(3.W)
  val sourceId = UInt(sourceIdBits.W) // tilelink sourceID
  val meta = new MetaEntry()
}

// We generate a Task for every TL request
// this is the info that flows in Mainpipe
class TaskBundle(implicit p: Parameters) extends L2Bundle
  with HasTLChannelBits
  with HasCHIMsgParameters
  with HasCHIChannelBits {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val off = UInt(offsetBits.W)
  val alias = aliasBitsOpt.map(_ => UInt(aliasBitsOpt.get.W)) // color bits in cache-alias issue
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W)) // vaddr passed by client cache
  // from L1 load miss require 
  val isKeyword = isKeywordBitsOpt.map(_ => Bool())
  val opcode = UInt(3.W)                  // type of the task operation
  val param = UInt(3.W)
  val size = UInt(msgSizeBits.W)
  val sourceId = UInt(sourceIdBits.W)     // tilelink sourceID
  val bufIdx = UInt(bufIdxBits.W)         // idx of SinkC buffer
  val needProbeAckData = Bool()           // only used for SinkB reqs, whether L3 needs probeAckData

  // MSHR may send Release(Data) or Grant(Data) or ProbeAck(Data) through Main Pipe
  val mshrTask = Bool()                   // is task from mshr
  val mshrId = UInt(mshrBits.W)           // mshr entry index (used only in mshr-task)
  val aliasTask = aliasBitsOpt.map(_ => Bool()) // Anti-alias
  val useProbeData = Bool()               // data source, true for ReleaseBuf and false for RefillBuf
  val mshrRetry = Bool()                  // is retry task for mshr conflict

  // For Intent
  val fromL2pft = prefetchOpt.map(_ => Bool()) // Is the prefetch req from L2(BOP) or from L1 prefetch?
                                          // If true, MSHR should send an ack to L2 prefetcher.
  val needHint = prefetchOpt.map(_ => Bool())

  // For DirtyKey in Release
  val dirty = Bool()

  // if this is an mshr task and it needs to write dir
  val way = UInt(wayBits.W)
  val meta = new MetaEntry()
  val metaWen = Bool()
  val tagWen = Bool()
  val dsWen = Bool()

  // for Dir to choose a way inside wayMask
  val wayMask = UInt(cacheParams.ways.W)

  // for Grant to read replacer to choose a replaced way
  // for Release to read refillBuf and write to DS
  val replTask = Bool()

  // for TopDown Monitor (# TopDown)
  val reqSource = UInt(MemReqSource.reqSourceBits.W)

  def hasData = opcode(0)

  // for merged MSHR tasks(Acquire & late Prefetch)
  val mergeA = Bool()
  val aMergeTask = new MergeTaskBundle()

  // for TP meta (read & write req)
  val tpmeta = Bool()
  val tpmetaWen = Bool()
  val tpmetaWenRepl = Bool()
  val tpReplTag = UInt(tagBits.W)

  // Used for get data from ReleaseBuf when snoop hit with same PA 
  val snpHitRelease = Bool()
  val snpHitReleaseWithData = Bool()
  val snpHitReleaseIdx = UInt(mshrBits.W) 
  // CHI
  val tgtID = chiOpt.map(_ => UInt(TGTID_WIDTH.W))
  val srcID = chiOpt.map(_ => UInt(SRCID_WIDTH.W))
  val txnID = chiOpt.map(_ => UInt(TXNID_WIDTH.W))
  val homeNID = chiOpt.map(_ => UInt(SRCID_WIDTH.W))
  val dbID = chiOpt.map(_ => UInt(DBID_WIDTH.W))
  val fwdNID = chiOpt.map(_ => UInt(FWDNID_WIDTH.W))
  val fwdTxnID = chiOpt.map(_ => UInt(FWDTXNID_WIDTH.W))
  val chiOpcode = chiOpt.map(_ => UInt(OPCODE_WIDTH.W))
  val resp = chiOpt.map(_ => UInt(RESP_WIDTH.W))
  val fwdState = chiOpt.map(_ => UInt(FWDSTATE_WIDTH.W))
  val pCrdType = chiOpt.map(_ => UInt(PCRDTYPE_WIDTH.W))
  val retToSrc = chiOpt.map(_ => Bool()) // only used in snoop
  val expCompAck = chiOpt.map(_ => Bool())
  val allowRetry = chiOpt.map(_ => Bool())
  val memAttr = chiOpt.map(_ => new MemAttr)

  def toCHIREQBundle(): CHIREQ = {
    val req = WireInit(0.U.asTypeOf(new CHIREQ()))
    req.tgtID := tgtID.getOrElse(0.U)
    req.srcID := srcID.getOrElse(0.U)
    req.txnID := txnID.getOrElse(0.U)
    req.opcode := chiOpcode.getOrElse(0.U)
    req.addr := Cat(tag, set, 0.U(offsetBits.W))
    req.allowRetry := allowRetry.getOrElse(true.B)  //TODO: consider retry
    req.pCrdType := pCrdType.getOrElse(0.U)
    req.expCompAck := expCompAck.getOrElse(false.B)
    req.memAttr := memAttr.getOrElse(MemAttr())
    req.snpAttr := true.B
    req.order := OrderEncodings.None
    req
  }
}

class PipeStatus(implicit p: Parameters) extends L2Bundle
  with HasTLChannelBits

class PipeEntranceStatus(implicit p: Parameters) extends L2Bundle {
  val tags = Vec(5, UInt(tagBits.W))
  val sets = Vec(5, UInt(setBits.W))

  def c_tag = tags(0)
  def b_tag = tags(1)
  def a_tag = tags(2)
  def g_tag = tags(3) // replRead-Grant
  def tp_tag = tags(4)

  def c_set = sets(0)
  def b_set = sets(1)
  def a_set = sets(2)
  def g_set = sets(3)
  def tp_set = sets(4)
}

// MSHR Task that MainPipe sends to MSHRCtl
class MSHRRequest(implicit p: Parameters) extends L2Bundle {
  val dirResult = new DirResult()
  val state = new FSMState()
  val task = new TaskBundle()
}

// MSHR info to ReqBuf and SinkB
class MSHRInfo(implicit p: Parameters) extends L2Bundle with HasTLChannelBits {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val reqTag = UInt(tagBits.W)
  val willFree = Bool()
  val aliasTask = aliasBitsOpt.map(_ => Bool())

  // to block Acquire for to-be-replaced data until Release done (indicated by ReleaseAck received)
  val needRelease = Bool()
  // MSHR needs to send ReleaseTask but has not in mainpipe s3, RefillTask in MP need to block
  // PS: ReleaseTask is also responsible for writing refillData to DS when A miss
  val blockRefill = Bool()

  val metaTag = UInt(tagBits.W)
  val metaState = UInt(stateBits.W)
  val dirHit = Bool()

  // to drop duplicate prefetch reqs
  val isAcqOrPrefetch = Bool()
  val isPrefetch = Bool()

  // whether the mshr_task already in mainpipe
  val param = UInt(3.W)
  val mergeA = Bool() // whether the mshr already merge an acquire(avoid alias merge)

  val w_grantfirst = Bool()
  val s_refill = Bool()
  val w_releaseack = Bool()
  val w_replResp = Bool()
  val w_rprobeacklast = Bool()

  val replaceData = Bool() // If there is a replace, WriteBackFull or Evict
}

class RespInfoBundle(implicit p: Parameters) extends L2Bundle
    with HasCHIMsgParameters
{
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val last = Bool() // last beat
  val dirty = Bool() // only used for sinkD resps
  val isHit = Bool() // only used for sinkD resps
 //CHI
  val chiOpcode = chiOpt.map(_ => UInt(OPCODE_WIDTH.W))
  val txnID = chiOpt.map(_ => UInt(TXNID_WIDTH.W))
  val srcID = chiOpt.map(_ => UInt(SRCID_WIDTH.W))
  val homeNID = chiOpt.map(_ => UInt(SRCID_WIDTH.W))
  val dbID = chiOpt.map(_ => UInt(DBID_WIDTH.W))
  val resp = chiOpt.map(_ => UInt(RESP_WIDTH.W))
  val pCrdType = chiOpt.map(_ => UInt(PCRDTYPE_WIDTH.W))
}

class RespBundle(implicit p: Parameters) extends L2Bundle {
  val valid = Bool()
  val mshrId = UInt(mshrBits.W)
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val respInfo = new RespInfoBundle
}

class FSMState(implicit p: Parameters) extends L2Bundle {
  // schedule
  val s_acquire = Bool()  // acquire downwards
  val s_rprobe = Bool()   // probe upwards, caused by replace
  val s_pprobe = Bool()   // probe upwards, casued by probe
  val s_release = Bool()  // release downwards
  val s_probeack = Bool() // respond probeack downwards
  val s_refill = Bool()   // respond grant upwards
  // val s_grantack = Bool() // respond grantack downwards, moved to GrantBuf
  // val s_triggerprefetch = prefetchOpt.map(_ => Bool())
  val s_retry = Bool()    // need retry when conflict

  // wait
  val w_rprobeackfirst = Bool()
  val w_rprobeacklast = Bool()
  val w_pprobeackfirst = Bool()
  val w_pprobeacklast = Bool()
  val w_pprobeack = Bool()
  val w_grantfirst = Bool()
  val w_grantlast = Bool()
  val w_grant = Bool()
  val w_releaseack = Bool()
  val w_replResp = Bool()

  // CHI
  val s_compack = chiOpt.map(_ => Bool())
  val s_cbwrdata = chiOpt.map(_ => Bool())
  val s_reissue = chiOpt.map(_ => Bool())
  val s_dct = chiOpt.map(_ => Bool())
}

class SourceAReq(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val off = UInt(offsetBits.W)
  val opcode = UInt(3.W)
  val param = UInt(aWidth.W)
  val size = UInt(msgSizeBits.W)
  val source = UInt(mshrBits.W)
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
}

class SourceBReq(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val off = UInt(offsetBits.W)
  val opcode = UInt(3.W)
  val param = UInt(bdWidth.W)
  val alias = aliasBitsOpt.map(_ => UInt(aliasBitsOpt.get.W))
}

class BlockInfo(implicit p: Parameters) extends L2Bundle {
  val blockG_s1 = Bool()
  val blockA_s1 = Bool()
  val blockB_s1 = Bool()
  val blockC_s1 = Bool()
  val blockTP_s1 = Bool()
}

// used for nested C Release
class NestedWriteback(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  // Nested ReleaseData sets block dirty
  val c_set_dirty = Bool()
  // Nested Snoop invalidates block
  val b_inv_dirty = Bool()

  val b_toB = chiOpt.map(_ => Bool())
  val b_toN = chiOpt.map(_ => Bool())
}

class PrefetchRecv extends Bundle {
  val addr = UInt(64.W)
  val pf_source = UInt(MemReqSource.reqSourceBits.W)
  val addr_valid = Bool()
  val l2_pf_en = Bool()
}

// custom l2 - l1 interface
class L2ToL1Hint(implicit p: Parameters) extends Bundle {
  val sourceId = UInt(32.W)    // tilelink sourceID
  val isKeyword = Bool()       // miss entry keyword
}

// custom l2 - l1 tlb
// FIXME lyq: Tlbcmd and TlbExceptionBundle, how to use L1 corresponding bundles?
object TlbCmd {
  def read  = "b00".U
  def write = "b01".U
  def exec  = "b10".U

  def atom_read  = "b100".U // lr
  def atom_write = "b101".U // sc / amo

  def apply() = UInt(3.W)
  def isRead(a: UInt) = a(1,0)===read
  def isWrite(a: UInt) = a(1,0)===write
  def isExec(a: UInt) = a(1,0)===exec

  def isAtom(a: UInt) = a(2)
  def isAmo(a: UInt) = a===atom_write // NOTE: sc mixed
}
class TlbExceptionBundle extends Bundle {
  val ld = Output(Bool())
  val st = Output(Bool())
  val instr = Output(Bool())
}
class L2TlbReq(implicit p: Parameters) extends L2Bundle{
  val vaddr = Output(UInt((fullVAddrBits+offsetBits).W))
  val cmd = Output(TlbCmd())
  val size = Output(UInt(log2Ceil(log2Ceil(XLEN/8) + 1).W))
  val kill = Output(Bool()) // Use for blocked tlb that need sync with other module like icache
  val no_translate = Output(Bool()) // do not translate, but still do pmp/pma check
}
class L2TlbResp(nDups: Int = 1)(implicit p: Parameters) extends L2Bundle {
  val paddr = Vec(nDups, Output(UInt(fullAddressBits.W)))
  val miss = Output(Bool())
  val excp = Vec(nDups, new Bundle {
    val gpf = new TlbExceptionBundle()
    val pf = new TlbExceptionBundle()
    val af = new TlbExceptionBundle()
  })
}

class PMPRespBundle(implicit p: Parameters) extends L2Bundle {
  val ld = Output(Bool())
  val st = Output(Bool())
  val instr = Output(Bool())
  val mmio = Output(Bool())
  val atomic = Output(Bool())
}

class L2ToL1TlbIO(nRespDups: Int = 1)(implicit p: Parameters) extends L2Bundle{
  val req = DecoupledIO(new L2TlbReq)
  val req_kill = Output(Bool())
  val resp = Flipped(DecoupledIO(new L2TlbResp(nRespDups)))
  val pmp_resp = Flipped(new PMPRespBundle())
}

class TPmetaL2ReqBundle(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val bank = UInt(bankBits.W)
  val off = UInt(offsetBits.W)
}

class TPmetaData extends Bundle {
  val hartid = UInt(4.W)
  val rawData = UInt(512.W)
}

class TPmetaReq(implicit p: Parameters) extends L2Bundle {
  val hartid = UInt(4.W) // max 16 harts
  val l2ReqBundle = new TPmetaL2ReqBundle()
  val wmode = Bool()
  val rawData = Vec(16, UInt((36-6).W))
}

class TPmetaResp extends Bundle {
  val hartid = UInt(4.W)
  val rawData = Vec(16, UInt((36-6).W))
}

class TPmetaL2Req(implicit p: Parameters) extends L2Bundle {
  val l2ReqBundle = new TPmetaL2ReqBundle()
  val wmode = Bool()
  // [511, 508] = hartid; [479, 0] = rawData
  val rawData = UInt(512.W)
}

class TPmetaL2Resp extends Bundle {
  val exist = Bool()
  val rawData = UInt(512.W)
}
