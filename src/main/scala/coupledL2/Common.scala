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
import freechips.rocketchip.tilelink.TLPermissions._
import utility.MemReqSource

abstract class L2Module(implicit val p: Parameters) extends MultiIOModule with HasCoupledL2Parameters
abstract class L2Bundle(implicit val p: Parameters) extends Bundle with HasCoupledL2Parameters

class ReplacerInfo(implicit p: Parameters) extends L2Bundle {
  val channel = UInt(3.W)
  val opcode = UInt(3.W)
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
}

trait HasChannelBits { this: Bundle =>
  val channel = UInt(3.W)
  def fromA = channel(0).asBool
  def fromB = channel(1).asBool
  def fromC = channel(2).asBool
}

// We generate a Task for every TL request
// this is the info that flows in Mainpipe
class TaskBundle(implicit p: Parameters) extends L2Bundle with HasChannelBits {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val off = UInt(offsetBits.W)
  val alias = aliasBitsOpt.map(_ => UInt(aliasBitsOpt.get.W)) // color bits in cache-alias issue
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W)) // vaddr passed by client cache
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
}

class PipeStatus(implicit p: Parameters) extends L2Bundle with HasChannelBits

class PipeEntranceStatus(implicit p: Parameters) extends L2Bundle {
  val tags = Vec(4, UInt(tagBits.W))
  val sets = Vec(4, UInt(setBits.W))

  def c_tag = tags(0)
  def b_tag = tags(1)
  def a_tag = tags(2)
  def g_tag = tags(3) // replRead-Grant

  def c_set = sets(0)
  def b_set = sets(1)
  def a_set = sets(2)
  def g_set = sets(3)
}

// MSHR exposes signals to MSHRCtl
class MSHRStatus(implicit p: Parameters) extends L2Bundle with HasChannelBits {
  val set         = UInt(setBits.W)
  val reqTag      = UInt(tagBits.W)
  val metaTag     = UInt(tagBits.W)
  val needsRepl = Bool()
  val w_c_resp = Bool()
  val w_d_resp = Bool()
  val w_e_resp = Bool()
  val will_free = Bool()

  //  val way = UInt(wayBits.W)
//  val off = UInt(offsetBits.W)
//  val opcode = UInt(3.W)
//  val param = UInt(3.W)
//  val size = UInt(msgSizeBits.W)
//  val source = UInt(sourceIdBits.W)
//  val alias = aliasBitsOpt.map(_ => UInt(aliasBitsOpt.get.W))
//  val aliasTask = aliasBitsOpt.map(_ => Bool())
//  val needProbeAckData = Bool() // only for B reqs
//  val fromL2pft = prefetchOpt.map(_ => Bool())
//  val needHint = prefetchOpt.map(_ => Bool())

  // for TopDown usage
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
  val is_miss = Bool()
  val is_prefetch = Bool()
}

// MSHR Task that MainPipe sends to MSHRCtl
class MSHRRequest(implicit p: Parameters) extends L2Bundle {
  val dirResult = new DirResult()
  val state = new FSMState()
  val task = new TaskBundle()
}

// MSHR info to ReqBuf and SinkB
class MSHRInfo(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val reqTag = UInt(tagBits.W)
  val willFree = Bool()

  // to block Acquire for to-be-replaced data until Release done (indicated by ReleaseAck received)
  val needRelease = Bool()
  // MSHR needs to send ReleaseTask but has not yet sent it
  // PS: ReleaseTask is also responsible for writing refillData to DS when A miss
  val releaseNotSent = Bool()

  val metaTag = UInt(tagBits.W)
  val dirHit = Bool()

  // decide whether can nest B (req same-addr) or merge B with release (meta same-addr)
  val nestB = Bool()
  val mergeB = Bool()

  // to drop duplicate prefetch reqs
  val isAcqOrPrefetch = Bool()
  val isPrefetch = Bool()
}

class RespInfoBundle(implicit p: Parameters) extends L2Bundle {
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val last = Bool() // last beat
  val dirty = Bool() // only used for sinkD resps
  val isHit = Bool() // only used for sinkD resps
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
  val s_merge_probeack = Bool() // respond probeack downwards, Probe merge into A-replacement-Release
  // val s_grantack = Bool() // respond grantack downwards, moved to GrantBuf
  // val s_triggerprefetch = prefetchOpt.map(_ => Bool())

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
  val w_grantack = Bool()
  val w_replResp = Bool()
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
}

// used for nested C Release
class NestedWriteback(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val c_set_dirty = Bool()
}

class PrefetchRecv extends Bundle {
  val addr = UInt(64.W)
  val addr_valid = Bool()
  val l2_pf_en = Bool()
}

// custom l2 - l1 interface
class L2ToL1Hint(implicit p: Parameters) extends Bundle {
  val sourceId = UInt(32.W)    // tilelink sourceID
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
  val fast_miss = Output(Bool()) // without sram part for timing optimization
  val excp = Vec(nDups, new Bundle {
    val pf = new TlbExceptionBundle()
    val af = new TlbExceptionBundle()
  })
  val static_pm = Output(Valid(Bool())) // valid for static, bits for mmio result from normal entries
  val ptwBack = Output(Bool()) // when ptw back, wake up replay rs's state
}
class L2ToL1TlbIO(nRespDups: Int = 1)(implicit p: Parameters) extends L2Bundle{
  val req = DecoupledIO(new L2TlbReq)
  val req_kill = Output(Bool())
  val resp = Flipped(DecoupledIO(new L2TlbResp(nRespDups)))
}

// indicates where the memory access request comes from
// a dupliacte of this is in xiangShan/package.scala utility/TLUtils/BusKeyField.scala
object MemReqSource extends Enumeration {
  val NoWhere = Value("NoWhere")

  val CPUInst = Value("CPUInst")
  val CPULoadData = Value("CPULoadData")
  val CPUStoreData = Value("CPUStoreData")
  val CPUAtomicData = Value("CPUAtomicData")
  val L1InstPrefetch = Value("L1InstPrefetch")
  val L1DataPrefetch = Value("L1DataPrefetch")
  val PTW = Value("PTW")
  val L2Prefetch = Value("L2Prefetch")
  val ReqSourceCount = Value("ReqSourceCount")

  val reqSourceBits = log2Ceil(ReqSourceCount.id)
}
