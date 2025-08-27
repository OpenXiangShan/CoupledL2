package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import chisel3.experimental.AffectsChiselPrefix
import scala.collection.mutable.ArrayBuffer
import oceanus.chi.opcode._
import utility._


class L2MSHRAllocREQ(implicit val p: Parameters) extends Bundle with HasL2Params {
    val opcode = UInt(paramCHI.reqOpcodeWidth.W)
    // TODO
}

class L2MSHRAllocSNP(implicit val p: Parameters) extends Bundle with HasL2Params {
    val opcode = UInt(paramCHI.snpOpcodeWidth.W)
    // TODO
}

class L2MSHRBufferWrite(implicit val p: Parameters) extends Bundle with HasL2Params {
    val dataId = UInt(paramCHI.datDataIDWidth.W)
    val resp = UInt(paramCHI.datRespWidth.W)
    val respErr = UInt(paramCHI.datRespErrWidth.W)
    val bypassed = Bool() // whether the write has been bypassed to upper/lower TXDAT
}

class L2MSHRDirectoryReadResp(implicit val p: Parameters) extends Bundle with HasL2Params {
    val stateL2 = L2LocalDirectoryState()
    val stateL1D = L2ClientState()
    val way = UInt() // TODO
    val set = UInt() // TODO
}

class L2MSHRDataStorageResp(implicit val p: Parameters) extends Bundle with HasL2Params {
    // TODO
}

class L2MSHRStates(implicit val p: Parameters) extends AffectsChiselPrefix 
    with HasL2Params {
    
    //
    var listAllocReset = new ArrayBuffer[L2MSHRState[_]]

    var listEntryActive = new ArrayBuffer[L2MSHRState[Bool]]
    var listReqActive = new ArrayBuffer[L2MSHRState[Bool]]
    var listSnpActive = new ArrayBuffer[L2MSHRState[Bool]]

    //
    def resetOnAlloc(enable: Bool) = listAllocReset.foreach(_.nextInitial(enable))

    // Local transient information
    val m_valid = RegInit(false.B)

    val m_sleep = RegInit(false.B)
    val m_wake_valid = RegInit(false.B)
    val m_wake_index = Reg(UInt(mshrIndexWidth.W))
    
    val m_addr = Reg(UInt(paramL2.physicalAddrWidth.W))

    val m_req_opcode = Reg(UInt(paramCHI.reqOpcodeWidth.W))
    val m_req_txnId = Reg(UInt(paramCHI.reqTxnIDWidth.W)) // TODO: should be width of upstream TxnID

    val m_snp_opcode = Reg(UInt(paramCHI.snpOpcodeWidth.W))
    val m_snp_txnId = Reg(UInt(paramCHI.snpTxnIDWidth.W))

    val m_state_me = Reg(L2LocalIntermediateState())
    val m_state_l1d = Reg(L2ClientState())

    val m_snp_doNotGoToSD = Reg(Bool())
    val m_snp_retToSrc = Reg(Bool())

    val m_buf_active0 = L2MSHRState(false.B, listAllocReset)
    val m_buf_active1 = L2MSHRState(false.B, listAllocReset)

    // Auxiliary indicators
    val m_req_active = RegInit(false.B)
    val m_snp_active = RegInit(false.B)

    // Local states
    val w_me_dir_read = L2MSHRState(false.B, listEntryActive)
    val s_me_dir_write = L2MSHRState(false.B, listEntryActive)

    val w_me_ds_read = L2MSHRState(false.B, listEntryActive)
    val s_me_ds_write = L2MSHRState(false.B, listEntryActive)

    // Upstream read transaction states
    val s_dn_read_txreq = L2MSHRState(false.B, listEntryActive, listReqActive)
    val s_dn_read_compack = L2MSHRState(false.B, listEntryActive, listReqActive)

    val w_dn_read_resp = L2MSHRState(false.B, listEntryActive, listReqActive)
    val w_dn_read_data0 = L2MSHRState(false.B, listEntryActive, listReqActive)
    val w_dn_read_data1 = L2MSHRState(false.B, listEntryActive, listReqActive)

    val s_up_read_txdat_buf0 = L2MSHRState(false.B, listEntryActive, listReqActive)
    val s_up_read_txdat_buf1 = L2MSHRState(false.B, listEntryActive, listReqActive)
    val s_up_read_txdat_ds = L2MSHRState(false.B, listEntryActive, listReqActive)

    // Upstream write transaction states

    // Replace write transaction states

    // Snoop transaction states
    val s_dn_snp_resp = L2MSHRState(false.B, listEntryActive, listSnpActive)
    val s_dn_snp_resp_data = L2MSHRState(false.B, listEntryActive, listSnpActive)
}
class L2MSHR(implicit val p: Parameters) extends Module
    with HasL2Params
    with CHIRNFOpcodesREQ
    with CHIRNFOpcodesSNP
    with CHIRNFOpcodesRSP
    with CHIRNFOpcodesDAT {

    // I/O
    val io = IO(new Bundle {
        val allocReq = Input(ValidIO(new L2MSHRAllocREQ))
        val allocSnp = Input(ValidIO(new L2MSHRAllocSNP))
        // TODO
        val dirReadResp = Input(ValidIO(new L2MSHRDirectoryReadResp))
        // TODO
    })

    // MSHR states
    val states = new L2MSHRStates

    // MSHR allocation
    val mshr_alloc_req = io.allocReq.fire
    val mshr_alloc_snp = io.allocSnp.fire
    val mshr_alloc = mshr_alloc_req || mshr_alloc_snp
    val mshr_alloc_first = mshr_alloc && !states.m_valid
    val mshr_alloc_nest = mshr_alloc && states.m_valid

    states.resetOnAlloc(mshr_alloc_first)

    when (mshr_alloc_first) {
        states.m_valid := true.B
    }

    when (mshr_alloc) {
        // TODO
    }

    assert(!(mshr_alloc_req && mshr_alloc_snp), "L2 MSHR cannot be allocated for both request and snoop at the same cycle")

    // MSHR deallocation
    val mshr_curr_busy = ParallelOR(states.listEntryActive.map(_.bits).toSeq)
    val mshr_next_idle = ParallelAND(states.listEntryActive.map(!_.next).toSeq)

    val mshr_next_dealloc = mshr_curr_busy && mshr_next_idle

    when (mshr_next_dealloc) {
        states.m_valid := false.B
    }

    // TODO

    // Directory read response
    states.w_me_dir_read.nextHold

    when (io.dirReadResp.fire) {
        states.w_me_dir_read.nextInWhen(false.B)
    }

    // TODO

    // Req & Snp transaction activity indication (for timing optimization)
    //
    //                      |  Current States   | Next States
    // ==============================================================
    //  - next_activate     | all idle          | some busy
    //. - next_deactivate   | some busy         | all idle
    //
    val req_curr_idle = ParallelAND(states.listReqActive.map(!_.bits).toSeq)
    val req_curr_busy = ParallelOR(states.listReqActive.map(_.bits).toSeq)
    val req_next_idle = ParallelAND(states.listReqActive.map(!_.next).toSeq)
    val req_next_busy = ParallelOR(states.listReqActive.map(_.next).toSeq)

    val req_next_activate = req_curr_idle && req_next_busy
    val req_next_deactivate = req_curr_busy && req_next_idle

    when (req_next_activate) {
        states.m_req_active := true.B
    }

    when (req_next_deactivate) {
        states.m_req_active := false.B
    }

    val snp_curr_idle = ParallelAND(states.listSnpActive.map(!_.bits).toSeq)
    val snp_curr_busy = ParallelOR(states.listSnpActive.map(_.bits).toSeq)
    val snp_next_idle = ParallelAND(states.listSnpActive.map(!_.next).toSeq)
    val snp_next_busy = ParallelOR(states.listSnpActive.map(_.next).toSeq)

    val snp_next_activate = snp_curr_idle && snp_next_busy
    val snp_next_deactivate = snp_curr_busy && snp_next_idle

    when (snp_next_activate) {
        states.m_snp_active := true.B
    }

    when (snp_next_deactivate) {
        states.m_snp_active := false.B
    }

}