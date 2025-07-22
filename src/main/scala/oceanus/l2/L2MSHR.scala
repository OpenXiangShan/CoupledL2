package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import chisel3.experimental.AffectsChiselPrefix
import scala.collection.mutable.ArrayBuffer


class L2MSHRAlloc(implicit val p: Parameters) extends Bundle with HasL2Params {
    // TODO
}

class L2MSHRBufferWrite(implicit val p: Parameters) extends Bundle with HasL2Params {
    val dataId = UInt(paramCHI.datDataIDWidth.W)
    val resp = UInt(paramCHI.datRespWidth.W)
    val respErr = UInt(paramCHI.datRespErrWidth.W)
    val bypassed = Bool() // whether the write has been bypassed to upper/lower TXDAT
}

class L2MSHRDirectoryResp(implicit val p: Parameters) extends Bundle with HasL2Params {
    val stateL2 = L2LocalDirectoryState()
    val stateL1D = L2ClientState()
    val way = UInt() // TODO
    val set = UInt() // TODO
}

class L2MSHRDataStorageResp(implicit val p: Parameters) extends Bundle with HasL2Params {
    // TODO
}

class L2MSHRStates(implicit val p: Parameters) extends AffectsChiselPrefix with HasL2Params {
    
    //
    var listAllocReset = new ArrayBuffer[L2MSHRState[_]]

    var listEntryActive = new ArrayBuffer[L2MSHRState[_]]

    // Local transient information (with no reset)
    val mn_addr = Reg(UInt(paramL2.physicalAddrWidth.W))

    val mn_state_me = Reg(L2LocalIntermediateState())
    val mn_state_l1d = Reg(L2ClientState())

    val mn_snp_doNotGoToSD = Reg(Bool())
    val mn_snp_retToSrc = Reg(Bool())

    // Local transient states (with reset)
    val mr_buf_active0 = L2MSHRState(false.B, listAllocReset)
    val mr_buf_active1 = L2MSHRState(false.B, listAllocReset)

    // Local states
    val w_me_dir_read = L2MSHRState(false.B, listEntryActive)
    val s_me_dir_write = L2MSHRState(false.B, listEntryActive)

    val w_me_ds_read = L2MSHRState(false.B, listEntryActive)
    val s_me_ds_write = L2MSHRState(false.B, listEntryActive)

    // Read transaction states
    val s_dn_read_txreq = L2MSHRState(false.B, listEntryActive)
    val s_dn_read_compack = L2MSHRState(false.B, listEntryActive)

    val w_dn_read_resp = L2MSHRState(false.B, listEntryActive)
    val w_dn_read_data0 = L2MSHRState(false.B, listEntryActive)
    val w_dn_read_data1 = L2MSHRState(false.B, listEntryActive)

    val s_up_read_txdat_buf0 = L2MSHRState(false.B, listEntryActive)
    val s_up_read_txdat_buf1 = L2MSHRState(false.B, listEntryActive)
    val s_up_read_txdat_ds = L2MSHRState(false.B, listEntryActive)

    // Write transaction states

    // Snoop transaction states
    val s_dn_snp_resp = L2MSHRState(false.B, listEntryActive)
    val s_dn_snp_resp_data = L2MSHRState(false.B, listEntryActive)
}
class L2MSHR(implicit val p: Parameters) extends Module
    with HasL2Params {

    val io = IO(new Bundle {
        // TODO
    })

    val state = new L2MSHRStates

}