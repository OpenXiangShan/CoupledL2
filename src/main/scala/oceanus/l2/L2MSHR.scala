package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}


class L2MSHRAlloc(implicit val p: Parameters) extends Bundle with HasL2Params {
    // TODO
}

class L2MSHRBufferWrite(implicit val p: Parameters) extends Bundle with HasL2Params {
    val dataId = UInt(paramCHI.datDataIDWidth.W)
    val bypassed = Bool() // Whether the write has been bypassed to upper/lower TXDAT
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

class L2MSHRStates(implicit val p: Parameters) extends Bundle with HasL2Params {
    
    // Local persistent information
    val m_addr = UInt(paramL2.physicalAddrWidth.W)

    val m_buf_active0 = Bool()
    val m_buf_active1 = Bool()

    val m_state_me = L2LocalIntermediateState()
    val m_state_l1d = L2ClientState()

    val m_snp_doNotGoToSD = Bool()
    val m_snp_retToSrc = Bool()

    // Local states
    val w_me_dir_read = Bool()
    val s_me_dir_write = Bool()

    val w_me_ds_read = Bool()
    val s_me_ds_write = Bool()

    // Read transaction states
    val s_dn_read_txreq = Bool()
    val s_dn_read_compack = Bool()

    val w_dn_read_resp = Bool()
    val w_dn_read_data0 = Bool()
    val w_dn_read_data1 = Bool()

    val s_up_read_txdat_buf0 = Bool()
    val s_up_read_txdat_buf1 = Bool()
    val s_up_read_txdat_ds = Bool()

    // Write transaction states

    // Snoop transaction states
    val s_dn_snp_resp = Bool()
    val s_dn_snp_resp_data = Bool()
}
class L2MSHR(implicit val p: Parameters) extends Module
    with HasL2Params {

    val io = IO(new Bundle {
        // TODO
    })

}