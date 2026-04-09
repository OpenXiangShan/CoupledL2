package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.l2._
import oceanus.l2.L2TSHR._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters

object L2TSHR {

  class DirReadFSM extends Bundle {
    val PreArb = Bool()     // sending Read Request to Directory, waiting for Arbiter completion
    val PostArb = Bool()    // Read Request has been accepted by Directory
    val Done = Bool()       // Read Response has been received from Directory
    def NotYet =            // haven't sent Read Request to Directory
      !PreArb && !PostArb && !Done                
  }

  object DirReadFSM {
    def init = {
      val initState = Wire(new DirReadFSM)
      initState.elements.foreach(_._2 := false.B)
      initState
    }
  }

  class DirWriteFSM extends Bundle {
    val PreArb = Bool()     // sending Write Request to Directory, waiting for Arbiter completion
    val Done = Bool()       // Write Request has been accepted by Directory and observable to later requests
    def NotYet =            // haven't sent Write Request to Directory
      !PreArb && !Done
  }

  object DirWriteFSM {
    def init = {
      val initState = Wire(new DirWriteFSM)
      initState.elements.foreach(_._2 := false.B)
      initState
    }
  }

  class DSReadFSM extends Bundle {
    val PreArb = Bool()     // sending Read Request to Data Storage, waiting for Arbiter completion
    val PostArb = Bool()    // Read Request has been accepted by Data Storage
    val Done = Bool()       // Read Response has been received from Data Storage
    def NotYet =            // haven't sent Read Request to Data Storage
      !PreArb && !PostArb
  }

  object DSReadFSM {
    def init = {
      val initState = Wire(new DSReadFSM)
      initState.elements.foreach(_._2 := false.B)
      initState
    }
  }

  class DSWriteFSM extends Bundle {
    val PreArb = Bool()     // sending Write Request to Data Storage, waiting for Arbiter completion
    val PostArb = Bool()    // Write Request has been accepted by Data Storage 
    val Done = Bool()       // Write Request has been completed and observable to later requests
    def NotYet =            // haven't sent Write Request to Data Storage
      !PreArb && !PostArb && !Done

    // All possible combinations of state bits
    def DSWrite_NotYet          = !PreArb && !PostArb && !Done
    def DSWrite_PreArb          =  PreArb && !PostArb && !Done
    def DSWrite_PostArb         = !PreArb &&  PostArb && !Done
    def DSWrite_PostArb_PreArb  =  PreArb &&  PostArb && !Done
    def DSWrite_Done            = !PreArb && !PostArb &&  Done
    def DSWrite = DSWrite_NotYet || DSWrite_PreArb || DSWrite_PostArb || DSWrite_PostArb_PreArb || DSWrite_Done
  }

  object DSWriteFSM {
    def init = {
      val initState = Wire(new DSWriteFSM)
      initState.elements.foreach(_._2 := false.B)
      initState
    }
  }
}

class L2TSHR(id: Int)(implicit val p: Parameters) extends Module {

  val io = IO(new Bundle {

    val toDir = Output(new L2Directory.PathToDirectory)
    val fromDir = Input(new L2Directory.PathFromDirectory)

    val toDS = Output(new L2DataStorage.PathTSHRToDataStorage)
    val fromDS = Output(new L2DataStorage.PathDataStorageToTSHR)

    val RXEVT = Flipped(Decoupled(new FlitEVT))       // L1 EVT
    val RXSNP = Flipped(Decoupled(new CHIBundleSNP))  // HN SNP
    val RXREQ = Flipped(Decoupled(new FlitREQ))       // L1/L2 REQ

    val UpRXRSP = Flipped(Valid(new FlitUpRSP))       // RSP from L1
    val UpRXDAT = Flipped(Valid(new FlitUpDAT))       // DAT from L1

    val DnRXRSP = Flipped(Valid(new CHIBundleRSP))    // RSP from HN
    val DnRXDAT = Flipped(Valid(new CHIBundleDAT))    // DAT from HN

    val alloc = Input(Bool())
    val valid = Output(Bool())
  })

  // miscs and enchantments
  val req_need_dirRead = Wire(Bool())

  val ds_resp_hit = Wire(Bool()) // TODO: connect with Data Storage interactions, valid only when DSBufResp
  val ds_resp_miss = Wire(Bool()) // TODO: connect with Data Storage interactions, valid only when DSBufResp

  val ds_read_ahead_en = Wire(Bool()) // TODO: Data Storage Read Ahead valid on TSHR alloc/reuse
  val ds_read_ahead_q = RegInit(false.B) // TODO: Data Storage Read Ahead flag bit

  val ds_read_rbe_en = Wire(Bool()) // TODO: Data Storage Read on requests passed RBE with valid meta


  // TSHR valid
  val tshr_enter = io.RXEVT.fire || io.RXSNP.fire || io.RXREQ.fire
  val tshr_alloc = io.alloc
  val tshr_reuse = tshr_enter && !tshr_alloc

  val tshr_inactive_rbe = Wire(Bool())
  val tshr_inactive_vpipe = Wire(Bool())
  val tshr_inactive = tshr_inactive_rbe && tshr_inactive_vpipe

  val tshr_inactivate = Wire(Bool()) // TODO: connect with vPipes and RBEs, act as instant 'willFree'

  val tshr_wb_done_dir = Wire(Bool())
  val tshr_wb_done_ds = Wire(Bool()) // TODO: connect with DSWriteFSM

  val tshr_dealloc = tshr_inactive && tshr_wb_done_dir && tshr_wb_done_ds && !tshr_enter

  val tshr_valid = RegInit(false.B)

  when (tshr_alloc) {
    tshr_valid := true.B
  }

  when (tshr_dealloc) {
    tshr_valid := false.B
  }

  io.valid := tshr_valid


  // meta
  val meta = Reg(Bool()) // TODO: replace with Directory Result type
  val meta_valid = Wire(Bool())
  val meta_modified = RegInit(false.B)

  val metaWrite = Wire(Bool()) // TODO: replace with Directory Result type or something or write masks
  val metaWrite_valid = Wire(Bool()) // TODO: write masks might be needed

  // TODO: TSHR local meta interactions here

  
  // TSHR Buffer
  val tshr_buffer_0 = Reg(UInt(256.W))
  val tshr_buffer_2 = Reg(UInt(256.W))

  val tshr_buffer_modified_0 = RegInit(false.B)
  val tshr_buffer_modified_2 = RegInit(false.B)

  val tshr_buffer_modified = tshr_buffer_modified_0 || tshr_buffer_modified_2

  val tshr_buffer_wen_0 = Wire(Bool())
  val tshr_buffer_wen_2 = Wire(Bool())

  val tshr_buffer_wen = tshr_buffer_wen_0 || tshr_buffer_wen_2

  val tshr_buffer_wen_last = Wire(Bool())

  // TODO: TSHR Buffer interactions here

  
  // RBEs
  val rbeEVT = Module(new L2RBE(new FlitEVT /*TODO: strip PA here*/))
  val rbeSNP = Module(new L2RBE(new CHIBundleSNP /*TODO: strip PA here*/))
  val rbeREQ = Module(new L2RBE(new FlitREQ /*TODO: strip PA here*/))

  io.RXEVT <> rbeEVT.io.in
  io.RXSNP <> rbeSNP.io.in
  io.RXREQ <> rbeREQ.io.in

  rbeEVT.io.directoryReadNeed := true.B // TODO: Some specific EVT requests do not require Directory Read under Inclusive
  rbeSNP.io.directoryReadNeed := true.B
  rbeREQ.io.directoryReadNeed := true.B

  tshr_inactive_rbe := !rbeEVT.io.valid && !rbeSNP.io.valid && !rbeREQ.io.valid


  // vPipes
  // TODO


  // Directory read states
  val state_dirRead = RegInit(new DirReadFSM, DirReadFSM.init)

  when (tshr_enter && req_need_dirRead && state_dirRead.NotYet) {
    // [] -> DirRead_PreArb
    state_dirRead.PreArb := true.B
  }

  when (io.fromDir.DirRdArbComp) {
    // DirRead_PreArb -> DirRead_PostArb
    state_dirRead.PreArb := false.B
    state_dirRead.PostArb := true.B
  }

  when (io.fromDir.DirRdResp) {
    // DirRead_PostArb -> DirRead_Done
    state_dirRead.PostArb := false.B
    state_dirRead.Done := true.B
  }

  when (tshr_dealloc) {
    state_dirRead := DirReadFSM.init
  }

  meta_valid := state_dirRead.Done

  assert(!(io.fromDir.DirRdArbComp && !state_dirRead.PreArb), "receiving DirRdArbComp on unexpected state (expecting PreArb)")
  assert(!(io.fromDir.DirRdResp && !state_dirRead.PostArb), "receiving DirRdResp on unexpected state (expecting PostArb)")
  assert(PopCount(state_dirRead.asUInt) <= 1.U, "multiple active states in DirReadFSM")

  // Directory write states
  val state_dirWrite = RegInit(new DirWriteFSM, DirWriteFSM.init)

  when (tshr_inactive && state_dirWrite.NotYet) {
    when (meta_modified) {
      // [] -> DirWrite_PreArb
      state_dirWrite.PreArb := true.B
    }.otherwise {
      // [] -> DirWrite_Done
      state_dirWrite.Done := true.B
    }
  }

  when (tshr_reuse && state_dirWrite.PreArb) {
    // DirWrite_PreArb -> []
    state_dirWrite.PreArb := false.B
  }

  when (io.fromDir.DirWbArbComp) {
    // DirWrite_PreArb -> DirWrite_Done
    state_dirWrite.PreArb := false.B
    state_dirWrite.Done := true.B
  }

  when (metaWrite_valid && state_dirWrite.Done) {
    // DirWrite_Done -> []
    state_dirWrite.Done := false.B
  }

  when (tshr_dealloc) {
    state_dirWrite := DirWriteFSM.init
  }

  tshr_wb_done_dir := state_dirWrite.Done

  assert(!(io.fromDir.DirWbArbComp && !state_dirWrite.PreArb), "receiving DirWbArbComp on unexpected state (expecting PreArb)")
  assert(PopCount(state_dirWrite.asUInt) <= 1.U, "multiple active states in DirWriteFSM")


  // Data Storage read states
  val state_dsRead = RegInit(new DSReadFSM, DSReadFSM.init)

  when (state_dsRead.NotYet) {
    when (ds_read_ahead_en || ds_read_rbe_en) {
      state_dsRead.PreArb := true.B
    }
  }

  when (state_dsRead.PreArb) {
    when (io.UpRXDAT.fire || io.DnRXDAT.fire || ds_read_ahead_q) {
      state_dsRead.PreArb := false.B
    }
    when (io.fromDS.DSBufRdArbComp) {
      state_dsRead.PreArb := false.B
      state_dsRead.PostArb := true.B
    }
  }

  when (state_dsRead.PostArb) {
    when (ds_resp_miss) {
      state_dsRead.PostArb := false.B
      state_dsRead.PreArb := state_dirRead.Done
    }
    when (ds_resp_hit) {
      state_dsRead.PostArb := false.B
      state_dsRead.Done := true.B
    }
  }

  when (tshr_dealloc) {
    state_dsRead := DSReadFSM.init
  }

  assert(!(io.fromDS.DSBufRdArbComp && !state_dsRead.PreArb), "receiving DSBufRdArbComp on unexpected state (expecting PreArb)")
  assert(!(io.fromDS.DSBufRdResp && !state_dsRead.PostArb), "receiving DSBufRdResp on unexpected state (expecting PostArb)")
  assert(PopCount(state_dsRead.asUInt) <= 1.U, "multiple active states in DSReadFSM")
  
  // Data Storage write states
  val state_dsWrite = RegInit(new DSWriteFSM, DSWriteFSM.init)

  when (!state_dsWrite.PreArb) {
    when (tshr_buffer_wen_last) {
      state_dsWrite.PreArb := true.B
    }
  }.otherwise {
    when (io.fromDS.DSBufWbArbComp && !tshr_buffer_wen_last) {
      state_dsWrite.PreArb := false.B
    }
  }

  when (!state_dsWrite.PostArb) {
    when (io.fromDS.DSBufWbArbComp && !io.fromDS.DSBufWbComp) {
      state_dsWrite.PostArb := true.B
    }
  }.otherwise {
    when (io.fromDS.DSBufWbComp) {
      state_dsWrite.PostArb := false.B
    }
  }

  when (!state_dsWrite.Done) {
    when ((tshr_inactivate && !tshr_buffer_modified) || (!tshr_buffer_wen_last && io.fromDS.DSBufWbComp && !state_dsWrite.PreArb)) {
      state_dsWrite.Done := true.B
    }
  }.otherwise {
    when (tshr_buffer_wen_last) {
      state_dsWrite.Done := false.B
    }
  }

  when (tshr_dealloc) {
    state_dsWrite := DSWriteFSM.init
  }

  tshr_wb_done_ds := state_dsWrite.Done

  assert(state_dsWrite.DSWrite, "Illegal combination of DSWrite FSM bits")

  Seq((state_dsWrite, tshr_buffer_wen_last, io.fromDS.DSBufWbArbComp, io.fromDS.DSBufWbComp)).foreach { case (s, bufWr, wbArbComp, wbComp) =>
    assert(!(s.DSWrite_NotYet && !bufWr && !wbArbComp &&  wbComp), "Illegal transition #1 under DSWrite_NotYet")
    assert(!(s.DSWrite_NotYet && !bufWr &&  wbArbComp && !wbComp), "Illegal transition #2 under DSWrite_NotYet")
    assert(!(s.DSWrite_NotYet && !bufWr &&  wbArbComp &&  wbComp), "Illegal transition #3 under DSWrite_NotYet")
    assert(!(s.DSWrite_NotYet &&  bufWr && !wbArbComp &&  wbComp), "Illegal transition #5 under DSWrite_NotYet")
    assert(!(s.DSWrite_NotYet &&  bufWr &&  wbArbComp && !wbComp), "Illegal transition #6 under DSWrite_NotYet")
    assert(!(s.DSWrite_NotYet &&  bufWr &&  wbArbComp &&  wbComp), "Illegal transition #7 under DSWrite_NotYet")

    assert(!(s.DSWrite_PreArb && !bufWr && !wbArbComp &&  wbComp), "Illegal transition #1 under DSWrite_PreArb")
    assert(!(s.DSWrite_PreArb &&  bufWr && !wbArbComp &&  wbComp), "Illegal transition #5 under DSWrite_PreArb")

    assert(!(s.DSWrite_PostArb && !bufWr &&  wbArbComp && !wbComp), "Illegal transition #2 under DSWrite_PostArb")
    assert(!(s.DSWrite_PostArb && !bufWr &&  wbArbComp &&  wbComp), "Illegal transition #3 under DSWrite_PostArb")
    assert(!(s.DSWrite_PostArb &&  bufWr &&  wbArbComp && !wbComp), "Illegal transition #6 under DSWrite_PostArb")
    assert(!(s.DSWrite_PostArb &&  bufWr &&  wbArbComp &&  wbComp), "Illegal transition #7 under DSWrite_PostArb")

    assert(!(s.DSWrite_PostArb_PreArb && !bufWr &&  wbArbComp && !wbComp), "Illegal transition #2 under DSWrite_PostArb_PreArb")
    assert(!(s.DSWrite_PostArb_PreArb && !bufWr &&  wbArbComp &&  wbComp), "Illegal transition #3 under DSWrite_PostArb_PreArb")
    assert(!(s.DSWrite_PostArb_PreArb &&  bufWr &&  wbArbComp && !wbComp), "Illegal transition #6 under DSWrite_PostArb_PreArb")
    assert(!(s.DSWrite_PostArb_PreArb &&  bufWr &&  wbArbComp &&  wbComp), "Illegal transition #7 under DSWrite_PostArb_PreArb")

    assert(!(s.DSWrite_Done && !bufWr && !wbArbComp &&  wbComp), "Illegal transition #1 under DSWrite_Done")
    assert(!(s.DSWrite_Done && !bufWr &&  wbArbComp && !wbComp), "Illegal transition #2 under DSWrite_Done")
    assert(!(s.DSWrite_Done && !bufWr &&  wbArbComp &&  wbComp), "Illegal transition #3 under DSWrite_Done")
    assert(!(s.DSWrite_Done &&  bufWr && !wbArbComp &&  wbComp), "Illegal transition #5 under DSWrite_Done")
    assert(!(s.DSWrite_Done &&  bufWr &&  wbArbComp && !wbComp), "Illegal transition #6 under DSWrite_Done")
    assert(!(s.DSWrite_Done &&  bufWr &&  wbArbComp &&  wbComp), "Illegal transition #7 under DSWrite_Done")
  }

  XSPerfAccumulate(s"L2TSHR_${id}_DSWrite_PreArb_cycleCnt", state_dsWrite.DSWrite_PreArb)
  XSPerfAccumulate(s"L2TSHR_${id}_DSWrite_PostArb_cycleCnt", state_dsWrite.DSWrite_PostArb)
  XSPerfAccumulate(s"L2TSHR_${id}_DSWrite_PostArb_PreArb_cycleCnt", state_dsWrite.DSWrite_PostArb_PreArb)
  XSPerfAccumulate(s"L2TSHR_${id}_DSWrite_Done_cycleCnt", state_dsWrite.DSWrite_Done)


  // interactions with Directory
  io.toDir.DirRd := state_dirRead.PreArb
  io.toDir.DirWb := state_dirWrite.PreArb

  // interactions with Data Storage
  io.toDS.DSBufRd := state_dsRead.PreArb
  io.toDS.DSBufWb := state_dsWrite.PreArb

  // interactions between Directory read states and RBEs
  rbeEVT.io.directoryReadDone := state_dirRead.Done
  rbeSNP.io.directoryReadDone := state_dirRead.Done
  rbeREQ.io.directoryReadDone := state_dirRead.Done
}
