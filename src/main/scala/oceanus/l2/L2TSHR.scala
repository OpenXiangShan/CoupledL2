package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.l2._
import oceanus.l2.L2TSHR._
import oceanus.l2.L2Directory._
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
}

class L2TSHR(implicit val p: Parameters) extends Module {

  val io = IO(new Bundle {

    val toDir = Output(new L2Directory.PathToDirectory)
    val fromDir = Input(new L2Directory.PathFromDirectory)

    val RXEVT = Flipped(Decoupled(new FlitEVT))       // L1 EVT
    val RXSNP = Flipped(Decoupled(new CHIBundleSNP))  // L3 SNP
    val RXREQ = Flipped(Decoupled(new FlitREQ))       // L1/L2 REQ

    val alloc = Input(Bool())
    val valid = Output(Bool())
  })

  // miscs and enchantments
  val req_need_dirRead = Wire(Bool())


  // TSHR valid
  val tshr_enter = io.RXEVT.fire || io.RXSNP.fire || io.RXREQ.fire
  val tshr_alloc = io.alloc
  val tshr_reuse = tshr_enter && !tshr_alloc

  val tshr_inactive_rbe = Wire(Bool())
  val tshr_inactive_vpipe = Wire(Bool())
  val tshr_inactive = tshr_inactive_rbe && tshr_inactive_vpipe

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

  tshr_wb_done_dir := state_dirWrite.Done

  assert(!(io.fromDir.DirWbArbComp && !state_dirWrite.PreArb), "receiving DirWbArbComp on unexpected state (expecting PreArb)")
  assert(PopCount(state_dirWrite.asUInt) <= 1.U, "multiple active states in DirWriteFSM")


  // interactions with Directory
  io.toDir.DirRd := state_dirRead.PreArb
  io.toDir.DirWb := state_dirWrite.PreArb

  // interactions between Directory read states and RBEs
  rbeEVT.io.directoryReadDone := state_dirRead.Done
  rbeSNP.io.directoryReadDone := state_dirRead.Done
  rbeREQ.io.directoryReadDone := state_dirRead.Done
}
