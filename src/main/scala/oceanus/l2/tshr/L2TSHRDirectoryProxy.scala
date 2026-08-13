package oceanus.l2.tshr

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.compactchi.CCHIOpcode._
import oceanus.l2._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters


class L2TSHRDirectoryProxy(val id: Int)(implicit val p: Parameters) extends Module with HasL2Params {

  class DirReadFSM extends Bundle {
    val PreArb = Bool()     // sending Read Request to Directory, waiting for Arbiter completion
    val PostArb = Bool()    // Read Request has been accepted by Directory
    val Done = Bool()       // Read Response has been received from Directory
    val ReplPreArb = Bool() // sending Replacement Read Request to Directory, waiting for Arbiter completion
    val ReplPostArb = Bool()// Replacement Read Request has been accepted by Directory
    val ReplDone = Bool()   // Replacement Read Response has been received from Directory
    val ReplRetry = Bool()  // Replacement Read Retry Ack has been received from Directory
    def NotYet =            // haven't sent Read Request to Directory
      !PreArb && !PostArb && !Done && !ReplPreArb && !ReplPostArb && !ReplDone && !ReplRetry
    def Repl =
      ReplPreArb || ReplPostArb || ReplDone || ReplRetry
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

  val io = IO(new Bundle {

    val toDir = Output(new L2Directory.PathToDirectory)
    val fromDir = Input(new L2Directory.PathFromDirectory)

    val tshr_paddr = Input(UInt(paramL2.physicalAddrWidth.W))

    val tshr_alloc = Input(Bool())
    val tshr_reuse = Input(Bool())
    val tshr_inactive = Input(Bool())
    val tshr_dealloc = Input(Bool())

    val read_arbed = Input(Bool())
    val read_en = Input(Bool())
    val repl_en = Input(Bool())

    val meta = Input(new L2Directory.Meta)
    val meta_way = Input(UInt(4.W)) // TODO: parameterize with l2 way count
    val meta_modified = Input(new L2Directory.MetaWriteMask)
    val tag_modifed = Input(Bool())

    val rd_idle = Output(Bool())
    val rd_accept = Output(Bool())
    val rd_done = Output(Bool())

    val repl_idle = Output(Bool())
    val repl_ready = Output(Bool())
    val repl_accept = Output(Bool())
    val repl_done = Output(Bool())
    val repl_retry = Output(Bool())

    val wb_locked = Input(Bool())
    val wb_accept = Output(Bool())
    val wb_done = Output(Bool())
  })

  // Whether the meta was aggressively immediately trying to be written back on every modification.
  val configAggressiveWrite = false

  // Whether ReplRd only happens after a done DirRd.
  //  - Under most situations, this should always be set to 'true', except debug or feature evaluation,
  //    since we always need hit/miss info (from any done Directory Read) before any replacement action.
  //  - When set to 'false', the "rd_done" and "repl_ready" signals might not behave as expected.
  val configReplReadAfterReadOnly = true

  val fromDir_en = io.fromDir.TSHRID === id.U

  val fromDir_DirRdArbComp = fromDir_en && io.fromDir.DirRdArbComp
  val fromDir_DirRdResp = fromDir_en && io.fromDir.DirRdResp
  val fromDir_DirWbArbComp = fromDir_en && io.fromDir.DirWbArbComp
  val fromDir_ReplRdArbComp = fromDir_en && io.fromDir.ReplRdArbComp
  val fromDir_ReplRdResp = fromDir_en && io.fromDir.ReplRdResp
  val fromDir_ReplRdRetryAck = fromDir_en && io.fromDir.ReplRdRetryAck

  // Directory read states
  val state_dirRead = RegInit(new DirReadFSM, DirReadFSM.init)
  val state_dirRead_next = WireInit(state_dirRead)

  state_dirRead := state_dirRead_next

  when (state_dirRead.NotYet) {

    /*
    1. 
    */
    whenOpt ()(io.read_en) {
      when (io.read_arbed) {
        // 1. [] -> DirRead_PostArb
        state_dirRead_next.PostArb := true.B
      }.otherwise {
        // 2. [] -> DirRead_PreArb
        state_dirRead_next.PreArb := true.B
      }
    }.elsewhenOpt (!configReplReadAfterReadOnly)(io.repl_en) {
      // 3. [] -> DirRead_ReplPreArb
      state_dirRead_next.ReplPreArb := true.B
    }
  }

  //when (state_dirRead.PreArb) {

    /*
    1. 

    *NOTICE: The outer "when" context of DirRead_PreArb state could be omitted, because 
             DirRdArbComp was always expected to be received only under DirRead_PreArb state.
             Ensure this always stands when adding state transitions under DirRead_PreArb.
    */
    when (fromDir_DirRdArbComp) {
      // 1. DirRead_PreArb -> DirRead_PostArb
      state_dirRead_next.PreArb := false.B
      state_dirRead_next.PostArb := true.B
    }
  //}

//when (state_dirRead.PostArb) {

    /*
    1.

    *NOTICE: The outer "when" context of DirRead_PostArb state could be omitted, becasue
             DirRdResp was always expected to be received only under DirRead_PostArb state.
             Ensure this always stands when adding state transitions under DirRead_PostArb.
    */
    when (fromDir_DirRdResp) {
      // 1. DirRead_PostArb -> DirRead_Done
      state_dirRead_next.PostArb := false.B
      state_dirRead_next.Done := true.B
    }
//}

  when (state_dirRead.Done) {
    
    /*
    1.
    */
    when (io.repl_en) {
      // 1. DirRead_Done -> DirRead_ReplPreArb
      state_dirRead_next.Done := false.B
      state_dirRead_next.ReplPreArb := true.B
    }
  }

//when (state_dirRead.ReplPreArb) {

    /* 
    1.

    *NOTICE: The outer "when" context of DirRead_ReplPreArb state could be omitted, because
             ReplRdArbComp was always expected to be received only under DirRead_ReplPreArb state.
             Ensure this always stands when adding state transitions under DirRead_ReplPreArb.
    */
    when (fromDir_ReplRdArbComp) {
      // 1. DirRead_ReplPreArb -> DirRead_ReplPostArb
      state_dirRead_next.ReplPreArb := false.B
      state_dirRead_next.ReplPostArb := true.B
    }
//}

//when (state_dirRead.ReplPostArb) {

    /*
    1.

    *NOTICE: The outer "when" context of DirRead_ReplPostArb state could be omitted, because
             ReplRdResp and ReplRdRetryAck were always expected to be received only under DirRead_ReplPostArb state.
             Ensure this always stands when adding state transitions under DirRead_ReplPostArb.
    */
    when (fromDir_ReplRdResp) {
      // 1. DirRead_ReplPostArb -> DirRead_ReplDone
      state_dirRead_next.ReplPostArb := false.B
      state_dirRead_next.ReplDone := true.B
    }
    when (fromDir_ReplRdRetryAck) {
      // 2. DirRead_ReplPostArb -> DirRead_ReplRetry
      state_dirRead_next.ReplPostArb := false.B
      state_dirRead_next.ReplRetry := true.B
    }
//}

  when (state_dirRead.ReplRetry) {

    /*
    1. 
    */
    when (io.repl_en) {
      // 1. DirRead_ReplRetry -> DirRead_ReplPreArb
      state_dirRead_next.ReplRetry := false.B
      state_dirRead_next.ReplPreArb := true.B
    }
  }

  when (io.tshr_dealloc) {
    state_dirRead_next := DirReadFSM.init
  }

  io.rd_idle := state_dirRead.NotYet
  io.rd_accept := io.toDir.DirRd && fromDir_DirRdArbComp
  io.rd_done := state_dirRead.Done || state_dirRead.Repl

  io.repl_idle := !state_dirRead.ReplPreArb && !state_dirRead.ReplPostArb
  io.repl_ready := state_dirRead.Done || state_dirRead.ReplRetry
  io.repl_accept := io.toDir.ReplRd && fromDir_ReplRdArbComp
  io.repl_done := state_dirRead.ReplDone
  io.repl_retry := state_dirRead.ReplRetry

  assert(!(fromDir_DirRdArbComp && !state_dirRead.PreArb), "receiving DirRdArbComp on unexpected state (expecting PreArb)")
  assert(!(fromDir_DirRdResp && !state_dirRead.PostArb), "receiving DirRdResp on unexpected state (expecting PostArb)")
  assert(!(fromDir_ReplRdArbComp && !state_dirRead.ReplPreArb), "receiving ReplRdArbComp on unexpected state (expected ReplPreArb)")
  assert(!(fromDir_ReplRdResp && !state_dirRead.ReplPostArb), "receiving ReplRdResp on unexpected state (expected ReplPostArb)")
  assert(!(fromDir_ReplRdRetryAck && !state_dirRead.ReplPostArb), "receiving ReplRdRetryAck on unexpected state (expected ReplPostArb)")

  assert(!(fromDir_ReplRdResp && fromDir_ReplRdRetryAck), "ReplRdResp and ReplRdRetryAck must be exclusive to each other")

  assert(PopCount(state_dirRead.asUInt) <= 1.U, "multiple active states in DirReadFSM")

  FSMPerfHistogram(s"L2TSHR_${id}_DirReadFSM_PreArb", state_dirRead.PreArb, state_dirRead_next.PreArb)
  FSMPerfHistogram(s"L2TSHR_${id}_DirReadFSM_PostArb", state_dirRead.PostArb, state_dirRead_next.PostArb)
  FSMPerfHistogram(s"L2TSHR_${id}_DirReadFSM_Done", state_dirRead.Done, state_dirRead_next.Done)
  FSMPerfHistogram(s"L2TSHR_${id}_DirReadFSM_ReplPreArb", state_dirRead.ReplPreArb, state_dirRead_next.ReplPreArb)
  FSMPerfHistogram(s"L2TSHR_${id}_DirReadFSM_ReplPostArb", state_dirRead.ReplPostArb, state_dirRead_next.ReplPostArb)
  FSMPerfHistogram(s"L2TSHR_${id}_DirReadFSM_ReplDone", state_dirRead.ReplDone, state_dirRead_next.ReplDone)
  FSMPerfHistogram(s"L2TSHR_${id}_DirReadFSM_ReplRetry", state_dirRead.ReplRetry, state_dirRead_next.ReplRetry)
  FSMPerfHistogram(s"L2TSHR_${id}_DirReadFSM_NotYet", state_dirRead.NotYet, state_dirRead_next.NotYet)

  FSMTransPerfHistogram(s"L2TSHR_${id}_DirReadFSM_Done_to_ReplPreArb", state_dirRead.Done, state_dirRead_next.ReplPreArb)
  FSMTransPerfHistogram(s"L2TSHR_${id}_DirReadFSM_Done_to_NotYet", state_dirRead.Done, state_dirRead_next.NotYet)

  // Directory write states
  val state_dirWrite = RegInit(new DirWriteFSM, DirWriteFSM.init)
  val state_dirWrite_next = WireInit(state_dirWrite)

  state_dirWrite := state_dirWrite_next

  if (configAggressiveWrite) {

    when (state_dirWrite.NotYet) {

      /*
      1.
      */
      when (io.meta_modified.any) {
        // 1. [] -> DirWrite_PreArb
        state_dirWrite_next.PreArb := true.B
      }.elsewhen (io.tshr_inactive) {
        // 2. [] -> DirWrite_Done
        state_dirWrite_next.Done := true.B
      }
    }

    when (state_dirWrite.PreArb) {

      /*
      1. 
      */
      when (fromDir_DirWbArbComp) {
        // 1. DirWrite_PreArb -> DirWrite_Done
        state_dirWrite_next.PreArb := false.B
        state_dirWrite_next.Done := true.B
      }
    }

    when (state_dirWrite.Done) {

      /*
      1. 
      */
      when (io.meta_modified.any) {
        // 1. DirWrite_Done -> DirWrite_PreArb
        state_dirWrite_next.Done := false.B
        state_dirWrite_next.PreArb := true.B
      }
    }
  } else {

    when (state_dirWrite.NotYet) {

      /* 
      1.  
      */
      when (io.tshr_inactive) {
        when (io.meta_modified.any) {
        // 1. [] -> DirWrite_PreArb
        state_dirWrite_next.PreArb := true.B
        }.otherwise {
          // 2. [] -> DirWrite_Done
          state_dirWrite_next.Done := true.B
        }
      }
    }

    when (state_dirWrite.PreArb) {

      /*
      1.
      */
      when (io.tshr_reuse) {
        // 1. DirWrite_PreArb -> []
        state_dirWrite_next.PreArb := false.B
      }.elsewhen (fromDir_DirWbArbComp) {
        // 2. DirWrite_PreArb -> DirWrite_Done
        state_dirWrite_next.PreArb := false.B
        state_dirWrite_next.Done := true.B
      }
    }

    when (state_dirWrite.Done) {

      /*
      1.
      */
      when (io.meta_modified.any) {
        // 1. DirWrite_Done -> []
        state_dirWrite_next.Done := false.B
      }
    }
  }

  when (io.tshr_dealloc) {
    state_dirWrite := DirWriteFSM.init
  }

  io.wb_accept := io.toDir.DirWb && fromDir_DirWbArbComp
  io.wb_done := state_dirWrite.Done

  assert(!(fromDir_DirWbArbComp && !state_dirWrite.PreArb), "receiving DirWbArbComp on unexpected state (expecting PreArb)")
  assert(PopCount(state_dirWrite.asUInt) <= 1.U, "multiple active states in DirWriteFSM")

  // interactions with Directory
  io.toDir.TSHRID := id.U
  io.toDir.PADDR := io.tshr_paddr
  io.toDir.WAY := io.meta_way
  io.toDir.META := io.meta
  io.toDir.META_WEN := io.meta_modified
  io.toDir.TAG_WEN := io.tag_modifed
  io.toDir.DirRd := state_dirRead.PreArb
  io.toDir.DirWb := state_dirWrite.PreArb && !io.wb_locked
  io.toDir.ReplRd := state_dirRead.ReplPreArb

  assert(PopCount(Seq(io.toDir.DirRd, io.toDir.DirWb, io.toDir.ReplRd)) <= 1.U,
    "DirRd, DirWb and ReplRd operations to Directory overlapped")

  FSMPerfHistogram(s"L2TSHR_${id}_DirWriteFSM_PreArb", state_dirWrite.PreArb, state_dirWrite_next.PreArb)
  FSMPerfHistogram(s"L2TSHR_${id}_DirWriteFSM_Done", state_dirWrite.Done, state_dirWrite_next.Done)
  FSMPerfHistogram(s"L2TSHR_${id}_DirWriteFSM_NotYet", state_dirWrite.NotYet, state_dirWrite_next.NotYet)
}
