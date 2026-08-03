package oceanus.l2

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
import chisel3.simulator.PeekPokeAPI.TestableData
import freechips.rocketchip.util.RotateVector.left
import freechips.rocketchip.util.SeqToAugmentedSeq

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

class L2TSHRDataStorageProxy(val id: Int)(implicit val p: Parameters) extends Module with HasL2Params {

  class DSReadFSM extends Bundle {
    val AheadPreArb_S1 = Bool() // sending Ahead Read Request to Data Storage at S1, waiting for Arbiter completion
    val AheadPreArb_S2 = Bool() // sending Ahead Read Request to Data Storage at S2, waiting for Arbiter completion
    val AheadPostArb = Bool()   // Ahead Read Request has been accepted by Data Storage
    val AheadDone = Bool()      // Ahead Read Response has been received from Data Storage, but no verified
    val PreArb = Bool()         // sending Read Request to Data Storage, waiting for Arbiter completion
    val PostArb = Bool()        // Read Request has been accepted by Data Storage
    val Done = Bool()           // Read Response has been received from Data Storage
    def NotYet =                // haven't sent Read Request to Data Storage
      !AheadPreArb_S1 && !AheadPreArb_S2 && !AheadPostArb && !AheadDone && !PreArb && !PostArb && !Done
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
    val PreArb_PostArb = Bool()
    val Done = Bool()       // Write Request has been completed and observable to later requests
    def NotYet =            // haven't sent Write Request to Data Storage
      !PreArb && !PostArb && !PreArb_PostArb && !Done
  }

  object DSWriteFSM {
    def init = {
      val initState = Wire(new DSWriteFSM)
      initState.elements.foreach(_._2 := false.B)
      initState
    }
  }

  val io = IO(new Bundle {

    val fromDir = Input(new L2Directory.PathFromDirectory)

    val toDS = Output(new L2DataStorage.PathTSHRToDataStorage)
    val fromDS = Input(new L2DataStorage.PathDataStorageToTSHR)

    val tshr_paddr = Input(UInt(48.W)) // TODO: parameterize with L2 physical address width

    val meta_valid = Input(Bool())
    val meta_way = Input(UInt(4.W)) // TODO: parameterize with L2 way count
    val meta_state = Input(L2Directory.MetaState())
    
    val tbuf_wen_last = Input(Bool())
    val tbuf_modified = Input(Bool())
    val tbuf_data_0 = Input(UInt(256.W))
    val tbuf_data_2 = Input(UInt(256.W))

    val tshr_inactivate = Input(Bool())
    val tshr_inactive = Input(Bool())
    val tshr_dealloc = Input(Bool())

    val ds_read_ahead_en = Input(Bool())
    val ds_read_ahead_way = Input(UInt(4.W)) // TODO: parameterize with L2 way count
    val ds_read_ahead_arbed = Input(Bool())

    val ds_read_rbeEVT_en = Input(Bool())
    val ds_read_rbeSNP_en = Input(Bool())
    val ds_read_rbeREQ_en = Input(Bool())

    val ds_read_vPipeEVT_en = Input(Bool())
    val ds_read_vPipeSNP_en = Input(Bool())
    val ds_read_vPipeREQ_en = Input(Bool())

    // TODO: support cancel here?
    val ds_read_EVT_cancel = Input(Bool())
    val ds_read_SNP_cancel = Input(Bool())
    val ds_read_REQ_cancel = Input(Bool())

    val ds_read_aux_en = Input(Bool()) // aux DS read enable that overrides all other conditions

    val rd_idle = Output(Bool())
    val rd_accept = Output(Bool())
    val rd_done = Output(Bool())

    val wb_locked = Input(Bool())
    val wb_accept = Output(Bool())
    val wb_done = Output(Bool())

    val RXDAT_fire = Input(Bool()) // io.UpRXDAT.fire || io.DnRXDAT.fire
  })

  // configuration parameters
  val configS1ReadAhead = true
  val configS2ReadAhead = true

  val configFlowDirRdResp = true
  val configFlowAheadRdResp = true

  //
  val ds_read_rbe_en = io.ds_read_rbeEVT_en || io.ds_read_rbeSNP_en || io.ds_read_rbeREQ_en
  val ds_read_vPipe_en = io.ds_read_vPipeEVT_en || io.ds_read_vPipeSNP_en || io.ds_read_vPipeREQ_en

  val ds_read_postRBE_en = ds_read_rbe_en || ds_read_vPipe_en

  val ds_read_postRBE_en_q = RegInit(false.B)

  when (ds_read_postRBE_en) {
    ds_read_postRBE_en_q := true.B
  }

  when (io.tshr_dealloc) {
    ds_read_postRBE_en_q := false.B
  }

  val ds_read_nonAhead_en = ds_read_postRBE_en || ds_read_postRBE_en_q || io.ds_read_aux_en

  //
  val fromDir_en = io.fromDir.TSHRID === id.U

  val fromDir_DirRdResp = fromDir_en && io.fromDir.DirRdResp

  //
  val fromDS_en = io.fromDS.TSHRID === id.U

  val fromDS_DSBufAheadRdArbComp = fromDS_en && io.fromDS.DSBufAheadRdArbComp
  val fromDS_DSBufAheadRdResp = fromDS_en && io.fromDS.DSBufAheadRdResp
  val fromDS_DSBufRdArbComp = fromDS_en && io.fromDS.DSBufRdArbComp
  val fromDS_DSBufRdResp = fromDS_en && io.fromDS.DSBufRdResp
  val fromDS_DSBufWbArbComp = fromDS_en && io.fromDS.DSBufWbArbComp
  val fromDS_DSBufWbComp = fromDS_en && io.fromDS.DSBufWbComp

  //
  val miss_on_fromDS_dirRdResp = fromDir_DirRdResp && (io.fromDir.META.state === MetaState.I || io.fromDir.META.way =/= io.fromDS.WAY)
  val miss_on_fromDS_metaValid = io.meta_valid && (io.meta_state === MetaState.I || io.meta_way =/= io.fromDS.WAY)

  val hit_on_fromDS_dirRdResp = fromDir_DirRdResp && (io.fromDir.META.state =/= MetaState.I && io.fromDir.META.way === io.fromDS.WAY)
  val hit_on_fromDS_metaValid = io.meta_valid && (io.meta_state =/= MetaState.I && io.meta_way === io.fromDS.WAY)

  val ds_read_ahead_way_q = RegInit(0.U(4.W)) // TODO: parameterize with L2 way count

  when (io.ds_read_ahead_en) {
    ds_read_ahead_way_q := io.ds_read_ahead_way
  }

  val miss_after_fromDS_dirRdResp = fromDir_DirRdResp && (io.fromDir.META.state === MetaState.I || io.fromDir.META.way =/= ds_read_ahead_way_q)
  val miss_after_fromDS_metaValid = io.meta_valid && (io.meta_state === MetaState.I || io.meta_way =/= ds_read_ahead_way_q)

  val hit_after_fromDS_dirRdResp = fromDir_DirRdResp && (io.fromDir.META.state =/= MetaState.I && io.fromDir.META.way === ds_read_ahead_way_q)
  val hit_after_fromDS_metaValid = io.meta_valid && (io.meta_state =/= MetaState.I && io.meta_way === ds_read_ahead_way_q)
  

  // Data Storage read states
  val state_dsRead = RegInit(new DSReadFSM, DSReadFSM.init)
  val state_dsRead_next = WireInit(state_dsRead)

  state_dsRead := state_dsRead_next

  when (state_dsRead.NotYet) {

    /*
    1.  
    */
    when (ds_read_nonAhead_en) {
      // 1. [] -> DSRead_PreArb
      state_dsRead_next.PreArb := true.B
    }.elsewhen (io.ds_read_ahead_en) {
      when (io.ds_read_ahead_arbed) {
        // 2. [] -> DSRead_AheadPostArb
        state_dsRead_next.AheadPostArb := true.B
      }.otherwise {
        if (configS1ReadAhead) {
          // 3. [] -> DSRead_AheadPreArb_S1
          state_dsRead_next.AheadPreArb_S1 := true.B
        }
      }
    }
  }

  when (state_dsRead.PreArb) {

    /*
    1. 
    */
    when (fromDS_DSBufRdArbComp) {
      // 1. DSRead_PreArb -> DSRead_PostArb
      state_dsRead_next.PreArb := false.B
      state_dsRead_next.PostArb := true.B
    }
  }

  when (state_dsRead.PostArb) {
    
    /*
    1.
    */
    when (fromDS_DSBufRdResp) {
      // 1. DSRead_PostArb -> DSRead_Done
      state_dsRead_next.PostArb := false.B
      state_dsRead_next.Done := true.B
    }
  }

  when (state_dsRead.AheadPreArb_S1) {
    
    /*
    1. 
    */
    whenOpt ()(fromDS_DSBufAheadRdArbComp) {
      // 1. DSRead_AheadPreArb_S1 -> DSRead_AheadPostArb
      state_dsRead_next.AheadPreArb_S1 := false.B
      state_dsRead_next.AheadPostArb := true.B
    }.elsewhen (ds_read_nonAhead_en) {
      // 2.1. DSRead_AheadPreArb_S1 -> DSRead_PreArb
      state_dsRead_next.AheadPreArb_S1 := false.B
      state_dsRead_next.PreArb := true.B
    }.otherwise {
      if (configS2ReadAhead) {
        // 3. DSRead_AheadPreArb_S1 -> DSRead_AheadPreArb_S2
        state_dsRead_next.AheadPreArb_S1 := false.B
        state_dsRead_next.AheadPreArb_S2 := true.B
      } else {
        // 4. DSRead_AheadPreArb_S1 -> []
        state_dsRead_next.AheadPreArb_S1 := false.B
      }
    }
  }

  when (state_dsRead.AheadPreArb_S2) {

    /*
    1.
    */
    whenOpt ()(fromDS_DSBufAheadRdArbComp) {
      // 1. DSRead_AheadPreArb_S2 -> DSRead_AheadPostArb
      state_dsRead_next.AheadPreArb_S2 := false.B
      state_dsRead_next.AheadPostArb := true.B
    }.elsewhen (ds_read_nonAhead_en) {
      // 2.3. DSRead_AheadPreArb_S2 -> DSRead_PreArb
      state_dsRead_next.AheadPreArb_S2 := false.B
      state_dsRead_next.PreArb := true.B
    }.otherwise {
      // . DSRead_AheadPreArb_S2 -> []
      state_dsRead_next.AheadPreArb_S2 := false.B
    }
  }

  when (state_dsRead.AheadPostArb) {

    /*
    1. 
    */
    when (fromDS_DSBufAheadRdResp) {
      whenOpt (configFlowAheadRdResp)(hit_on_fromDS_metaValid) {
        // 1.1. DSRead_AheadPostArb -> DSRead_Done
        state_dsRead_next.AheadPostArb := false.B
        state_dsRead_next.Done := true.B
      }.elsewhenOpt (configFlowAheadRdResp && configFlowDirRdResp)(hit_on_fromDS_dirRdResp) {
        // 1.2. DSRead_AheadPostArb -> DSRead_Done
        state_dsRead_next.AheadPostArb := false.B
        state_dsRead_next.Done := true.B
      }.elsewhenOpt (configFlowAheadRdResp)(miss_on_fromDS_metaValid) {
        when (ds_read_nonAhead_en) {
          // 2.1. DSRead_AheadPostArb -> DSRead_PreArb
          state_dsRead_next.AheadPostArb := false.B
          state_dsRead_next.PreArb := true.B
        }.otherwise {
          // 3.1. DSRead_AheadPostArb -> []
          state_dsRead_next.AheadPostArb := false.B
        }
      }.elsewhenOpt (configFlowAheadRdResp && configFlowDirRdResp)(miss_on_fromDS_dirRdResp) {
        when (ds_read_nonAhead_en) {
          // 2.2. DSRead_AheadPostArb -> DSRead_PreArb
          state_dsRead_next.AheadPostArb := false.B
          state_dsRead_next.PreArb := true.B
        }.otherwise {
          // 3.2. DSRead_AheadPostArb -> []
          state_dsRead_next.AheadPostArb := false.B
        }
      }.otherwise {
        // 4. DSRead_AheadPostArb -> DSRead_AheadDone
        state_dsRead_next.AheadPostArb := false.B
        state_dsRead_next.AheadDone := true.B
      }
    }
  }

  when (state_dsRead.AheadDone) {

    /*
    1. 
    */
    whenOpt (true)(hit_after_fromDS_metaValid) {
      // 1.1. DSRead_AheadDone -> DSRead_Done
      state_dsRead_next.AheadDone := false.B
      state_dsRead_next.Done := true.B
    }.elsewhenOpt (configFlowDirRdResp)(hit_after_fromDS_dirRdResp) {
      // 1.2. DSRead_AheadDone -> DSRead_Done
      state_dsRead_next.AheadDone := false.B
      state_dsRead_next.Done := true.B
    }.elsewhen (miss_after_fromDS_metaValid) {
      when (ds_read_nonAhead_en) {
        // 2.1. DSRead_AheadDone -> DSRead_PreArb
        state_dsRead_next.AheadDone := false.B
        state_dsRead_next.PreArb := true.B
      }.otherwise {
        // 3.1. DSRead_AheadDone -> []
        state_dsRead_next.AheadDone := false.B
      }
    }.elsewhenOpt (configFlowDirRdResp)(miss_after_fromDS_dirRdResp) {
      when (ds_read_nonAhead_en) {
        // 2.2. DSRead_AheadDone -> DSRead_PreArb
        state_dsRead_next.AheadDone := false.B
        state_dsRead_next.PreArb := true.B
      }.otherwise {
        // 3.2. DSRead_AheadDone -> []
        state_dsRead_next.AheadDone := false.B
      }
    }
  }

  when (io.RXDAT_fire) {
    /*

    */
    state_dsRead_next.PreArb := false.B
    state_dsRead_next.AheadPreArb_S1 := false.B
    state_dsRead_next.AheadPreArb_S2 := false.B
  }

  when (io.tshr_dealloc) {
    state_dsRead_next := DSReadFSM.init
  }

  io.rd_idle := state_dsRead.NotYet
  io.rd_done := state_dsRead.Done

  assert(PopCount(state_dsRead.asUInt) <= 1.U, "multiple active state in DSReadFSM")

  assert(!(io.fromDS.DSBufAheadRdArbComp && !state_dsRead.AheadPreArb_S1 && !state_dsRead.AheadPreArb_S2),
    "unexpected DSBufAheadRdArbComp from Data Storage on non-AheadPreArb state")
  assert(!(io.fromDS.DSBufAheadRdResp && !state_dsRead.AheadPostArb),
    "unexpected DSBufAheadRdResp from Data Storage on non-AheadPostArb state")
  assert(!(io.fromDS.DSBufRdArbComp && !state_dsRead.PreArb),
    "unexpected DSBufRdArbComp from Data Storage on non-PreArb state")
  assert(!(io.fromDS.DSBufRdResp && !state_dsRead.PostArb),
    "unexpected DSBufRdResp from Data Storage on non-PostArb state")

  assert(!(state_dsRead.PreArb && !io.meta_valid), "meta not valid on DSReadFSM state PreArb")
  assert(!(state_dsRead.PostArb && !io.meta_valid), "meta not valid on DSReadFSM state PostArb")
  assert(!(state_dsRead.Done && !io.meta_valid), "meta not valid on DSReadFSM state Done")

  val perf_PreArb_cycleCnt = RegInit(0.U(32.W))
  val perf_PostArb_cycleCnt = RegInit(0.U(32.W))
  val perf_AheadPostArb_cycleCnt = RegInit(0.U(32.W))
  val perf_AheadDone_cycleCnt = RegInit(0.U(32.W))

  when (state_dsRead.PreArb) {
    when (!state_dsRead_next.PreArb) {
      perf_PreArb_cycleCnt := 0.U
    }.otherwise {
      perf_PreArb_cycleCnt := perf_PreArb_cycleCnt + 1.U
    }
  }

  when (state_dsRead.PostArb) {
    when (!state_dsRead_next.PostArb) {
      perf_PostArb_cycleCnt := 0.U
    }.otherwise {
      perf_PostArb_cycleCnt := perf_PostArb_cycleCnt + 1.U
    }
  }

  when (state_dsRead.AheadPostArb) {
    when (!state_dsRead_next.AheadPostArb) {
      perf_AheadPostArb_cycleCnt := 0.U
    }.otherwise {
      perf_AheadPostArb_cycleCnt := perf_AheadPostArb_cycleCnt + 1.U
    }
  }

  when (state_dsRead.AheadDone) {
    when (!state_dsRead_next.AheadDone) {
      perf_AheadDone_cycleCnt := 0.U
    }.otherwise {
      perf_AheadDone_cycleCnt := perf_AheadDone_cycleCnt + 1.U
    }
  }

  assert(!(io.fromDir.DirRdResp && io.meta_valid), "DirRdResp on local meta valid, multiple Directory read might be happened")

  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S1_cnt", state_dsRead.AheadPreArb_S1)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S2_cnt", state_dsRead.AheadPreArb_S2)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_PreArb_cnt", state_dsRead.PreArb)
  XSPerfHistogram(s"L2TSHR_DSReadFSM_PreArb_cnt", perf_PreArb_cycleCnt, state_dsRead.PreArb && !state_dsRead_next.PreArb, 0, 40, 2, right_strict = true)
  XSPerfHistogram(s"L2TSHR_DSReadFSM_PreArb_cnt", perf_PreArb_cycleCnt, state_dsRead.PreArb && !state_dsRead_next.PreArb, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPostArb", state_dsRead.AheadPostArb)
  XSPerfHistogram(s"L2TSHR_DSReadFSM_AheadPostArb", perf_AheadPostArb_cycleCnt, state_dsRead.AheadPostArb && !state_dsRead_next.AheadPostArb, 0, 40, 2, right_strict = true)
  XSPerfHistogram(s"L2TSHR_DSReadFSM_AheadPostArb", perf_AheadPostArb_cycleCnt, state_dsRead.AheadPostArb && !state_dsRead_next.AheadPostArb, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_PostArb", state_dsRead.PostArb)
  XSPerfHistogram(s"L2TSHR_DSReadFSM_PostArb", perf_PostArb_cycleCnt, state_dsRead.PostArb && !state_dsRead_next.PostArb, 0, 40, 2, right_strict = true)
  XSPerfHistogram(s"L2TSHR_DSReadFSM_PostArb", perf_PostArb_cycleCnt, state_dsRead.PostArb && !state_dsRead_next.PostArb, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadDone", state_dsRead.AheadDone)
  XSPerfHistogram(s"L2TSHR_DSReadFSM_AheadDone", perf_AheadDone_cycleCnt, state_dsRead.AheadDone && !state_dsRead_next.AheadDone, 0, 40, 2, right_strict = true)
  XSPerfHistogram(s"L2TSHR_DSReadFSM_AheadDone", perf_AheadDone_cycleCnt, state_dsRead.AheadDone && !state_dsRead_next.AheadDone, 40, 800, 40, left_strict = true)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_Done", state_dsRead.Done)

  XSPerfAccumulate(s"L2TSHR_DSReadFSM_NotYet_dirRdResp_read", io.fromDir.DirRdResp && state_dsRead.NotYet && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_NotYet_metaValid_read", io.meta_valid && state_dsRead.NotYet && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S1_dirRdResp_read", io.fromDir.DirRdResp && state_dsRead.AheadPreArb_S1 && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S1_dirRdResp_noRead", io.fromDir.DirRdResp && state_dsRead.AheadPreArb_S1 && state_dsRead_next.NotYet)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S1_metaValid_read", io.meta_valid && state_dsRead.AheadPreArb_S1 && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S1_metaValid_noRead", io.meta_valid && state_dsRead.AheadPreArb_S1 && state_dsRead_next.NotYet)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S2_dirRdResp_read", io.fromDir.DirRdResp && state_dsRead.AheadPreArb_S2 && state_dsRead_next.PreArb)
//XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S2_dirRdResp_noRead")
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S2_metaValid_read", io.meta_valid && state_dsRead.AheadPreArb_S2 && state_dsRead_next.PreArb)
//XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S2_metaValid_noRead")
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPostArb_dirRdResp_hit", io.fromDir.DirRdResp && state_dsRead.AheadPostArb && state_dsRead_next.Done)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPostArb_dirRdResp_miss_read", io.fromDir.DirRdResp && state_dsRead.AheadPostArb && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPostArb_dirRdResp_miss_noRead", io.fromDir.DirRdResp && state_dsRead.AheadPostArb && state_dsRead_next.NotYet)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPostArb_metaValid_hit", io.meta_valid && state_dsRead.AheadPostArb && state_dsRead_next.Done)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPostArb_metaValid_miss_read", io.meta_valid && state_dsRead.AheadPostArb && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPostArb_metaValid_miss_noRead", io.meta_valid && state_dsRead.AheadPostArb && state_dsRead_next.NotYet)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadDone_dirRdResp_hit", io.fromDir.DirRdResp && state_dsRead.AheadDone && state_dsRead_next.Done)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadDone_dirRdResp_miss_read", io.fromDir.DirRdResp && state_dsRead.AheadDone && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadDone_dirRdResp_miss_noRead", io.fromDir.DirRdResp && state_dsRead.AheadDone && state_dsRead_next.NotYet)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadDone_metaValid_hit", io.meta_valid && state_dsRead.AheadDone && state_dsRead_next.Done)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadDone_metaValid_miss_read", io.meta_valid && state_dsRead.AheadDone && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadDone_metaValid_miss_noRead", io.meta_valid && state_dsRead.AheadDone && state_dsRead_next.NotYet)

  // Data Storage write states
  val state_dsWrite = RegInit(new DSWriteFSM, DSWriteFSM.init)
  val state_dsWrite_next = WireInit(state_dsWrite)

  state_dsWrite := state_dsWrite_next

  when (state_dsWrite.NotYet) {

    /*
    1.
    */
    when (io.tbuf_wen_last) {
      // 1. [] -> DSWrite_PreArb
      state_dsWrite_next.PreArb := true.B
    }.elsewhen (io.tshr_inactivate && !io.tbuf_modified) {
      // 2. [] -> DSWrite_Done
      state_dsWrite_next.Done := true.B
    }
  }

  when (state_dsWrite.PreArb) {

    /*
    1. 
    */
    when (io.tbuf_wen_last) {
      when (fromDS_DSBufWbArbComp && fromDS_DSBufWbComp) {
        // 1.1. DSWrite_PreArb -> DSWrite_PreArb
      }.elsewhen (fromDS_DSBufWbArbComp) {
        // 2. DSWrite_PreArb -> DSWrite_PreArb_PostArb
        state_dsWrite_next.PreArb := false.B
        state_dsWrite_next.PreArb_PostArb := true.B
      }.otherwise {
        // 1.2. DSWrite_PreArb -> DSWrite_PreArb
      }
    }.elsewhen (fromDS_DSBufWbArbComp && fromDS_DSBufWbComp) {
      // 3. DSWrite_PreArb -> DSWrite_Done
      state_dsWrite_next.PreArb := false.B
      state_dsWrite_next.Done := true.B
    }.elsewhen (fromDS_DSBufWbArbComp) {
      // 4. DSWrite_PreArb -> DSWrite_PostArb
      state_dsWrite_next.PreArb := false.B
      state_dsWrite_next.PostArb := true.B
    }
  }

  when (state_dsWrite.PostArb) {

    /*
    1. 
    */
    when (io.tbuf_wen_last) {
      when (fromDS_DSBufWbComp) {
        // 1. DSWrite_PostArb -> DSWrite_PreArb
        state_dsWrite_next.PostArb := false.B
        state_dsWrite_next.PreArb := true.B
      }.otherwise {
        // 2. DSWrite_PostArb -> DSWrite_PreArb_PostArb
        state_dsWrite_next.PostArb := false.B
        state_dsWrite_next.PreArb_PostArb := true.B
      }
    }.elsewhen (fromDS_DSBufWbComp) {
      // 3. DSWrite_PostArb -> DSWrite_Done
      state_dsWrite_next.PostArb := false.B
      state_dsWrite_next.Done := true.B
    }
  }

  when (state_dsWrite.PreArb_PostArb) {

    /*
    1.
    */
    when (fromDS_DSBufWbArbComp && fromDS_DSBufWbComp) {
      // 1. DSWrite_PreArb_PostArb -> DSWrite_PostArb
      state_dsWrite_next.PreArb_PostArb := false.B
      state_dsWrite_next.PostArb := true.B
    }.elsewhen (fromDS_DSBufWbComp) {
      // 2. DSWrite_PreArb_PostArb -> DSWrite_PreArb
      state_dsWrite_next.PreArb_PostArb := false.B
      state_dsWrite_next.PreArb := true.B
    }.elsewhen (io.tbuf_wen_last) {
      // 3. DSWrite_PreArb_PostArb -> DSWrite_PreArb_PostArb
    }
  }

  when (state_dsWrite.Done) {

    /*
    1.
    */
    when (io.tbuf_wen_last) {
      // . DSWrite_Done -> DSWrite_PreArb
      state_dsWrite_next.Done := false.B
      state_dsWrite_next.PreArb := true.B
    }
  }

  when (io.tshr_dealloc) {
    state_dsWrite_next := DSWriteFSM.init
  }

  io.wb_accept := io.toDS.DSBufWb && fromDS_DSBufWbArbComp
  io.wb_done := state_dsWrite.Done

  assert(PopCount(state_dsWrite.asUInt) <= 1.U, "multiple active state in DSWriteFSM")

  assert(!(fromDS_DSBufWbArbComp && !state_dsWrite.PreArb && !state_dsWrite.PreArb_PostArb),
    "unexpected DSBufWbArbComp from Data Storage on non-PreArb state")
  assert(!(fromDS_DSBufWbComp && !state_dsWrite.PreArb && !state_dsWrite.PostArb && !state_dsWrite.PreArb_PostArb),
    "unexpected DSBufWbComp from Data Storage on non-PreArb/PostArb state")

  assert(!(state_dsWrite.PreArb && state_dsRead.AheadPreArb_S1), "DSWrite.PreArb not exclusive with DSRead.AheadPreArb_S1")
  assert(!(state_dsWrite.PreArb && state_dsRead.AheadPreArb_S2), "DSWrite.PreArb not exclusive with DSRead.AheadPreArb_S2")
  assert(!(state_dsWrite.PreArb_PostArb && state_dsRead.AheadPreArb_S1), "DSWrite.PreArb_PostArb not exclusive with DSRead.AheadPreArb_S1")
  assert(!(state_dsWrite.PreArb_PostArb && state_dsRead.AheadPreArb_S2), "DSWrite.PreArb_PostArb not exclusive with DSRead.AheadPreArb_S2")

  XSPerfAccumulate(s"L2TSHR_${id}_DSWrite_PreArb_cycleCnt", state_dsWrite.PreArb)
  XSPerfAccumulate(s"L2TSHR_${id}_DSWrite_PostArb_cycleCnt", state_dsWrite.PostArb)
  XSPerfAccumulate(s"L2TSHR_${id}_DSWrite_PostArb_PreArb_cycleCnt", state_dsWrite.PreArb_PostArb)
  XSPerfAccumulate(s"L2TSHR_${id}_DSWrite_Done_cycleCnt", state_dsWrite.Done)

  // interactions with Data Storage
  io.toDS.TSHRID := id.U
  // *NOTICE: The AheadPreArb_S1 and AheadPreArb_S2 state should never overlap with any DSWrite states.
  //          It is assumed that no Data could be fast enough to be returned to TSHR Buffer in S0, S1, S2 from L1 and L3,
  //          otherwise consider clear all AheadPreArb on any RXDAT fire.
  io.toDS.WAY := Mux(state_dsRead.AheadPreArb_S1 || state_dsRead.AheadPreArb_S2, ds_read_ahead_way_q, io.meta_way) // TODO: meta assertions on PreArb, PostArb, Done
  io.toDS.SET := L2Address.set(io.tshr_paddr)
  io.toDS.DATA := Cat(io.tbuf_data_2, io.tbuf_data_0)

  io.toDS.DSBufRd := state_dsRead.PreArb
  io.toDS.DSBufAheadRd := state_dsRead.AheadPreArb_S1 || state_dsRead.AheadPreArb_S2
  io.toDS.DSBufWb := (state_dsWrite.PreArb || state_dsWrite.PreArb_PostArb) && !io.wb_locked
}


class L2TSHR(val id: Int)(implicit val p: Parameters) extends Module with HasL2Params {

  val io = IO(new Bundle {

    val toDir = Output(new L2Directory.PathToDirectory)
    val fromDir = Input(new L2Directory.PathFromDirectory)

    val toDS = Output(new L2DataStorage.PathTSHRToDataStorage)
    val fromDS = Output(new L2DataStorage.PathDataStorageToTSHR)

    val toAlloc = Output(new L2TSHRAlloc.PathFromTSHR)
    val fromAlloc = Input(new L2TSHRAlloc.PathToTSHR)

    val RXEVT = Input(new FlitEVT)                    // L1 EVT
    val RXSNP = Input(new CHIBundleSNP)               // HN SNP
    val RXREQ = Input(new FlitREQ)                    // L1/L2 REQ

    val UpRXRSP = Flipped(Valid(new FlitUpRSP))       // RSP from L1
    val UpRXDAT = Flipped(Valid(new FlitUpDAT))       // DAT from L1
    val txSnp = Decoupled(new FlitSNP)                // SNP to L1

    val DnRXRSP = Flipped(Valid(new CHIBundleRSP))    // RSP from HN
    val DnRXDAT = Flipped(Valid(new CHIBundleDAT))    // DAT from HN

    val valid = Output(Bool())
  })

  // TSHR payloads
  val tshr_paddr = Reg(UInt(paramL2.physicalAddrWidth.W))

  when (tshr_alloc) {
    tshr_paddr := io.fromAlloc.paddr
  }
  io.toAlloc.paddr := tshr_paddr


  // TSHR valid
  val tshr_alloc = io.fromAlloc.alloc.asUInt.orR
  val tshr_reuse = io.fromAlloc.reuse.asUInt.orR
  val tshr_enter = tshr_alloc || tshr_reuse

  val tshr_enter_EVT = io.fromAlloc.alloc.EVT || io.fromAlloc.reuse.EVT
  val tshr_enter_SNP = io.fromAlloc.alloc.SNP || io.fromAlloc.reuse.SNP
  val tshr_enter_REQ = io.fromAlloc.alloc.REQ || io.fromAlloc.reuse.REQ

  val tshr_enter_EVT_WayValid_Evict = tshr_enter_EVT && io.RXEVT.WayValid && io.RXEVT.Opcode === Evict.U
  val tshr_enter_EVT_WayValid_WriteBackFull = tshr_enter_EVT && io.RXEVT.WayValid && io.RXEVT.Opcode === WriteBackFull.U

  val tshr_enter_dirRead = !tshr_enter_EVT_WayValid_Evict &&
                           !tshr_enter_EVT_WayValid_WriteBackFull

  val tshr_inactive_rbe = Wire(Bool())
  val tshr_inactive_vpipe = Wire(Bool())
  val tshr_inactive = tshr_inactive_rbe && tshr_inactive_vpipe

  val tshr_inactivate = Wire(Bool()) // TODO: connect with vPipes and RBEs, act as instant 'willFree'

  val tshr_wb_done_dir = Wire(Bool())
  val tshr_wb_done_ds = Wire(Bool())

  val tshr_dealloc = tshr_inactive && tshr_wb_done_dir && tshr_wb_done_ds && !tshr_enter

  val tshr_valid = RegInit(false.B)

  when (tshr_alloc) {
    tshr_valid := true.B
  }

  when (tshr_dealloc) {
    tshr_valid := false.B
  }

  io.valid := tshr_valid


  // miscs and enchantments


  // meta
  val dirResult = Reg(new L2Directory.MetaReadResult)
  val replResult = Reg(new L2Directory.ReplReadResult)

  /* NOTICE: For current design, any partial write to meta would never assert 'meta_valid'.
             Any later read request on full meta line would result in a Directory Read if no any read done yet.
             Because it was extremely rare that partial meta write could be merged into a full meta line.
             And the later result from Directory Read would not override modified fields except 'way' and 'hit' fields,
             which were only possible to be accurate after a Directory Read. */
  val meta = dirResult
  val meta_valid = Wire(Bool())
  val meta_modified = RegInit(L2Directory.MetaWriteMask.empty)
  val tag_modified = RegInit(false.B)

  val repl = replResult

  val meta_write_EVT_meta = Wire(new L2Directory.Meta)
  val meta_write_EVT_mask = Wire(new L2Directory.MetaWriteMask)

  val meta_write_SNP_meta = Wire(new L2Directory.Meta)
  val meta_write_SNP_mask = Wire(new L2Directory.MetaWriteMask)

  val meta_write_REQ_meta = Wire(new L2Directory.Meta)
  val meta_write_REQ_mask = Wire(new L2Directory.MetaWriteMask)
  val tag_write_REQ_mask = Wire(Bool())

  val meta_commit_valid = Wire(Bool())

  when (meta_commit_valid) {
    meta_modified := L2Directory.MetaWriteMask.empty
    tag_modified := false.B
  }

  when (io.fromDir.DirRdResp && io.fromDir.TSHRID === id.U) {
    meta_modified.unmaskAndWrite(meta, io.fromDir.META)
    meta.way := io.fromDir.META.way
    meta.hit := io.fromDir.META.hit
  }
  
  when (io.fromDir.ReplRdResp && io.fromDir.TSHRID === id.U) {
    replResult := io.fromDir.REPL
    meta.way := io.fromDir.REPL.way
  }

  meta_write_EVT_mask.maskAndWrite(meta, meta_modified, meta_write_EVT_meta)
  meta_write_SNP_mask.maskAndWrite(meta, meta_modified, meta_write_SNP_meta)
  meta_write_REQ_mask.maskAndWrite(meta, meta_modified, meta_write_REQ_meta)

  when (tag_write_REQ_mask) {
    tag_modified := true.B
  }

  assert(PopCount(Seq(meta_write_EVT_mask, meta_write_SNP_mask, meta_write_REQ_mask).map(_.asUInt.orR)) <= 1.U,
    s"TSHR #${id} multiple active meta writes on one cycle")
  assert(PopCount(Seq(meta_write_EVT_mask, meta_write_SNP_mask, meta_write_REQ_mask).map(_.state)) <= 1.U, 
    s"TSHR #${id} multiple active write on meta.state")
  assert(PopCount(Seq(meta_write_EVT_mask, meta_write_SNP_mask, meta_write_REQ_mask).map(_.dirty)) <= 1.U, 
    s"TSHR #${id} multiple active write on meta.dirty")
  assert(PopCount(Seq(meta_write_EVT_mask, meta_write_SNP_mask, meta_write_REQ_mask).map(_.clients.asUInt.orR)) <= 1.U, 
    s"TSHR #${id} multiple active write on meta.clients")
  assert(PopCount(Seq(meta_write_EVT_mask, meta_write_SNP_mask, meta_write_REQ_mask).map(_.asUInt.orR)) <= 1.U, 
    s"TSHR #${id} multiple active write on meta")

  assert(!(tshr_dealloc && meta_modified.asUInt.orR), s"TSHR #${id} deallocated with un-committed modified meta")

  
  // TSHR Buffer
  val tshr_buffer_0 = Reg(UInt(256.W))
  val tshr_buffer_2 = Reg(UInt(256.W))

  val tshr_buffer_wen_UpRXDAT_0 = io.UpRXDAT.fire && io.UpRXDAT.bits.DataID === 0.U
  val tshr_buffer_wen_UpRXDAT_2 = io.UpRXDAT.fire && io.UpRXDAT.bits.DataID === 2.U

  val tshr_buffer_wen_DnRXDAT_0 = io.DnRXDAT.fire && io.DnRXDAT.bits.DataID.get === 0.U
  val tshr_buffer_wen_DnRXDAT_2 = io.DnRXDAT.fire && io.DnRXDAT.bits.DataID.get === 2.U

  val tshr_buffer_wen_RXDAT_0 = tshr_buffer_wen_UpRXDAT_0 || tshr_buffer_wen_DnRXDAT_0
  val tshr_buffer_wen_RXDAT_2 = tshr_buffer_wen_UpRXDAT_2 || tshr_buffer_wen_DnRXDAT_2

  val tshr_buffer_commit = WireInit(false.B)

  val tshr_buffer_halfWritten_0_q = RegInit(false.B)
  val tshr_buffer_halfWritten_2_q = RegInit(false.B)

  val tshr_buffer_fullModified_q = RegInit(false.B)
  val tshr_buffer_halfModified = tshr_buffer_halfWritten_0_q || tshr_buffer_halfWritten_2_q
  val tshr_buffer_modified = tshr_buffer_fullModified_q || tshr_buffer_halfModified

  val tshr_buffer_wen_DS = io.fromDS.DSBufRdResp && !tshr_buffer_modified

  val tshr_buffer_wen_last = WireInit(false.B)

  when (tshr_buffer_wen_DS) {
    tshr_buffer_0 := io.fromDS.DATA(0, 255)
    tshr_buffer_2 := io.fromDS.DATA(256, 511)
  }

  assert(!(tshr_dealloc && tshr_buffer_modified), s"TSHR #${id} deallocated with un-committed modified data")

  when (tshr_buffer_wen_UpRXDAT_0) { tshr_buffer_0 := io.UpRXDAT.bits.Data }
  when (tshr_buffer_wen_UpRXDAT_2) { tshr_buffer_2 := io.UpRXDAT.bits.Data }

  when (tshr_buffer_wen_DnRXDAT_0) { tshr_buffer_0 := io.DnRXDAT.bits.Data.get }
  when (tshr_buffer_wen_DnRXDAT_2) { tshr_buffer_2 := io.DnRXDAT.bits.Data.get }

  /*
  tshr_buffer_halfWritten_x_q: Indicating that the TSHR Buffer was partially/halfy written.

  tshr_buffer_fullModified_q: Indicating that there were dirty data in the TSHR Buffer, but not asserted 
                              on TSHR Buffer was partially/halfly written, preventing writing to Data Storage
                              too early.

  tshr_buffer_wen_last: Indicating that the whole line TSHR Buffer is getting ready, and going to be no more
                        partially/halfly written.
  */

  when (tshr_buffer_wen_RXDAT_0) {
    when (tshr_buffer_halfWritten_2_q) {
      tshr_buffer_halfWritten_2_q := false.B
      tshr_buffer_fullModified_q := true.B
      tshr_buffer_wen_last := true.B
    }.otherwise {
      tshr_buffer_halfWritten_0_q := true.B
      tshr_buffer_fullModified_q := false.B // TODO: Add comments here
    }
  }

  when (tshr_buffer_wen_RXDAT_2) {
    when (tshr_buffer_halfWritten_0_q) {
      tshr_buffer_halfWritten_0_q := false.B
      tshr_buffer_fullModified_q := true.B
      tshr_buffer_wen_last := true.B
    }.otherwise {
      tshr_buffer_halfWritten_2_q := true.B
      tshr_buffer_fullModified_q := false.B
    }
  }

  when (tshr_buffer_commit) {
    tshr_buffer_fullModified_q := false.B
  }

  /*
  *NOTICE: Multiple write on a partial/half entry of TSHR Buffer is not supported for now, while this could be
           easy to implement with seperate counters.
           Consider this support on future changes or assertions.
  */
  assert(!(tshr_buffer_halfWritten_0_q && tshr_buffer_wen_RXDAT_0), "double write on buffer DataID 0 from RXDAT")
  assert(!(tshr_buffer_halfWritten_2_q && tshr_buffer_wen_RXDAT_2), "double write on buffer DataID 2 from RXDAT")

  /*
  *NOTICE: Multiple source concurrent write on TSHR Buffer is not supported for now. 
  */
  private val tshr_buffer_halfWritten_UpRXDAT_0_q = RegInit(false.B) // Debug only for now
  private val tshr_buffer_halfWritten_UpRXDAT_2_q = RegInit(false.B) // Debug only for now
  private val tshr_buffer_halfWritten_DnRXDAT_0_q = RegInit(false.B) // Debug only for now
  private val tshr_buffer_halfWritten_DnRXDAT_2_q = RegInit(false.B) // Debug only for now

  when (tshr_buffer_wen_UpRXDAT_0) {
    when (tshr_buffer_halfWritten_UpRXDAT_2_q) {
      tshr_buffer_halfWritten_UpRXDAT_2_q := false.B
    }.otherwise {
      tshr_buffer_halfWritten_UpRXDAT_0_q := true.B
    }
  }
  when (tshr_buffer_wen_UpRXDAT_2) {
    when (tshr_buffer_halfWritten_UpRXDAT_0_q) {
      tshr_buffer_halfWritten_UpRXDAT_0_q := false.B
    }.otherwise {
      tshr_buffer_halfWritten_UpRXDAT_2_q := true.B
    }
  }
  when (tshr_buffer_wen_DnRXDAT_0) {
    when (tshr_buffer_halfWritten_DnRXDAT_2_q) {
      tshr_buffer_halfWritten_DnRXDAT_2_q := false.B
    }.otherwise {
      tshr_buffer_halfWritten_DnRXDAT_0_q := true.B
    }
  }
  when (tshr_buffer_wen_DnRXDAT_2) {
    when (tshr_buffer_halfWritten_DnRXDAT_0_q) {
      tshr_buffer_halfWritten_DnRXDAT_0_q := false.B
    }.otherwise {
      tshr_buffer_halfWritten_DnRXDAT_2_q := true.B
    }
  }

  assert(!(tshr_buffer_wen_UpRXDAT_0 && tshr_buffer_halfWritten_UpRXDAT_0_q), "double write on buffer from UpRXDAT (0, 0)")
  assert(!(tshr_buffer_wen_UpRXDAT_0 && tshr_buffer_halfWritten_DnRXDAT_0_q), "multiple source write on buffer from DnRXDAT then UpRXDAT (0, 0)")
  assert(!(tshr_buffer_wen_UpRXDAT_0 && tshr_buffer_halfWritten_DnRXDAT_2_q), "multiple source write on buffer from DnRXDAT then UpRXDAT (2, 0)")
  assert(!(tshr_buffer_wen_UpRXDAT_2 && tshr_buffer_halfWritten_UpRXDAT_2_q), "double write on buffer from UpRXDAT (2, 2)")
  assert(!(tshr_buffer_wen_UpRXDAT_2 && tshr_buffer_halfWritten_DnRXDAT_0_q), "multiple source write on buffer from DnRXDAT then UpRXDAT (0, 2)")
  assert(!(tshr_buffer_wen_UpRXDAT_2 && tshr_buffer_halfWritten_DnRXDAT_2_q), "multiple source write on buffer from DnRXDAT then UpRXDAT (2, 2)")
  assert(!(tshr_buffer_wen_DnRXDAT_0 && tshr_buffer_halfWritten_DnRXDAT_0_q), "double write on buffer from DnRXDAT (0, 0)")
  assert(!(tshr_buffer_wen_DnRXDAT_0 && tshr_buffer_halfWritten_UpRXDAT_0_q), "multiple source write on buffer from UpRXDAT then DnRXDAT (0, 0)")
  assert(!(tshr_buffer_wen_DnRXDAT_0 && tshr_buffer_halfWritten_UpRXDAT_2_q), "multiple source write on buffer from UpRXDAT then DnRXDAT (2, 0)")
  assert(!(tshr_buffer_wen_DnRXDAT_2 && tshr_buffer_halfWritten_DnRXDAT_2_q), "double write on buffer from DnRXDAT (2, 2)")
  assert(!(tshr_buffer_wen_DnRXDAT_2 && tshr_buffer_halfWritten_UpRXDAT_0_q), "multiple source write on buffer from UpRXDAT then DnRXDAT (0, 2)")
  assert(!(tshr_buffer_wen_DnRXDAT_2 && tshr_buffer_halfWritten_UpRXDAT_2_q), "multiple source write on buffer from UpRXDAT then DnRXDAT (2, 2)")
  

  // RBEs
  val rbeEVT = Module(new L2RBE(new FlitEVT /*TODO: strip PA here*/))
  val rbeSNP = Module(new L2RBE(new CHIBundleSNP /*TODO: strip PA here*/))
  val rbeREQ = Module(new L2RBE(new FlitREQ /*TODO: strip PA here*/))

  io.toAlloc.busy.EVT := !rbeEVT.io.in.ready
  io.toAlloc.busy.SNP := !rbeSNP.io.in.ready
  io.toAlloc.busy.REQ := !rbeREQ.io.in.ready

  rbeEVT.io.in.bits := io.RXEVT
  rbeSNP.io.in.bits := io.RXSNP
  rbeREQ.io.in.bits := io.RXREQ

  rbeEVT.io.in.valid := tshr_enter_EVT
  rbeSNP.io.in.valid := tshr_enter_SNP
  rbeREQ.io.in.valid := tshr_enter_REQ

  rbeEVT.io.directoryReadNeed := !((rbeEVT.io.out.bits.Opcode === Evict.U || rbeEVT.io.out.bits.Opcode === WriteBackFull.U) && rbeEVT.io.out.bits.WayValid)
  rbeSNP.io.directoryReadNeed := true.B
  rbeREQ.io.directoryReadNeed := true.B

  tshr_inactive_rbe := !rbeEVT.io.valid && !rbeSNP.io.valid && !rbeREQ.io.valid


  // Post RBE Data Storage Read Decision
  val ds_read_rbeEVT_en = Wire(Bool())
  val ds_read_rbeSNP_en = Wire(Bool())
  val ds_read_rbeREQ_en = Wire(Bool())

  ds_read_rbeEVT_en := false.B
  
  // TODO


  // vPipes
  val vPipeEVT = Module(new L2VPipeEVT(Seq(/*TODO: client devices*/)))
  val vPipeSNP = Module(new L2VPipeSNP(Seq(/*TODO: client devices*/)))
  val vPipeREQ = Module(new L2VPipeREQ(Seq(/*TODO: client devices*/), id, 0))
  val snoopAgent = Module(new L2SnoopAgent(id))

  snoopAgent.io.uopFromSNP.valid := vPipeSNP.io.toSA.SnpMakeInvalid ||
                                    vPipeSNP.io.toSA.SnpToInvalid ||
                                    vPipeSNP.io.toSA.SnpToShared ||
                                    vPipeSNP.io.toSA.SnpToClean
  snoopAgent.io.uopFromSNP.bits := vPipeSNP.io.toSA
  snoopAgent.io.uopFromREQ.valid := vPipeREQ.io.toSA.SnpMakeInvalid ||
                                    vPipeREQ.io.toSA.SnpToInvalid ||
                                    vPipeREQ.io.toSA.SnpToShared ||
                                    vPipeREQ.io.toSA.SnpToClean
  snoopAgent.io.uopFromREQ.bits := vPipeREQ.io.toSA
  snoopAgent.io.tshr_paddr := tshr_paddr
  snoopAgent.io.tshr_dirResult := meta
  snoopAgent.io.UpRXRSP := io.UpRXRSP
  snoopAgent.io.UpRXDAT := io.UpRXDAT
  io.txSnp <> snoopAgent.io.txSnp
  vPipeSNP.io.fromSA := snoopAgent.io.fromSA
  vPipeREQ.io.fromSA := snoopAgent.io.fromSA

  vPipeEVT.io.tshr_meta_write_en := meta_write_EVT_mask
  vPipeEVT.io.tshr_meta_write_meta := meta_write_EVT_meta
  vPipeSNP.io.tshr_meta_write_en := meta_write_SNP_mask
  vPipeSNP.io.tshr_meta_write_meta := meta_write_SNP_meta
  vPipeREQ.io.tshr_meta_write_en := meta_write_REQ_mask
  vPipeREQ.io.tshr_meta_write_meta := meta_write_REQ_meta
  tag_write_REQ_mask := vPipeEVT.io.tshr_tag_write_en || vPipeREQ.io.tshr_tag_write_en

  // TODO

  // Directory Proxy
  val proxyDir = Module(new L2TSHRDirectoryProxy(id))

  io.toDir := proxyDir.io.toDir
  proxyDir.io.fromDir := io.fromDir

  proxyDir.io.tshr_paddr := tshr_paddr

  proxyDir.io.tshr_alloc := tshr_alloc
  proxyDir.io.tshr_reuse := tshr_reuse
  proxyDir.io.tshr_inactive := tshr_inactive || tshr_inactivate /* immediate inactivate if timing allows */
  proxyDir.io.tshr_dealloc := tshr_dealloc

  proxyDir.io.read_arbed := false.B // TODO: S0 Directory Arbitration from L2TSHRCtrl
  proxyDir.io.read_en := tshr_enter_dirRead
  proxyDir.io.repl_en := false.B // TODO: Replacement Read

  proxyDir.io.meta_modified := meta_modified.asUInt.orR || tag_modified

  meta_commit_valid := proxyDir.io.wb_accept

  meta_valid := proxyDir.io.rd_done
  tshr_wb_done_dir := proxyDir.io.wb_done

  // Data Storage Proxy
  val proxyDS = Module(new L2TSHRDataStorageProxy(id))

  proxyDS.io.fromDir := io.fromDir

  io.toDS := proxyDS.io.toDS
  proxyDS.io.fromDS := io.fromDS

  proxyDS.io.tshr_paddr := tshr_paddr

  proxyDS.io.meta_valid := meta_valid
  proxyDS.io.meta_way := meta.way
  proxyDS.io.meta_state := meta.state

  proxyDS.io.tbuf_wen_last := tshr_buffer_wen_last
  proxyDS.io.tbuf_modified := tshr_buffer_fullModified_q
  proxyDS.io.tbuf_data_0 := tshr_buffer_0
  proxyDS.io.tbuf_data_2 := tshr_buffer_2

  proxyDS.io.tshr_inactivate := tshr_inactivate
  proxyDS.io.tshr_dealloc := tshr_dealloc

  proxyDS.io.ds_read_ahead_en := false.B // TODO: S0 Data Storage Ahead Read from TSHRCtrl
  proxyDS.io.ds_read_ahead_way := 0.U // TODO: S0 Data Storage Ahead Read from TSHRCtrl
  proxyDS.io.ds_read_ahead_arbed := false.B // TODO: S0 Data Storage Ahead Read from TSHRCtrl

  proxyDS.io.ds_read_rbeEVT_en := false.B // TODO: Decode and decide with DirResult in L2TSHR/L2RBEDSRead
  proxyDS.io.ds_read_rbeSNP_en := false.B // TODO: Decode and decide with DirResult in L2TSHR/L2RBEDSRead
  proxyDS.io.ds_read_rbeREQ_en := false.B // TODO: Decode and decide with DirResult in L2TSHR/L2RBEDSRead

  proxyDS.io.ds_read_vPipeEVT_en := false.B // TODO: connect EVT vPipe
  proxyDS.io.ds_read_vPipeSNP_en := false.B // TODO: connect SNP vPipe
  proxyDS.io.ds_read_vPipeREQ_en := false.B // TODO: connect REQ vPipe

  proxyDS.io.ds_read_aux_en := false.B

  tshr_buffer_commit := proxyDS.io.wb_accept

  tshr_wb_done_ds := proxyDS.io.wb_done

  proxyDS.io.RXDAT_fire := io.UpRXDAT.fire || io.DnRXDAT.fire


  // wb-locking refuses Directory Write & Data Storage Write on write-ready state
  // proxyDir.io.wb_locked := vPipeEVT.io.dir_wb_locked || vPipeSNP.io.dir_wb_locked || vPipeREQ.io.dir_wb_locked
  // proxyDS.io.wb_locked := vPipeEVT.io.ds_wb_locked || vPipeSNP.io.ds_wb_locked || vPipeREQ.io.ds_wb_locked


  // interactions between Directory read states and RBEs
  rbeEVT.io.directoryReadDone := proxyDir.io.rd_done
  rbeSNP.io.directoryReadDone := proxyDir.io.rd_done
  rbeREQ.io.directoryReadDone := proxyDir.io.rd_done
}
