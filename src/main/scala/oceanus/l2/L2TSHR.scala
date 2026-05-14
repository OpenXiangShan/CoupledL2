package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.compactchi.CCHIOpcode._
import oceanus.l2._
import oceanus.l2.L2TSHR._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters
import chisel3.simulator.PeekPokeAPI.TestableData
import freechips.rocketchip.util.RotateVector.left

class L2TSHRDirectoryProxy(val id: Int)(implicit val p: Parameters) extends Module with HasL2Params {

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

  val io = IO(new Bundle {

    val toDir = Output(new L2Directory.PathToDirectory)
    val fromDir = Output(new L2Directory.PathFromDirectory)

    val tshr_paddr = Input(UInt(paramL2.physicalAddrWidth.W))

    val tshr_alloc = Input(Bool())
    val tshr_reuse = Input(Bool())
    val tshr_inactive = Input(Bool())
    val tshr_dealloc = Input(Bool())

    val read_arbed = Input(Bool())
    val read_en = Input(Bool())
    val repl_en = Input(Bool())

    val modify = Input(Bool())
    val modified = Input(Bool())

    val read_done = Output(Bool())
    val write_done = Output(Bool())
  })

  val tshr_enter = io.tshr_alloc || io.tshr_reuse

  val fromDir_en = io.fromDir.TSHRADDR === id.U
  val fromDir_DirRdArbComp = fromDir_en && io.fromDir.DirRdArbComp
  val fromDir_DirRdResp = fromDir_en && io.fromDir.DirRdResp
  val fromDir_DirWbArbComp = fromDir_en && io.fromDir.DirWbArbComp

  // Directory read states
  val state_dirRead = RegInit(new DirReadFSM, DirReadFSM.init)

  when (tshr_enter) {
    when (io.read_en && state_dirRead.NotYet) {
      // [] -> DirRead_PreArb
      state_dirRead.PreArb := true.B
    }
    when (io.read_arbed) {
      // [] -> DirRead_PostArb
      state_dirRead.PostArb := true.B
    }
  }

  when (fromDir_DirRdArbComp) {
    // DirRead_PreArb -> DirRead_PostArb
    state_dirRead.PreArb := false.B
    state_dirRead.PostArb := true.B
  }

  when (fromDir_DirRdResp) {
    // DirRead_PostArb -> DirRead_Done
    state_dirRead.PostArb := false.B
    state_dirRead.Done := true.B
  }

  when (io.tshr_dealloc) {
    state_dirRead := DirReadFSM.init
  }

  io.read_done := state_dirRead.Done

  assert(!io.read_en || !io.read_arbed, "asserting both TSHR read and TSHRCtrl read")

  assert(!(fromDir_DirRdArbComp && !state_dirRead.PreArb), "receiving DirRdArbComp on unexpected state (expecting PreArb)")
  assert(!(fromDir_DirRdResp && !state_dirRead.PostArb), "receiving DirRdResp on unexpected state (expecting PostArb)")
  assert(PopCount(state_dirRead.asUInt) <= 1.U, "multiple active states in DirReadFSM")

  // Directory write states
  val state_dirWrite = RegInit(new DirWriteFSM, DirWriteFSM.init)

  when (io.tshr_inactive && state_dirWrite.NotYet) {
    when (io.modified) {
      // [] -> DirWrite_PreArb
      state_dirWrite.PreArb := true.B
    }.otherwise {
      // [] -> DirWrite_Done
      state_dirWrite.Done := true.B
    }
  }

  when (io.tshr_reuse && state_dirWrite.PreArb) {
    // DirWrite_PreArb -> []
    state_dirWrite.PreArb := false.B
  }

  when (fromDir_DirWbArbComp) {
    // DirWrite_PreArb -> DirWrite_Done
    state_dirWrite.PreArb := false.B
    state_dirWrite.Done := true.B
  }

  when (io.modify && state_dirWrite.Done) {
    // DirWrite_Done -> []
    state_dirWrite.Done := false.B
  }

  when (io.tshr_dealloc) {
    state_dirWrite := DirWriteFSM.init
  }

  io.write_done := state_dirWrite.Done

  assert(!(fromDir_DirWbArbComp && !state_dirWrite.PreArb), "receiving DirWbArbComp on unexpected state (expecting PreArb)")
  assert(PopCount(state_dirWrite.asUInt) <= 1.U, "multiple active states in DirWriteFSM")

  // interactions with Directory
  io.toDir.TSHRADDR := id.U
  io.toDir.PADDR := io.tshr_paddr
  io.toDir.META := false.B // TODO: meta
  io.toDir.DirRd := state_dirRead.PreArb
  io.toDir.DirWb := state_dirWrite.PreArb
  io.toDir.ReplRd := io.repl_en

  assert(PopCount(Seq(io.toDir.DirRd, io.toDir.DirWb, io.toDir.ReplRd)) <= 1.U,
    "DirRd, DirWb and ReplRd operations to Directory overlapped")
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

  val io = IO(new Bundle {

    val fromDir = Output(new L2Directory.PathFromDirectory)

    val toDS = Output(new L2DataStorage.PathTSHRToDataStorage)
    val fromDS = Input(new L2DataStorage.PathDataStorageToTSHR)

    val meta_valid = Input(Bool())
    val meta_way = Input(UInt(4.W)) // TODO: parameterize with L2 way count
    val meta_state = Input(L2Directory.MetaState())

    val tshr_dealloc = Input(Bool())

    val ds_read_ahead_en = Input(Bool())
    val ds_read_ahead_arbed = Input(Bool())

    val ds_read_rbeEVT_valid = Input(Bool())
    val ds_read_rbeEVT_mask = Input(new L2DSReadMask)
    val ds_read_rbeEVT_aux = Input(Bool())

    val ds_read_rbeSNP_valid = Input(Bool())
    val ds_read_rbeSNP_mask = Input(new L2DSReadMask)
    val ds_read_rbeSNP_aux = Input(Bool())

    val ds_read_rbeREQ_valid = Input(Bool())
    val ds_read_rbeREQ_mask = Input(new L2DSReadMask)
    val ds_read_rbeREQ_aux = Input(Bool())

    val ds_read_aux_en = Input(Bool()) // aux DS read enable that overrides all other conditions


    val RXDAT_fire = Input(Bool()) // io.UpRXDAT.fire || io.DnRXDAT.fire
  })

  // configuration parameters
  val configS1ReadAhead = true
  val configS2ReadAhead = true

  val configFlowDirRdResp = true
  val configFlowAheadRdResp = true

  val configS0DirResp = true
  val configS1DirResp = true
  val configS2DirResp = true

  //
  val ds_read_rbe_en_metaValid = io.meta_valid && (
    io.ds_read_rbeEVT_valid && io.ds_read_rbeEVT_mask.testDirMeta(io.meta_state) ||
    io.ds_read_rbeSNP_valid && io.ds_read_rbeSNP_mask.testDirMeta(io.meta_state) ||
    io.ds_read_rbeREQ_valid && io.ds_read_rbeREQ_mask.testDirMeta(io.meta_state))

  val ds_read_rbe_en_dirRdResp = if (!configFlowDirRdResp) false.B else io.fromDir.DirRdResp && (
    io.ds_read_rbeEVT_valid && io.ds_read_rbeEVT_mask.testDirMeta(io.fromDir.META.state) ||
    io.ds_read_rbeSNP_valid && io.ds_read_rbeSNP_mask.testDirMeta(io.fromDir.META.state) ||
    io.ds_read_rbeREQ_valid && io.ds_read_rbeREQ_mask.testDirMeta(io.fromDir.META.state))

  val ds_read_rbe_en_aux =
    io.ds_read_rbeEVT_valid && io.ds_read_rbeEVT_aux ||
    io.ds_read_rbeSNP_valid && io.ds_read_rbeSNP_aux ||
    io.ds_read_rbeREQ_valid && io.ds_read_rbeREQ_aux

  val ds_read_rbe_en = ds_read_rbe_en_metaValid || ds_read_rbe_en_dirRdResp || ds_read_rbe_en_aux

  //
  val ds_read_mask_q = RegInit(L2DSReadMask.never)

  ds_read_mask_q := ParallelOR(Seq(ds_read_mask_q,
    Mux(io.ds_read_rbeEVT_valid, io.ds_read_rbeEVT_mask, L2DSReadMask.never),
    Mux(io.ds_read_rbeSNP_valid, io.ds_read_rbeSNP_mask, L2DSReadMask.never),
    Mux(io.ds_read_rbeREQ_valid, io.ds_read_rbeREQ_mask, L2DSReadMask.never)))

  when (io.tshr_dealloc) {
    ds_read_mask_q := L2DSReadMask.never
  }

  val ds_read_tshr_en_metaValid = io.meta_valid && ds_read_mask_q.testDirMeta(io.meta_state)
  val ds_read_tshr_en_dirRdResp = io.fromDir.DirRdResp && ds_read_mask_q.testDirMeta(io.fromDir.META.state)

  //
  val miss_on_fromDS_dirRdResp = io.fromDir.DirRdResp && (io.fromDir.META.state === MetaState.I || io.fromDir.META.way =/= io.fromDS.WAY)
  val miss_on_fromDS_metaValid = io.meta_valid && (io.meta_state === MetaState.I || io.meta_way =/= io.fromDS.WAY)

  val hit_on_fromDS_dirRdResp = io.fromDir.DirRdResp && (io.fromDir.META.state =/= MetaState.I && io.fromDir.META.way === io.fromDS.WAY)
  val hit_on_fromDS_metaValid = io.meta_valid && (io.meta_state =/= MetaState.I && io.meta_way === io.fromDS.WAY)

  val ds_ahead_read_way_q = RegInit(0.U(4.W)) // TODO: parameterize with L2 way count

  when (io.toDS.DSBufAheadRd && io.fromDS.DSBufAheadRdArbComp) {
    ds_ahead_read_way_q := io.toDS.WAY
  }

  val miss_after_fromDS_dirRdResp = io.fromDir.DirRdResp && (io.fromDir.META.state === MetaState.I || io.fromDir.META.way =/= ds_ahead_read_way_q)
  val miss_after_fromDS_metaValid = io.meta_valid && (io.meta_state === MetaState.I || io.meta_way =/= ds_ahead_read_way_q)

  val hit_after_fromDS_dirRdResp = io.fromDir.DirRdResp && (io.fromDir.META.state =/= MetaState.I && io.fromDir.META.way === ds_ahead_read_way_q)
  val hit_after_fromDS_metaValid = io.meta_valid && (io.meta_state =/= MetaState.I && io.meta_way === ds_ahead_read_way_q)
  

  // Data Storage read states
  val state_dsRead = RegInit(new DSReadFSM, DSReadFSM.init)
  val state_dsRead_next = WireInit(state_dsRead)

  state_dsRead := state_dsRead_next

  when (state_dsRead.NotYet) {

    /*
    1.  
    */
    when (ds_read_rbe_en || io.ds_read_aux_en) {
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
    when (io.fromDS.DSBufRdArbComp) {
      // 1. DSRead_PreArb -> DSRead_PostArb
      state_dsRead_next.PreArb := false.B
      state_dsRead_next.PostArb := true.B
    }
  }

  when (state_dsRead.PostArb) {
    
    /*
    1.
    */
    when (io.fromDS.DSBufRdResp) {
      // 1. DSRead_PostArb -> DSRead_Done
      state_dsRead_next.PostArb := false.B
      state_dsRead_next.Done := true.B
    }
  }

  when (state_dsRead.AheadPreArb_S1) {
    
    /*
    1. 
    */
    whenOpt ()(io.fromDS.DSBufAheadRdArbComp) {
      // 1. DSRead_AheadPreArb_S1 -> DSRead_AheadPostArb
      state_dsRead_next.AheadPreArb_S1 := false.B
      state_dsRead_next.AheadPostArb := true.B
    }.elsewhen (ds_read_rbe_en_aux || io.ds_read_aux_en) {
      // 2.1. DSRead_AheadPreArb_S1 -> DSRead_PreArb
      state_dsRead_next.AheadPreArb_S1 := false.B
      state_dsRead_next.PreArb := true.B
    }.elsewhenOpt (configS0DirResp)(io.meta_valid) {
      when (ds_read_rbe_en_metaValid || ds_read_tshr_en_metaValid) {
        // 2.2. DSRead_AheadPreArb_S1 -> DSRead_PreArb
        state_dsRead_next.AheadPreArb_S1 := false.B
        state_dsRead_next.PreArb := true.B
      }.otherwise {
        // . DSRead_AheadPreArb_S1 -> []
        state_dsRead_next.AheadPreArb_S1 := false.B
      }
    }.elsewhenOpt (configS1DirResp)(io.fromDir.DirRdResp) {
      when (ds_read_rbe_en_dirRdResp || ds_read_tshr_en_dirRdResp) {
        // 2.3. DSRead_AheadPreArb_S1 -> DSRead_PreArb
        state_dsRead_next.AheadPreArb_S1 := false.B
        state_dsRead_next.PreArb := true.B
      }.otherwise {
        // . DSRead_AheadPreArb_S1 -> []
        state_dsRead_next.AheadPreArb_S1 := false.B
      }
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
    whenOpt ()(io.fromDS.DSBufAheadRdArbComp) {
      // 1. DSRead_AheadPreArb_S2 -> DSRead_AheadPostArb
      state_dsRead_next.AheadPreArb_S2 := false.B
      state_dsRead_next.AheadPostArb := true.B
    }.elsewhen (ds_read_rbe_en_aux || io.ds_read_aux_en) {
      // 2.3. DSRead_AheadPreArb_S2 -> DSRead_PreArb
      state_dsRead_next.AheadPreArb_S2 := false.B
      state_dsRead_next.PreArb := true.B
    }.elsewhenOpt (configS1DirResp)(io.meta_valid) {
      when (ds_read_rbe_en_metaValid || ds_read_tshr_en_metaValid) {
        // 2.1. DSRead_AheadPreArb_S2 -> DSRead_PreArb
        state_dsRead_next.AheadPreArb_S2 := false.B
        state_dsRead_next.PreArb := true.B
      }.otherwise {
        // . DSRead_AheadPreArb_S2 -> []
        state_dsRead_next.AheadPreArb_S2 := false.B
      }
    }.elsewhenOpt (configS2DirResp)(io.fromDir.DirRdResp) {
      when (ds_read_rbe_en_dirRdResp || ds_read_tshr_en_dirRdResp) {
        // 2.2. DSRead_AheadPreArb_S2 -> DSRead_PreArb
        state_dsRead_next.AheadPreArb_S2 := false.B
        state_dsRead_next.PreArb := true.B
      }.otherwise {
        // . DSRead_AheadPreArb_S2 -> []
        state_dsRead_next.AheadPreArb_S2 := false.B
      }
    }.otherwise {
      // . DSRead_AheadPreArb_S2 -> []
      state_dsRead_next.AheadPreArb_S2 := false.B
    }
  }

  when (state_dsRead.AheadPostArb) {

    /*
    1. 
    */
    when (io.fromDS.DSBufAheadRdResp) {
      whenOpt (configFlowAheadRdResp)(hit_on_fromDS_metaValid) {
        // 1.1. DSRead_AheadPostArb -> DSRead_Done
        state_dsRead_next.AheadPostArb := false.B
        state_dsRead_next.Done := true.B
      }.elsewhenOpt (configFlowAheadRdResp && configFlowDirRdResp)(hit_on_fromDS_dirRdResp) {
        // 1.2. DSRead_AheadPostArb -> DSRead_Done
        state_dsRead_next.AheadPostArb := false.B
        state_dsRead_next.Done := true.B
      }.elsewhenOpt (configFlowAheadRdResp)(miss_on_fromDS_metaValid) {
        when (ds_read_rbe_en_aux || ds_read_rbe_en_metaValid || ds_read_tshr_en_metaValid) {
          // 2.1. DSRead_AheadPostArb -> DSRead_PreArb
          state_dsRead_next.AheadPostArb := false.B
          state_dsRead_next.PreArb := true.B
        }.otherwise {
          // 3.1. DSRead_AheadPostArb -> []
          state_dsRead_next.AheadPostArb := false.B
        }
      }.elsewhenOpt (configFlowAheadRdResp && configFlowDirRdResp)(miss_on_fromDS_dirRdResp) {
        when (ds_read_rbe_en_aux || ds_read_rbe_en_dirRdResp || ds_read_tshr_en_dirRdResp) {
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
      when (ds_read_rbe_en_aux || ds_read_rbe_en_metaValid || ds_read_tshr_en_metaValid) {
        // 2.1. DSRead_AheadDone -> DSRead_PreArb
        state_dsRead_next.AheadDone := false.B
        state_dsRead_next.PreArb := true.B
      }.otherwise {
        // 3.1. DSRead_AheadDone -> []
        state_dsRead_next.AheadDone := false.B
      }
    }.elsewhenOpt (configFlowDirRdResp)(miss_after_fromDS_dirRdResp) {
      when (ds_read_rbe_en_aux || ds_read_rbe_en_dirRdResp || ds_read_tshr_en_dirRdResp) {
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

  assert(PopCount(state_dsRead.asUInt) <= 1.U, "multiple active state in DSReadFSM")

  assert(!(io.fromDS.DSBufAheadRdArbComp && !state_dsRead.AheadPreArb_S1 && !state_dsRead.AheadPreArb_S2),
    "unexpected DSBufAheadRdArbComp from Data Storage on non-AheadPreArb state")
  assert(!(io.fromDS.DSBufAheadRdResp && !state_dsRead.AheadPostArb),
    "unexpected DSBufAheadRdResp from Data Storage on non-AheadPostArb state")
  assert(!(io.fromDS.DSBufRdArbComp && !state_dsRead.PreArb),
    "unexpected DSBufRdArbComp from Data Storage on non-PreArb state")
  assert(!(io.fromDS.DSBufRdResp && !state_dsRead.PostArb),
    "unexpected DSBufRdResp from Data Storage on non-PostArb state")

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
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S1_dirRdResp_read", io.fromDir.DirRdResp && state_dsRead.AheadPreArb_S1 && state_dsRead_next.PreArb)
  XSPerfAccumulate(s"L2TSHR_DSReadFSM_AheadPreArb_S1_dirRdResp_noRead", io.fromDir.DirRdResp && state_dsRead.AheadPreArb_S1 && state_dsRead_next.NotYet)
  

  // TODO

  // Data Storage write states
  /*
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

      tshr_buffer_modified_0_q := false.B
      tshr_buffer_modified_2_q := false.B
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
  */


  // TODO
}

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


  // miscs and enchantments
  val req_need_dirRead = Wire(Bool())

  val ds_resp_miss = Wire(Bool()) // TODO: connect with Data Storage interactions, valid only when DSBufResp

  val ds_read_ahead_en = Wire(Bool()) // TODO: Data Storage Read Ahead valid on TSHR alloc/reuse
  val ds_read_ahead_way = RegInit(0.U(4.W)) // TODO: configure with actual way index
  val ds_read_ahead_q = RegInit(false.B) // TODO: Data Storage Read Ahead flag bit

  val ds_read_rbe_en = Wire(Bool()) // TODO: Data Storage Read on requests passed RBE with valid meta

  val ds_read_skip_en = Wire(Bool()) // TODO: connect with TSHR Buffer write except from DS
  val ds_read_skip_q = RegInit(false.B)


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

  val tshr_buffer_wen_UpRXDAT_0 = io.UpRXDAT.fire && io.UpRXDAT.bits.DataID === 0.U
  val tshr_buffer_wen_UpRXDAT_2 = io.UpRXDAT.fire && io.UpRXDAT.bits.DataID === 2.U

  val tshr_buffer_wen_DnRXDAT_0 = io.DnRXDAT.fire && io.DnRXDAT.bits.DataID.get === 0.U
  val tshr_buffer_wen_DnRXDAT_2 = io.DnRXDAT.fire && io.DnRXDAT.bits.DataID.get === 2.U

  val tshr_buffer_wen_RXDAT_0 = tshr_buffer_wen_UpRXDAT_0 || tshr_buffer_wen_DnRXDAT_0
  val tshr_buffer_wen_RXDAT_2 = tshr_buffer_wen_UpRXDAT_2 || tshr_buffer_wen_DnRXDAT_2

  val tshr_buffer_modified_0_q = RegInit(false.B)
  val tshr_buffer_modified_2_q = RegInit(false.B)

  val tshr_buffer_modified = tshr_buffer_modified_0_q || tshr_buffer_modified_2_q

  val tshr_buffer_wen_DS = io.fromDS.DSBufRdResp && !tshr_buffer_modified

  val tshr_buffer_halfWritten_0_q = RegInit(false.B)
  val tshr_buffer_halfWritten_2_q = RegInit(false.B)

  val tshr_buffer_wen_last = WireInit(false.B)

  when (tshr_buffer_wen_DS) {
    tshr_buffer_0 := io.fromDS.DATA(0, 255)
    tshr_buffer_2 := io.fromDS.DATA(256, 511)
  }

  when (tshr_buffer_wen_UpRXDAT_0) { tshr_buffer_0 := io.UpRXDAT.bits.Data }
  when (tshr_buffer_wen_UpRXDAT_2) { tshr_buffer_2 := io.UpRXDAT.bits.Data }

  when (tshr_buffer_wen_DnRXDAT_0) { tshr_buffer_0 := io.DnRXDAT.bits.Data.get }
  when (tshr_buffer_wen_DnRXDAT_2) { tshr_buffer_2 := io.DnRXDAT.bits.Data.get }

  when (tshr_buffer_wen_RXDAT_0) {
    when (tshr_buffer_halfWritten_2_q) {
      tshr_buffer_halfWritten_2_q := false.B
      tshr_buffer_wen_last := true.B
    }.otherwise {
      tshr_buffer_halfWritten_0_q := true.B
    }
  }

  when (tshr_buffer_wen_RXDAT_2) {
    when (tshr_buffer_halfWritten_0_q) {
      tshr_buffer_halfWritten_0_q := false.B
      tshr_buffer_wen_last := true.B
    }.otherwise {
      tshr_buffer_halfWritten_2_q := true.B
    }
  }

  assert(!(tshr_buffer_halfWritten_0_q && tshr_buffer_wen_RXDAT_0), "double write on buffer DataID 0 from RXDAT")
  assert(!(tshr_buffer_halfWritten_2_q && tshr_buffer_wen_RXDAT_2), "double write on buffer DataID 2 from RXDAT")

  
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

  rbeEVT.io.directoryReadNeed := !(rbeEVT.io.out.bits.Opcode === EvictBack.U && rbeEVT.io.out.bits.WayValid)
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

  // TODO: Delete these FSMs here, these will be all moved to *Proxy module.
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
    when (io.fromDS.DSBufRdResp) {
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

      tshr_buffer_modified_0_q := false.B
      tshr_buffer_modified_2_q := false.B
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
