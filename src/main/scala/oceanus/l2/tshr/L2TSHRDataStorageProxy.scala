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
