package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.compactchi.CCHIOpcode._
import oceanus.l2._
import oceanus.l2.L2Common._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.RotateVector.left
import freechips.rocketchip.util.SeqToAugmentedSeq
import oceanus.l2.tshr.L2TSHRDirectoryProxy


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

    val TXREQ = Decoupled(new CHIBundleREQ)           // HN REQ

    val TXSNP = Decoupled(new FlitSNP)                // SNP to L1

    val UpRXRSP = Flipped(Valid(new FlitUpRSP))       // RSP from L1
    val UpRXDAT = Flipped(Valid(new FlitUpDAT))       // DAT from L1
    val UpTXRSP = Decoupled(new FlitDnRSP)            // RSP to L1
    val UpTXDAT = Decoupled(new FlitDnDAT)            // DAT to L1

    val DnRXRSP = Flipped(Valid(new CHIBundleRSP))    // RSP from HN
    val DnRXDAT = Flipped(Valid(new CHIBundleDAT))    // DAT from HN
    val DnTXRSP = Decoupled(new CHIBundleRSP)         // RSP to HN
    val DnTXDAT = Decoupled(new CHIBundleDAT)         // DAT to HN

    val toPCreditPool = Valid(new L2PCreditPool.Entry)
    val fromPCreditPool = Input(Bool())

    val toClientTableREQ = Output(UInt(8.W)) // TODO: configurable with upstream nodeId width
    val fromClientTableREQ = Input(Vec(1, Bool())) // TODO: parameterize with coherent l2 client count

    val toClientTableEVT = Output(UInt(8.W)) // TODO: configurable with upstream nodeId width
    val fromClientTableEVT = Input(Vec(1, Bool())) // TODO: parameterize with coherent l2 client count

    val peer_unlock_dir = Output(Vec(paramL2.mshrSize, Bool()))
    val peer_unlock_ds = Output(Vec(paramL2.mshrSize, Bool()))

    val self_unlock_dir = Input(Bool())
    val self_unlock_ds = Input(Bool())

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
  val rbeREQ = Module(new L2RBE(new FlitREQStripped))

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


  // -- vPipes and TSHR local modules
  val vPipeEVT = Module(new L2VPipeEVT(Seq(/*TODO: client devices*/)))
  val vPipeSNP = Module(new L2VPipeSNP(Seq(/*TODO: client devices*/)))
  val vPipeREQ = Module(new L2VPipeREQ(Seq(/*TODO: client devices*/), id, 0))
  val snoopAgent = Module(new L2SnoopAgent(id))

  tshr_inactive_vpipe := vPipeEVT.io.free && vPipeSNP.io.free && vPipeREQ.io.free

  // connections between SNP vPipe and Snoop Agent
  snoopAgent.io.uopFromSNP.valid := vPipeSNP.io.toSA.SnpMakeInvalid ||
                                    vPipeSNP.io.toSA.SnpToInvalid ||
                                    vPipeSNP.io.toSA.SnpToShared ||
                                    vPipeSNP.io.toSA.SnpToClean
  snoopAgent.io.uopFromSNP.bits := vPipeSNP.io.toSA
  vPipeSNP.io.fromSA := snoopAgent.io.fromSA

  // connections between REQ vPipe and Snoop Agent
  snoopAgent.io.uopFromREQ.valid := vPipeREQ.io.toSA.SnpMakeInvalid ||
                                    vPipeREQ.io.toSA.SnpToInvalid ||
                                    vPipeREQ.io.toSA.SnpToShared ||
                                    vPipeREQ.io.toSA.SnpToClean
  snoopAgent.io.uopFromREQ.bits := vPipeREQ.io.toSA
  vPipeREQ.io.fromSA := snoopAgent.io.fromSA

  // connections between TSHR local / RX and Snoop Agent
  snoopAgent.io.tshr_paddr := tshr_paddr
  snoopAgent.io.tshr_dirResult := meta
  snoopAgent.io.UpRXRSP := io.UpRXRSP
  snoopAgent.io.UpRXDAT := io.UpRXDAT

  // connections between RBEs / RX and EVT vPipe
  rbeEVT.io.blockFromVPipe.EVT := vPipeEVT.io.blockRBE.EVT
  rbeSNP.io.blockFromVPipe.EVT := vPipeEVT.io.blockRBE.SNP
  rbeREQ.io.blockFromVPipe.EVT := vPipeEVT.io.blockRBE.REQ

  vPipeEVT.io.UpRXEVT := rbeEVT.io.out

  vPipeEVT.io.UpRXDAT := io.UpRXDAT

  // connections between RBEs / RX and SNP vPipe
  rbeEVT.io.blockFromVPipe.SNP := vPipeSNP.io.blockRBE.EVT
  rbeSNP.io.blockFromVPipe.SNP := vPipeSNP.io.blockRBE.SNP
  rbeREQ.io.blockFromVPipe.SNP := vPipeSNP.io.blockRBE.REQ

  vPipeSNP.io.DnRXSNP := rbeSNP.io.out

  // connections between RBEs / RX and REQ vPipe
  rbeEVT.io.blockFromVPipe.REQ := vPipeREQ.io.blockRBE.EVT
  rbeSNP.io.blockFromVPipe.REQ := vPipeREQ.io.blockRBE.SNP
  rbeREQ.io.blockFromVPipe.REQ := vPipeREQ.io.blockRBE.REQ

  vPipeREQ.io.UpRXREQ := rbeREQ.io.out

  vPipeREQ.io.DnRXRSP := io.DnRXRSP
  vPipeREQ.io.DnRXDAT := io.DnRXDAT

  vPipeREQ.io.UpRXRSP := io.UpRXRSP
  vPipeREQ.io.UpRXDAT := io.UpRXDAT

  // connections between TSHR local and EVT vPipe
  vPipeEVT.io.tshr_paddr := tshr_paddr
  vPipeEVT.io.tshr_dirResult := dirResult

  meta_write_EVT_mask := vPipeEVT.io.tshr_meta_write_en
  meta_write_EVT_meta := vPipeEVT.io.tshr_meta_write_meta

  io.toClientTableEVT := 0.U // connect this when supports multiple coherent upstreams

  // connections between TSHR local and SNP vPipe
  vPipeSNP.io.tshr_paddr := tshr_paddr
  vPipeSNP.io.tshr_dirResult := dirResult

  meta_write_SNP_mask := vPipeSNP.io.tshr_meta_write_en
  meta_write_SNP_meta := vPipeSNP.io.tshr_meta_write_meta

  vPipeSNP.io.EVT_active := vPipeEVT.io.EVT_active
  vPipeSNP.io.REQ_evict := false.B // TODO: connect with REQ vPipe

  // connections between TSHR local and REQ vPipe
  vPipeREQ.io.tshr_paddr := tshr_paddr
  vPipeREQ.io.tshr_dirResult := dirResult

  meta_write_REQ_mask := vPipeREQ.io.tshr_meta_write_en
  meta_write_REQ_meta := vPipeREQ.io.tshr_meta_write_meta
  tag_write_REQ_mask := vPipeREQ.io.tshr_tag_write_en

  io.toPCreditPool := vPipeREQ.io.toPCreditPool
  vPipeREQ.io.fromPCreditPool := io.fromPCreditPool

  io.toClientTableREQ := vPipeREQ.io.toClientTable
  vPipeREQ.io.fromClientTable := io.fromClientTableREQ

  io.peer_unlock_dir := vPipeREQ.io.peer_unlock_dir
  io.peer_unlock_ds := vPipeREQ.io.peer_unlock_ds

  vPipeREQ.io.self_unlock_dir := io.self_unlock_dir
  vPipeREQ.io.self_unlock_ds := io.self_unlock_ds

  vPipeREQ.io.L1EVT_active := vPipeEVT.io.EVT_active

  // connections between TX channels and TSHR local modules
  io.TXREQ := vPipeREQ.io.DnTXREQ

  io.TXSNP <> snoopAgent.io.txSnp

  fastArb(Seq(vPipeEVT.io.UpTXRSP, vPipeREQ.io.UpTXRSP), io.UpTXRSP, Some("UpTXRSP"))

  io.UpTXDAT <> vPipeREQ.io.UpTXDAT
  io.UpTXDAT.bits.Data := Mux(io.UpTXDAT.bits.DataID === 0.U, tshr_buffer_0, tshr_buffer_2)

  fastArb(Seq(vPipeSNP.io.DnTXRSP, vPipeREQ.io.DnTXRSP), io.DnTXRSP, Some("DnTXRSP"))

  fastArb(Seq(vPipeSNP.io.DnTXDAT, vPipeREQ.io.DnTXDAT), io.DnTXDAT, Some("DnTXDAT"))
  io.DnTXDAT.bits.Data.get := Mux(io.DnTXDAT.bits.DataID.get === 0.U, tshr_buffer_0, tshr_buffer_2)

  // ----------------------------------------------------------------

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
  proxyDir.io.repl_en := vPipeREQ.io.repl_en

  proxyDir.io.meta_modified := meta_modified.asUInt.orR || tag_modified

  vPipeREQ.io.dir_wb_aux // TODO: wb_aux

  rbeEVT.io.directoryReadDone := proxyDir.io.rd_done
  rbeSNP.io.directoryReadDone := proxyDir.io.rd_done
  rbeREQ.io.directoryReadDone := proxyDir.io.rd_done

  vPipeREQ.io.repl_retry := proxyDir.io.repl_retry
  vPipeREQ.io.repl_done := proxyDir.io.repl_done
  vPipeREQ.io.repl_resp := replResult

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
  proxyDS.io.ds_read_vPipeSNP_en := vPipeSNP.io.ds_read_en
  proxyDS.io.ds_read_vPipeREQ_en := vPipeREQ.io.ds_rd_en
  proxyDS.io.ds_read_aux_en := false.B

  proxyDS.io.ds_read_EVT_cancel := false.B
  proxyDS.io.ds_read_SNP_cancel := false.B
  proxyDS.io.ds_read_REQ_cancel := vPipeREQ.io.ds_rd_cancel

  vPipeSNP.io.ds_read_done := proxyDS.io.rd_done
  vPipeREQ.io.ds_rd_done := proxyDS.io.rd_done

  tshr_buffer_commit := proxyDS.io.wb_accept

  tshr_wb_done_ds := proxyDS.io.wb_done

  proxyDS.io.RXDAT_fire := io.UpRXDAT.fire || io.DnRXDAT.fire


  // wb-locking refuses Directory Write & Data Storage Write on write-ready state
  // TODO: check EVT wb lock / clear
  proxyDir.io.wb_locked := /*vPipeEVT.io.dir_wb_locked || vPipeSNP.io.dir_wb_locked ||*/ vPipeREQ.io.dir_wb_locked
  proxyDS.io.wb_locked := /*vPipeEVT.io.ds_wb_locked || vPipeSNP.io.ds_wb_locked ||*/ vPipeREQ.io.ds_wb_locked

}
