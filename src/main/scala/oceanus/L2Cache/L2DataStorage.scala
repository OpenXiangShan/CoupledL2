class DataStorage(implicit p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle() {

    val fromTSHR = Flipped(Vec(paramL2.nMSHR, new L2DataStorage.PathTSHRToDataStorage))
    val toTSHR   = Vec(paramL2.nMSHR, new L2DataStorage.PathDataStorageToTSHR)
    val error    = Output(Bool())
  })

  // ── internal arbiter ────────────────────────────────────────────
  val arbiter = Module(new DSArbiter)
  arbiter.io.fromTSHR <> io.fromTSHR
  io.toTSHR <> arbiter.io.toTSHR

  // ── physical SRAM (MCP2) ──────────────────────────────────────────────
  val array = Module(new GatedSplittedSRAM(
    gen           = new DSECCBankBlock,
    set           = paramL2.blocks,
    way           = 1,
    dataSplit     = paramL2.dataSRAMSplit,
    singlePort    = true,
    readMCP2      = true,
    hasMbist      = paramL2.hasMbist,
    hasSramCtl    = paramL2.hasSramCtl,
    extraHold     = true,
    withClockGate = true
  ))
  array.io_en := arbiter.io.dsEn        // io.en drivenby  arbite
  private val mbistPl =
    MbistPipeline.PlaceMbistPipeline(1, "L2DataStorage", paramL2.hasMbist)

  // ── 用 arbiter 输出替换原 io.req / io.wdata ───────────────────────
  val req   = arbiter.io.dsReq          // 原 io.req
  val wdata = arbiter.io.dsWdata        // 原 io.wdata

  val arrayIdx = Cat(req.bits.way, req.bits.set)
  val wen = req.valid &&  req.bits.wen
  val ren = req.valid && !req.bits.wen

  // ── Write path (with optional ECC encode) ─────────────────────────
  val arrayWrite = Wire(new DSECCBankBlock)
  val arrayWriteData = if (paramL2.enableDataECC) {
    Cat(
      0.U(paramL2.encDataPadBits.W),
      Cat(VecInit(Seq.tabulate(paramL2.dataBankSplit)(i =>
        wdata.data(paramL2.dataBankBits * (i + 1) - 1, paramL2.dataBankBits * i)
      )).map(data => cacheParams.dataCode.encode(data)))
    )
  } else {
    wdata.data
  }
  arrayWrite.data := arrayWriteData

  // ── Read path (with optional ECC strip) ───────────────────────────
  val arrayRead = array.io.r.resp.data(0)
  val dataRead  = Wire(new DSBlock)
  val bankDataRead = if (paramL2.enableDataECC) {
    Cat(VecInit(Seq.tabulate(paramL2.dataBankSplit)(i =>
      arrayRead.data(paramL2.encBankBits * (i + 1) - 1, paramL2.encBankBits * i)(paramL2.dataBankBits - 1, 0)
    )))
  } else {
    arrayRead.data
  }
  dataRead.data := bankDataRead

  array.io.w.apply(wen, arrayWrite, arrayIdx, 1.U)
  array.io.r.apply(ren, arrayIdx)

  // ── ECC error (valid 2 cycles after the read request) ─────────────
  val error = if (paramL2.enableDataECC) {
    VecInit(Seq.tabulate(paramL2.dataBankSplit)(i =>
      arrayRead.data(paramL2.encBankBits * (i + 1) - 1, paramL2.encBankBits * i)
    )).map(data => cacheParams.dataCode.decode(data).error).reduce(_ | _) &&
      RegNext(RegNext(req.valid && !req.bits.wen))
  } else {
    false.B
  }

  // ── 读数据回灌 arbiter (N+2) ──────────────────────────────────────
  arbiter.io.dsRdata := dataRead

  io.error := error

  // ══════════════════════════════════════════════════════════════════
  // MCP2 timing-contract assertions
  // ══════════════════════════════════════════════════════════════════

  assert(!arbiter.io.dsEn || !RegNext(arbiter.io.dsEn, false.B),
    "DataStorage: continuous SRAM req prohibited under MCP2 (en high two cycles)")

  assert(!(RegNext(arbiter.io.dsEn) && (req.asUInt =/= RegNext(req.asUInt))),
    "DataStorage: req fails to hold for 2 cycles")

  assert(!(RegNext(arbiter.io.dsEn && req.bits.wen) && (wdata.asUInt =/= RegNext(wdata.asUInt))),
    "DataStorage: wdata fails to hold for 2 cycles")
}
