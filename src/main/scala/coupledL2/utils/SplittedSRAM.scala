package coupledL2.utils

import chisel3._
import chisel3.util._

// split SRAM by set/way/data
// 1. use lower-bits of set to select bank
// 2. split ways and parallel access
// 3. split data and parallel access
// * a simple graph is shown below

class SplittedSRAM[T <: Data](
  gen: T,
  set: Int,
  way: Int,
  setSplit: Int = 1,
  waySplit: Int = 1,
  dataSplit: Int = 1,
  shouldReset: Boolean = false,
  holdRead: Boolean = false,
  singlePort: Boolean = true,
  bypassWrite: Boolean = false,
  clkDivBy2: Boolean = false,
  readMCP2: Boolean = true
) extends Module {
  val io = IO(new Bundle() {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  require(set % setSplit == 0, "sets must be divisible by setSplit")
  val innerSets = set / setSplit
  val bankBits = log2Ceil(setSplit)
  val innerSetBits = log2Up(set) - bankBits
  val r_setIdx = io.r.req.bits.setIdx.head(innerSetBits)
  val r_bankSel = if (setSplit == 1) 0.U else io.r.req.bits.setIdx(bankBits - 1, 0)
  val w_setIdx = io.w.req.bits.setIdx.head(innerSetBits)
  val w_bankSel = if (setSplit == 1) 0.U else io.w.req.bits.setIdx(bankBits - 1, 0)

  require(way % waySplit == 0, "ways must be divisible by waySplit")
  val innerWays = way / waySplit

  require(gen.getWidth % dataSplit == 0, "data width must be divisible by dataSplit")
  val innerWidth = gen.getWidth / dataSplit

  val array = Seq.fill(setSplit)(Seq.fill(waySplit)(Seq.fill(dataSplit)(
    Module(new SRAMTemplate(
      UInt(innerWidth.W),
      innerSets,
      innerWays,
      shouldReset = shouldReset,
      holdRead = holdRead,
      singlePort = singlePort,
      bypassWrite = bypassWrite,
      clkDivBy2 = clkDivBy2,
      readMCP2 = readMCP2
    ))
  )))

  for (i <- 0 until setSplit) {
    val ren = i.U === r_bankSel
    val wen = i.U === w_bankSel

    for (j <- 0 until waySplit) {
      val waymask = if (way > 1) io.w.req.bits.waymask.get(innerWays * (j + 1) - 1, innerWays * j) else 1.U
      // if waymask-part is 0.U, we need not set array(i)(j)(_).io.w.req
      // TODO: consider whether to add this, which may lower power consumption, but will add burden to timing
      // val needWrite = waymask.orR

      for (k <- 0 until dataSplit) {
        array(i)(j)(k).io.r.req.valid := io.r.req.valid && ren
        array(i)(j)(k).io.r.req.bits.apply(r_setIdx)
        array(i)(j)(k).io.w.req.valid := io.w.req.valid && wen // && needWrite
        array(i)(j)(k).io.w.req.bits.apply(
          VecInit(io.w.req.bits.data.slice(innerWays * j, innerWays * (j + 1)).map(_.asUInt(
            innerWidth * (k + 1) - 1,
            innerWidth * k
          ))),
          w_setIdx,
          waymask
        )
      }
    }
  }

  val ren_vec_0 = VecInit((0 until setSplit).map(i => i.U === r_bankSel))
  val ren_vec_1 = RegNext(ren_vec_0, 0.U.asTypeOf(ren_vec_0))
  val ren_vec = if (clkDivBy2) {
    RegNext(ren_vec_1, 0.U.asTypeOf(ren_vec_0))
  } else ren_vec_1

  // only one read/write
  assert({ PopCount(ren_vec) <= 1.U })

  // TODO: we should consider the readys of all sram to be accessed, and orR them
  // but since waySplitted and dataSplitted smaller srams should have the same behavior
  // we just use one of them for ready, for better timing
  io.r.req.ready := VecInit((0 until setSplit).map(i => array(i).head.head.io.r.req.ready))(r_bankSel)
  io.w.req.ready := VecInit((0 until setSplit).map(i => array(i).head.head.io.w.req.ready))(w_bankSel)

  // * an example of "setSplit 2, waySplit 2, dataSplit 4" of an SRAM with way 2 *
  // =========================================================================================
  //                               / way 0  -- [data 3] | [data 2] | [data 1] | [data 0]
  //  set[0] == 0.U -> waySplit 0 |- way 1  -- [data 3] | [data 2] | [data 1] | [data 0]
  // -----------------------------------------------------------------------------------------
  //                   waySplit 1 |- way 0  -- [data 3] | [data 2] | [data 1] | [data 0]
  //                               \ way 1  -- [data 3] | [data 2] | [data 1] | [data 0]
  // =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  //                               / way 0  -- [data 3] | [data 2] | [data 1] | [data 0]
  //  set[0] == 0.U -> waySplit 0 |- way 1  -- [data 3] | [data 2] | [data 1] | [data 0]
  // -----------------------------------------------------------------------------------------
  //                   waySplit 1 |- way 0  -- [data 3] | [data 2] | [data 1] | [data 0]
  //                               \ way 1  -- [data 3] | [data 2] | [data 1] | [data 0]
  // =========================================================================================
  // 1. aggregate data of the same line first
  // 2. collect all data lines in the same `WaySplit`
  // 3. use flatMap to collect all `WaySplit`, and we can get the targetData (Vec[T])
  // 4. use ren_vec to select the certain set
  val allData = (0 until setSplit).map(i =>
    VecInit((0 until waySplit).flatMap(j =>
      (0 until innerWays).map(w =>
        Cat((0 until dataSplit).map(k => array(i)(j)(k).io.r.resp.data(w)).reverse)
      )
    ))
  )

  io.r.resp.data := Mux1H(ren_vec, allData).asTypeOf(Vec(way, gen))
}
