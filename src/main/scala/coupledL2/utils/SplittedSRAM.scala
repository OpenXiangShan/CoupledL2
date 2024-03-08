package coupledL2.utils

import chisel3._
import chisel3.util._

// split SRAM by set/way/data
// 1. use lower-bits of set to select bank
// 2. split ways and parallel access
// 3. split data and parallel access

// * an example of "setSplit 2, waySplit 2, dataSplit 4"
// ==================================================================
//                   |- way 0  -- [data 0] [data 1] [data 2] [data 3]
//  set[0] == 0.U -> |- way 1  -- [data 0] [data 1] [data 2] [data 3]
// ------------------------------------------------------------------
//  set[0] == 1.U -> |- way 0  -- [data 0] [data 1] [data 2] [data 3]
//                   |- way 1  -- [data 0] [data 1] [data 2] [data 3]
// ==================================================================
class SplittedSRAM[T <: Data]
(
  gen: T, sets: Int, ways: Int,
  setSplit: Int = 1, waySplit: Int = 1, dataSplit: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = true, bypassWrite: Boolean = false,
  clk_div_by_2: Boolean = false
) extends Module {
  val io = IO(new Bundle() {
    val r = Flipped(new SRAMReadBus(gen, sets, ways))
    val w = Flipped(new SRAMWriteBus(gen, sets, ways))
  })


  require(sets % setSplit == 0, "sets must be divisible by setSplit")
  val innerSets = sets / setSplit
  val bankBits = log2Ceil(setSplit)
  val innerSetBits = log2Up(sets) - bankBits
  val r_setIdx = io.r.req.bits.setIdx.head(innerSetBits)
  val r_bankSel = if(setSplit == 1) 0.U else io.r.req.bits.setIdx(bankBits - 1, 0)
  val w_setIdx = io.w.req.bits.setIdx.head(innerSetBits)
  val w_bankSel = if(setSplit == 1) 0.U else io.w.req.bits.setIdx(bankBits - 1, 0)

  require(ways % waySplit == 0, "ways must be divisible by waySplit")
  val innerWays = ways / waySplit

  require(gen.getWidth % dataSplit == 0, "data width must be divisible by dataSplit")
  val innerWidth = gen.getWidth / dataSplit

  val array = Seq.fill(setSplit)(Seq.fill(waySplit)(Seq.fill(dataSplit)(
    Module(new SRAMTemplate(
      UInt(innerWidth.W), innerSets, innerWays,
      shouldReset = shouldReset, holdRead = holdRead,
      singlePort = singlePort, bypassWrite = bypassWrite,
      clk_div_by_2 = clk_div_by_2
    ))
  )))

  for (i <- 0 until setSplit) {
    val ren = i.U === r_bankSel
    val wen = i.U === w_bankSel

    for (j <- 0 until waySplit) {
      val waymask = if (ways > 1) io.w.req.bits.waymask.get(innerWays * (j+1) - 1, innerWays * j) else 1.U
      // if waymask-part is 0.U, we need not set array(i)(j)(_).io.w.req
      // TODO: consider whether to add this, which may lower power consumption, but will add burden to timing
      // val needWrite = waymask.orR

      for (k <- 0 until dataSplit) {
        array(i)(j)(k).io.r.req.valid := io.r.req.valid && ren
        array(i)(j)(k).io.r.req.bits.apply(r_setIdx)
        array(i)(j)(k).io.w.req.valid := io.w.req.valid && wen // && needWrite
        array(i)(j)(k).io.w.req.bits.apply(
          io.w.req.bits.data.asUInt(innerWidth * (k+1) - 1, innerWidth * k), w_setIdx, waymask
        )
      }
    }
  }

  val ren_vec_0 = VecInit((0 until setSplit).map(i => i.U === r_bankSel))
  val ren_vec_1 = RegNext(ren_vec_0, 0.U.asTypeOf(ren_vec_0))
  val ren_vec = if(clk_div_by_2){
    RegNext(ren_vec_1, 0.U.asTypeOf(ren_vec_0))
  } else ren_vec_1

  // only one read/write
  assert({PopCount(ren_vec) <= 1.U})

  // TODO: we should consider the readys of all sram to be accessed, and orR them
  // but since waySplitted and dataSplitted smaller srams should have the same behavior
  // we can just use one of them for ready, for better timing
  io.r.req.ready := VecInit((0 until setSplit).map(i => array(i).head.head.io.r.req.ready))(r_bankSel)
  io.w.req.ready := VecInit((0 until setSplit).map(i => array(i).head.head.io.w.req.ready))(w_bankSel)


  // aggregate data first, then way, finally use set to select
  val rdata = Mux1H(ren_vec,
    (0 until setSplit).map(i =>
      (0 until waySplit).map(j =>
        (0 until innerWays).map(w =>
          Cat((0 until dataSplit).map(k => array(i)(j)(k).io.r.resp.data(w)).reverse)
        )
      ).flatten
    ).flatten
  )

  io.r.resp.data := VecInit(Seq.fill(ways)(rdata.asTypeOf(gen)))
}
