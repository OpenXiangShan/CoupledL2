package coupledL2

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import chisel3._
import chisel3.util._
import coupledL2.utils._
import utility._

class RequestBuffer(flow: Boolean = true, entries: Int = 4)(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val in          = Flipped(DecoupledIO(new TaskBundle))
    val out         = DecoupledIO(new TaskBundle)
    val mshrStatus  = Vec(mshrsAll, Flipped(ValidIO(new MSHRStatus)))
  })

  /* ======== Data Structure ======== */
  val reqEntry = new L2Bundle(){
    val valid    = Bool()
    val rdy      = Bool()
    val task     = new TaskBundle()

    /* which mshr the entry is waiting for */
    val waitMS  = UInt(mshrsAll.W)

    /*  buffer_dep_mask[i][j] => entry i should wait entry j
      this is used to make sure that same set requests will be sent
      to MSHR in order
    */
    val depMask = Vec(entries, Bool())
  }

  // TODO: consider using RAM?
  val buffer = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(reqEntry))))

  /* ======== Enchantment ======== */
  val NWay = cacheParams.ways
  def sameAddr(a: TaskBundle, b: TaskBundle): Bool = Cat(a.tag, a.set) === Cat(b.tag, b.set)
  def sameAddr(a: TaskBundle, b: MSHRStatus): Bool = Cat(a.tag, a.set) === Cat(b.tag, b.set)
  def sameSet (a: TaskBundle, b: MSHRStatus): Bool = a.set === b.set

  val full         = Cat(buffer.map(_.valid)).andR
  val conflictMask = io.mshrStatus.map(s =>
    s.valid && sameAddr(io.in.bits, s.bits) && !s.bits.will_free
  )
  val conflict     = Cat(conflictMask).orR
  val noReadyEntry = Wire(Bool())
  val noFreeWay    = Wire(Bool())
  val canFlow      = flow.B && noReadyEntry
  val doFlow       = canFlow && io.out.ready && !noFreeWay

  // TODO: remove depMatrix
  val depMask    = buffer.map(e => e.valid && sameAddr(io.in.bits, e.task))
  val isPrefetch = io.in.bits.fromA && io.in.bits.opcode === Hint
  val dup        = io.in.valid && isPrefetch && Cat(depMask).orR // duplicate prefetch

  /* ======== Alloc ======== */
  io.in.ready   := !full || doFlow

  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val alloc = !full && io.in.valid && !doFlow && !dup
  when(alloc){
    val entry = buffer(insertIdx)
    entry.valid   := true.B
    entry.rdy     := !conflict && !Cat(depMask).orR //TODO
    entry.task    := io.in.bits
    entry.waitMS  := VecInit(conflictMask).asUInt
    entry.depMask := depMask //TODO
    assert(PopCount(conflictMask) <= 2.U)
  }

  /* ======== Issue ======== */
  val issueArb = Module(new FastArbiter(new TaskBundle, entries))
  for (i <- 0 until entries) {
    issueArb.io.in(i).valid := buffer(i).valid && buffer(i).rdy
    issueArb.io.in(i).bits  := buffer(i).task

    // ! clear this when io.out fire
    // cuz may be blocked by noFreeWay
    when(issueArb.io.in(i).fire && io.out.fire) {
      buffer(i).valid := false.B
    }
  }
  issueArb.io.out.ready := io.out.ready
  noReadyEntry := !issueArb.io.out.valid

  /* ======== Waymask Info ======== */
  // ways in the set of issued-A-req that are occupied by unfinished MSHR task
  val occWays =
    VecInit(io.mshrStatus.map(s =>
      Mux(
        s.valid && !s.bits.will_free && sameSet(io.out.bits, s.bits),
        UIntToOH(s.bits.way, NWay),
        0.U(NWay.W)
      )
    )).reduceTree(_ | _)

  noFreeWay := !Cat(~occWays).orR

  /* ======== Update rdy and masks ======== */
  for (e <- buffer) {
    when(e.valid) {
      val waitMSUpdate  = WireInit(e.waitMS)
      val depMaskUpdate = WireInit(e.depMask)

      // when mshr will_free, clear it in other reqs' waitMS
      val freeMask = VecInit(io.mshrStatus.map(s => s.valid && s.bits.will_free)).asUInt
      waitMSUpdate := e.waitMS.asUInt & (~freeMask.asUInt).asUInt

      // when issue fire, clear it in other reqs' depMask
      when(issueArb.io.out.fire) {
        depMaskUpdate(issueArb.io.chosen) := false.B
      }

      // update info
      e.waitMS  := waitMSUpdate
      e.depMask := depMaskUpdate
      e.rdy     := !waitMSUpdate.orR && !Cat(depMaskUpdate).orR
    }
  }

  /* ======== Output ======== */
  io.out.valid := !noFreeWay && (issueArb.io.out.valid || (io.in.valid && canFlow))
  io.out.bits  := Mux(canFlow, io.in.bits, issueArb.io.out.bits)
  // for Dir to choose a way not occupied by some unfinished MSHR task
  io.out.bits.wayMask := ~occWays
}
