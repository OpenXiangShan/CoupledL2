package coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.sram.SRAMTemplate
import utility.XSPerfAccumulate

class PrefetchFilterTable(size: Int = 256, tagWidth: Int = 12)(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new PrefetchReq))
    val out = DecoupledIO(new PrefetchReq)
  })

  private val idxBits = log2Up(size)
  private val lineAddrBits = fullAddressBits - offsetBits
  private val pfTagBits = if(tagWidth > 0) tagWidth else lineAddrBits - idxBits

  def lineAddr(addr: UInt): UInt = addr(fullAddressBits - 1, offsetBits)
  def idx(addr: UInt): UInt = lineAddr(addr)(idxBits - 1, 0)
  def tag(addr: UInt): UInt = lineAddr(addr)(lineAddrBits - 1, idxBits)

  class FilterEntry extends Bundle {
    val valid = Bool()
    val tag = UInt(pfTagBits.W)
  }

  val filterRam = Module(
    new SRAMTemplate(
      new FilterEntry,
      set = size,
      way = 1,
      shouldReset = true,
      singlePort = false,
      hasMbist = cacheParams.hasMbist,
      hasSramCtl = cacheParams.hasSramCtl
    )
  )

  val s0_fire = io.in.fire
  val s0_addr = io.in.bits.addr

  filterRam.io.r.req.valid := s0_fire
  filterRam.io.r.req.bits.setIdx := idx(s0_addr)

  val s1_valid = RegNext(s0_fire, false.B)
  val s1_req = RegEnable(io.in.bits, s0_fire)
  val s1_tag = RegEnable(tag(s0_addr), s0_fire)
  val s1_data = filterRam.io.r.resp.data(0)
  val s1_hit = s1_valid && s1_data.valid && s1_data.tag === s1_tag
  val s1_miss = s1_valid && !s1_hit

  filterRam.io.w.req.valid := s1_miss
  filterRam.io.w.req.bits.setIdx := idx(s1_req.addr)
  filterRam.io.w.req.bits.data(0).valid := true.B
  filterRam.io.w.req.bits.data(0).tag := s1_tag

  val out_valid = RegInit(false.B)
  val out_bits = Reg(new PrefetchReq)
  val accept_s1 = s1_miss && (!out_valid || io.out.ready)

  when(accept_s1) {
    out_valid := true.B
    out_bits := s1_req
  }.elsewhen(out_valid && io.out.ready) {
    out_valid := false.B
  }

  io.out.valid := s1_miss
  io.out.bits := s1_req

  io.in.ready := !io.out.valid || io.out.ready

  XSPerfAccumulate("pf_filter_in", io.in.fire)
  XSPerfAccumulate("pf_filter_out", io.out.fire)
  XSPerfAccumulate("pf_filter_stall", !io.in.ready)
  XSPerfAccumulate("pf_filter_drop_by_hit", s1_hit)
  XSPerfAccumulate("pf_filter_rwconflict", io.in.valid && !filterRam.io.r.req.ready && filterRam.io.w.req.fire)
}