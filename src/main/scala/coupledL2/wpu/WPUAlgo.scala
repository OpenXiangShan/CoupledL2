package coupledL2.wpu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import coupledL2._

case class WPUParameters
(
  algoName: String,
  debug: Boolean = false
)

object AlgoWPUMap {
  def apply(wpuParam: WPUParameters)(implicit p: Parameters) = {
    wpuParam.algoName.toLowerCase match {
      case "utag" => Module(new UtagWPU(wpuParam))
      case t => throw new IllegalArgumentException(s"unknown WPU Algorithm $t")
    }
  }
}

class WPUUpdate(implicit P: Parameters) extends L2Bundle{
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val isReplace = Bool()
  val isEvict = Bool()
  val predWay = UInt(wayBits.W)
  val predHit = Bool()
  val actualWay = UInt(wayBits.W)
  val actualHit = Bool()
}

class WPURead(implicit P: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
}

class WPUResult(implicit P: Parameters) extends L2Bundle {
  val predWay = UInt(wayBits.W)
  val predHit = Bool()
}

abstract class BaseWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends L2Module {
  println(s"  WPU type: ${wpuParam.algoName}")
  suggestName(s" ${wpuParam.algoName}WPU")
  val sets = cacheParams.sets
  val ways = cacheParams.ways
  val mbist = cacheParams.hasMbist
  val hasSramCtl = cacheParams.hasSramCtl

  val in = IO(Input(new Bundle {
    val read = Flipped(DecoupledIO(new WPURead()))
    val upd = Flipped(ValidIO(new WPUUpdate()))
  }))
  val out = IO(Output(new Bundle {
    val res = new WPUResult()
  }))
}

class UtagWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
  def utag_hash(in: UInt) = {
    val utag_bits = math.min(in.getWidth / 2, 8)
    if (utag_bits < 8) {
      in(2 * utag_bits - 1, utag_bits) ^ in(utag_bits - 1, 0)
    } else {
      in(15, 8) ^ Cat(Reverse(in(2, 0)), in(7, 3))
    }
  }

  val utagBits = utag_hash(0.U(tagBits.W)).getWidth
  println(s"utagBits: ${utagBits}")

  class Entry(implicit p: Parameters) extends L2Bundle {
    val valid = Bool()
    val utag = UInt(utagBits.W)
  }
  val stateRegs = Module(new SRAMTemplate(UInt((new Entry).getWidth.W), sets, ways, singlePort = true, hasMbist = mbist, hasSramCtl = hasSramCtl, shouldReset = true))

  val stateRead = stateRegs.io.r(in.read.valid, in.read.bits.set)
  val utagMatchVec = VecInit(stateRead.resp.data.map(_.asTypeOf(new Entry))
    .map(entry => entry.utag === utag_hash(in.read.bits.tag) & entry.valid)
  ).asUInt
  val hit = utagMatchVec.orR
  val way = OHToUInt(utagMatchVec)

  val updInfo = in.upd.bits
  val updUtag = utag_hash(updInfo.tag)
  val updSet = in.upd.bits.set
  val updWay = in.upd.bits.actualWay
  val updFlag = !updInfo.predHit & updInfo.actualHit |
    updInfo.predHit & updInfo.actualHit & updInfo.predWay === updInfo.actualWay |
    updInfo.isReplace
  val invalidFlag = updInfo.isEvict | updInfo.predHit & !updInfo.actualHit
  val updState = Wire(new Entry)
  updState.valid := !invalidFlag
  updState.utag := updUtag

  // perf logic
  stateRegs.io.w(
    updFlag || invalidFlag,
    updState.asUInt,
    updSet,
    UIntToOH(updWay)
  )

  in.read.ready := stateRead.req.ready
  out.res.predHit := hit
  out.res.predWay := way

  if (cacheParams.wpuParam.debug) {
    val debug_stateRegs = RegInit(VecInit(Seq.fill(cacheParams.sets)(VecInit(Seq.fill(cacheParams.ways)(0.U.asTypeOf(new Entry))))))
    when (in.upd.valid) {
      when (updFlag) {
        debug_stateRegs(updSet)(updWay).utag := updUtag
        debug_stateRegs(updSet)(updWay).valid := true.B
      }.elsewhen(invalidFlag) {
        debug_stateRegs(updSet)(updWay).valid := false.B
      }
    }

    val debug_hashConflict = (PopCount(utagMatchVec) > 1.U) & in.read.valid
    val debug_hashConflictTimes = RegInit(0.U(64.W))
    when (debug_hashConflict) {
      debug_hashConflictTimes := debug_hashConflictTimes + 1.U
    }
    XSPerfAccumulate(s"${wpuParam.algoName}WPU_hash_conflict", debug_hashConflict)
    val lookupUpd = in.upd.valid && !updInfo.isReplace && !updInfo.isEvict
    val phit_ahit_unmatch = updInfo.predHit && updInfo.actualHit && updInfo.actualWay =/= updInfo.predWay
    val phit_amiss = !updInfo.predHit && updInfo.actualHit
    val pmiss_ahit = updInfo.predHit && !updInfo.actualHit
    val utagerr = debug_stateRegs(updSet)(updWay).utag =/= updUtag && debug_stateRegs(updSet)(updWay).valid
    val validerr = !debug_stateRegs(updSet)(updWay).valid
    XSPerfAccumulate(s"${wpuParam.algoName}WPU_ph_ah_um_utagerr", phit_ahit_unmatch && utagerr && lookupUpd)
    XSPerfAccumulate(s"${wpuParam.algoName}WPU_ph_ah_um_validerr", phit_ahit_unmatch && validerr && lookupUpd)
    XSPerfAccumulate(s"${wpuParam.algoName}WPU_ph_am_utagerr", phit_amiss && utagerr && lookupUpd)
    XSPerfAccumulate(s"${wpuParam.algoName}WPU_ph_am_validerr", phit_amiss && validerr && lookupUpd)
    XSPerfAccumulate(s"${wpuParam.algoName}WPU_pm_ah_utagerr", pmiss_ahit && utagerr && lookupUpd)
    XSPerfAccumulate(s"${wpuParam.algoName}WPU_pm_ah_validerr", pmiss_ahit && validerr && lookupUpd)
  }
}