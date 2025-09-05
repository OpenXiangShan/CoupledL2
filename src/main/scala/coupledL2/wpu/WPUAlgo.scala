package coupledL2.wpu

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import utility.XSPerfAccumulate
import coupledL2._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

case object WPUParamsKey extends Field[WPUParameters]
case class WPUParameters
(
  algoName: String,
  enCfPred: Boolean = false,
)

object AlgoWPUMap {
  def apply(wpuParam: WPUParameters)(implicit p: Parameters) = {
    wpuParam.algoName.toLowerCase match {
      case "mru" => Module(new MruWPU(wpuParam))
      case "mmru" => Module(new MmruWPU(wpuParam))
      case "mru1" => Module(new Mru1WPU(wpuParam))
      case "mmru1" => Module(new Mmru1WPU(wpuParam))
      case "utag1" => Module(new Utag1(wpuParam))
      case "utag2" => Module(new Utag2(wpuParam))
      case "utag3" => Module(new Utag3(wpuParam))
      case "utag4" => Module(new Utag4(wpuParam))
      case t => throw new IllegalArgumentException(s"unknown WPU Algorithm $t")
    }
  }
}

abstract class BaseWPUBundle(implicit P: Parameters) extends L2Bundle

abstract class WPUModule(implicit P: Parameters) extends L2Module

class WPUUpdate(implicit P: Parameters) extends BaseWPUBundle{
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val isReplace = Bool()
  val isEvict = Bool()
  val predWay = UInt(wayBits.W)
  val predHit = Bool()
  val actualWay = UInt(wayBits.W)
  val actualHit = Bool()
}

class WPURead(implicit P: Parameters) extends BaseWPUBundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
}

class WPUResult(implicit P: Parameters) extends BaseWPUBundle {
  val predWay = UInt(wayBits.W)
  val predHit = Bool()
}

abstract class BaseWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends WPUModule {
  println(s"  WPU type: ${wpuParam.algoName}")
  suggestName(s" ${wpuParam.algoName}_wpu")

  val in = IO(Input(new Bundle {
    val read = Flipped(ValidIO(new WPURead()))
    val upd = Flipped(ValidIO(new WPUUpdate()))
  }))
  val out = IO(Output(new Bundle {
    val res = ValidIO(new WPUResult())
  }))
}

class MruWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
  val mruRegs = RegInit(VecInit(Seq.fill(cacheParams.sets)(0.U((wayBits).W))))

  val readSet = in.read.bits.set
  val readRes = Wire(new WPUResult())
  readRes.predWay := mruRegs(readSet)
  readRes.predHit := true.B

  val updInfo = in.upd.bits
  val updOnUnmatch = updInfo.actualHit & updInfo.predWay =/= updInfo.actualWay
  val updSet = in.upd.bits.set

  when (in.upd.valid) {
    when (updOnUnmatch | updInfo.isReplace) {
      mruRegs(updSet) := updInfo.actualWay
    }.elsewhen(updInfo.isEvict & mruRegs(updSet) === updInfo.actualWay) {
      mruRegs(updSet) := 0.U
    }
  }

  out.res.valid := in.read.valid
  out.res.bits := readRes
}

class MmruWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
  val mmru_regs_ways = cacheParams.ways
  val mmruRegs = RegInit(VecInit(Seq.fill(cacheParams.sets)(VecInit(Seq.fill(mmru_regs_ways)(0.U(wayBits.W))))))

  val readSet = in.read.bits.set
  val readWay = in.read.bits.tag(wayBits - 1, 0)
  val readRes = Wire(new WPUResult())
  readRes.predWay := mmruRegs(readSet)(readWay)
  readRes.predHit := true.B

  val updInfo = in.upd.bits
  val updOnUnmatch = updInfo.actualHit & updInfo.predWay =/= updInfo.actualWay
  val updSet = in.upd.bits.set
  val updWay = in.upd.bits.tag(wayBits - 1, 0)

  when (in.upd.valid) {
    when (updOnUnmatch | updInfo.isReplace) {
      mmruRegs(updSet)(updWay) := updInfo.actualWay
    }.elsewhen(updInfo.isEvict & mmruRegs(updSet)(updWay) === updInfo.actualWay) {
      mmruRegs(updSet)(updWay) := 0.U
    }
  }

  out.res.valid := in.read.valid
  out.res.bits := readRes
}

class Mru1WPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
  val mruRegs = RegInit(VecInit(Seq.fill(cacheParams.sets)(0.U((wayBits).W))))

  val readSet = in.read.bits.set
  val readRes = Wire(new WPUResult())
  readRes.predWay := mruRegs(readSet)
  readRes.predHit := true.B

  val updInfo = in.upd.bits
  val updOnUnmatch = updInfo.actualHit & updInfo.predWay =/= updInfo.actualWay
  val updSet = in.upd.bits.set

  when (in.upd.valid) {
    when (updOnUnmatch | updInfo.isReplace) {
      mruRegs(updSet) := updInfo.actualWay
    }
  }

  out.res.valid := in.read.valid
  out.res.bits := readRes
}

class Mmru1WPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
  val mmru_regs_ways = cacheParams.ways
  val mmruRegs = RegInit(VecInit(Seq.fill(cacheParams.sets)(VecInit(Seq.fill(mmru_regs_ways)(0.U(wayBits.W))))))

  val readSet = in.read.bits.set
  val readWay = in.read.bits.tag(wayBits - 1, 0)
  val readRes = Wire(new WPUResult())
  readRes.predWay := mmruRegs(readSet)(readWay)
  readRes.predHit := true.B

  val updInfo = in.upd.bits
  val updOnUnmatch = updInfo.actualHit & updInfo.predWay =/= updInfo.actualWay
  val updSet = in.upd.bits.set
  val updWay = in.upd.bits.tag(wayBits - 1, 0)

  when (in.upd.valid) {
    when (updOnUnmatch | updInfo.isReplace) {
      mmruRegs(updSet)(updWay) := updInfo.actualWay
    }
  }

  out.res.valid := in.read.valid
  out.res.bits := readRes
}

abstract class UtagWPU(wpuParam: WPUParameters)(implicit p:Parameters) extends BaseWPU(wpuParam){
  def utag_hash(in: UInt): UInt
  val utagBits = utag_hash(0.U(tagBits.W)).getWidth
  println(s"utagBits: ${utagBits}")

  val utagRegs = RegInit(VecInit(Seq.fill(cacheParams.sets)(VecInit(Seq.fill(cacheParams.ways)(0.U(utagBits.W))))))
  val validRegs = RegInit(VecInit(Seq.fill(cacheParams.sets)(VecInit(Seq.fill(cacheParams.ways)(false.B)))))

  val readSet = in.read.bits.set
  val reqUtag = utag_hash(in.read.bits.tag)
  val utagMatchVec = VecInit(utagRegs(readSet).zip(validRegs(readSet)).map {
    case (utag, valid) => utag === reqUtag & valid
  }).asUInt
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

  when (in.upd.valid) {
    when (updFlag) {
      utagRegs(updSet)(updWay) := updUtag
      validRegs(updSet)(updWay) := true.B
    }.elsewhen(invalidFlag) {
      validRegs(updSet)(updWay) := false.B
    }
  }

  out.res.valid := in.read.valid
  out.res.bits.predHit := hit
  out.res.bits.predWay := way

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
  val utagerr = utagRegs(updSet)(updWay) =/= updUtag && validRegs(updSet)(updWay)
  val validerr = !validRegs(updSet)(updWay)
  XSPerfAccumulate(s"${wpuParam.algoName}WPU_ph_ah_um_utagerr", phit_ahit_unmatch && utagerr && lookupUpd)
  XSPerfAccumulate(s"${wpuParam.algoName}WPU_ph_ah_um_validerr", phit_ahit_unmatch && validerr && lookupUpd)
  XSPerfAccumulate(s"${wpuParam.algoName}WPU_ph_am_utagerr", phit_amiss && utagerr && lookupUpd)
  XSPerfAccumulate(s"${wpuParam.algoName}WPU_ph_am_validerr", phit_amiss && validerr && lookupUpd)
  XSPerfAccumulate(s"${wpuParam.algoName}WPU_pm_ah_utagerr", pmiss_ahit && utagerr && lookupUpd)
  XSPerfAccumulate(s"${wpuParam.algoName}WPU_pm_ah_validerr", pmiss_ahit && validerr && lookupUpd)
}

class Utag1(wpuParam: WPUParameters)(implicit P: Parameters) extends UtagWPU(wpuParam) {
  override def utag_hash(in: UInt) = {
    val utag_bits = math.min(in.getWidth / 2, 8)
    if (utag_bits < 8) {
      in(2 * utag_bits - 1, utag_bits) ^ in(utag_bits - 1, 0)
    } else {
      in(15, 8) ^ in(7, 0)
    }
  }
}

class Utag2(wpuParam: WPUParameters)(implicit P: Parameters) extends UtagWPU(wpuParam) {
  override def utag_hash(in: UInt) = {
    val utag_bits = math.min(in.getWidth / 2, 8)
    if (utag_bits < 8) {
      in(2 * utag_bits - 1, utag_bits) ^ in(utag_bits - 1, 0)
    } else {
      in(15, 8) ^ Cat(Reverse(in(2, 0)), in(7, 3))
    }
  }
}

class Utag3(wpuParam: WPUParameters)(implicit P: Parameters) extends UtagWPU(wpuParam) {
  override def utag_hash(in: UInt) = {
    val utag_width = in.getWidth / 2
    in(in.getWidth - 1, in.getWidth - utag_width) ^ in(utag_width - 1, 0)
  }
}

class Utag4(wpuParam: WPUParameters)(implicit P: Parameters) extends UtagWPU(wpuParam) {
  override def utag_hash(in: UInt) = {
    in
  }

  val eq = in.upd.valid && updInfo.isReplace && utagRegs(updSet)(updWay) === updUtag && validRegs(updSet)(updWay) === true.B
  assert(!eq, "Only update in replace so tag must change")
}