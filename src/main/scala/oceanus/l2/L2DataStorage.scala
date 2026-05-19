package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

object L2DataStorage {

  class PathTSHRToDataStorageUOPs extends Bundle {
    val DSBufAheadRd = Bool()
//  val DSTXDATUpRd = Bool()
    val DSBufRd = Bool()
    val DSBufWb = Bool()
  }

  class PathTSHRToDataStorage(implicit val p: Parameters) extends PathTSHRToDataStorageUOPs with HasL2Params {
    val TSHRADDR = UInt(mshrIndexWidth.W)
    val WAY = UInt(32.W) // TODO: replace with actual way index width
    val SET = UInt(32.W) // TODO: replace with actual set index width
    val DATA = UInt(512.W)
  }

  class PathDataStorageToTSHRUOPs extends Bundle {
    val DSBufAheadRdArbComp = Bool()
    val DSBufAheadRdResp = Bool()
    val DSBufRdArbComp = Bool()
    val DSBufRdResp = Bool()
    val DSBufWbArbComp = Bool()
    val DSBufWbComp = Bool()
  }

  class PathDataStorageToTSHR(implicit val p: Parameters) extends PathDataStorageToTSHRUOPs with HasL2Params {
    val TSHRADDR = UInt(mshrIndexWidth.W)
    val DATA = UInt(512.W)
    val WAY = UInt(4.W) // TODO: parameterize with L2 way count
  }
}
