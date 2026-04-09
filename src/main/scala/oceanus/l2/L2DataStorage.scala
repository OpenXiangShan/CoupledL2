package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters

object L2DataStorage {

    class PathTSHRToDataStorage(implicit val p: Parameters) extends Bundle with HasL2Params {

        val TSHRADDR = UInt(mshrIndexWidth.W)
        val WAY = UInt(32.W) // TODO: replace with actual way index width
        val SET = UInt(32.W) // TODO: replace with actual set index width
        val DATA = UInt(512.W)

    //  val DSTXDATUpRd = Bool()
        val DSBufRd = Bool()
        val DSBufWb = Bool()
    }

    class PathDataStorageToTSHR(implicit val p: Parameters) extends Bundle with HasL2Params {

        val TSHRADDR = UInt(mshrIndexWidth.W)
        val DATA = UInt(512.W)

        val DSBufRdArbComp = Bool()
        val DSBufRdResp = Bool()
        val DSBufWbArbComp = Bool()
        val DSBufWbComp = Bool()
    }
}
