package oceanus.l2

import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.HasCHIParameters
import chisel3.util.{log2Ceil, log2Up}
import utility.Code

case class L2Params(
    physicalAddrWidth: Int = 48,
    mshrSize: Int = 64,
    ways: Int = 4,
    sets: Int = 128,
    blockBytes: Int = 64,
    dataSRAMSplit: Int = 8,
    enableDataECC: Boolean = false,
    hasMbist: Boolean = false,
    hasSramCtl: Boolean = false,
    dataECC: Option[String] = Some("secded"),
)

case object L2ParamsKey extends Field[L2Params](L2Params())
case object L2SliceDirStateProbeEnableKey extends Field[Boolean](false)
case object L2SliceDirPlruProbeEnableKey extends Field[Boolean](false)
case object L2SliceDisableReplRetryReplayKey extends Field[Boolean](false)

trait HasL2Params extends HasCHIParameters {

    implicit val p: Parameters

    val paramL2 = p(L2ParamsKey)

    def mshrIndexWidth = log2Up(paramL2.mshrSize) + 1
    def nMSHR = paramL2.mshrSize
    def offsetBits = log2Ceil(paramL2.blockBytes)
    def setBits = log2Ceil(paramL2.sets)
    def wayBits = log2Ceil(paramL2.ways)
    def tagWidth = paramL2.physicalAddrWidth - offsetBits - setBits
    def blocks = paramL2.sets * paramL2.ways
    def blockBits = paramCHI.dataWidth * 2
    def beatBytes = paramCHI.dataWidth / 8
    def dataBankSplit = paramL2.dataSRAMSplit
    def dataBankBits = blockBits / dataBankSplit
    def encBankBits = cacheParams.dataCode.width(dataBankBits)
    def encDataPadBits = 0
    def cacheParams = paramL2
    def enableDataECC = paramL2.enableDataECC

    require(paramCHI.dataWidth == 256, "L2 requires CHI data width to be 256 bits")
}

object L2Params {
    implicit final class L2ParamsOps(private val p: L2Params) extends AnyVal {
        def dataCode: Code = Code.fromString(p.dataECC)
    }
}
