package oceanus.l2

import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.HasCHIParameters

case class L2Params(
    physicalAddrWidth: Int = 48,
    mshrSize: Int = 64,
)

case object L2ParamsKey extends Field[L2Params](L2Params())

trait HasL2Params extends HasCHIParameters {

    implicit val p: Parameters

    val paramL2 = p(L2ParamsKey)

    require(paramCHI.dataWidth == 256, "L2 requires CHI data width to be 256 bits")
}
