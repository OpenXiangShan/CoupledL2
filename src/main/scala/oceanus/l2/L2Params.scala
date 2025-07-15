package oceanus.l2

import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters


case class L2Params(
    physicalAddrWidth: Int = 48,
    mshrSize: Int = 64,
)

case object L2ParamsKey extends Field[L2Params](L2Params())

trait HasL2Params {
    val p: Parameters
    def l2Params = p(L2ParamsKey)
}
