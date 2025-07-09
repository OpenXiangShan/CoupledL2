package oceanus.chi

import org.chipsalliance.cde.config.Parameters


trait HasCHIParameters {

    implicit val p: Parameters
    
    val paramCHI = p(CHIParametersKey)
}
