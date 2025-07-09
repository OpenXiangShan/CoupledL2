package oceanus.chi.field

import org.chipsalliance.cde.config.Parameters
import chisel3._
import oceanus.chi._


/*
* Issue-configurable CHI Field.
* 
* Example:
*
*   * For QoS field that exists in all supported CHI Issue:
*       {@code val qos      = CHIFieldUInt(p(CHIParametersKey).qosWidth, EnumCHIIssue.B, EnumCHIIssue.E) }
*     or
*       {@code val qos      = CHIFieldUInt(p(CHIParametersKey).qosWidth) }
* 
*   * For MPAM field that only applicable in CHI Issue E:
*       {@code val mpam     = CHIFieldUInt(p(CHIParametersKey.mpamWidth), EnumCHIIssue.E) }
* 
*   * For DoNotDataPull that only applicable in CHI Issue B:
*       {@code val donotdatapull = CHIFieldUInt(p(CHIParametersKey).donotdatapullWidth, EnumCHIIssue.B) }
*/
object CHIFieldUInt {

    def apply(width: Int, issues: EnumCHIIssue*)
             (implicit p: Parameters): Option[UInt] = {

        val paramCHI = p(CHIParametersKey)

        if (width <= 0)
            return None

        if (issues.isEmpty)
            return Some(UInt(width.W))

        for (issue <- issues)
            if (issue == paramCHI.issue)
                return Some(UInt(width.W))

        return None
    }
    
    def apply(msb: Int, lsb: Int, off: Int, field: Option[UInt], issues: EnumCHIIssue*)
             (implicit p: Parameters): Option[UInt] = {

        val paramCHI = p(CHIParametersKey)

        if (field.isEmpty)
            return None

        if ((msb + off) < 0 || (lsb + off) < 0 || msb < lsb)
            return None

        if (issues.isEmpty)
            return Some(field.get(msb + off, lsb + off))

        for (issue <- issues)
            if (issue == paramCHI.issue)
                return Some(field.get(msb + off, lsb + off))

        return None
    }

    def apply(width: Int, off: Int, field: Option[UInt], issues: EnumCHIIssue*)
             (implicit p: Parameters): Option[UInt] = {
        apply(width - 1, 0, off, field, issues:_*)
    }
}
/**/
