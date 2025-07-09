package oceanus.chi


/* 
* Enumeration of CHI Channels (non-directional).
*/
sealed class EnumCHIChannel(ordinal             : Int,
                            name                : String,
                            val canonicalName   : String)
    extends Enum[EnumCHIChannel](name, ordinal)

object EnumCHIChannel {

    // REQ
    val REQ: EnumCHIChannel     = new EnumCHIChannel(0, "REQ", "REQ")

    // DAT
    val DAT: EnumCHIChannel     = new EnumCHIChannel(1, "DAT", "DAT")

    // RSP
    val RSP: EnumCHIChannel     = new EnumCHIChannel(2, "RSP", "RSP")

    // SNP
    val SNP: EnumCHIChannel     = new EnumCHIChannel(3, "SNP", "SNP")
}