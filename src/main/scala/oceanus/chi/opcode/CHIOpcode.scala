package oceanus.chi.opcode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import oceanus.chi._


/*
* CHI Opcode container.
* 
* @param channel    Specified CHI channel
* @param opcode     Opcode value
* @param name       Opcode name
* @param issues     Applicable CHI issues (empty for all issues)
*/
class CHIOpcode(val channel : EnumCHIChannel,
                val opcode  : Int,
                val name    : String,
                val issues  : Seq[EnumCHIIssue])
    (implicit val p: Parameters) extends HasCHIParameters {

    /*
    * Convert to literal UInt instance with the width of REQ channel Opcode
    * regardless of the originally specified CHI channel.
    *
    * @return Literal UInt instance of opcode
    */
    def asUIntForREQ: UInt = opcode.U(paramCHI.reqOpcodeWidth.W)

    /*
    * Convert to literal UInt instance with the width of DAT channel Opcode
    * regardless of the originally specified CHI channel.
    *
    * @return Literal UInt instance of opcode
    */
    def asUIntForDAT: UInt = opcode.U(paramCHI.datOpcodeWidth.W)

    /*
    * Convert to literal UInt instance with the width of RSP channel Opcode
    * regardless of the originally specified CHI channel.
    *
    * @return Literal UInt instance of opcode
    */
    def asUIntForRSP: UInt = opcode.U(paramCHI.rspOpcodeWidth.W)

    /*
    * Convert to literal UInt instance with the width of SNP channel Opcode
    * regardless of the originally specified CHI channel.
    *
    * @return Literal UInt instance of opcode
    */
    def asUIntForSNP: UInt = opcode.U(paramCHI.snpOpcodeWidth.W)

    /*
    * Convert to literal UInt instance with the width of originally specified
    * channel Opcode.
    *
    * @return Literal UInt instance of opcode
    * 
    * @throws IllegalArgumentException If the originally specified channel is 
    *   not supported or malformed.
    */
    def asUInt: UInt = channel match {
        case EnumCHIChannel.REQ => asUIntForREQ
        case EnumCHIChannel.DAT => asUIntForDAT
        case EnumCHIChannel.RSP => asUIntForRSP
        case EnumCHIChannel.SNP => asUIntForSNP
        case _: EnumCHIChannel  => throw new IllegalArgumentException(s"unknown CHI channel: ${channel}")
    }
    
    /*
    * @return If this Opcode is applicable to parameterized CHI Issue. 
    */
    def applicable: Boolean = if (issues.isEmpty) true else issues.contains(paramCHI.issue)

    /*
    * Require this opcode to be applicable
    *
    * @return Opcode itself 
    */
    def require: CHIOpcode = {
        Predef.require(applicable, s"in-applicable CHI opcode: ${name} (${opcode})")
        this
    }

    /*
    * Software elaboration-time comparsion of opcode. 
    * 
    * @param opcode Integer value of opcode
    */
    def is(opcode: Int): Boolean = opcode == this.opcode

    /* 
    * Hardware comparsion of opcode. 
    * 
    * @param opcode Hardware instance of opcode source, with X-state propagation compatiblity
    * @param valid  Hardware instance of valid signal
    */
    def is(opcode: UInt, valid: Bool = true.B): Bool 
        // with X-state propagation compatibility
        = Mux(valid, opcode === asUInt, dontTouch(WireInit(false.B)))
}


object CHIOpcode {

    def apply(channel : EnumCHIChannel,
              opcode  : Int, 
              name    : String, 
              issues  : EnumCHIIssue*)
        (implicit p: Parameters) = new CHIOpcode(channel, opcode, name, issues)
}
