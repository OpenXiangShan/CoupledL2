package oceanus.chi.opcode

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi._
import oceanus.chi.EnumCHIIssue._
import oceanus.chi.EnumCHIChannel._
import oceanus.chi.opcode.CHIOpcodeDecoder


/* 
* CHI Opcodes permitted for DAT channel of SN-F interface
*/
trait CHISNFOpcodesDAT extends HasCHIParameters {

    //  ======================================================================================
    val DataLCrdReturn          = CHIOpcode(DAT, 0x00, "DataLCrdReturn"             , B, C, E)
    //  --------------------------------------------------------------------------------------
    val CompData                = CHIOpcode(DAT, 0x04, "CompData"                   , B, C, E)
    //  --------------------------------------------------------------------------------------
    val DataSepResp             = CHIOpcode(DAT, 0x0B, "DataSepResp"                , B, C, E)
    //  --------------------------------------------------------------------------------------
    val WriteDataCancel         = CHIOpcode(DAT, 0x07, "WriteDataCancel"            , B, C, E)
    //  --------------------------------------------------------------------------------------
    val NonCopyBackWrData       = CHIOpcode(DAT, 0x03, "NonCopyBackWrData"          , B, C, E)
    //  ======================================================================================


    /* 
    * Decoder for CHI Opcodes of SN-F DAT
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder}
    */ 
    class DATOpcodeDecoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(DAT, paramOpcodeSupported, Seq(
        //  ========================
            DataLCrdReturn,
        //  ------------------------
            CompData,
        //  ------------------------
            DataSepResp,
        //  ------------------------
            WriteDataCancel,
        //  ------------------------
            NonCopyBackWrData
        //  ========================
        ), paramEnableUnsupportedCheck)
}
