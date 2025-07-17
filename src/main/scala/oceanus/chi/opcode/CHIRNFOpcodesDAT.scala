package oceanus.chi.opcode

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi._
import oceanus.chi.EnumCHIIssue._
import oceanus.chi.EnumCHIChannel._

/*
* CHI Opcodes permitted for DAT channel of RN-F interface
*/
trait CHIRNFOpcodesDAT extends HasCHIParameters {

    //  ======================================================================================
    val DataLCrdReturn          = CHIOpcode(DAT, 0x00, "DataLCrdReturn"             , B, C, E)
    //  --------------------------------------------------------------------------------------
    val CompData                = CHIOpcode(DAT, 0x04, "CompData"                   , B, C, E)
    //  --------------------------------------------------------------------------------------
    val DataSepResp             = CHIOpcode(DAT, 0x0B, "DataSepResp"                   , C, E)
    //  --------------------------------------------------------------------------------------
    val CopyBackWrData          = CHIOpcode(DAT, 0x02, "CopyBackWrData"             , B, C, E)
    //  --------------------------------------------------------------------------------------
    val WriteDataCancel         = CHIOpcode(DAT, 0x07, "WriteDataCancel"            , B, C, E)
    //  --------------------------------------------------------------------------------------
    val NonCopyBackWrData       = CHIOpcode(DAT, 0x03, "NonCopyBackWrData"          , B, C, E)
    //  --------------------------------------------------------------------------------------
    val NCBWrDataCompAck        = CHIOpcode(DAT, 0x0C, "NCBWrDataCompAck"              , C, E)
    //  --------------------------------------------------------------------------------------
    val SnpRespData             = CHIOpcode(DAT, 0x01, "SnpRespData"                , B, C, E)
    val SnpRespDataFwded        = CHIOpcode(DAT, 0x06, "SnpRespDataFwded"           , B, C, E)
    val SnpRespDataPtl          = CHIOpcode(DAT, 0x05, "SnpRespDataPtl"             , B, C, E)
    //  ======================================================================================

    
    /*
    * Decoder for CHI Opcodes of RN-F DAT
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder} 
    */
    class Decoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
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
            CopyBackWrData,
        //  ------------------------
            WriteDataCancel,
        //  ------------------------
            NonCopyBackWrData,
        //  ------------------------
            NCBWrDataCompAck,
        //  ------------------------
            SnpRespData,
            SnpRespDataFwded,
            SnpRespDataPtl
        //  ========================   
        ), paramEnableUnsupportedCheck)
}
