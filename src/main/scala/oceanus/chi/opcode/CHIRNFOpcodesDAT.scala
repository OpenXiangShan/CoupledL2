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

    //  ==========================================================================================
    val CHI_DataLCrdReturn          = CHIOpcode(DAT, 0x00, "DataLCrdReturn"             , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_CompData                = CHIOpcode(DAT, 0x04, "CompData"                   , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_DataSepResp             = CHIOpcode(DAT, 0x0B, "DataSepResp"                   , C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_CopyBackWrData          = CHIOpcode(DAT, 0x02, "CopyBackWrData"             , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_WriteDataCancel         = CHIOpcode(DAT, 0x07, "WriteDataCancel"            , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_NonCopyBackWrData       = CHIOpcode(DAT, 0x03, "NonCopyBackWrData"          , B, C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_NCBWrDataCompAck        = CHIOpcode(DAT, 0x0C, "NCBWrDataCompAck"              , C, E)
    //  ------------------------------------------------------------------------------------------
    val CHI_SnpRespData             = CHIOpcode(DAT, 0x01, "SnpRespData"                , B, C, E)
    val CHI_SnpRespDataFwded        = CHIOpcode(DAT, 0x06, "SnpRespDataFwded"           , B, C, E)
    val CHI_SnpRespDataPtl          = CHIOpcode(DAT, 0x05, "SnpRespDataPtl"             , B, C, E)
    //  ==========================================================================================

    
    /*
    * Decoder for CHI Opcodes of RN-F DAT
    * 
    * @see {@code oceanus.chi.opcode.CHIOpcodeDecoder} 
    */
    class DATOpcodeDecoder(paramOpcodeSupported          : Seq[CHIOpcode]    = Seq(),
                  paramEnableUnsupportedCheck   : Boolean           = false)
        (implicit p: Parameters)
        extends CHIOpcodeDecoder(DAT, paramOpcodeSupported, Seq(
        //  ========================
            CHI_DataLCrdReturn,
        //  ------------------------
            CHI_CompData,
        //  ------------------------
            CHI_DataSepResp,
        //  ------------------------
            CHI_CopyBackWrData,
        //  ------------------------
            CHI_WriteDataCancel,
        //  ------------------------
            CHI_NonCopyBackWrData,
        //  ------------------------
            CHI_NCBWrDataCompAck,
        //  ------------------------
            CHI_SnpRespData,
            CHI_SnpRespDataFwded,
            CHI_SnpRespDataPtl
        //  ========================   
        ), paramEnableUnsupportedCheck)
}
