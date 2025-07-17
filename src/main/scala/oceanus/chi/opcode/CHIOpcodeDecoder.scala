package oceanus.chi.opcode

import chisel3._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.HasCHIParameters
import oceanus.chi.EnumCHIChannel
import oceanus.chi.opcode.CHIOpcode
import oceanus.debug.DebugBundle
import oceanus.debug.DebugSignal
import freechips.rocketchip.util.SeqToAugmentedSeq
import oceanus.debug.{DebugBundle, DebugSignal}

/*
* CHI Opcode Decoder
* 
* @param paramChannel   Specify the targeted channel of CHI for this decoder.
* 
* @param paramOpcodeSupported   Specify all supported CHI Opcodes, all supported if empty.
* 
* @param paramOpcodeAll         Specify all CHI Opcodes.
* 
* @param paramEnableUnsupportedCheck    Whether enable assertions for unsupported CHI Opcodes.
*                                       Unsupported CHI Opcodes: CHI Opcodes exist in paramOpcodeAll
*                                                                but absent in paramOpcodeSupported.
*/
abstract class CHIOpcodeDecoder(val paramChannel                    : EnumCHIChannel,
                                val paramOpcodeSupported            : Seq[CHIOpcode],
                                val paramOpcodeAll                  : Seq[CHIOpcode],
                                val paramEnableUnsupportedCheck     : Boolean           = false)
    (implicit val p: Parameters) 
        extends Module with HasCHIParameters {

    //
    private def unknownChannel = 
        throw new IllegalArgumentException(s"unknown or unsupported CHI Channel: ${paramChannel}")
    //

    // check parameters
    paramOpcodeAll.foreach(u =>
        require(u.channel == paramChannel, s"wrong channel in 'paramOpcodeAll' (decoder: ${paramChannel}, opcode: ${u.channel})"))

    paramOpcodeSupported.foreach(u =>
        require(u.channel == paramChannel, s"wrong channel in 'paramOpcodeSupported' (decoder: ${paramChannel}, opcode: ${u.channel})"))

    // local parameters
    protected val paramOpcodeWidth  = paramChannel match {
        case EnumCHIChannel.REQ => paramCHI.reqOpcodeWidth
        case EnumCHIChannel.DAT => paramCHI.datOpcodeWidth
        case EnumCHIChannel.RSP => paramCHI.rspOpcodeWidth
        case EnumCHIChannel.SNP => paramCHI.snpOpcodeWidth
        case _: EnumCHIChannel  => unknownChannel
    }

    protected val paramDecodedWidth = 1 << paramOpcodeWidth


    /* 
    * Port I/O: debug
    */
    class DebugPort extends DebugBundle {
        val OpcodeUnsupported       = Output(Bool())
        val OpcodeUnknown           = Output(Bool())
    }

    @DebugSignal
    val debug   = IO(new DebugPort)
    

    /*
    * Module I/O:
    *
    * @io input     valid       : CHI Opcode Input Valid.
    * @io input     opcode      : CHI Opcode Input.
    * @io output    decoded     : CHI Decoded Onehot Output.
    */
    val io = IO(new Bundle {
        // opcode input
        val valid       = Input(Bool())
        val opcode      = Input(UInt(paramOpcodeWidth.W))

        // decoded output
        val decoded     = Output(Vec(paramDecodedWidth, Bool()))
    })


    // default value and logic wires for opcode decoding
    protected val seqLogicDecoded   = Seq.fill(io.decoded.length)(Wire(Bool()))

    (0 until paramDecodedWidth).foreach(i => {
        seqLogicDecoded(i)  := false.B
    })

    // decoding supported CHI Opcodes
    paramOpcodeSupported.foreach(u => {

        if (u.applicable)
        {
            seqLogicDecoded(u.opcode)   := u.is(io.opcode, io.valid)

            dontTouch(
                seqLogicDecoded(u.opcode).suggestName(s"decoded_${u.name}"))
        }
    })

    // decoding (unsupported / all) CHI Opcodes
    protected var seqLogicUnsupported  = Seq[Bool]()

    paramOpcodeAll.foreach(u => {

        if (!paramOpcodeSupported.isEmpty)
        {
            if (u.applicable && !paramOpcodeSupported.contains(u))
            {
                seqLogicDecoded(u.opcode)   := u.is(io.opcode, io.valid)

                if (paramEnableUnsupportedCheck)
                    assert(!seqLogicDecoded(u.opcode),
                        s"Unsupported CHI Opcode: ${u.name} (0x${u.opcode.toHexString})")

                dontTouch(
                    seqLogicDecoded(u.opcode).suggestName(s"decoded_${u.name}_UNSUPPORTED"))

                seqLogicUnsupported
                    = seqLogicUnsupported :+ seqLogicDecoded(u.opcode)
            }
        }
        else
        {
            if (u.applicable)
            {
                seqLogicDecoded(u.opcode)   := u.is(io.opcode, io.valid)

                dontTouch(
                    seqLogicDecoded(u.opcode).suggestName(s"decoded_${u.name}"))
            }
        }
    })

    // decoding unknown CHI Opcodes
    protected var seqLogicUnknown   = Seq[Bool]()

    (0 until seqLogicDecoded.length).foreach(i => {
        if (!paramOpcodeAll.map(u => u.applicable && u.is(i)).reduce(_ || _))
        {
            seqLogicDecoded(i)  := 
                // with X-state propagation compatibility
                Mux(io.valid, io.opcode === i.U, dontTouch(WireInit(false.B)))

            assert(!seqLogicDecoded(i),
                s"Unknown CHI Opcode: 0x${i.toHexString}")

            dontTouch(
                seqLogicDecoded(i).suggestName(s"decoded_${i.toHexString}_UNKNOWN"))

            seqLogicUnknown
                = seqLogicUnknown :+ seqLogicDecoded(i)
        }
    })

    
    // decoded output
    seqLogicDecoded.zipWithIndex.foreach(u => {
        io.decoded(u._2)    := u._1
    })

    
    // debug output
    if (seqLogicUnsupported.isEmpty)
        debug.OpcodeUnsupported  := false.B
    else
        debug.OpcodeUnsupported  := seqLogicUnsupported.reduce(_ || _)

    if (seqLogicUnknown.isEmpty)
        debug.OpcodeUnknown      := false.B
    else
        debug.OpcodeUnknown      := seqLogicUnknown.reduce(_ || _)


    // utility functions
    def is(opcode: CHIOpcode): Bool =
        if (opcode.applicable) io.decoded(opcode.opcode) else false.B

    def is(opcode0: CHIOpcode, opcodes: CHIOpcode*): Bool = {
        (opcodes :+ opcode0).map(opcode => {
            if (opcode.applicable) io.decoded(opcode.opcode) else false.B
        }).asUInt.orR
    }
}
