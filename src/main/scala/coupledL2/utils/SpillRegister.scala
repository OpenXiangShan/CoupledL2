package coupledL2.utils

import chisel3._
import chisel3.util._


/*
* Spill Register for pipelining ready signals.
*/
object SpillRegister {

    def apply[T <: Data](gen: T) = {
        require(!gen.isInstanceOf[ReadyValidIO[?]],
            "use 'applyReadyValid(...)' instead when passing in ReadyValidIO")
        Module(new SpillRegister[T, T](gen))
    }

    def applyReadyValid[D <: Data](gen: ReadyValidIO[D]) = {
        Module(new SpillRegister[D, ReadyValidIO[D]](gen))
    }

    def attachReadyValid[D <: ReadyValidIO[D]](in: ReadyValidIO[D], out: ReadyValidIO[D]) = {
        val uSpillRegister = Module(new SpillRegister(
            chiselTypeOf(out)
        ))
        uSpillRegister.io.in    <> in
        uSpillRegister.io.out   <> out
        uSpillRegister
    }

    def attachIn[T <: ReadyValidIO[Data]](in: T): T = {
        val uSpillRegister = Module(new SpillRegister(
            chiselTypeOf(in)
        ))
        uSpillRegister.io.in    <> in
        uSpillRegister.io.out.asInstanceOf[T]
    }

    def attachOut[T <: ReadyValidIO[Data]](out: T): T = {
        val uSpillRegister = Module(new SpillRegister(
            chiselTypeOf(out)
        ))
        uSpillRegister.io.out   <> out
        uSpillRegister.io.in.asInstanceOf[T]
    }
}

class SpillRegister[+D <: Data, +T <: Data](gen: T) extends Module {

    /*
    * Module I/O
    * 
    * @io   input       in  : Input Decoupled Channel, ready-valid.
    * @io   output      out : Output Decoupeld Channel, ready-valid.
    */
    val io = IO(new Bundle {
        // upstream input
        val in              : ReadyValidIO[D] = {
            if (gen.isInstanceOf[ReadyValidIO[Data]])
                Flipped(gen.asInstanceOf[Data]).asInstanceOf[ReadyValidIO[D]]
            else
                Flipped(Decoupled(gen)).asInstanceOf[ReadyValidIO[D]]
        }

        // downstream output
        val out             : ReadyValidIO[D] = {
            if (gen.isInstanceOf[ReadyValidIO[Data]])
                Flipped(Flipped(gen.asInstanceOf[ReadyValidIO[D]]))
            else
                Flipped(Flipped(Decoupled(gen).asInstanceOf[ReadyValidIO[D]]))
        }
    })

    
    //
    protected def extractDataType: Data   = chiselTypeOf(io.out.bits)

    
    // Spill Registers
    protected val reg1stValid   = RegInit(init = false.B)
    protected val reg1stData    = Reg(extractDataType)

    protected val reg2ndValid   = RegInit(init = false.B)
    protected val reg2ndData    = Reg(extractDataType)

    when (io.out.ready) {

        when (!reg2ndValid) {
            reg1stValid := false.B
        }

        reg2ndValid := false.B
    }

    when (io.in.fire) {
        reg1stValid := true.B
        reg1stData  := io.in.bits

        when (!io.out.ready) {
            reg2ndValid := reg1stValid
            reg2ndData  := reg1stData
        }
    }

    
    // output logic
    io.in.ready     := !reg2ndValid

    io.out.valid    := reg1stValid | reg2ndValid
    io.out.bits     := Mux(reg2ndValid, reg2ndData, reg1stData)
}
