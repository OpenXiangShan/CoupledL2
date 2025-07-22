package oceanus.l2

import chisel3._
import chisel3.util._
import chisel3.experimental.AffectsChiselPrefix
import utility.ParallelOR
import utility.ParallelMux
import scala.collection.mutable.ArrayBuffer

class L2MSHRState[T <: Data](initial: T, singleton: Boolean = false) extends AffectsChiselPrefix {

    var nextValids: Seq[Bool] = Seq()
    var nextValues: Seq[T] = Seq()

    private val _next = Wire(Valid(chiselTypeOf(initial)))

    val bits = RegInit(initial)
    val next = Mux(_next.valid, _next.bits, bits)
    
    bits := next

    private def rebuild = {
        _next.valid := ParallelOR(nextValids)
        _next.bits := ParallelMux(nextValids.zip(nextValues))
        if (singleton) assert(PopCount(nextValids) <= 1.U, s"multiple valid next state on L2 MSHR state ${bits.circuitName}")
    }

    def next(valid: Bool, value: T) = {
        nextValids = nextValids :+ valid
        nextValues = nextValues :+ value
        rebuild
    }

    def nextInitial(valid: Bool) = {
        nextValids = nextValids :+ Wire(Bool())
        nextValids.last := valid
        nextValues = nextValues :+ Wire(chiselTypeOf(initial))
        nextValues.last := initial
        rebuild
    }

    def nextHold = nextInitial(false.B)

    def nextInWhen(value: T) = {
        nextValids.last := true.B
        nextValues.last := value
    }

    def isNext(value: T) = next.asUInt === value.asUInt
    def isTransition(value: T) = isNext(value) && bits.asUInt =/= value.asUInt
    def isNextInitial = isNext(initial)
    def isTransitionInitial = isTransition(initial)
}

object L2MSHRState {
    def apply[T <: Data](initial: T, categoryLists: ArrayBuffer[L2MSHRState[_]]*) = {
        val inst = new L2MSHRState(initial)
        categoryLists.foreach(_.addOne(inst))
        inst
    }
}
