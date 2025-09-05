package coupledL2

import chisel3._
import chisel3.util._

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

object DebugAccumulate {
//  def applyImpl(c: blackbox.Context)(signal: c.Expr[Bool]): c.Expr[UInt] = {
//    import c.universe._
//
//    val signalName = signal.tree match {
//      case Ident(name) => name.decodedName.toString
//      case Select(_, name) => name.decodedName.toString
//      case _ => c.abort(c.enclosingPosition, "Argument must be a simple identifier")
//    }
//
//    val result = q"""
//      {
//        val counter = RegInit(0.U(64.W))
//        counter.suggestName(${signalName} + "_times")
//        dontTouch(counter)
//
//        when($signal) {
//          counter := counter + 1.U
//        }
//
//        counter
//      }
//    """
//
//    c.Expr[UInt](result)
//  }
//
//  def apply(signal: Bool): UInt = macro applyImpl
  def apply(signal: Bool, name: String) = {
    val counter = RegInit(0.U(64.W))
    counter.suggestName(s"${name}_times")
    dontTouch(counter)

    when(signal) {
      counter := counter + 1.U
    }

    counter
  }
}
