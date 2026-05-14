package oceanus.l2

import chisel3._
import chisel3.util._
import chisel3.experimental._

object whenOpt {

  def apply(enable: Boolean = true)(
    cond: => Bool
  )(block: => Any)(
    implicit sourceInfo: SourceInfo
  ): OptionWhenContext = {
    if (enable)
      new OptionWhenContext(Some(when(cond)(block)(sourceInfo)))
    else
      new OptionWhenContext(None)
  }

  def apply()(
    cond: => Bool
  )(block: => Any)(
    implicit sourceInfo: SourceInfo
  ): OptionWhenContext = {
    apply(true)(cond)(block)(sourceInfo)
  }
}

class OptionWhenContext(private val _when: Option[WhenContext]) {

  def elsewhenOpt(enable: Boolean)(
    elseCond: => Bool
  )(block: => Any)(
    implicit sourceInfo: SourceInfo
  ): OptionWhenContext = {
    if (enable) {
      _when match {
        case Some(_w) => new OptionWhenContext(Some(_w.elsewhen(elseCond)(block)(sourceInfo)))
        case None => new OptionWhenContext(Some(when(elseCond)(block)(sourceInfo)))
      }
    } else this
  }

  def elsewhen(
    elseCond: => Bool
  )(block: => Any)(
    implicit sourceInfo: SourceInfo
  ): OptionWhenContext = {
    elsewhenOpt(true)(elseCond)(block)(sourceInfo)
  }

  def otherwiseOpt(enable: Boolean)(block: => Any)(implicit sourceInfo: SourceInfo): Unit = {
    if (enable) {
      _when match {
        case Some(_w) => _w.otherwise(block)(sourceInfo)
        case None => when(true.B)(block)(sourceInfo)
      }
    }
  }

  def otherwise(block: => Any)(implicit sourceInfo: SourceInfo): Unit = {
    otherwiseOpt(true)(block)(sourceInfo)
  }
}
