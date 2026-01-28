package oceanus.compactchi

import chisel3._
import chisel3.util._

class FlitEVT extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L1 parameter
  val Opcode = UInt(1.W)
  val Addr = UInt(48.W)
  val NS = Bool()
  val WayValid = Bool()
  val Way = UInt(2.W) // TODO: configured by L2 way count
  val TraceTag = UInt(1.W)
}

class FlitREQ extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L1 parameter
  val Opcode = UInt(6.W) // TODO: variable width between different types of components
  val Size = UInt(3.W)
  val Addr = UInt(48.W)
  val NS = Bool()
  val Order = UInt(2.W)
  val MemAttr = UInt(4.W)
  val Excl = Bool()
  def ExpCompStash = Excl
  val WayValid = Bool()
  val Way = UInt(2.W) // TODO: configured by L2 way count
  val TraceTag = UInt(1.W)
}

class FlitSNP extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L2 parameter
  val Opcode = UInt(2.W) // TODO: variable width between different types of components
  val Addr = UInt((48 - 3).W)
  val NS = Bool()
  val TraceTag = UInt(1.W)
}

class FlitDnRSP extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L1 parameter
  val DBID = UInt(8.W) // TODO: configured by L2 parameter
  val Opcode = UInt(3.W) // TODO: variable width between different types of components
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val CBusy = UInt(3.W)
  val WayValid = Bool()
  val Way = UInt(2.W) // TODO: configured by L2 way count
  val TraceTag = UInt(1.W)
}

class FlitUpRSP extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L2 parameter
  val Opcode = UInt(1.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val TraceTag = UInt(1.W)
}

class FlitDnDATWithoutData extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L1 parameter
  val DBID = UInt(8.W) // TODO: configured by L2 parameter
  val Opcode = UInt(1.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val DataSource = UInt(5.W)
  val CBusy = UInt(3.W)
  val WayValid = Bool()
  val Way = UInt(2.W) // TODO: configured by L2 way count
  val TraceTag = UInt(1.W)
}

class FlitDnDAT extends FlitDnDATWithoutData {
  val Data = UInt(256.W)
}

class FlitUpDATWithoutData extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by maximum value of L1 parameter and L2 parameter
  val Opcode = UInt(2.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val TraceTag = UInt(1.W)
}

class FlitUpDAT extends FlitUpDATWithoutData {
  val Data = UInt(256.W)
  val BE = UInt(32.W)
}
