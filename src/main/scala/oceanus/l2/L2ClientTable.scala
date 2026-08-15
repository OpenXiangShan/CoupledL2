package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters


class L2ClientTable(val bank: Int)(implicit val p: Parameters) extends Module with HasL2Params {

  val theOnlyDCacheNID = 0

  val io = IO(new Bundle {
    val queryREQ = Input(Vec(bank, Vec(paramL2.mshrSize, UInt(8.W)))) // TODO: configurable with upstream nodeId width
    val clientsREQ = Output(Vec(bank, Vec(paramL2.mshrSize, Vec(1, Bool())))) // TODO: parameterize with coherent l2 client count

    val queryEVT = Input(Vec(bank, Vec(paramL2.mshrSize, UInt(8.W)))) // TODO: configurable with upstream nodeId width
    val clientsEVT = Output(Vec(bank, Vec(paramL2.mshrSize, Vec(1, Bool())))) // TODO: parameterize with coherent l2 client count
  })

  io.queryREQ.zip(io.clientsREQ).foreach { case (query, clients) => {
    query.zip(clients).foreach { case (query, clients) =>
      clients.head := query === theOnlyDCacheNID.U
  }}}

  io.queryEVT.zip(io.clientsEVT).foreach { case (query, clients) => {
    query.zip(clients).foreach { case (query, clients) =>
      clients.head := query === theOnlyDCacheNID.U
  }}}
}
