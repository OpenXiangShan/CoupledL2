package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters


class L2ClientTable()(implicit val p: Parameters) extends Module with HasL2Params {

    val theOnlyDCacheNID = 0

    val io = IO(new Bundle {
        val query0 = Input(Vec(paramL2.mshrSize, UInt(8.W))) // TODO: configurable with upstream nodeId width
        val clients0 = Output(Vec(paramL2.mshrSize, Vec(1, Bool()))) // TODO: parameterize with coherent l2 client count

        val query1 = Input(Vec(paramL2.mshrSize, UInt(8.W))) // TODO: configurable with upstream nodeId width
        val clients1 = Output(Vec(paramL2.mshrSize, Vec(1, Bool()))) // TODO: parameterize with coherent l2 client count
    })

    io.query0.zip(io.clients0).foreach { case (query, client) => {
        client := query === theOnlyDCacheNID.U
    }}

    io.query1.zip(io.clients1).foreach { case (query, client) => {
        client := query === theOnlyDCacheNID.U
    }}
}
