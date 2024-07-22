/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package coupledL2.tl2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import utility.ChiselDB
import CHIOpcode._

class CHILogMessage extends Bundle with HasCHIMsgParameters  {
  val channel = UInt(3.W)
  val address = UInt(ADDR_WIDTH.W)
  val opcode = UInt(OPCODE_WIDTH.W)

  // TODO: just pack everything into here for now, consider split? by script when anaylze
  // Since DB entry format must be unified, we just use UInt(widestWidth.W) for all channels
  val flit = UInt(400.W) // TODOTODO! check width

  // TODO: .elements.filterNot(_._1 == "data")
  // this can be used to exclude data from flit
}

class CHILogger(name: String, enable: Boolean)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val up = Flipped(new PortIO)
    val down = new PortIO
  })
  io.down <> io.up
  CHILogger.track(io.down, this.clock, this.reset)(name)
  dontTouch(io) // TODOTODO
}

object CHILogger extends HasCHIMsgParameters {

  val table = ChiselDB.createTable("CHILog", new CHILogMessage, basicDB = true)

  def track(out: PortIO, clock: Clock, reset: Reset)(name: String) = {
    // only txreq and txsnp contains addr
    // therefore we need to store addrs for their responses to match
    val txreq_addrs = Reg(Vec((1 << TXNID_WIDTH), UInt(ADDR_WIDTH.W)))
    val rxsnp_addrs = Reg(Vec((1 << TXNID_WIDTH), UInt(ADDR_WIDTH.W)))
    val dbid_addrs = Reg(Vec((1 << DBID_WIDTH), UInt(ADDR_WIDTH.W)))
    // Here are some cases:
    // read (Normal): req(TxnID)->comp(TxnID); comp(DBID)->ack(TxnID)
    // read (DMT): TODO
    // read (DCT): TODO
    // write (CopyBack): req(TxnID)->compDBID(TxnID); compDBID(DBID)->data(TxnID)
    // write (NCb): TODO
    // WriteNoSnp (combined resp): same as write (CopyBack)
    // WriteNoSnp (separate resp): TODO
    // dataless: same as read (Normal)
    // Snoop: snp(TxnID)->resp(TxnID)

    val txreq = out.tx.req  // channel = 0
    val rxrsp = out.rx.rsp  // channel = 1
    val rxdat = out.rx.dat  // channel = 2
    val rxsnp = out.rx.snp  // channel = 3
    val txrsp = out.tx.rsp  // channel = 4
    val txdat = out.tx.dat  // channel = 5
    val all_chns = Seq(txreq, rxrsp, rxdat, rxsnp, txrsp, txdat)

    val txreq_flit = WireInit(0.U.asTypeOf(new CHIREQ))
    val rxrsp_flit = WireInit(0.U.asTypeOf(new CHIRSP))
    val rxdat_flit = WireInit(0.U.asTypeOf(new CHIDAT))
    val rxsnp_flit = WireInit(0.U.asTypeOf(new CHISNP))
    val txrsp_flit = WireInit(0.U.asTypeOf(new CHIRSP))
    val txdat_flit = WireInit(0.U.asTypeOf(new CHIDAT))
    val all_flits = Seq(txreq_flit, rxrsp_flit, rxdat_flit, rxsnp_flit, txrsp_flit, txdat_flit)
    // TODO:? does this module work for txreq/txrsp/txdat?

    all_flits.zip(all_chns).map {
      case (flit, chn) =>
        var lsb = 0
        flit.getElements.reverse.foreach { case e =>
          e := chn.flit(lsb + e.asUInt.getWidth - 1, lsb).asTypeOf(e.cloneType)
          lsb += e.asUInt.getWidth
        }
    }
    
    val txreq_log, rxrsp_log, rxdat_log, rxsnp_log, txrsp_log, txdat_log = WireInit(0.U.asTypeOf(new CHILogMessage))
    val all_logs = Seq(txreq_log, rxrsp_log, rxdat_log, rxsnp_log, txrsp_log, txdat_log)

    // ======== log entry assignment ========
    all_logs.zipWithIndex.map {
      case (log, i) => log.channel := i.U
    }

    all_logs.zip(all_flits).map {
      case (log, flit) => log.opcode := flit.elements.filter(_._1 == "opcode").head._2
    }


    txreq_log.address := txreq_flit.addr
    // rxrsp may be Comp or CompDBIDResp (or RetryAck)
    rxrsp_log.address := txreq_addrs(rxrsp_flit.txnID)
    // rxdat is CompData alone
    rxdat_log.address := txreq_addrs(rxdat_flit.txnID)

    val rxsnp_addr_full = Cat(rxsnp_flit.addr, 0.U(3.W)) // Snp addr is [PA_MSB-1:3]
    rxsnp_log.address := rxsnp_addr_full
    // txrsp may be SnpResp or CompAck (or SnpRespFwded: TODO)
    txrsp_log.address := Mux(txrsp_flit.opcode === RSPOpcodes.CompAck, dbid_addrs(txrsp_flit.txnID), rxsnp_addrs(txrsp_flit.txnID))
    // txdat may be SnpRespData or CbWriteData (or NcbWriteData or SnpRespDataFwded:TODO)
    txdat_log.address := Mux(txdat_flit.opcode === DATOpcodes.CopyBackWrData, dbid_addrs(txdat_flit.txnID), rxsnp_addrs(txdat_flit.txnID))

    all_logs.zip(all_chns).map {
      case (log, chn) => log.flit := chn.flit
    }

    // ======== record addrs at Reqs ========
    when(txreq.flitv) {
      txreq_addrs(txreq_flit.txnID) := txreq_flit.addr
    }
    when(rxsnp.flitv) {
      rxsnp_addrs(rxsnp_flit.txnID) := rxsnp_addr_full
    }
    // TODO: record CompData/Comp for CompAck?
    when(rxrsp.flitv && (rxrsp_flit.opcode === RSPOpcodes.CompDBIDResp || rxrsp_flit.opcode === RSPOpcodes.Comp)) {
      val addr = txreq_addrs(rxrsp_flit.txnID)
      dbid_addrs(rxrsp_flit.dbID) := addr
    }
    when(rxdat.flitv && rxdat_flit.opcode === DATOpcodes.CompData) {
      val addr = txreq_addrs(rxdat_flit.txnID)
      dbid_addrs(rxdat_flit.dbID) := addr
    }

    // TODO: only record certain reqs? or keep them all (CompAck, PCrdGrant)

    // TODO: link active??
    table.log(txreq_log, txreq.flitv, name, clock, reset)
    table.log(rxrsp_log, rxrsp.flitv, name, clock, reset)
    table.log(rxdat_log, rxdat.flitv, name, clock, reset)
    table.log(rxsnp_log, rxsnp.flitv, name, clock, reset)
    table.log(txrsp_log, txrsp.flitv, name, clock, reset)
    table.log(txdat_log, txdat.flitv, name, clock, reset)

  }

  def apply(name: String, enable: Boolean = true)(implicit p: Parameters) = {
    val logger = Module(new CHILogger(name, enable))
    logger
  }
}
