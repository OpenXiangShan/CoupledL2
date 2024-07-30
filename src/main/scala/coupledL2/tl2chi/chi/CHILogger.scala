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
import freechips.rocketchip.util._
import scala.collection.immutable.{ListMap, SeqMap}
import utility.ChiselDB
import CHIOpcode._

class CHILogger(name: String, enable: Boolean, flit_as_is: Boolean = false)
               (implicit val p: Parameters) extends Module with HasCHIMsgParameters {

  val io = IO(new Bundle() {
    val up = Flipped(new PortIO)
    val down = new PortIO
  })
  io.down <> io.up
  track(io.down, this.clock, this.reset)(name)

  // packed_flit = true: just dump the whole flit as UInt
  // packed_flit = false: separate every field
  val packed_flit = flit_as_is

  // ** !! gather all distinct fields from CHI bundles !! **
  val all_bundles = Seq(new CHIREQ, new CHIRSP, new CHIDAT, new CHISNP)
  val all_fields = all_bundles.map(_.elements).reduce(_.concat(_))
  // 1. ORDER is SQL key word, avoid it
  // 2. opcode width differs among channels, use Max of them
  val all_fields_sub = all_fields.removedAll(Seq("order", "opcode"))
  val all_fields_plus = ListMap.from(all_fields_sub) ++ ListMap("channel" -> UInt(3.W)) ++
    ListMap("ordering" -> UInt(3.W)) ++ ListMap("opcode" -> UInt(OPCODE_WIDTH.W))

  val max_flit_width = all_bundles.map(ChannelIO(_).flit.getWidth).max
  if(packed_flit) { println(s"max_flit_width: ${max_flit_width}") }
  val all_fields_packed = ListMap("channel" -> UInt(3.W), "flitAll" -> UInt(max_flit_width.W))

  val all_fields_final = {
    if (packed_flit) all_fields_packed else all_fields_plus
  }

  // use RecordMap to generate a Record from ListMap
  val CHILogMessage = RecordMap(all_fields_final)
  val table = ChiselDB.createTable("CHILog", CHILogMessage, basicDB = true)
  // println(s"test: ${CHILogMessage.elements}")

  def track(out: PortIO, clock: Clock, reset: Reset)(name: String) = {
    val txreq = out.tx.req  // channel = 0
    val rxrsp = out.rx.rsp  // channel = 1
    val rxdat = out.rx.dat  // channel = 2
    val rxsnp = out.rx.snp  // channel = 3
    val txrsp = out.tx.rsp  // channel = 4
    val txdat = out.tx.dat  // channel = 5
    val all_chns = Seq(txreq, rxrsp, rxdat, rxsnp, txrsp, txdat)

    // ======== log ========
    val txreq_log, rxrsp_log, rxdat_log, rxsnp_log, txrsp_log, txdat_log = WireInit(0.U.asTypeOf(CHILogMessage))
    val all_logs = Seq(txreq_log, rxrsp_log, rxdat_log, rxsnp_log, txrsp_log, txdat_log)

    if (!packed_flit) {
      val txreq_flit = WireInit(0.U.asTypeOf(new CHIREQ))
      val rxrsp_flit = WireInit(0.U.asTypeOf(new CHIRSP))
      val rxdat_flit = WireInit(0.U.asTypeOf(new CHIDAT))
      val rxsnp_flit = WireInit(0.U.asTypeOf(new CHISNP))
      val txrsp_flit = WireInit(0.U.asTypeOf(new CHIRSP))
      val txdat_flit = WireInit(0.U.asTypeOf(new CHIDAT))
      val all_flits = Seq(txreq_flit, rxrsp_flit, rxdat_flit, rxsnp_flit, txrsp_flit, txdat_flit)

      // ** parse flit into CHI bundles **
      all_flits.zip(all_chns).map {
        case (flit, chn) =>
          var lsb = 0
          flit.getElements.reverse.foreach { case e =>
            e := chn.flit(lsb + e.asUInt.getWidth - 1, lsb).asTypeOf(e.cloneType)
            lsb += e.asUInt.getWidth
          }
      }

      // only txreq and txsnp contains addr
      // therefore we need to store addrs for their responses to match
      val txreq_addrs = Reg(Vec((1 << TXNID_WIDTH), UInt(ADDR_WIDTH.W)))
      val rxsnp_addrs = Reg(Vec((1 << TXNID_WIDTH), UInt(ADDR_WIDTH.W)))
      val dbid_addrs = Reg(Vec((1 << DBID_WIDTH), UInt(ADDR_WIDTH.W)))

      // Here are some cases:
      // read (Normal): req(TxnID)->comp(TxnID); comp(DBID)->ack(TxnID)
      // read (DMT): TODO
      // read (DCT): RN0:req(TxnID)->RN1:SnpFwd(FwdTxnID)->RN0:CompData(TxnID); comp(DBID)->ack(TxnID)
      //                                 SnpFwd(TxnID)   ->RN0:CompData(DBID) √
      //                                 SnpFwd(TxnID)   ->ICN:SnpResp[Data]Fwded(TxnID)
      // write (CopyBack): req(TxnID)->compDBID(TxnID); compDBID(DBID)->data(TxnID)
      // write (NCb): TODO
      // WriteNoSnp (combined resp): same as write (CopyBack)
      // WriteNoSnp (separate resp): req(TxnID)->DBID(TxnID); DBID(DBID)->data(TxnID);
      //                             req(TxnID)|DBID(DBID)->Comp(TxnID|DBID)
      // dataless: same as read (Normal)
      // Snoop: snp(TxnID)->resp(TxnID)

      all_logs.zip(all_flits).map {
        case (log, flit) =>
          log.elements.foreach {
            case (name, data) =>
              data := flit.elements.getOrElse(name, 0.U).asTypeOf(data)
          }
      }

      // ** overwrite some special fields **
      txreq_log.elements("ordering") := txreq_flit.order

      // ** Address **
      txreq_log.elements("addr") := txreq_flit.addr
      // rxrsp may be Comp or CompDBIDResp (or RetryAck)
      rxrsp_log.elements("addr") := txreq_addrs(rxrsp_flit.txnID)
      // rxdat is CompData alone
      rxdat_log.elements("addr") := txreq_addrs(rxdat_flit.txnID)

      val rxsnp_addr_full = Cat(rxsnp_flit.addr, 0.U(3.W)) // Snp addr is [PA_MSB-1:3]
      rxsnp_log.elements("addr") := rxsnp_addr_full
      // txrsp may be CompAck or SnpResp[Fwded]
      txrsp_log.elements("addr") := Mux(txrsp_flit.opcode === RSPOpcodes.CompAck, dbid_addrs(txrsp_flit.txnID), rxsnp_addrs(txrsp_flit.txnID))
      // txdat may be SnpResp[Fwded]Data or CompData(DCT) CbWriteData (or NcbWriteData TODO)
      txdat_log.elements("addr") := Mux(txdat_flit.opcode === DATOpcodes.CopyBackWrData, dbid_addrs(txdat_flit.txnID),
        Mux(txdat_flit.opcode === DATOpcodes.CompData, rxsnp_addrs(txdat_flit.dbID), rxsnp_addrs(txdat_flit.txnID)))

      // ======== record addrs at Reqs ========
      when(txreq.flitv) {
        txreq_addrs(txreq_flit.txnID) := txreq_flit.addr
      }
      when(rxsnp.flitv) {
        rxsnp_addrs(rxsnp_flit.txnID) := rxsnp_addr_full
      }
      when(rxrsp.flitv && (rxrsp_flit.opcode === RSPOpcodes.CompDBIDResp || rxrsp_flit.opcode === RSPOpcodes.Comp)) {
        val addr = txreq_addrs(rxrsp_flit.txnID)
        dbid_addrs(rxrsp_flit.dbID) := addr
      }
      when(rxdat.flitv && rxdat_flit.opcode === DATOpcodes.CompData) {
        val addr = txreq_addrs(rxdat_flit.txnID)
        dbid_addrs(rxdat_flit.dbID) := addr
      }
    }
    // ** packed flit **
    else {
      all_logs.zip(all_chns).map {
        case (log, chn) =>
          log.elements("flitAll") := chn.flit
      }
    }

    all_logs.zipWithIndex.map {
      case (log, i) => log.elements("channel") := i.U
    }

    // TODO: consider link active??
    table.log(txreq_log, txreq.flitv, name, clock, reset)
    table.log(rxrsp_log, rxrsp.flitv, name, clock, reset)
    table.log(rxdat_log, rxdat.flitv, name, clock, reset)
    table.log(rxsnp_log, rxsnp.flitv, name, clock, reset)
    table.log(txrsp_log, txrsp.flitv, name, clock, reset)
    table.log(txdat_log, txdat.flitv, name, clock, reset)
  }
}

object CHILogger {
  
  def apply(name: String, enable: Boolean = true)(implicit p: Parameters) = {
    val logger = Module(new CHILogger(name, enable))
    logger
  }
}
