/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package coupledL2.tl2chi

import chisel3._
import chisel3.util._

trait HasCHIOpcodes extends HasCHIMsgParameters {

  def Eb_OPCODE(opcode: UInt, width: Int): UInt = {
    require(
      opcode.getWidth <= width && issue.compareTo(Issue.Eb) >= 0,
      s"Illegal opcode of issue ${issue}, please use onIssueXXOrElse or ifIssueXX."
    )
    opcode(width - 1, 0)
  }

  def onIssueEbOrElse[T <: Data](block: => T, otherwise: => T) = {
    if (issue.compareTo(Issue.Eb) >= 0) block
    else otherwise
  }

  def ifIssueEb(block: => Any) = {
    if (issue.compareTo(Issue.Eb) >= 0) block
  }

  /**
    * REQ
    */
  def ReqLCrdReturn         = 0x00.U(REQ_OPCODE_WIDTH.W)
  def ReadShared            = 0x01.U(REQ_OPCODE_WIDTH.W)
  def ReadClean             = 0x02.U(REQ_OPCODE_WIDTH.W)
  def ReadOnce              = 0x03.U(REQ_OPCODE_WIDTH.W)
  def ReadNoSnp             = 0x04.U(REQ_OPCODE_WIDTH.W)
  def PCrdReturn            = 0x05.U(REQ_OPCODE_WIDTH.W)

  def ReadUnique            = 0x07.U(REQ_OPCODE_WIDTH.W)
  def CleanShared           = 0x08.U(REQ_OPCODE_WIDTH.W)
  def CleanInvalid          = 0x09.U(REQ_OPCODE_WIDTH.W)
  def MakeInvalid           = 0x0A.U(REQ_OPCODE_WIDTH.W)
  def CleanUnique           = 0x0B.U(REQ_OPCODE_WIDTH.W)
  def MakeUnique            = 0x0C.U(REQ_OPCODE_WIDTH.W)
  def Evict                 = 0x0D.U(REQ_OPCODE_WIDTH.W)

  def DVMOp                 = 0x14.U(REQ_OPCODE_WIDTH.W)
  def WriteEvictFull        = 0x15.U(REQ_OPCODE_WIDTH.W)

  def WriteCleanFull        = 0x17.U(REQ_OPCODE_WIDTH.W)
  def WriteUniquePtl        = 0x18.U(REQ_OPCODE_WIDTH.W)
  def WriteUniqueFull       = 0x19.U(REQ_OPCODE_WIDTH.W)
  def WriteBackPtl          = 0x1A.U(REQ_OPCODE_WIDTH.W)
  def WriteBackFull         = 0x1B.U(REQ_OPCODE_WIDTH.W)
  def WriteNoSnpPtl         = 0x1C.U(REQ_OPCODE_WIDTH.W)
  def WriteNoSnpFull        = 0x1D.U(REQ_OPCODE_WIDTH.W)

  def WriteUniqueFullStash  = 0x20.U(REQ_OPCODE_WIDTH.W)
  def WriteUniquePtlStash   = 0x21.U(REQ_OPCODE_WIDTH.W)
  def StashOnceShared       = 0x22.U(REQ_OPCODE_WIDTH.W)
  def StashOnceUnique       = 0x23.U(REQ_OPCODE_WIDTH.W)
  def ReadOnceCleanInvalid  = 0x24.U(REQ_OPCODE_WIDTH.W)
  def ReadOnceMakeInvalid   = 0x25.U(REQ_OPCODE_WIDTH.W)
  def ReadNotSharedDirty    = 0x26.U(REQ_OPCODE_WIDTH.W)
  def CleanSharedPersist    = 0x27.U(REQ_OPCODE_WIDTH.W)

  def AtomicStore_ADD       = 0x28.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_CLR       = 0x29.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_EOR       = 0x2A.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_SET       = 0x2B.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_SMAX      = 0x2C.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_SMIN      = 0x2D.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_UMAX      = 0x2E.U(REQ_OPCODE_WIDTH.W)
  def AtomicStore_UMIN      = 0x2F.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_ADD        = 0x30.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_CLR        = 0x31.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_EOR        = 0x32.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_SET        = 0x33.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_SMAX       = 0x34.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_SMIN       = 0x35.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_UMAX       = 0x36.U(REQ_OPCODE_WIDTH.W)
  def AtomicLoad_UMIN       = 0x37.U(REQ_OPCODE_WIDTH.W)
  def AtomicSwap            = 0x38.U(REQ_OPCODE_WIDTH.W)
  def AtomicCompare         = 0x39.U(REQ_OPCODE_WIDTH.W)
  def PrefetchTgt           = 0x3A.U(REQ_OPCODE_WIDTH.W)

  def WriteEvictOrEvict     = Eb_OPCODE(0x42.U, REQ_OPCODE_WIDTH)

  /**
    * RSP
    */
  def RespLCrdReturn  = 0x0.U(RSP_OPCODE_WIDTH.W)
  def SnpResp         = 0x1.U(RSP_OPCODE_WIDTH.W)
  def CompAck         = 0x2.U(RSP_OPCODE_WIDTH.W)
  def RetryAck        = 0x3.U(RSP_OPCODE_WIDTH.W)
  def Comp            = 0x4.U(RSP_OPCODE_WIDTH.W)
  def CompDBIDResp    = 0x5.U(RSP_OPCODE_WIDTH.W)
  def DBIDResp        = 0x6.U(RSP_OPCODE_WIDTH.W)
  def PCrdGrant       = 0x7.U(RSP_OPCODE_WIDTH.W)
  def ReadReceipt     = 0x8.U(RSP_OPCODE_WIDTH.W)
  def SnpRespFwded    = 0x9.U(RSP_OPCODE_WIDTH.W)
  // E.b
  def RespSepData     = Eb_OPCODE(0xB.U, RSP_OPCODE_WIDTH)
  def DBIDRespOrd     = Eb_OPCODE(0xE.U, RSP_OPCODE_WIDTH)

  /**
    * SNP
    */
  def SnpLCrdReturn         = 0x00.U(SNP_OPCODE_WIDTH.W)
  def SnpShared             = 0x01.U(SNP_OPCODE_WIDTH.W)
  def SnpClean              = 0x02.U(SNP_OPCODE_WIDTH.W)
  def SnpOnce               = 0x03.U(SNP_OPCODE_WIDTH.W)
  def SnpNotSharedDirty     = 0x04.U(SNP_OPCODE_WIDTH.W)
  def SnpUniqueStash        = 0x05.U(SNP_OPCODE_WIDTH.W)
  def SnpMakeInvalidStash   = 0x06.U(SNP_OPCODE_WIDTH.W)
  def SnpUnique             = 0x07.U(SNP_OPCODE_WIDTH.W)
  def SnpCleanShared        = 0x08.U(SNP_OPCODE_WIDTH.W)
  def SnpCleanInvalid       = 0x09.U(SNP_OPCODE_WIDTH.W)
  def SnpMakeInvalid        = 0x0A.U(SNP_OPCODE_WIDTH.W)
  def SnpStashUnique        = 0x0B.U(SNP_OPCODE_WIDTH.W)
  def SnpStashShared        = 0x0C.U(SNP_OPCODE_WIDTH.W)
  def SnpDVMOp              = 0x0D.U(SNP_OPCODE_WIDTH.W)

  def SnpQuery              = Eb_OPCODE(0x10.U, SNP_OPCODE_WIDTH)
  def SnpSharedFwd          = 0x11.U(SNP_OPCODE_WIDTH.W)
  def SnpCleanFwd           = 0x12.U(SNP_OPCODE_WIDTH.W)
  def SnpOnceFwd            = 0x13.U(SNP_OPCODE_WIDTH.W)
  def SnpNotSharedDirtyFwd  = 0x14.U(SNP_OPCODE_WIDTH.W)

  def SnpUniqueFwd          = 0x17.U(SNP_OPCODE_WIDTH.W)

  def isSnpXStash(opcode: UInt): Bool = {
    opcode === SnpUniqueStash || opcode === SnpMakeInvalidStash
  }

  def isSnpStashX(opcode: UInt): Bool = {
    opcode === SnpStashUnique || opcode === SnpStashShared
  }

  def isSnpXFwd(opcode: UInt): Bool = {
    opcode === SnpSharedFwd || 
    opcode === SnpCleanFwd || 
    opcode === SnpOnceFwd ||
    opcode === SnpNotSharedDirtyFwd ||
    opcode === SnpUniqueFwd
  }

  def isSnpQuery(opcode: UInt): Bool = {
    onIssueEbOrElse(opcode === SnpQuery, false.B)
  }

  def isSnpOnceX(opcode: UInt): Bool = {
    opcode === SnpOnce || opcode === SnpOnceFwd
  }

  def isSnpOnce(opcode: UInt): Bool = {
    opcode === SnpOnce
  }

  def isSnpOnceFwd(opcode: UInt): Bool = {
    opcode === SnpOnceFwd
  }

  def isSnpCleanX(opcode: UInt): Bool = {
    opcode === SnpClean || opcode === SnpCleanFwd
  }

  def isSnpSharedX(opcode: UInt): Bool = {
    opcode === SnpShared || opcode === SnpSharedFwd
  }

  def isSnpNotSharedDirtyX(opcode: UInt): Bool = {
    opcode === SnpNotSharedDirty || opcode === SnpNotSharedDirtyFwd
  }

  def isSnpToB(opcode: UInt): Bool = {
    isSnpCleanX(opcode) || isSnpSharedX(opcode) || isSnpNotSharedDirtyX(opcode)
  }

  def isSnpToN(opcode: UInt): Bool = {
    isSnpUniqueX(opcode) || opcode === SnpCleanInvalid || isSnpMakeInvalidX (opcode)
  }

  def isSnpCleanShared(opcode: UInt): Bool = {
    opcode === SnpCleanShared
  }

  def isSnpToBNonFwd(opcode: UInt): Bool = {
    opcode === SnpClean ||
    opcode === SnpNotSharedDirty ||
    opcode === SnpShared
  }

  def isSnpToBFwd(opcode: UInt): Bool = {
    opcode === SnpCleanFwd ||
    opcode === SnpNotSharedDirtyFwd ||
    opcode === SnpSharedFwd
  }

  def isSnpToNNonFwd(opcode: UInt): Bool = { 
    opcode === SnpUnique || opcode === SnpUniqueStash
  }

  def isSnpToNFwd(opcode: UInt): Bool = { 
    opcode === SnpUniqueFwd 
  }

  def isSnpUniqueX(opcode: UInt): Bool = {
    opcode === SnpUnique || opcode === SnpUniqueFwd || opcode === SnpUniqueStash
  }

  def isSnpMakeInvalidX(opcode: UInt): Bool = {
    opcode === SnpMakeInvalid || opcode === SnpMakeInvalidStash
  }

  /**
    * DAT
    */
  def DataLCrdReturn    = 0x0.U(DAT_OPCODE_WIDTH.W)
  def SnpRespData       = 0x1.U(DAT_OPCODE_WIDTH.W)
  def CopyBackWrData    = 0x2.U(DAT_OPCODE_WIDTH.W)
  def NonCopyBackWrData = 0x3.U(DAT_OPCODE_WIDTH.W)
  def CompData          = 0x4.U(DAT_OPCODE_WIDTH.W)
  def SnpRespDataPtl    = 0x5.U(DAT_OPCODE_WIDTH.W)
  def SnpRespDataFwded  = 0x6.U(DAT_OPCODE_WIDTH.W)
  def WriteDataCancel   = 0x7.U(DAT_OPCODE_WIDTH.W)
  // E.b
  def DataSepResp       = Eb_OPCODE(0xB.U, DAT_OPCODE_WIDTH)

  def isSnpRespDataX(opcode: UInt): Bool = {
    opcode === SnpRespData || opcode === SnpRespDataPtl || opcode === SnpRespDataFwded
  }
}
