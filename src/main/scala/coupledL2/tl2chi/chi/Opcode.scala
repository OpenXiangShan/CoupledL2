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

import org.chipsalliance.cde.config.Parameters

object CHIOpcode {

  object REQOpcodes {
    val width_map = Map(
      Issue.B -> 6,
      Issue.Eb -> 7,
    )

    def width(implicit p: Parameters) = width_map(p(CHIIssue))

    def ReqLCrdReturn(implicit p: Parameters)         = 0x00.U(width.W)
    def ReadShared(implicit p: Parameters)            = 0x01.U(width.W)
    def ReadClean(implicit p: Parameters)             = 0x02.U(width.W)
    def ReadOnce(implicit p: Parameters)              = 0x03.U(width.W)
    def ReadNoSnp(implicit p: Parameters)             = 0x04.U(width.W)
    def PCrdReturn(implicit p: Parameters)            = 0x05.U(width.W)

    def ReadUnique(implicit p: Parameters)            = 0x07.U(width.W)
    def CleanShared(implicit p: Parameters)           = 0x08.U(width.W)
    def CleanInvalid(implicit p: Parameters)          = 0x09.U(width.W)
    def MakeInvalid(implicit p: Parameters)           = 0x0A.U(width.W)
    def CleanUnique(implicit p: Parameters)           = 0x0B.U(width.W)
    def MakeUnique(implicit p: Parameters)            = 0x0C.U(width.W)
    def Evict(implicit p: Parameters)                 = 0x0D.U(width.W)

    def DVMOp(implicit p: Parameters)                 = 0x14.U(width.W)
    def WriteEvictFull(implicit p: Parameters)        = 0x15.U(width.W)

    def WriteCleanFull(implicit p: Parameters)        = 0x17.U(width.W)
    def WriteUniquePtl(implicit p: Parameters)        = 0x18.U(width.W)
    def WriteUniqueFull(implicit p: Parameters)       = 0x19.U(width.W)
    def WriteBackPtl(implicit p: Parameters)          = 0x1A.U(width.W)
    def WriteBackFull(implicit p: Parameters)         = 0x1B.U(width.W)
    def WriteNoSnpPtl(implicit p: Parameters)         = 0x1C.U(width.W)
    def WriteNoSnpFull(implicit p: Parameters)        = 0x1D.U(width.W)

    def WriteUniqueFullStash(implicit p: Parameters)  = 0x20.U(width.W)
    def WriteUniquePtlStash(implicit p: Parameters)   = 0x21.U(width.W)
    def StashOnceShared(implicit p: Parameters)       = 0x22.U(width.W)
    def StashOnceUnique(implicit p: Parameters)       = 0x23.U(width.W)
    def ReadOnceCleanInvalid(implicit p: Parameters)  = 0x24.U(width.W)
    def ReadOnceMakeInvalid(implicit p: Parameters)   = 0x25.U(width.W)
    def ReadNotSharedDirty(implicit p: Parameters)    = 0x26.U(width.W)
    def CleanSharedPersist(implicit p: Parameters)    = 0x27.U(width.W)

    def AtomicStore_ADD(implicit p: Parameters)       = 0x28.U(width.W)
    def AtomicStore_CLR(implicit p: Parameters)       = 0x29.U(width.W)
    def AtomicStore_EOR(implicit p: Parameters)       = 0x2A.U(width.W)
    def AtomicStore_SET(implicit p: Parameters)       = 0x2B.U(width.W)
    def AtomicStore_SMAX(implicit p: Parameters)      = 0x2C.U(width.W)
    def AtomicStore_SMIN(implicit p: Parameters)      = 0x2D.U(width.W)
    def AtomicStore_UMAX(implicit p: Parameters)      = 0x2E.U(width.W)
    def AtomicStore_UMIN(implicit p: Parameters)      = 0x2F.U(width.W)
    def AtomicLoad_ADD(implicit p: Parameters)        = 0x30.U(width.W)
    def AtomicLoad_CLR(implicit p: Parameters)        = 0x31.U(width.W)
    def AtomicLoad_EOR(implicit p: Parameters)        = 0x32.U(width.W)
    def AtomicLoad_SET(implicit p: Parameters)        = 0x33.U(width.W)
    def AtomicLoad_SMAX(implicit p: Parameters)       = 0x34.U(width.W)
    def AtomicLoad_SMIN(implicit p: Parameters)       = 0x35.U(width.W)
    def AtomicLoad_UMAX(implicit p: Parameters)       = 0x36.U(width.W)
    def AtomicLoad_UMIN(implicit p: Parameters)       = 0x37.U(width.W)
    def AtomicSwap(implicit p: Parameters)            = 0x38.U(width.W)
    def AtomicCompare(implicit p: Parameters)         = 0x39.U(width.W)
    def PrefetchTgt(implicit p: Parameters)           = 0x3A.U(width.W)
  }

  object RSPOpcodes {
    val width_map = Map(
      Issue.B -> 4,
      Issue.Eb -> 5,
    )

    def width(implicit p: Parameters) = width_map(p(CHIIssue))

    def RespLCrdReturn(implicit p: Parameters)  = 0x0.U(width.W)
    def SnpResp(implicit p: Parameters)         = 0x1.U(width.W)
    def CompAck(implicit p: Parameters)         = 0x2.U(width.W)
    def RetryAck(implicit p: Parameters)        = 0x3.U(width.W)
    def Comp(implicit p: Parameters)            = 0x4.U(width.W)
    def CompDBIDResp(implicit p: Parameters)    = 0x5.U(width.W)
    def DBIDResp(implicit p: Parameters)        = 0x6.U(width.W)
    def PCrdGrant(implicit p: Parameters)       = 0x7.U(width.W)
    def ReadReceipt(implicit p: Parameters)     = 0x8.U(width.W)
    def SnpRespFwded(implicit p: Parameters)    = 0x9.U(width.W)

    // E.b new opcodes
    def RespSepData     = 0xB.U(width_map(Issue.Eb).W)
  }

  object SNPOpcodes {
    val width_map = Map(
      Issue.B -> 5,
      Issue.Eb -> 5,
    )

    def width(implicit p: Parameters) = width_map(p(CHIIssue))

    def SnpLCrdReturn(implicit p: Parameters)         = 0x00.U(width.W)
    def SnpShared(implicit p: Parameters)             = 0x01.U(width.W)
    def SnpClean(implicit p: Parameters)              = 0x02.U(width.W)
    def SnpOnce (implicit p: Parameters)              = 0x03.U(width.W)
    def SnpNotSharedDirty(implicit p: Parameters)     = 0x04.U(width.W)
    def SnpUniqueStash(implicit p: Parameters)        = 0x05.U(width.W)
    def SnpMakeInvalidStash(implicit p: Parameters)   = 0x06.U(width.W)
    def SnpUnique(implicit p: Parameters)             = 0x07.U(width.W)
    def SnpCleanShared(implicit p: Parameters)        = 0x08.U(width.W)
    def SnpCleanInvalid(implicit p: Parameters)       = 0x09.U(width.W)
    def SnpMakeInvalid(implicit p: Parameters)        = 0x0A.U(width.W)
    def SnpStashUnique(implicit p: Parameters)        = 0x0B.U(width.W)
    def SnpStashShared(implicit p: Parameters)        = 0x0C.U(width.W)
    def SnpDVMOp(implicit p: Parameters)              = 0x0D.U(width.W)

    def SnpSharedFwd(implicit p: Parameters)          = 0x11.U(width.W)
    def SnpCleanFwd(implicit p: Parameters)           = 0x12.U(width.W)
    def SnpOnceFwd(implicit p: Parameters)            = 0x13.U(width.W)
    def SnpNotSharedDirtyFwd(implicit p: Parameters)  = 0x14.U(width.W)

    def SnpUniqueFwd(implicit p: Parameters)          = 0x17.U(width.W)

    def widthCheck(opcode: UInt)(implicit p: Parameters): Unit = { require (opcode.getWidth >= width) }

    def isSnpXStash(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpUniqueStash || opcode === SnpMakeInvalidStash
    }

    def isSnpStashX(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpStashUnique || opcode === SnpStashShared
    }

    def isSnpXFwd(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode >= SnpSharedFwd
    }


    def isSnpOnceX(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpOnce || opcode === SnpOnceFwd
    }

    def isSnpCleanX(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpClean || opcode === SnpCleanFwd
    }

    def isSnpSharedX(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpShared || opcode === SnpSharedFwd
    }

    def isSnpNotSharedDirtyX(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpNotSharedDirty || opcode === SnpNotSharedDirtyFwd
    }

    def isSnpToB(opcode: UInt)(implicit p: Parameters): Bool = {
      isSnpCleanX(opcode) || isSnpSharedX(opcode) || isSnpNotSharedDirtyX(opcode)
    }

    def isSnpToN(opcode: UInt)(implicit p: Parameters): Bool = {
      isSnpUniqueX(opcode) || opcode === SnpCleanInvalid || isSnpMakeInvalidX (opcode)
    }

    def isSnpCleanShared(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpCleanShared
    }

    def isSnpToBNonFwd(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpClean ||
      opcode === SnpNotSharedDirty ||
      opcode === SnpShared
    }

    def isSnpToBFwd(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpCleanFwd ||
      opcode === SnpNotSharedDirtyFwd ||
      opcode === SnpSharedFwd
    }

    def isSnpToNNonFwd(opcode: UInt)(implicit p: Parameters): Bool = {
     widthCheck(opcode) 
     opcode === SnpUnique || opcode === SnpUniqueStash
    }

    def isSnpToNFwd(opcode: UInt)(implicit p: Parameters): Bool = {
     widthCheck(opcode) 
     opcode === SnpUniqueFwd 
    }

    def isSnpUniqueX(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpUnique || opcode === SnpUniqueFwd || opcode === SnpUniqueStash
    }

    def isSnpMakeInvalidX(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpMakeInvalid || opcode === SnpMakeInvalidStash
    }
  }

  object DATOpcodes {
    val width_map = Map(
      Issue.B -> 3,
      Issue.Eb -> 4,
    )

    def width(implicit p: Parameters) = width_map(p(CHIIssue))

    def DataL2RdData(implicit p: Parameters)        = 0x0.U(width.W)
    def DataL2RdDataFwded(implicit p: Parameters)  = 0x1.U(width.W)
    def DataL2RdDataCancel(implicit p: Parameters) = 0x2.U(width.W)
    def DataL2RdDataFwdedCancel(implicit p: Parameters) = 0x3.U(width.W)
    def DataL2RdDataFwdedCancel2(implicit p: Parameters) = 0x4.U(width.W)

    def DataLCrdReturn(implicit p: Parameters)    = 0x0.U(width.W)
    def SnpRespData(implicit p: Parameters)       = 0x1.U(width.W)
    def CopyBackWrData(implicit p: Parameters)    = 0x2.U(width.W)
    def NonCopyBackWrData(implicit p: Parameters) = 0x3.U(width.W)
    def CompData(implicit p: Parameters)          = 0x4.U(width.W)
    def SnpRespDataPtl(implicit p: Parameters)    = 0x5.U(width.W)
    def SnpRespDataFwded(implicit p: Parameters)  = 0x6.U(width.W)
    def WriteDataCancel(implicit p: Parameters)   = 0x7.U(width.W)

    // E.b new opcodes
    def DataSepResp       = 0xB.U(width_map(Issue.Eb).W)

    def widthCheck(opcode: UInt)(implicit p: Parameters): Unit = { require (opcode.getWidth >= width) }
    def isSnpRespDataX(opcode: UInt)(implicit p: Parameters): Bool = {
      widthCheck(opcode)
      opcode === SnpRespData || opcode === SnpRespDataPtl || opcode === SnpRespDataFwded
    }
  }
}
