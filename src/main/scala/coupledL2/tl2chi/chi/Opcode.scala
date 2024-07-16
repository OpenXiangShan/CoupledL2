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

object CHIOpcode {

  object REQOpcodes {
    val width_map = Map(
      CHIIssue.B -> 6,
      CHIIssue.Eb -> 7,
    )

    val width = width_map(CHIIssue.curr_issue)

    def ReqLCrdReturn         = 0x00.U(width.W)
    def ReadShared            = 0x01.U(width.W)
    def ReadClean             = 0x02.U(width.W)
    def ReadOnce              = 0x03.U(width.W)
    def ReadNoSnp             = 0x04.U(width.W)
    def PCrdReturn            = 0x05.U(width.W)

    def ReadUnique            = 0x07.U(width.W)
    def CleanShared           = 0x08.U(width.W)
    def CleanInvalid          = 0x09.U(width.W)
    def MakeInvalid           = 0x0A.U(width.W)
    def CleanUnique           = 0x0B.U(width.W)
    def MakeUnique            = 0x0C.U(width.W)
    def Evict                 = 0x0D.U(width.W)

    def DVMOp                 = 0x14.U(width.W)
    def WriteEvictFull        = 0x15.U(width.W)

    def WriteCleanFull        = 0x17.U(width.W)
    def WriteUniquePtl        = 0x18.U(width.W)
    def WriteUniqueFull       = 0x19.U(width.W)
    def WriteBackPtl          = 0x1A.U(width.W)
    def WriteBackFull         = 0x1B.U(width.W)
    def WriteNoSnpPtl         = 0x1C.U(width.W)
    def WriteNoSnpFull        = 0x1D.U(width.W)

    def WriteUniqueFullStash  = 0x20.U(width.W)
    def WriteUniquePtlStash   = 0x21.U(width.W)
    def StashOnceShared       = 0x22.U(width.W)
    def StashOnceUnique       = 0x23.U(width.W)
    def ReadOnceCleanInvalid  = 0x24.U(width.W)
    def ReadOnceMakeInvalid   = 0x25.U(width.W)
    def ReadNotSharedDirty    = 0x26.U(width.W)
    def CleanSharedPersist    = 0x27.U(width.W)

    def AtomicStore_ADD       = 0x28.U(width.W)
    def AtomicStore_CLR       = 0x29.U(width.W)
    def AtomicStore_EOR       = 0x2A.U(width.W)
    def AtomicStore_SET       = 0x2B.U(width.W)
    def AtomicStore_SMAX      = 0x2C.U(width.W)
    def AtomicStore_SMIN      = 0x2D.U(width.W)
    def AtomicStore_UMAX      = 0x2E.U(width.W)
    def AtomicStore_UMIN      = 0x2F.U(width.W)
    def AtomicLoad_ADD        = 0x30.U(width.W)
    def AtomicLoad_CLR        = 0x31.U(width.W)
    def AtomicLoad_EOR        = 0x32.U(width.W)
    def AtomicLoad_SET        = 0x33.U(width.W)
    def AtomicLoad_SMAX       = 0x34.U(width.W)
    def AtomicLoad_SMIN       = 0x35.U(width.W)
    def AtomicLoad_UMAX       = 0x36.U(width.W)
    def AtomicLoad_UMIN       = 0x37.U(width.W)
    def AtomicSwap            = 0x38.U(width.W)
    def AtomicCompare         = 0x39.U(width.W)
    def PrefetchTgt           = 0x3A.U(width.W)
  }

  object RSPOpcodes {
    val width_map = Map(
      CHIIssue.B -> 4,
      CHIIssue.Eb -> 5,
    )

    val width = width_map(CHIIssue.curr_issue)

    def RespLCrdReturn  = 0x0.U(width.W)
    def SnpResp         = 0x1.U(width.W)
    def CompAck         = 0x2.U(width.W)
    def RetryAck        = 0x3.U(width.W)
    def Comp            = 0x4.U(width.W)
    def CompDBIDResp    = 0x5.U(width.W)
    def DBIDResp        = 0x6.U(width.W)
    def PCrdGrant       = 0x7.U(width.W)
    def ReadReceipt     = 0x8.U(width.W)
    def SnpRespFwded    = 0x9.U(width.W)
  }

  object SNPOpcodes {
    val width_map = Map(
      CHIIssue.B -> 5,
      CHIIssue.Eb -> 6,
    )

    val width = width_map(CHIIssue.curr_issue)

    def SnpLCrdReturn         = 0x00.U(width.W)
    def SnpShared             = 0x01.U(width.W)
    def SnpClean              = 0x02.U(width.W)
    def SnpOnce               = 0x03.U(width.W)
    def SnpNotSharedDirty     = 0x04.U(width.W)
    def SnpUniqueStash        = 0x05.U(width.W)
    def SnpMakeInvalidStash   = 0x06.U(width.W)
    def SnpUnique             = 0x07.U(width.W)
    def SnpCleanShared        = 0x08.U(width.W)
    def SnpCleanInvalid       = 0x09.U(width.W)
    def SnpMakeInvalid        = 0x0A.U(width.W)
    def SnpStashUnique        = 0x0B.U(width.W)
    def SnpStashShared        = 0x0C.U(width.W)
    def SnpDVMOp              = 0x0D.U(width.W)

    def SnpSharedFwd          = 0x11.U(width.W)
    def SnpCleanFwd           = 0x12.U(width.W)
    def SnpOnceFwd            = 0x13.U(width.W)
    def SnpNotSharedDirtyFwd  = 0x14.U(width.W)

    def SnpUniqueFwd          = 0x17.U(width.W)

    def widthCheck(opcode: UInt): Unit = { require (opcode.getWidth >= width) }

    def isSnpXStash(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpUniqueStash || opcode === SnpMakeInvalidStash
    }

    def isSnpStashX(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpStashUnique || opcode === SnpStashShared
    }

    def isSnpXFwd(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode >= SnpSharedFwd
    }


    def isSnpOnceX(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpOnce || opcode === SnpOnceFwd
    }

    def isSnpCleanX(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpClean || opcode === SnpCleanFwd
    }

    def isSnpSharedX(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpShared || opcode === SnpSharedFwd
    }

    def isSnpNotSharedDirtyX(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpNotSharedDirty || opcode === SnpNotSharedDirtyFwd
    }

    def isSnpToB(opcode: UInt): Bool = {
      isSnpCleanX(opcode) || isSnpSharedX(opcode) || isSnpNotSharedDirtyX(opcode)
    }

    def isSnpToN(opcode: UInt): Bool = {
      isSnpUniqueX(opcode) || opcode === SnpCleanInvalid || isSnpMakeInvalidX (opcode)
    }

    def isSnpCleanShared(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpCleanShared
    }

    def isSnpToBNonFwd(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpClean ||
      opcode === SnpNotSharedDirty ||
      opcode === SnpShared
    }

    def isSnpToBFwd(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpCleanFwd ||
      opcode === SnpNotSharedDirtyFwd ||
      opcode === SnpSharedFwd
    }

    def isSnpToNNonFwd(opcode: UInt): Bool = {
     widthCheck(opcode) 
     opcode === SnpUnique || opcode === SnpUniqueStash
    }

    def isSnpToNFwd(opcode: UInt): Bool = {
     widthCheck(opcode) 
     opcode === SnpUniqueFwd 
    }

    def isSnpUniqueX(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpUnique || opcode === SnpUniqueFwd || opcode === SnpUniqueStash
    }

    def isSnpMakeInvalidX(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpMakeInvalid || opcode === SnpMakeInvalidStash
    }
  }

  object DATOpcodes {
    val width_map = Map(
      CHIIssue.B -> 3,
      CHIIssue.Eb -> 4,
    )

    val width = width_map(CHIIssue.curr_issue)

    def DataLCrdReturn    = 0x0.U(width.W)
    def SnpRespData       = 0x1.U(width.W)
    def CopyBackWrData    = 0x2.U(width.W)
    def NonCopyBackWrData = 0x3.U(width.W)
    def CompData          = 0x4.U(width.W)
    def SnpRespDataPtl    = 0x5.U(width.W)
    def SnpRespDataFwded  = 0x6.U(width.W)
    def WriteDataCancel   = 0x7.U(width.W)

    def widthCheck(opcode: UInt): Unit = { require (opcode.getWidth >= width) }
    def isSnpRespDataX(opcode: UInt): Bool = {
      widthCheck(opcode)
      opcode === SnpRespData || opcode === SnpRespDataPtl || opcode === SnpRespDataFwded
    }
  }
}
