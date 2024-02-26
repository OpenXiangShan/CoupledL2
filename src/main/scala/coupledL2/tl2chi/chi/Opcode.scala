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
    val width = 6

    def ReqLCrdReturn         = 0x00.U
    def ReadShared            = 0x01.U
    def ReadClean             = 0x02.U
    def ReadOnce              = 0x03.U
    def ReadNoSnp             = 0x04.U
    def PCrdReturn            = 0x05.U

    def ReadUnique            = 0x07.U
    def CleanShared           = 0x08.U
    def CleanInvalid          = 0x09.U
    def MakeInvalid           = 0x0A.U
    def CleanUnique           = 0x0B.U
    def MakeUnique            = 0x0C.U
    def Evict                 = 0x0D.U

    def DVMOp                 = 0x14.U
    def WriteEvictFull        = 0x15.U

    def WriteCleanFull        = 0x17.U
    def WriteUniquePtl        = 0x18.U
    def WriteUniqueFull       = 0x19.U
    def WriteBackPtl          = 0x1A.U
    def WriteBackFull         = 0x1B.U
    def WriteNoSnpPtl         = 0x1C.U
    def WriteNoSnpFull        = 0x1D.U

    def WriteUniqueFullStash  = 0x20.U
    def WriteUniquePtlStash   = 0x21.U
    def StashOnceShared       = 0x22.U
    def StashOnceUnique       = 0x23.U
    def ReadOnceCleanInvalid  = 0x24.U
    def ReadOnceMakeInvalid   = 0x25.U
    def ReadNotSharedDirty    = 0x26.U
    def CleanSharedPersist    = 0x27.U

    def AtomicStore_ADD       = 0x28.U
    def AtomicStore_CLR       = 0x29.U
    def AtomicStore_EOR       = 0x2A.U
    def AtomicStore_SET       = 0x2B.U
    def AtomicStore_SMAX      = 0x2C.U
    def AtomicStore_SMIN      = 0x2D.U
    def AtomicStore_UMAX      = 0x2E.U
    def AtomicStore_UMIN      = 0x2F.U
    def AtomicLoad_ADD        = 0x30.U
    def AtomicLoad_CLR        = 0x31.U
    def AtomicLoad_EOR        = 0x32.U
    def AtomicLoad_SET        = 0x33.U
    def AtomicLoad_SMAX       = 0x34.U
    def AtomicLoad_SMIN       = 0x35.U
    def AtomicLoad_UMAX       = 0x36.U
    def AtomicLoad_UMIN       = 0x37.U
    def AtomicSwap            = 0x38.U
    def AtomicCompare         = 0x39.U
    def PrefetchTgt           = 0x3A.U
  }

  object RSPOpcodes {
    val width = 4

    def RespLCrdReturn  = 0x0.U
    def SnpResp         = 0x1.U
    def CompAck         = 0x2.U
    def RetryAck        = 0x3.U
    def Comp            = 0x4.U
    def CompDBIDResp    = 0x5.U
    def DBIDResp        = 0x6.U
    def PCrdGrant       = 0x7.U
    def ReadReceipt     = 0x8.U
    def SnpRespFwded    = 0x9.U
  }

  object SNPOpcodes {
    val width = 5

    def SnpLCrdReturn         = 0x00.U
    def SnpShared             = 0x01.U
    def SnpClean              = 0x02.U
    def SnpOnce               = 0x03.U
    def SnpNotSharedDirty     = 0x04.U
    def SnpUniqueStash        = 0x05.U
    def SnpMakeInvalidStash   = 0x06.U
    def SnpUnique             = 0x07.U
    def SnpCleanShared        = 0x08.U
    def SnpCleanInvalid       = 0x09.U
    def SnpMakeInvalid        = 0x0A.U
    def SnpStashUnique        = 0x0B.U
    def SnpStashShared        = 0x0C.U
    def SnpDVMOp              = 0x0D.U

    def SnpSharedFwd          = 0x11.U
    def SnpCleanFwd           = 0x12.U
    def SnpOnceFwd            = 0x13.U
    def SnpNotSharedDirtyFwd  = 0x14.U

    def SnpUniqueFwd          = 0x17.U
  }

  object DATOpcodes {
    val width = 3

    def DataLCrdReturn    = 0x0.U
    def SnpRespData       = 0x1.U
    def CopyBackWrData    = 0x2.U
    def NonCopyBackWrData = 0x3.U
    def CompData          = 0x4.U
    def SnpRespDataPtl    = 0x5.U
    def SnpRespDataFwded  = 0x6.U
    def WriteDataCancel   = 0x7.U
  }
}