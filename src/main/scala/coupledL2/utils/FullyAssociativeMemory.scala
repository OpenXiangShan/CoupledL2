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

package coupledL2.utils

import chisel3._
import chisel3.util._
import utility.{XSPerfAccumulate}
import org.chipsalliance.cde.config.Parameters


class CAMEntry[T <: Data](gen: T, keyWidth: Int) extends Bundle {
  val valid = Bool()
  val key = UInt(keyWidth.W)
  val data = gen.cloneType
}

class CAMReadReq(keyWidth: Int) extends Bundle {
  val key = UInt(keyWidth.W)
}


class CAMReadResp[T <: Data](gen: T, numEntries: Int) extends Bundle {
  val hit = Bool()
  val hitIdx = UInt(log2Up(numEntries).W)  
  val data = gen.cloneType
  val multHit = Bool()
}

class CAMWriteReq[T <: Data](gen: T, keyWidth: Int, numEntries: Int) extends Bundle {
  val idx = UInt(log2Up(numEntries).W) 
  val key = UInt(keyWidth.W)
  val data = gen.cloneType
  val valid = Bool()  
}

class CAMReadPort[T <: Data](gen: T, keyWidth: Int, numEntries: Int) extends Bundle {
  val req = Input(new CAMReadReq(keyWidth))
  val resp = Output(new CAMReadResp(gen, numEntries))
}


class CAMWritePort[T <: Data](gen: T, keyWidth: Int, numEntries: Int) extends Bundle {
  val en = Input(Bool())
  val req = Input(new CAMWriteReq(gen, keyWidth, numEntries))
}


class FullyAssociativeMemory[T <: Data](
  gen: T,
  keyWidth: Int,
  numEntries: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  shouldReset: Boolean = false
) extends Module {
  require(keyWidth > 0, "keyWidth must be positive")
  require(numEntries > 0, "numEntries must be positive")
  require(numReadPorts > 0, "numReadPorts must be positive")
  require(numWritePorts > 0, "numWritePorts must be positive")

  val io = IO(new Bundle {
    val r = Vec(numReadPorts, new CAMReadPort(gen, keyWidth, numEntries))
    val w = Vec(numWritePorts, new CAMWritePort(gen, keyWidth, numEntries))
    
    //debug
    val debug = Output(new Bundle {
      val valid = Vec(numEntries, Bool())
      val keys  = Vec(numEntries, UInt(keyWidth.W))
    })
  })

  // Register array: stores all entries
  val entries = if (shouldReset) {
    RegInit(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new CAMEntry(gen, keyWidth)))))
  } else {
    Reg(Vec(numEntries, new CAMEntry(gen, keyWidth)))
  }

  // ==================== Read port logic, using combinational logic for reading ====================
  for (i <- 0 until numReadPorts) {
    val searchKey = io.r(i).req.key
    
    // Parallel compare all entries
    val matchVec = VecInit(entries.map(e => e.valid && (e.key === searchKey)))
    val hit = matchVec.asUInt.orR
    val hitIdx = PriorityEncoder(matchVec.asUInt)  //Priority encoder, find the first match
    
    
    assert(PopCount(matchVec) <= 1.U, "CAM key conflict detected!")//If multiple hit conflicts occur, it is a design issue.
    val hitData = entries(hitIdx).data
    
    // ==================== Write-after-read bypass ====================
    // Check all write ports, if any write port writes to the same entry, use the new data
    val bypassHit = Wire(Bool())
    val bypassData = Wire(gen.cloneType)
    val bypassIdx = Wire(UInt(log2Up(numEntries).W))
    
    bypassHit := false.B
    bypassData := hitData
    bypassIdx := hitIdx
    
   
    // Also need to check if any write port writes a new matching key entry
    for (j <- 0 until numWritePorts) {
      when(io.w(j).en && io.w(j).req.valid && io.w(j).req.key === searchKey) {
        // Whether a valid write port conflicts with the current read
        bypassHit := true.B
        bypassData := io.w(j).req.data
        bypassIdx := io.w(j).req.idx
      }
    }
    
    io.r(i).resp.hit := hit || bypassHit
    io.r(i).resp.hitIdx := Mux(bypassHit, bypassIdx, hitIdx)
    io.r(i).resp.data := Mux(hit || bypassHit, bypassData, 0.U.asTypeOf(gen))
    io.r(i).resp.multHit := PopCount(matchVec) > 1.U
  }  
  // ==================== write port====================
  for (i <- 0 until numWritePorts) {
    when(io.w(i).en) {
      val writeIdx = io.w(i).req.idx
      entries(writeIdx).valid := io.w(i).req.valid
      entries(writeIdx).key := io.w(i).req.key
      entries(writeIdx).data := io.w(i).req.data
    }
  }
  
  // ==================== debug interface ====================
  io.debug.valid := VecInit(entries.map(_.valid))
  io.debug.keys := VecInit(entries.map(_.key))
}

