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


class SetAssociativeMemoryReadReq(sets: Int, ways: Int) extends Bundle {
  val setIdx = UInt(log2Up(sets).W)
}

class SetAssociativeMemoryReadResp[T <: Data](gen: T, ways: Int) extends Bundle {
  val data = Vec(ways, gen)  
}

class SetAssociativeMemoryWriteReq[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  val setIdx = UInt(log2Up(sets).W)
  val wayMask = UInt(ways.W)  
  // Single write data element; will be replicated to selected ways internally
  val data = gen
}

class SetAssociativeMemoryReadPort[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  val req = Input(new SetAssociativeMemoryReadReq(sets, ways))
  val resp = Output(new SetAssociativeMemoryReadResp(gen, ways))
}

class SetAssociativeMemoryWritePort[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  // Use Valid to carry write request (valid + bits). no separate `en` signal.
  val req = Input(Valid(new SetAssociativeMemoryWriteReq(gen, sets, ways)))
}


class SetAssociativeMemory[T <: Data](
  gen: T,
  sets: Int,
  ways: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  shouldReset: Boolean = false
) extends Module {
  require(sets > 0, "sets must be positive")
  require(ways > 0, "ways must be positive")
  require(numReadPorts > 0, "numReadPorts must be positive")
  require(numWritePorts > 0, "numWritePorts must be positive")

  val io = IO(new Bundle {
    val r = Vec(numReadPorts,  new SetAssociativeMemoryReadPort(gen, sets, ways))
    val w = Vec(numWritePorts, new SetAssociativeMemoryWritePort(gen, sets, ways))
  })

  // Mutex write ports: handle write conflicts, higher port number has higher priority
  val mutex_w = Wire(Vec(numWritePorts, new SetAssociativeMemoryWritePort(gen, sets, ways)))
  
  // Default connect req data fields (copy bits & valid separately so we can mute valid on conflicts)
  for (i <- 0 until numWritePorts) {
    mutex_w(i).req.bits := io.w(i).req.bits
    mutex_w(i).req.valid := io.w(i).req.valid
  }

  // Handle write conflicts: if multiple ports write to the same address,
  // disable lower priority ports by clearing their `valid`.
  for (i <- 0 until numWritePorts) {
    for (j <- i + 1 until numWritePorts) {
      // Check if writing to the same set and way
      val sameSet = io.w(i).req.bits.setIdx === io.w(j).req.bits.setIdx
      val wayConflict = (io.w(i).req.bits.wayMask & io.w(j).req.bits.wayMask) =/= 0.U

      // If both ports are valid and conflict on set+way, mute the lower-priority one
      when(io.w(i).req.valid && io.w(j).req.valid && sameSet && wayConflict) {
        mutex_w(i).req.valid := false.B
      }
    }
  }
 
  //Register array: sets × ways
  val regArray = if (shouldReset) {
    RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U.asTypeOf(gen))))))
  } else {
    Reg(Vec(sets, Vec(ways, gen)))
  }

  //Read port: combinational logic, returns immediately (with read-after-write bypass)
  for (i <- 0 until numReadPorts) {
    val readSetIdx = io.r(i).req.setIdx
    val baseData = regArray(readSetIdx)
    val bypassData = Wire(Vec(ways, gen))
    
    // Default to using the data from the register array
    bypassData := baseData
    
    // Check all write ports,
    for (j <- 0 until numWritePorts) {
      when(mutex_w(j).req.valid && mutex_w(j).req.bits.setIdx === readSetIdx) { // If there is a write to the same set, bypass
        for (wayIdx <- 0 until ways) { // Check if writing to the same way
          when(mutex_w(j).req.bits.wayMask(wayIdx)) { // Same set and way, bypass the corresponding way's data with the write data
            bypassData(wayIdx) := mutex_w(j).req.bits.data
          }
        }
      }
    }
    
    io.r(i).resp.data := bypassData
  }

  // write
  for (i <- 0 until numWritePorts) {
    when(mutex_w(i).req.valid) {
      val setIdx    = mutex_w(i).req.bits.setIdx
      val wayMask   = mutex_w(i).req.bits.wayMask
      val writeData = mutex_w(i).req.bits.data

      for (wayIdx <- 0 until ways) {
        when(wayMask(wayIdx)) {
          // replicate single write data to the masked way
          regArray(setIdx)(wayIdx) := writeData
        }
      }
    }
  }
}

