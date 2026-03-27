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

class SetAssociativeRegsReadReq(sets: Int, ways: Int) extends Bundle {
  val setIdx = UInt(log2Up(sets).W)
}

class SetAssociativeRegsReadResp[T <: Data](gen: T, ways: Int) extends Bundle {
  val data = Vec(ways, gen)  
}

class SetAssociativeRegsWriteReq[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  val setIdx = UInt(log2Up(sets).W)
  val wayMask = UInt(ways.W)  
  val data = gen
}

class SetAssociativeRegsReadPort[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  val req = Input(new SetAssociativeRegsReadReq(sets, ways))
  val resp = Output(new SetAssociativeRegsReadResp(gen, ways))
}

class SetAssociativeRegsWritePort[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  val req = Input(Valid(new SetAssociativeRegsWriteReq(gen, sets, ways)))
}


class SetAssociativeRegs[T <: Data](
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
    val r = Vec(numReadPorts,  new SetAssociativeRegsReadPort(gen, sets, ways))
    val w = Vec(numWritePorts, new SetAssociativeRegsWritePort(gen, sets, ways))
  })

  //Register array: sets × ways
  val regArray = if (shouldReset) {
    RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U.asTypeOf(gen))))))
  } else {
    Reg(Vec(sets, Vec(ways, gen)))
  }

  // Mutex write ports: handle write conflicts, higher port number has higher priority
  val mutex_w = Wire(Vec(numWritePorts, new SetAssociativeRegsWritePort(gen, sets, ways)))
  for (i <- 0 until numWritePorts) {
    assert(mutex_w(i).req.bits.setIdx < sets.U, "SetAssociativeRegs: write setIdx out of range")
    assert(mutex_w(i).req.bits.wayMask < (1.U << ways).asUInt, "SetAssociativeRegs: wayMask width")
  }
  for (i <- 0 until numReadPorts) {
    assert(io.r(i).req.setIdx < sets.U, "SetAssociativeRegs: read setIdx out of range")
  }
  
  for (i <- 0 until numWritePorts) {
    mutex_w(i).req := io.w(i).req
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
 
  //Read port: combinational logic, returns immediately (with read-after-write bypass)
  for (i <- 0 until numReadPorts) {
    val readSetIdx = io.r(i).req.setIdx
    val baseData = regArray(readSetIdx)
    val bypassData = Wire(Vec(ways, gen))

    // SET conflict on the write port
    val wPortSetConflict = VecInit((0 until numWritePorts).map { j =>
      mutex_w(j).req.valid && (mutex_w(j).req.bits.setIdx === readSetIdx)
    })

    // Collect write data from ports for indexed access
    val writeDataVec = VecInit((0 until numWritePorts).map { j => mutex_w(j).req.bits.data })

    // For each way, build a small priority selection among write ports; default to baseData
    for (wayIdx <- 0 until ways) {
      
      // way conflict on write ports that have a set conflict
      val conflictSetVec = VecInit((0 until numWritePorts).map { j =>
        wPortSetConflict(j) && mutex_w(j).req.bits.wayMask(wayIdx)
      })
      val cases = (0 until numWritePorts).map { j => (conflictSetVec(j), writeDataVec(j)) }
      bypassData(wayIdx) := MuxCase(baseData(wayIdx), cases)
    }
    io.r(i).resp.data := bypassData
  }

  // Write Port
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

  for (s <- 0 until sets) {
    for (w <- 0 until ways) {
      val writers = VecInit((0 until numWritePorts).map { j =>
        (mutex_w(j).req.valid && (mutex_w(j).req.bits.setIdx === s.U) && mutex_w(j).req.bits.wayMask(w))
      })
      assert(PopCount(writers) <= 1.U, s"SetAssociativeRegs: multiple active writers to same (set=$s,way=$w)")
    }
  }
}

