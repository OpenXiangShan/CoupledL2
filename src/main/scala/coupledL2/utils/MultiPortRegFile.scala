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

/** 多端口读写接口
  * @param gen 数据类型
  * @param sets 组数
  * @param ways 路数(组相联度)
  */
class MultiPortRegFileReadReq(sets: Int, ways: Int) extends Bundle {
  val setIdx = UInt(log2Up(sets).W)
}

class MultiPortRegFileReadResp[T <: Data](gen: T, ways: Int) extends Bundle {
  val data = Vec(ways, gen)  // 返回该 set 的所有 way 的数据
}

class MultiPortRegFileWriteReq[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  val setIdx = UInt(log2Up(sets).W)
  val wayMask = UInt(ways.W)  // One-Hot 或 Bit Mask, 指定写入哪些 way
  val data = Vec(ways, gen)   // 每个 way 的数据
}

class MultiPortRegFileReadPort[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  val req = Input(new MultiPortRegFileReadReq(sets, ways))
  val resp = Output(new MultiPortRegFileReadResp(gen, ways))//读出一个line的全部的way,这些way构成vec
}

class MultiPortRegFileWritePort[T <: Data](gen: T, sets: Int, ways: Int) extends Bundle {
  val req = Input(new MultiPortRegFileWriteReq(gen, sets, ways))
  val en = Input(Bool())  // 写使能
}

/** 多端口读写寄存器文件
  * 
  * 特点:
  * - 支持多个独立的读端口，组合逻辑内部对于读写冲突进行旁路处理，将读出的set数据中的对应way替换为写入数据
  * - 支持多个独立的写端口，内部会对写冲突（写入相同set的相同way视为写冲突）进行处理，优先级高（即port编号大的）写入生效
  * - 支持组相联(多路)
  * - 使用寄存器阵列实现,读取为组合逻辑(0 cycle)
  * - 写入在时钟上升沿生效
  * 
  * @param gen 存储的数据类型
  * @param sets 组数(行数)
  * @param ways 每组的路数(列数)
  * @param numReadPorts 读端口数量
  * @param numWritePorts 写端口数量
  * @param shouldReset 是否在复位时初始化为 0
  * 
  * @example {{{
  *   val regFile = Module(new MultiPortRegFile(
  *     gen = UInt(32.W),
  *     sets = 16,
  *     ways = 4,
  *     numReadPorts = 2,
  *     numWritePorts = 1,
  *     shouldReset = true
  *   ))
  *   
  *   // 读端口 0
  *   regFile.io.r(0).req.setIdx := addr0
  *   val data0 = regFile.io.r(0).resp.data  // Vec(4, UInt(32.W))
  *   
  *   // 写端口 0
  *   regFile.io.w(0).en := writeEn
  *   regFile.io.w(0).req.setIdx := writeAddr
  *   regFile.io.w(0).req.wayMask := "b0100".U  // 写入 way 2
  *   regFile.io.w(0).req.data(2) := writeData
  * }}}
  */
class MultiPortRegFile[T <: Data](
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
    val r = Vec(numReadPorts, new MultiPortRegFileReadPort(gen, sets, ways))
    val w = Vec(numWritePorts, new MultiPortRegFileWritePort(gen, sets, ways))
  })

  // 互斥写入端口: 处理写冲突,编号越靠后的优先级越高
  val mutex_w = Wire(Vec(numWritePorts, new MultiPortRegFileWritePort(gen, sets, ways)))
  
  // 默认连接 req 数据字段
  for (i <- 0 until numWritePorts) {
    mutex_w(i).req := io.w(i).req
  }
  
  // 处理写冲突: 如果多个端口写同一地址,禁用优先级低的端口
  // 更精确的冲突检测：检查 set 和 wayMask 是否重叠
  for (i <- 0 until numWritePorts) {
    mutex_w(i).en := io.w(i).en
    for (j <- i + 1 until numWritePorts) {
      // 检查是否写同一个 set 的同一个 way
      val sameSet = io.w(i).req.setIdx === io.w(j).req.setIdx //检查写入的set是否相同
      val wayConflict = (io.w(i).req.wayMask & io.w(j).req.wayMask) =/= 0.U //检查写入的way是否相同
      
      //如果两个写入的写入set和way都相同，则表示俩个写出现冲突了
      when(io.w(i).en && io.w(j).en && sameSet && wayConflict) {
        mutex_w(i).en := false.B  
      }
    }
  }
 
  
  
  // 寄存器阵列: sets × ways
  val regArray = if (shouldReset) {
    RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U.asTypeOf(gen))))))
  } else {
    Reg(Vec(sets, Vec(ways, gen)))
  }

  // 读端口: 组合逻辑,立即返回 (带写后读旁路)
  for (i <- 0 until numReadPorts) {
    val readSetIdx = io.r(i).req.setIdx
    val baseData = regArray(readSetIdx)
    val bypassData = Wire(Vec(ways, gen))
    
    // 默认使用寄存器数组的数据
    bypassData := baseData
    
    // 检查所有写端口,
    for (j <- 0 until numWritePorts) {
      when(mutex_w(j).en && mutex_w(j).req.setIdx === readSetIdx) { // 如果有写同一个 set,进行旁路
       
        for (wayIdx <- 0 until ways) { // 检查是不是写入同一个way
          when(mutex_w(j).req.wayMask(wayIdx)) {//同set同way，这将bypass对应way的数据修改成写入数据
            bypassData(wayIdx) := mutex_w(j).req.data(wayIdx) 
          }
        }
      }
    }
    
    io.r(i).resp.data := bypassData
  }

  // 写端口: 在时钟上升沿写入,使用经过冲突处理的 mutex_w
  for (i <- 0 until numWritePorts) {
    when(mutex_w(i).en) {
      val setIdx    = mutex_w(i).req.setIdx
      val wayMask   = mutex_w(i).req.wayMask
      val writeData = mutex_w(i).req.data
      
      for (wayIdx <- 0 until ways) {
        when(wayMask(wayIdx)) {
          regArray(setIdx)(wayIdx) := writeData(wayIdx)
        }
      }
    }
  }

  
}

