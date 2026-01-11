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

/** 全相连存储器 Entry
  * @param gen 数据类型
  * @param keyWidth Key 的位宽
  */
class CAMEntry[T <: Data](gen: T, keyWidth: Int) extends Bundle {
  val valid = Bool()
  val key = UInt(keyWidth.W)
  val data = gen.cloneType
}

/** 全相连存储器读请求 */
class CAMReadReq(keyWidth: Int) extends Bundle {
  val key = UInt(keyWidth.W)
}

/** 全相连存储器读响应 */
class CAMReadResp[T <: Data](gen: T, numEntries: Int) extends Bundle {
  val hit = Bool()
  val hitIdx = UInt(log2Up(numEntries).W)  // 命中的 entry 索引
  val data = gen.cloneType
}

/** 全相连存储器写请求 */
class CAMWriteReq[T <: Data](gen: T, keyWidth: Int, numEntries: Int) extends Bundle {
  val idx = UInt(log2Up(numEntries).W)  // 写入的 entry 索引
  val key = UInt(keyWidth.W)
  val data = gen.cloneType
  val valid = Bool()  // 是否将 entry 标记为有效
}

/** 全相连存储器读端口 */
class CAMReadPort[T <: Data](gen: T, keyWidth: Int, numEntries: Int) extends Bundle {
  val req = Input(new CAMReadReq(keyWidth))
  val resp = Output(new CAMReadResp(gen, numEntries))
}

/** 全相连存储器写端口 */
class CAMWritePort[T <: Data](gen: T, keyWidth: Int, numEntries: Int) extends Bundle {
  val en = Input(Bool())
  val req = Input(new CAMWriteReq(gen, keyWidth, numEntries))
}

/** N 端口读写全相连存储器 (CAM - Content Addressable Memory)
  * 
  * 特点:
  * - 全相连: 根据 key 查找,而不是地址索引，每个表项的标识符是key，这个key在存储器里面是唯一的
  * - 并行查找: 所有 entry 同时比较
  * - 多端口:
  *   - N 个独立读端口: 读写冲突采用写后读旁路处理
  *   - M 个独立写端口: 写冲突通过优先级仲裁处理 (端口编号越大优先级越高)
  * - 寄存器实现: 0 cycle 读延迟
  * - 支持命中检测: 自动返回命中状态和索引
  * - 泛型数据: 支持任意数据类型 (UInt, Bundle, Vec 等)
  * 
  * @param gen 数据类型
  * @param keyWidth Key 的位宽 (用于查找匹配)
  * @param numEntries Entry 总数
  * @param numReadPorts 读端口数量
  * @param numWritePorts 写端口数量
  * @param shouldReset 是否在复位时初始化
  * 
  * @example {{{
  *   val cam = Module(new FullyAssociativeMemory(
  *     gen = UInt(64.W),
  *     keyWidth = 32,
  *     numEntries = 16,
  *     numReadPorts = 2,
  *     numWritePorts = 1,
  *     shouldReset = true
  *   ))
  *   
  *   // 读端口 0: 根据 key 查找
  *   cam.io.r(0).req.key := searchKey
  *   val hit = cam.io.r(0).resp.hit
  *   val data = cam.io.r(0).resp.data
  *   val idx = cam.io.r(0).resp.hitIdx
  *   
  *   // 写端口 0: 更新指定 entry
  *   cam.io.w(0).en := writeEn
  *   cam.io.w(0).req.idx := victimIdx
  *   cam.io.w(0).req.key := newKey
  *   cam.io.w(0).req.data := newData
  *   cam.io.w(0).req.valid := true.B
  * }}}
  */
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
    
    // 调试接口: 查看所有 entry 的状态
    val debug = Output(new Bundle {
      val valid = Vec(numEntries, Bool())
      val keys = Vec(numEntries, UInt(keyWidth.W))
    })
  })

  // 寄存器阵列: 存储所有 entry
  val entries = if (shouldReset) {
    RegInit(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new CAMEntry(gen, keyWidth)))))
  } else {
    Reg(Vec(numEntries, new CAMEntry(gen, keyWidth)))
  }

  // ==================== 读端口逻辑，采用组合逻辑读 ====================
  // 并行查找: 所有 entry 同时与 key 比较 (带写后读旁路)
  for (i <- 0 until numReadPorts) {
    val searchKey = io.r(i).req.key
    
    // 并行比较所有 entry
    val matchVec = VecInit(entries.map(e => e.valid && (e.key === searchKey)))//获取每个entry是否命中
    val hit = matchVec.asUInt.orR
    val hitIdx = PriorityEncoder(matchVec.asUInt)  // 优先编码器,找到第一个匹配的
    
    
    assert(PopCount(matchVec) <= 1.U, "CAM key conflict detected!")//出现多命中冲突，则是设计问题
    val hitData = entries(hitIdx).data
    
    // ==================== 写后读旁路 ====================
    // 检查所有写端口,如果有写端口在写同一个 entry,使用新数据
    val bypassHit = Wire(Bool())
    val bypassData = Wire(gen.cloneType)
    val bypassIdx = Wire(UInt(log2Up(numEntries).W))
    
    bypassHit := false.B
    bypassData := hitData
    bypassIdx := hitIdx
    
   
    // 还需要检查写端口是否写入了新的匹配 key 的 entry
    for (j <- 0 until numWritePorts) {
      when(io.w(j).en && io.w(j).req.valid && io.w(j).req.key === searchKey) {
        // 一个有效的写端口是否和目前读的出现冲突
        bypassHit := true.B
        bypassData := io.w(j).req.data
        bypassIdx := io.w(j).req.idx
      }
    }
    
    io.r(i).resp.hit := hit || bypassHit
    io.r(i).resp.hitIdx := Mux(bypassHit, bypassIdx, hitIdx)
    io.r(i).resp.data := Mux(hit || bypassHit, bypassData, 0.U.asTypeOf(gen))
  }  // ==================== 写端口逻辑 ====================
  //直接写入 (假设多个写端口不会同时写同一个 entry)
  for (i <- 0 until numWritePorts) {
    when(io.w(i).en) {
      val writeIdx = io.w(i).req.idx
      entries(writeIdx).valid := io.w(i).req.valid
      entries(writeIdx).key := io.w(i).req.key
      entries(writeIdx).data := io.w(i).req.data
    }
  }
  
  // // 处理写冲突，如果多个端口写同一个 entry, 优先级高的端口(编号大的)优先
  // val mutex_w = Wire(Vec(numWritePorts, new CAMWritePort(gen, keyWidth, numEntries)))
  
  // // 默认连接所有写请求
  // for (i <- 0 until numWritePorts) {
  //   mutex_w(i).req := io.w(i).req
  //   mutex_w(i).en := io.w(i).en
  // }
  
  // // 检测并处理写冲突
  // for (i <- 0 until numWritePorts) {
  //   // 检查是否被更高优先级的端口抢占
  //   for (j <- i + 1 until numWritePorts) {
  //     // 如果端口 i 和端口 j 同时写入相同的 entry, 禁用优先级低的端口 i
  //     when(io.w(i).en && io.w(j).en && io.w(i).req.idx === io.w(j).req.idx) {
  //       mutex_w(i).en := false.B
  //     }
  //   }
  // }
  
  // // 使用经过冲突处理的写端口进行写入
  // for (i <- 0 until numWritePorts) {
  //   when(mutex_w(i).en) {
  //     val writeIdx = mutex_w(i).req.idx
  //     entries(writeIdx).valid := mutex_w(i).req.valid
  //     entries(writeIdx).key   := mutex_w(i).req.key
  //     entries(writeIdx).data  := mutex_w(i).req.data
  //   }
  // }

  // ==================== 调试接口 ====================
  io.debug.valid := VecInit(entries.map(_.valid))
  io.debug.keys := VecInit(entries.map(_.key))
}

