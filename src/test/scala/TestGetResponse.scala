package coupledL2

import chisel3._
import chisel3.simulator.{ChiselSim, HasSimulator}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import huancun.{AliasField, AliasKey, DirtyField}
import org.chipsalliance.cde.config.Parameters
import coupledL2.tl2tl.TL2TLCoupledL2
import utility._

class GetResponseTestTop()(implicit p: Parameters) extends LazyModule {
  val client = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(
    TLMasterParameters.v1(name = "get", sourceId = IdRange(0, 4))
  ), requestFields = Seq(AliasField(2)))))
  val cached = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(
    TLMasterParameters.v1(name = "idle", sourceId = IdRange(0, 4), supportsProbe = TransferSizes(64))
  ), requestFields = Seq(AliasField(2)))))
  val init = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(
    TLMasterParameters.v1(name = "init", sourceId = IdRange(0, 1))
  ))))
  val l2 = LazyModule(new TL2TLCoupledL2)
  val xbar = TLXbar()
  xbar := client
  xbar := cached
  val downstream = TLXbar()
  downstream := TLFragmenter(32, 64) := TLCacheCork() := l2.node := xbar
  downstream := init
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffff), beatBytes = 32))
  ram.node := downstream

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val request = client.makeIOs()(ValName("request"))
    val preload = init.makeIOs()(ValName("preload"))
    val idle = cached.out.head._1
    idle.a.valid := false.B
    idle.a.bits := 0.U.asTypeOf(idle.a.bits)
    idle.b.ready := true.B
    idle.c.valid := false.B
    idle.c.bits := 0.U.asTypeOf(idle.c.bits)
    idle.d.ready := true.B
    idle.e.valid := false.B
    idle.e.bits := 0.U.asTypeOf(idle.e.bits)
    l2.module.io.hartId := 0.U
    l2.module.io.pfCtrlFromCore := 0.U.asTypeOf(l2.module.io.pfCtrlFromCore)
    l2.module.io.debugTopDown <> DontCare
    l2.module.io.l2_tlb_req <> DontCare
  }
}

object TestGetResponse extends App with ChiselSim {
  implicit val config: Parameters = baseConfig(1).alterPartial {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))), echoField = Seq(DirtyField()),
      enablePerf = false, enableRollingDB = false, enableTLLog = false,
      enableMonitor = false, elaboratedTopDown = false
    )
    case BankBitsKey => 0
    case huancun.BankBitsKey => 0
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.withName("VERBOSE"), 0)
  }
  implicit val simulator: HasSimulator = HasSimulator.simulators.verilator(
    compilationSettings = svsim.CommonCompilationSettings(
      availableParallelism = svsim.CommonCompilationSettings.AvailableParallelism.UpTo(3)
    ),
    verilatorSettings = svsim.verilator.Backend.CompilationSettings(disableFatalExitOnWarnings = true)
  )
  ChiselDB.init(false)
  val top = LazyModule(new GetResponseTestTop)
  def word(address: Int): BigInt = (BigInt("a5a5", 16) << 48) | address
  def beat(address: Int): BigInt = (0 until 4).map(i => word(address + i * 8) << (i * 64)).reduce(_ | _)
  simulate(top.module, firtoolOpts = Array("--disable-annotation-unknown")) { s =>
    val request = s.request.head
    val preload = s.preload.head
    def idle(port: TLBundle): Unit = {
      port.a.valid.poke(false.B)
      if (port.params.hasBCE) {
        port.b.ready.poke(true.B)
        port.c.valid.poke(false.B)
        port.e.valid.poke(false.B)
      }
      port.d.ready.poke(false.B)
    }
    idle(request)
    idle(preload)
    s.clock.step(150)
    def send(port: TLBundle, opcode: Int, size: Int, address: Int, data: BigInt = 0, param: Int = 0): Unit = {
      port.a.bits.opcode.poke(opcode.U)
      port.a.bits.param.poke(param.U)
      port.a.bits.size.poke(size.U)
      port.a.bits.source.poke(0.U)
      port.a.bits.address.poke(address.U)
      val bytes = 1 << math.min(size, 5)
      port.a.bits.mask.poke((((BigInt(1) << bytes) - 1) << (address % 32)).U)
      port.a.bits.data.poke(data.U)
      port.a.bits.corrupt.poke(false.B)
      port.a.bits.user.lift(AliasKey).foreach(_.poke(0.U))
      port.a.valid.poke(true.B)
      var wait = 0
      while (!port.a.ready.peek().litToBoolean && wait < 20000) { s.clock.step(); wait += 1 }
      assert(wait < 20000, "A timeout")
      s.clock.step()
      port.a.valid.poke(false.B)
    }
    def receive(port: TLBundle, opcode: Int, size: Int, address: Int): Unit = {
      val beats = if ((opcode & 1) != 0 && size > 5) 2 else 1
      for (i <- 0 until beats) {
        port.d.ready.poke(false.B)
        var wait = 0
        while (!port.d.valid.peek().litToBoolean && wait < 20000) { s.clock.step(); wait += 1 }
        assert(wait < 20000, "D timeout")
        val data = port.d.bits.data.peek().litValue
        println(s"D op=${port.d.bits.opcode.peek().litValue} size=${port.d.bits.size.peek().litValue} addr=0x${address.toHexString} beat=$i data=0x${data.toString(16)}")
        port.d.bits.opcode.expect(opcode.U)
        port.d.bits.size.expect(size.U)
        port.d.bits.source.expect(0.U)
        port.d.bits.denied.expect(false.B)
        port.d.bits.corrupt.expect(false.B)
        if (opcode == 1) {
          val shift = if (size < 5) (address % 32) * 8 else 0
          val mask = ((BigInt(1) << (8 * (1 << math.min(size, 5)))) - 1) << shift
          val expected = beat((address & ~31) + i * 32)
          assert((data & mask) == (expected & mask), s"wrong addressed data at 0x${address.toHexString}")
        }
        s.clock.step(3)
        port.d.valid.expect(true.B)
        port.d.bits.data.expect(data.U)
        port.d.ready.poke(true.B)
        s.clock.step()
      }
      s.clock.step(5)
      port.d.valid.expect(false.B)
      port.d.ready.poke(false.B)
    }
    for (address <- 0x1000 until 0x1200 by 32) {
      send(preload, 0, 5, address, beat(address))
      receive(preload, 0, 5, address)
    }
    // A cold 8-byte Get followed by every naturally aligned size in the cached block.
    send(request, 4, 3, 0x1048)
    receive(request, 1, 3, 0x1048)
    for (size <- 0 to 6; off <- 0 until 64 by (1 << size)) {
      send(request, 4, size, 0x1040 + off)
      receive(request, 1, size, 0x1040 + off)
    }
    // Every small size also gets a distinct cold line.
    for (size <- 0 to 5) {
      val address = 0x1080 + size * 64 + 32
      // These lines are initialized through the direct RAM client.
      send(preload, 0, 5, address & ~63, beat(address & ~63))
      receive(preload, 0, 5, address & ~63)
      send(preload, 0, 5, address, beat(address))
      receive(preload, 0, 5, address)
      send(request, 4, size, address)
      receive(request, 1, size, address)
    }
    println("PASS 134 Get responses with size, data, beat count and backpressure checks")
  }

  val chi = LazyModule(new coupledL2.tl2chi.TL2CHICoupledL2()(config.alterPartial {
    case EnableCHI => true
  }))
  for (manager <- top.client.edges.out.head.manager.managers ++ chi.managerParameters.managers) {
    assert(manager.supportsPutFull.none && manager.supportsPutPartial.none,
      "Put requests must not be advertised without a write payload path")
    assert(manager.supportsArithmetic.none && manager.supportsLogical.none,
      "Atomic requests must not be advertised without an atomic payload path")
    assert(manager.supportsGet == TransferSizes(1, 64))
    assert(manager.supportsAcquireB == TransferSizes(64))
    assert(manager.supportsAcquireT == TransferSizes(64))
  }
  println("PASS TL and CHI manager capabilities")
}
