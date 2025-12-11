package surov3

import spinal.core._
import spinal.core.sim._

// S0 is a special stage the is idle
object Enum extends SpinalEnum {
  val Op, OpImm, Auipc, Lui, Load, Store, Branch, Jal, Jalr, Fence, Sys = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    Op    -> 0b01_100_11,
    OpImm -> 0b00_100_11,
    Auipc -> 0b00_101_11,
    Lui   -> 0b01_101_11,
    Load  -> 0b00_000_11,
    Store -> 0b01_000_11,
    Branch-> 0b11_000_11,
    Jal   -> 0b11_011_11,
    Jalr  -> 0b11_001_11,
    Fence -> 0b00_011_11,
    Sys   -> 0b11_100_11,
  )
}

// Minimal repro: higher-order scanLeft/fold on simPublic Vecs, and a simPublic enum.
case class SimPublicFoldBug(issueWidth: Int = 4, regCount: Int = 8) extends Component {
  val mem = Mem(Bits(32 bits), wordCount = 32).simPublic()
  val counter = RegInit(U"00000")
  counter := counter + 1
  def getEnumAt0 (x: Bits): Enum.C = { val o = Enum(); o.assignFromBits(x(6 downto 0)); o }

  // simPublic enum derived from bits (to test toEnum visibility)
  val op = getEnumAt0(mem(counter)).simPublic
  val opBitsOut = out(op.asBits)

  // Pipeline-like signals that previously disappeared when built with folds
  val stageReads   = Vec.fill(issueWidth)(Bits(regCount bits)).simPublic
  val willRead     = Vec.fill(issueWidth)(Bits(regCount bits)).simPublic
  val readScan     = Vec(willRead.scanLeft(B(0, regCount bits))(_ | _)).simPublic

  // Deterministic contents so the signals are driven
  stageReads.zipWithIndex.foreach { case (s, i) =>
    s := (B(1) << U(i, log2Up(regCount) bits)).resize(regCount bits)
  }

  // Higher-order fold version (suspected to be dropped with simPublic)
  willRead.zipWithIndex.foreach { case (w, i) =>
    w := stageReads.drop(i).fold(B(0, regCount bits))(_ | _)
  }

  // Original fold/scan experiment on a single word
  val folded = out (Bits(32 bits))
  switch (op) {
    is(Enum.Op) {
      folded := mem(counter).asBools.drop(1).scanLeft(False)(_ | _).foldLeft(B"")(_ ## _.asBits)
    }
    is(Enum.OpImm) {
      folded := mem(counter).asBools.drop(1).scanLeft(True)(_ & _).foldLeft(B"")(_ ## _.asBits)
    }
    default {
      folded := mem(counter).asBools.drop(1).scanLeft(True)(_ ^ _).foldLeft(B"")(_ ## _.asBits)
    }
  }

  // Expose the potentially missing signals so the sim can probe them
  val willReadOut     = out(willRead(0))
  val readScanOut     = out(readScan(1))
}

object SimPublicFoldBugSim extends App {
  SimConfig.withConfig(Config.spinal).compile(SimPublicFoldBug()).doSim { dut =>
    fork {
        while(true) {
          dut.clockDomain.clockToggle()
          sleep(5)
        }
      }
    for (i <- 0 to 15) {
      dut.mem.setBigInt(i*2,   0x13)
      dut.mem.setBigInt(i*2+1, 0x33)
    }
    dut.clockDomain.waitActiveEdge()
    for (i <- 0 to 32) {
      println(s"op.toEnum = ${dut.op.toEnum}")          // enum simPublic access
      println(s"opBitsOut = ${dut.opBitsOut.toLong}")
      println(s"willRead(0) = ${dut.willRead(0).toBigInt}")
      println(s"willReadOut = ${dut.willReadOut.toBigInt}")
      println(s"readScanOut = ${dut.readScanOut.toBigInt}")
      println(s"folded = ${dut.folded.toLong}")
      dut.clockDomain.waitActiveEdge()
    }
    simSuccess()
  }
}
