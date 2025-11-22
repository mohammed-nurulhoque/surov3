package projectname

import spinal.core._
import projectname.SoftStage.SoftS1
import projectname.SoftStage.SoftS2


abstract class Plugin(p: Pipeline) {
  val c = p.c
  def opcode: Opcode.C
  def getStage(ss: SoftStage) = {
    ss match {
      case SoftStage.SoftS1 => getS1()
      case SoftStage.SoftS2 => getS2()
      case SoftStage.SoftS3 => getS3()
    }
  }

  def getS1() = {
    c.r1 := c.rf1.read(rv.rs1(c.ir)).asUInt
    p.nextStage()
  }
  def getS2() {}
  def getS3() {}
}

class OpOpPlugin(p: Pipeline) extends Plugin(p) {
  override def opcode = Opcode.Op
  override def getS2() {
      c.r2 := c.rf1.read(rv.rs2(c.ir)).asUInt
      p.nextStage()
  }
  override def getS3() {
    val (res, shamt_rem) = c.compute(c.r1, c.r2)
    c.r1 := res
    c.r2 := shamt_rem.resize(c.r2.getWidth)      
    when(c.alu.ready) {
      c.rf1.write(rv.rd(c.ir), c.alu.result.asBits) // FIXME
      p.finish()
    }
  }
}

class OpImmPlugin(p: Pipeline) extends Plugin(p) {
  override def opcode = Opcode.OpImm
  override def getS2() {
    val (res, shamt_rem) = c.compute(c.r1, c.start ? rv.imm_i(c.ir).asUInt | c.r2)
    c.r1 := res
    c.r2 := shamt_rem.resize(c.r2.getWidth)      
    when(c.alu.ready) {
      c.rf1.write(rv.rd(c.ir), c.alu.result.asBits) // FIXME
      p.finish()
    }
  }
}

class AuipcPlugin(p: Pipeline) extends Plugin(p) {
  override def opcode = Opcode.Auipc
  override def getS1() {
    c.rf1.write(rv.rd(c.ir), c.add(c.pc, rv.imm_u(c.ir).asUInt).asBits)
    p.finish()
  }
}

class LuiPlugin(p: Pipeline) extends Plugin(p) {
  override def opcode = Opcode.Lui
  override def getS1() {
    c.rf1.write(rv.rd(c.ir), rv.imm_u(c.ir).asBits)
    p.finish()
  }
}