package surov3

import spinal.core._
import surov3.SoftStage.SoftS1
import surov3.SoftStage.SoftS2


abstract class InstImpl(p: Pipeline) {
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
    if (p.cfg.enableDualPort)
      c.r2 := c.rf2.get.read(rv.rs2(c.ir)).asUInt
    p.nextStage()
  }
  def getS2() {}
  def getS3() {}
}

class OpOpImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Op

  override def getS2() {
    if (p.cfg.enableDualPort) {
      getExe()
    } else {
      c.r2 := c.rf1.read(rv.rs2(c.ir)).asUInt
      p.nextStage()
    }
  }
  override def getS3() {
    if (!p.cfg.enableDualPort) getExe()
  }

  def getExe() {
    val (res, shamt_rem) = c.compute(c.r1, c.r2)
    c.r1 := res
    c.r2 := shamt_rem.resize(c.r2.getWidth)      
    when(c.alu.ready) {
      c.rf1.write(rv.rd(c.ir), c.alu.result.asBits) // FIXME
      p.finish()
    }
  }
}

class OpImmImpl(p: Pipeline) extends InstImpl(p) {
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

class AuipcImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Auipc
  override def getS1() {
    c.rf1.write(rv.rd(c.ir), c.add(c.pc, rv.imm_u(c.ir).asUInt).asBits)
    p.finish()
  }
}

class LuiImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Lui
  override def getS1() {
    c.rf1.write(rv.rd(c.ir), rv.imm_u(c.ir).asBits)
    p.finish()
  }
}

class LoadImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Load
  override def getS2() = {
    val address = c.add(c.r1, rv.imm_i(c.ir).asUInt)
    c.r1 := address
    c.dmem.readReq(address)
    p.nextStage()
  }
  override def getS3() = {
    c.rf1.write(rv.rd(c.ir), c.dmem.readRsp(c.r1, Some(rv.funct3(c.ir))))
    p.finish()
  }
}

class StoreImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Store
  override def getS2(): Unit = {
    c.dmem.write(
      c.add(c.r1, rv.imm_s(c.ir).asUInt),
      c.rf1.read(rv.rs2(c.ir)),
      Some (rv.funct3(c.ir))
    )
    p.finish()
  }
}
class JalImpl(p: Pipeline) extends  InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jal
  override def getS1(): Unit = {
    c.rf1.write(rv.rd(c.ir), c.pc + 4 asBits)
    p.fetch_jump(c.add(c.pc, rv.imm_j(c.ir).asUInt))
    p.take_jump()
  }
}

class JalrImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jalr
  override def getS2(): Unit = {
    p.fetch_jump(c.add(c.r1, rv.imm_i(c.ir).asUInt))
    p.nextStage()
  }
  override def getS3() = {
    c.rf1.write(rv.rd(c.ir), c.pc + 4 asBits)
    p.take_jump()

  }
}

class SysImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Sys
  override def getS1(): Unit = {
    c.trap := True
    p.finish()
  }
}

class FenceImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Fence
  override def getS2(): Unit = p.finish()
}

class BranchImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Branch

  override def getS1(): Unit = {
    super[InstImpl].getS1()
    if (p.cfg.enableDualPort) {
      // FIXME. InstImpls are not supposed to access pc2 directly
      // find a better way to do this
      c.pc2 := c.add(c.pc, rv.imm_b(c.ir).asUInt)
    }
  }
  override def getS2(): Unit = {
    if (p.cfg.enableDualPort) {
      p.fetch_jump(c.pc2)
      when(!c.compute(c.r1, c.r2)._1(0)) {
        p.finish()
      } otherwise p.nextStage()
    } else {
      p.fetch_jump(c.add(c.pc, rv.imm_b(c.ir).asUInt))
      c.r2 := c.rf1.read(rv.rs2(c.ir)).asUInt
      p.nextStage()
    }
  }

  override def getS3() = {
    if (p.cfg.enableDualPort) {
      p.take_jump()
    } else {
      when(c.compute(c.r1, c.r2)._1(0)) {
        p.take_jump()
      } otherwise {
        p.finish()
      }
    }
  }
}