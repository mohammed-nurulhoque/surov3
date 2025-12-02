package surov3

import spinal.core._
import surov3.SoftStage.SoftS1
import surov3.SoftStage.SoftS2


abstract class InstImpl(p: Pipeline) {
  def opcode: Opcode.C
  val KillFollowing = false
  def getReads(c: IExContext)  = B(1) << rv.rs1(c.ir)
  def getWrites(c: IExContext) = B(1) << rv.rd(c.ir)
  def getStage(c: IExContext, ss: SoftStage) = {
    ss match {
      case SoftStage.SoftS1 => getS1(c)
      case SoftStage.SoftS2 => getS2(c)
      case SoftStage.SoftS3 => getS3(c)
    }
  }

  def getS1(c: IExContext) = {
    c.r1 := c.rf1.read(rv.rs1(c.ir)).asUInt
    if (p.cfg.enableDualPort)
      c.r2 := c.rf2.get.read(rv.rs2(c.ir)).asUInt
    p.nextStage(c)
  }
  def getS2(c: IExContext) {}
  def getS3(c: IExContext) {}
}

class OpOpImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Op
  override def getReads(c: IExContext) = (B(1) << rv.rs1(c.ir)) | (B(1) << rv.rs2(c.ir))
  override def getS2(c: IExContext) {
    if (p.cfg.enableDualPort) {
      getExe(c)
    } else {
      c.r2 := c.rf1.read(rv.rs2(c.ir)).asUInt
      p.nextStage(c)
    }
  }
  override def getS3(c: IExContext) {
    if (!p.cfg.enableDualPort) getExe(c)
  }

  def getExe(c: IExContext) {
    val (res, shamt_rem) = c.compute(c.r1, c.r2)
    c.r1 := res
    c.r2 := shamt_rem.resize(c.r2.getWidth)      
    when(c.alu.ready) {
      c.rf1.write(c, rv.rd(c.ir), c.alu.result.asBits) // FIXME
      p.finish(c)
    }
  }
}

class OpImmImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.OpImm
  override def getS2(c: IExContext) {
    val (res, shamt_rem) = c.compute(c.r1, c.start ? rv.imm_i(c.ir).asUInt | c.r2)
    c.r1 := res
    c.r2 := shamt_rem.resize(c.r2.getWidth)      
    when(c.alu.ready) {
      c.rf1.write(c, rv.rd(c.ir), c.alu.result.asBits) // FIXME
      p.finish(c)
    }
  }
}

class AuipcImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Auipc
  override def getReads(c: IExContext) = B(0)
  override def getS1(c: IExContext) {
    c.rf1.write(c, rv.rd(c.ir), c.add(c.pc, rv.imm_u(c.ir).asUInt).asBits)
    p.finish(c)
  }
}

class LuiImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Lui
  override def getReads(c: IExContext) = B(0)

  override def getS1(c: IExContext) {
    c.rf1.write(c, rv.rd(c.ir), rv.imm_u(c.ir).asBits)
    p.finish(c)
  }
}

class LoadImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Load
  override def getS2(c: IExContext) = {
    val address = c.add(c.r1, rv.imm_i(c.ir).asUInt)
    c.r1 := address
    p.dmem.readReq(address)
    p.nextStage(c)
  }
  override def getS3(c: IExContext) = {
    c.rf1.write(c, rv.rd(c.ir), p.dmem.readRsp(c.r1, Some(rv.funct3(c.ir))))
    p.finish(c)
  }
}

class StoreImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Store
  override def getReads(c: IExContext) = (B(1) << rv.rs1(c.ir)) | (B(1) << rv.rs2(c.ir))
  override def getWrites(c: IExContext) = 0
  override def getS2(c: IExContext): Unit = {
    p.dmem.write(
      c.add(c.r1, rv.imm_s(c.ir).asUInt),
      c.rf1.read(rv.rs2(c.ir)),
      Some (rv.funct3(c.ir))
    )
    p.finish(c)
  }
}
class JalImpl(p: Pipeline) extends  InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jal
  override def getReads(c: IExContext) = 0
  override val KillFollowing = true
  override def getS1(c: IExContext): Unit = {
    c.rf1.write(c, rv.rd(c.ir), c.pc + 4 asBits)
    p.fetch_jump(c, c.add(c.pc, rv.imm_j(c.ir).asUInt))
    p.nextStage(c)
  }
  override def getS2(c: IExContext): Unit = {
    p.take_jump(c)
  }
}

class JalrImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jalr
  override val KillFollowing = true
  override def getS2(c: IExContext): Unit = {
    p.fetch_jump(c, c.add(c.r1, rv.imm_i(c.ir).asUInt))
    p.nextStage(c)
  }
  override def getS3(c: IExContext) = {
    c.rf1.write(c, rv.rd(c.ir), c.pc + 4 asBits)
    p.take_jump(c)

  }
}

class SysImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Sys
  override val KillFollowing = true
  override def getS2(c: IExContext): Unit = {
    p.finish(c)
  }
}

class FenceImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Fence
  override def getS2(c: IExContext): Unit = p.finish(c)
}

class BranchImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Branch
  override def getReads(c: IExContext) = (B(1) << rv.rs1(c.ir)) | (B(1) << rv.rs2(c.ir))
  override val KillFollowing = true

  override def getS1(c: IExContext): Unit = {
    super[InstImpl].getS1(c)
    if (p.cfg.enableDualPort) {
      // FIXME. InstImpls are not supposed to access pc2 directly
      // find a better way to do this
      p.pc2 := c.add(c.pc, rv.imm_b(c.ir).asUInt)
    }
  }

  override def getS2(c: IExContext): Unit = {
    if (p.cfg.enableDualPort) {
      p.fetch_jump(c, p.pc2)
      when(!c.compute(c.r1, c.r2)._1(0)) {
        p.finish(c)
      } otherwise p.nextStage(c)
    } else {
      p.fetch_jump(c, c.add(c.pc, rv.imm_b(c.ir).asUInt))
      c.r2 := c.rf1.read(rv.rs2(c.ir)).asUInt
      p.nextStage(c)
    }
  }

  override def getS3(c: IExContext) = {
    if (p.cfg.enableDualPort) {
      p.take_jump(c)
    } else {
      when(c.compute(c.r1, c.r2)._1(0)) {
        p.take_jump(c)
      } otherwise {
        p.finish(c)
      }
    }
  }
}