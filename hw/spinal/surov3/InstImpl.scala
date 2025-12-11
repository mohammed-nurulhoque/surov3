package surov3

import spinal.core._
import surov3.SoftStage.SoftS1
import surov3.SoftStage.SoftS2
import surov3.SoftStage.SoftS3


abstract class InstImpl(p: Pipeline) {
  def opcode: Opcode.C
  val KillFollowing = false
  protected final val NoAccess = B(0, p.cfg.regCount bits)

  // --- Default Stage Access Definitions ---

  // Default read: Most instructions read rs1 in S1.
  def getReadsS1(c: IExContext): Bits = B(1) << rv.rs1(c.ir)

  // Default read: No reads in S2 or S3.
  def getReadsS2(c: IExContext): Bits = 0
  def getReadsS3(c: IExContext): Bits = 0

  // Default write: No writes in S1, S2, or S3
  def getWritesS1(c: IExContext): Bits = 0
  def getWritesS2(c: IExContext): Bits = 0
  def getWritesS3(c: IExContext): Bits = 0

  // --- Core Access Methods (Template Implementation) ---

  /** Implements the required abstract method by calling the stage-specific defaults/overrides. */
  final def getStageReads(c: IExContext, ss: SoftStage): Bits = ss match {
    case SoftStage.SoftS1 => getReadsS1(c)
    case SoftStage.SoftS2 => getReadsS2(c)
    case SoftStage.SoftS3 => getReadsS3(c)
  }

  /** Implements the required abstract method by calling the stage-specific defaults/overrides. */
  final def getStageWrites(c: IExContext, ss: SoftStage): Bits = ss match {
    case SoftStage.SoftS1 => getWritesS1(c)
    case SoftStage.SoftS2 => getWritesS2(c)
    case SoftStage.SoftS3 => getWritesS3(c)
  }

  // --- Composite Access Definitions (Unchanged) ---

  final def getWillRead(c: IExContext, current: SoftStage): Bits =
    SoftStage.following(current).map(ss => getStageReads(c, ss))
    .fold(NoAccess)(_ | _)

  final def getWillWrite(c: IExContext, current: SoftStage): Bits =
    SoftStage.currentAndFollowing(current).map(ss => getStageWrites(c, ss))
    .fold(NoAccess)(_ | _)
  
  def getStageMemX(ss: SoftStage) = False
  def getWillMemX(ss: SoftStage)  = False

  final def getStage(c: IExContext, ss: SoftStage) = {
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
    p.nextStage(c.id)
  }
  def getS2(c: IExContext) {}
  def getS3(c: IExContext) {}
}

class OpOpImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Op
  override def getReadsS1(c: IExContext) =
    if (p.cfg.enableDualPort)
      B(1) << rv.rs1(c.ir)
    else
      (B(1) << rv.rs1(c.ir)) | (B(1) << rv.rs2(c.ir))
  override def getReadsS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else B(1) << rv.rs2(c.ir)
  override def getWritesS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) B(1) << rv.rd(c.ir)
    else 0
  override def getWritesS3(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else B(1) << rv.rd(c.ir)

  override def getS2(c: IExContext) {
    if (p.cfg.enableDualPort) {
      getExe(c)
    } else {
      c.r2 := c.rf1.read(rv.rs2(c.ir)).asUInt
      p.nextStage(c.id)
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
      p.finishIfNotStalled(c.id)
    }
  }
}

class OpImmImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.OpImm

  override def getWritesS2(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override def getS2(c: IExContext) {
    val (res, shamt_rem) = c.compute(c.r1, c.start ? rv.imm_i(c.ir).asUInt | c.r2)
    c.r1 := res
    c.r2 := shamt_rem.resize(c.r2.getWidth)
    when(c.alu.ready) {
      c.rf1.write(c, rv.rd(c.ir), c.alu.result.asBits) // FIXME
      p.finishIfNotStalled(c.id)
    }
  }
}

class AuipcImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Auipc
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override def getS1(c: IExContext) {
    c.rf1.write(c, rv.rd(c.ir), c.add(c.pc, rv.imm_u(c.ir).asUInt).asBits)
    p.finishIfNotStalled(c.id)
  }
}

class LuiImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Lui
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override def getS1(c: IExContext) {
    c.rf1.write(c, rv.rd(c.ir), rv.imm_u(c.ir).asBits)
    p.finishIfNotStalled(c.id)
  }
}

class LoadImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Load
  override def getWritesS3(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override def getStageMemX(ss: SoftStage): Bool = Bool(ss == SoftS2)
  override def getWillMemX(ss: SoftStage): Bool = True
  override def getS2(c: IExContext) = {
    val address = c.add(c.r1, rv.imm_i(c.ir).asUInt)
    c.r1 := address
    when (~c.stall) {
      p.dmem.readReq(address)
    }
    p.nextStage(c.id)
  }
  override def getS3(c: IExContext) = {
    c.rf1.write(c, rv.rd(c.ir), p.dmem.readRsp(c.r1, Some(rv.funct3(c.ir))))
    p.finishIfNotStalled(c.id)
  }
}

class StoreImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Store
  override def getReadsS2(c: IExContext) = B(1) << rv.rs2(c.ir)
  override def getStageMemX(ss: SoftStage): Bool = Bool(ss == SoftS2)
  override def getWillMemX(ss: SoftStage): Bool = True
  override def getS2(c: IExContext): Unit = {
    when (~c.stall) {
      p.dmem.write(
        c.add(c.r1, rv.imm_s(c.ir).asUInt),
        c.rf1.read(rv.rs2(c.ir)),
        rv.funct3(c.ir)
      )
    }
    p.finishIfNotStalled(c.id)
  }
}
class JalImpl(p: Pipeline) extends  InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jal
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override val KillFollowing = true
  override def getS1(c: IExContext): Unit = {
    c.rf1.write(c, rv.rd(c.ir), c.pc + 4 asBits)
    p.fetch_jump(c, c.add(c.pc, rv.imm_j(c.ir).asUInt))
    p.nextStage(c.id)
  }
  override def getS2(c: IExContext): Unit = {
    p.take_jump(c)
  }
}

class JalrImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jalr
  override val KillFollowing = true
override def getWritesS2(c: IExContext): Bits = B(1) << rv.rd(c.ir)

  override def getS2(c: IExContext): Unit = {
    p.fetch_jump(c, c.add(c.r1, rv.imm_i(c.ir).asUInt))
    p.nextStage(c.id)
  }
  override def getS3(c: IExContext) = {
    c.rf1.write(c, rv.rd(c.ir), c.pc + 4 asBits)
    p.take_jump(c)

  }
}

class SysImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Sys
  override val KillFollowing = true

  // This is a hack to flush pipeline by S3
  val counter = Reg(UInt(2 bits))
  override def getS1(c: IExContext): Unit = {
    counter := 3
    p.nextStage(c.id)
  }

  override def getS2(c: IExContext): Unit = {
    counter := counter - 1
    when (counter === U(0, 2 bits)) (p.nextStage(c.id))
  }

  override def getS3(c: IExContext): Unit = {
    p.trap(c.id) := True // breaks abstraction
    p.finishIfNotStalled(c.id)
  }
}

class FenceImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Fence
  override def getS2(c: IExContext): Unit = p.finishIfNotStalled(c.id)
}

class BranchImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Branch
  override def getReadsS1(c: IExContext) =
    if (p.cfg.enableDualPort)
      (B(1) << rv.rs1(c.ir)) | (B(1) << rv.rs2(c.ir))
    else
      B(1) << rv.rs1(c.ir)

  override def getReadsS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else B(1) << rv.rs2(c.ir)
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
        p.finishIfNotStalled(c.id)
      } otherwise p.nextStage(c.id)
    } else {
      p.fetch_jump(c, c.add(c.pc, rv.imm_b(c.ir).asUInt))
      c.r2 := c.rf1.read(rv.rs2(c.ir)).asUInt
      p.nextStage(c.id)
    }
  }

  override def getS3(c: IExContext) = {
    if (p.cfg.enableDualPort) {
      p.take_jump(c)
    } else {
      when(c.compute(c.r1, c.r2)._1(0)) {
        p.take_jump(c)
      } otherwise {
        p.finishIfNotStalled(c.id)
      }
    }
  }
}