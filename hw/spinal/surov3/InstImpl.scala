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

  // Default read: Most instructions issue read request for rs1 in S1.
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

  /** S1: Issue sync read request for rs1. Response available in S2 via readRsp(). */
  def getS1(c: IExContext) = {
    c.rf1.readReq(rv.rs1(c.ir))
    p.nextStage(c.id)
  }
  
  def getS2(c: IExContext) {}
  def getS3(c: IExContext) {}
}

class OpOpImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Op
  // S1: issue read request for rs1 (and rs2 if dual port)
  override def getReadsS1(c: IExContext) =
    if (p.cfg.enableDualPort)
      (B(1) << rv.rs1(c.ir)) | (B(1) << rv.rs2(c.ir))
    else
      B(1) << rv.rs1(c.ir)
  // S2: issue read request for rs2 (single port only)
  override def getReadsS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else B(1) << rv.rs2(c.ir)
  // Write happens in S2 for dual port, S3 for single port
  override def getWritesS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) B(1) << rv.rd(c.ir)
    else 0
  override def getWritesS3(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else B(1) << rv.rd(c.ir)
  
  override def getS1(c: IExContext): Unit = {
    c.rf1.readReq(rv.rs1(c.ir))
    if (p.cfg.enableDualPort)
      c.rf2.get.readReq(rv.rs2(c.ir))
    p.nextStage(c.id)
  }

  override def getS2(c: IExContext): Unit = {
    if (p.cfg.enableDualPort) {
      // Both values on readRsp, execute immediately
      getExe(c, c.rf1.readRsp().asUInt, c.rf2.get.readRsp().asUInt)
    } else {
      // Save rs1 before issuing rs2 request (overwrites readRsp)
      c.r1 := c.rf1.readRsp().asUInt
      c.rf1.readReq(rv.rs2(c.ir))
      p.nextStage(c.id)
    }
  }

  override def getS3(c: IExContext): Unit = {
    if (!p.cfg.enableDualPort) {
      getExe(c, c.r1, c.rf1.readRsp().asUInt)
    }
  }

  /** Execute ALU operation. r1Init/r2Init are initial operands; uses c.r1/c.r2 on cycle 2+ for multi-cycle ops */
  def getExe(c: IExContext, r1Init: UInt, r2Init: UInt): Unit = {
    val r1Val = c.start ? r1Init | c.r1
    val r2Val = c.start ? r2Init | c.r2
    val (res, shamt_rem) = c.compute(r1Val, r2Val)
    c.r1 := res
    c.r2 := shamt_rem.resize(c.r2.getWidth)
    when(c.alu.ready) {
      c.rf1.write(rv.rd(c.ir), c.alu.result.asBits, guardZero = true)
      p.finish(c.id)
    }
  }
}

class OpImmImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.OpImm

  override def getWritesS2(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  
  // S2: readRsp() has rs1 (persists since no other read), compute with immediate
  override def getS2(c: IExContext): Unit = {
    val r1Val = c.start ? c.rf1.readRsp().asUInt | c.r1
    val r2Val = c.start ? rv.imm_i(c.ir).asUInt | c.r2
    val (res, shamt_rem) = c.compute(r1Val, r2Val)
    c.r1 := res
    c.r2 := shamt_rem.resize(c.r2.getWidth)
    when(c.alu.ready) {
      c.rf1.write(rv.rd(c.ir), c.alu.result.asBits, guardZero = true)
      p.finish(c.id)
    }
  }
}

class AuipcImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Auipc
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override def getS1(c: IExContext) {
    c.rf1.write(rv.rd(c.ir), c.add(c.pc, rv.imm_u(c.ir).asUInt).asBits, guardZero = true)
    p.finish(c.id)
  }
}

class LuiImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Lui
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override def getS1(c: IExContext) {
    c.rf1.write(rv.rd(c.ir), rv.imm_u(c.ir).asBits, guardZero = true)
    p.finish(c.id)
  }
}

class LoadImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Load
  override def getWritesS3(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override def getStageMemX(ss: SoftStage): Bool = Bool(ss == SoftS2)
  override def getWillMemX(ss: SoftStage): Bool = True
  
  // S2: readRsp() has rs1 (persists), compute address, issue memory read
  override def getS2(c: IExContext): Unit = {
    val address = c.add(c.rf1.readRsp().asUInt, rv.imm_i(c.ir).asUInt)
    c.r1 := address  // Store address for S3 readRsp alignment
    p.dmem.readReq(address)
    p.nextStage(c.id)
  }
  
  override def getS3(c: IExContext): Unit = {
    c.rf1.write(rv.rd(c.ir), p.dmem.readRsp(c.r1, rv.funct3(c.ir)), guardZero = true)
    p.finish(c.id)
  }
}

class StoreImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Store
  // S2: issue read request for rs2
  override def getReadsS2(c: IExContext) = B(1) << rv.rs2(c.ir)
  // Memory access happens in S3
  override def getStageMemX(ss: SoftStage): Bool = Bool(ss == SoftS3)
  override def getWillMemX(ss: SoftStage): Bool = True
  
  // S2: Save rs1 (address base), issue read for rs2
  override def getS2(c: IExContext): Unit = {
    c.r1 := c.rf1.readRsp().asUInt  // Save before rs2 request overwrites readRsp
    c.rf1.readReq(rv.rs2(c.ir))
    p.nextStage(c.id)
  }
  
  // S3: readRsp() has rs2 (store data), write to memory
  override def getS3(c: IExContext): Unit = {
    p.dmem.write(
      c.add(c.r1, rv.imm_s(c.ir).asUInt),
      c.rf1.readRsp(),
      Some(rv.funct3(c.ir))
    )
    p.finish(c.id)
  }
}

class JalImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jal
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = B(1) << rv.rd(c.ir)
  override val KillFollowing = true
  override def getS1(c: IExContext): Unit = {
    c.rf1.write(rv.rd(c.ir), (c.pc + 4).asBits, guardZero = true)
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
  override def getWritesS3(c: IExContext): Bits = B(1) << rv.rd(c.ir)

  // S2: readRsp() has rs1 (persists), compute jump target
  override def getS2(c: IExContext): Unit = {
    p.fetch_jump(c, c.add(c.rf1.readRsp().asUInt, rv.imm_i(c.ir).asUInt))
    p.nextStage(c.id)
  }
  
  override def getS3(c: IExContext): Unit = {
    c.rf1.write(rv.rd(c.ir), (c.pc + 4).asBits, guardZero = true)
    p.take_jump(c)
  }
}

// Handles ECALL/EBREAK (funct3=0) and CSR instructions (funct3!=0)
// For ECALL/EBREAK: traps to simulation environment
// For CSR reads: supports Zicntr (cycle, time, instret)
class SysImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Sys
  // Stall until this is the only instruction (safe for syscalls)
  override def getReadsS1(c: IExContext): Bits = ~B(0, p.cfg.regCount bits)
  override def getWritesS1(c: IExContext): Bits = ~B(0, p.cfg.regCount bits)
  override def getStageMemX(ss: SoftStage): Bool = True
  override def getWillMemX(ss: SoftStage): Bool  = True

  override def getS1(c: IExContext): Unit = {
    val isCsr = rv.funct3(c.ir) =/= 0
    when (isCsr) {
      // CSR instruction - handle Zicntr reads
      val csrAddr = rv.csr(c.ir)
      val csrVal = UInt(p.cfg.xlen bits)
      csrVal := 0
      switch(csrAddr) {
        is(U(rv.CSR_CYCLE, 12 bits), U(rv.CSR_TIME, 12 bits))   { csrVal := p.mcycle(31 downto 0) }
        is(U(rv.CSR_CYCLEH, 12 bits), U(rv.CSR_TIMEH, 12 bits)) { csrVal := p.mcycle(63 downto 32) }
        is(U(rv.CSR_INSTRET, 12 bits))                          { csrVal := p.minstret(31 downto 0) }
        is(U(rv.CSR_INSTRETH, 12 bits))                         { csrVal := p.minstret(63 downto 32) }
      }
      c.rf1.write(rv.rd(c.ir), csrVal.asBits, guardZero = true)
      p.finish(c.id)
    } otherwise {
      // ECALL/EBREAK - trap to simulation
      p.trap(c.id) := True
      p.finish(c.id)
    }
  }
}

class FenceImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Fence
  override def getReadsS1(c: IExContext) = 0
  override def getS1(c: IExContext): Unit = p.finish(c.id)
}

class BranchImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Branch
  // S1: issue read request for rs1 (and rs2 if dual port)
  override def getReadsS1(c: IExContext) =
    if (p.cfg.enableDualPort)
      (B(1) << rv.rs1(c.ir)) | (B(1) << rv.rs2(c.ir))
    else
      B(1) << rv.rs1(c.ir)

  // S2: issue read request for rs2 (single port only)
  override def getReadsS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else B(1) << rv.rs2(c.ir)
  override val KillFollowing = true

  override def getS1(c: IExContext): Unit = {
    c.rf1.readReq(rv.rs1(c.ir))
    if (p.cfg.enableDualPort) {
      c.rf2.get.readReq(rv.rs2(c.ir))
      // Pre-compute branch target (pc+imm doesn't need register values)
      // so ALU is free for compare in S2
      p.pc2 := c.add(c.pc, rv.imm_b(c.ir).asUInt)
    }
    p.nextStage(c.id)
  }

  override def getS2(c: IExContext): Unit = {
    if (p.cfg.enableDualPort) {
      // Both values on readRsp
      val r1Val = c.rf1.readRsp().asUInt
      val r2Val = c.rf2.get.readRsp().asUInt
      p.fetch_jump(c, p.pc2)  // Use pre-computed target
      // Early branch resolution
      when(!c.compute(r1Val, r2Val)._1(0)) {
        p.finish(c.id)  // Branch not taken
      } otherwise {
        p.nextStage(c.id)  // Branch taken, go to S3
      }
    } else {
      // Save rs1 before rs2 request overwrites readRsp
      c.r1 := c.rf1.readRsp().asUInt
      c.rf1.readReq(rv.rs2(c.ir))
      p.fetch_jump(c, c.add(c.pc, rv.imm_b(c.ir).asUInt))
      p.nextStage(c.id)
    }
  }

  override def getS3(c: IExContext): Unit = {
    if (p.cfg.enableDualPort) {
      // Only reached if branch taken
      p.take_jump(c)
    } else {
      // readRsp() has rs2
      when(c.compute(c.r1, c.rf1.readRsp().asUInt)._1(0)) {
        p.take_jump(c)
      } otherwise {
        p.finish(c.id)
      }
    }
  }
}
