package surov3

import spinal.core._
import surov3.SoftStage.SoftS1
import surov3.SoftStage.SoftS2
import surov3.SoftStage.SoftS3

abstract class InstImpl(p: Pipeline) {
  def opcode: Opcode.C
  val KillFollowing = false
  protected final val NoAccess = B(0, p.cfg.regCount bits)
  protected final def regBits = log2Up(p.cfg.regCount)
  /** One-hot bitmask for register hazard tracking */
  protected def regMask(reg: UInt): Bits = B(1) << reg.resize(regBits)

  // --- Helpers for sync/async RF access ---
  /** Get rs1 via rf1: sync uses readRsp(), async uses c.r1 */
  protected def rs1(c: IExContext): UInt = if (p.cfg.syncRF) c.rf1.readRsp().asUInt else c.r1
  /** Get rs2 via rf2 (dual port): sync uses readRsp(), async uses c.r2 */
  protected def rs2Rf2(c: IExContext): UInt = if (p.cfg.syncRF) c.rf2.get.readRsp().asUInt else c.r2
  /** Get rs2 via rf1 (single port): sync uses readRsp(), async uses c.r2 */
  protected def rs2Rf1(c: IExContext): UInt = if (p.cfg.syncRF) c.rf1.readRsp().asUInt else c.r2

  // --- Default Stage Access Definitions ---

  // Default read: Most instructions issue read request for rs1 in S1.
  def getReadsS1(c: IExContext): Bits = regMask(rv.rs1(c.ir))
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

  /** S1: Read rs1 into c.r1. For sync RF, c.r1 gets stale data but rs1(c) works in S2. */
  def getS1(c: IExContext) = {
    c.r1 := c.rf1.readReq(rv.rs1(c.ir).resized).asUInt
    p.nextStage(c.id)
  }
  
  def getS2(c: IExContext) {}
  def getS3(c: IExContext) {}
}

class OpOpImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Op
  // S1: read rs1 (and rs2 if dual port)
  override def getReadsS1(c: IExContext) =
    if (p.cfg.enableDualPort)
      regMask(rv.rs1(c.ir)) | regMask(rv.rs2(c.ir))
    else
      regMask(rv.rs1(c.ir))
  override def getReadsS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else regMask(rv.rs2(c.ir))
  override def getWritesS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) regMask(rv.rd(c.ir)) else 0
  override def getWritesS3(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else regMask(rv.rd(c.ir))  
  override def getS1(c: IExContext): Unit = {
    c.r1 := c.rf1.readReq(rv.rs1(c.ir).resized).asUInt
    if (p.cfg.enableDualPort)
      c.r2 := c.rf2.get.readReq(rv.rs2(c.ir).resized).asUInt
    p.nextStage(c.id)
  }

  override def getS2(c: IExContext): Unit = {
    if (p.cfg.enableDualPort) {
      getExe(c, rs1(c), rs2Rf2(c))
    } else {
      c.r1 := rs1(c)  // Save rs1 before rf1 is used for rs2
      c.r2 := c.rf1.readReq(rv.rs2(c.ir).resized).asUInt
      p.nextStage(c.id)
    }
  }

  override def getS3(c: IExContext): Unit = {
    if (!p.cfg.enableDualPort)
      getExe(c, c.r1, rs2Rf1(c))
  }

  /** Execute ALU operation. r1Init/r2Init are initial operands;
   * uses c.r1/c.r2 on cycle 2+ for multi-cycle ops */
  def getExe(c: IExContext, r1Init: UInt, r2Init: UInt): Unit = {
    val r1Val = c.start ? r1Init | c.r1
    val r2Val = c.start ? r2Init | c.r2
    val (res, shamt_rem) = c.compute(r1Val, r2Val)
    c.r1 := res
    // For shadd: store rs2 only in cycle 1 (cycle 2 uses c.r2 which already has rs2)
    // For shifts: store shamt_rem every cycle for multi-cycle shift
    if (p.cfg.enableZba) {
      c.r2 := rv.shadd(c.ir) ? r2Init | shamt_rem.resized
    } else {
      c.r2 := shamt_rem.resized
    }
    when(c.alu.ready) {
      c.rf1.write(rv.rd(c.ir).resized, c.alu.result.asBits, guardZero = true)
      p.finish(c.id)
    }
  }
}

class OpImmImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.OpImm
  override def getWritesS2(c: IExContext): Bits = regMask(rv.rd(c.ir))
  override def getS2(c: IExContext): Unit = {
    val r1Val = c.start ? rs1(c) | c.r1
    val r2Val = c.start ? rv.imm_i(c.ir).asUInt | c.r2
    val (res, shamt_rem) = c.compute(r1Val, r2Val)
    c.r1 := res
    c.r2 := shamt_rem.resized
    when(c.alu.ready) {
      c.rf1.write(rv.rd(c.ir).resized, c.alu.result.asBits, guardZero = true)
      p.finish(c.id)
    }
  }
}

class AuipcImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Auipc
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = regMask(rv.rd(c.ir))
  override def getS1(c: IExContext) {
    val result = c.add(c.pc, rv.imm_u(c.ir).asUInt)
    c.rf1.write(rv.rd(c.ir).resized, result.asBits, guardZero = true)
    p.finish(c.id)
  }
}

class LuiImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode = Opcode.Lui
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = regMask(rv.rd(c.ir))
  override def getS1(c: IExContext) {
    c.rf1.write(rv.rd(c.ir).resized, rv.imm_u(c.ir).asBits, guardZero = true)
    p.finish(c.id)
  }
}

class LoadImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Load
  override def getWritesS3(c: IExContext): Bits = regMask(rv.rd(c.ir))
  override def getStageMemX(ss: SoftStage): Bool = Bool(ss == SoftS2)
  override def getWillMemX(ss: SoftStage): Bool = True
  
  override def getS2(c: IExContext): Unit = {
    val address = c.add(rs1(c), rv.imm_i(c.ir).asUInt)
    c.r1 := address  // Store for S3 alignment
    p.dmem.readReq(address)
    p.nextStage(c.id)
  }
  
  override def getS3(c: IExContext): Unit = {
    val data = p.dmem.readRsp(c.r1, rv.funct3(c.ir))
    c.rf1.write(rv.rd(c.ir).resized, data, guardZero = true)
    p.finish(c.id)
  }
}

class StoreImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Store
  override def getReadsS2(c: IExContext) = regMask(rv.rs2(c.ir))
  override def getStageMemX(ss: SoftStage): Bool = Bool(ss == (if (p.cfg.syncRF) SoftS3 else SoftS2))
  override def getWillMemX(ss: SoftStage): Bool = True
  
  override def getS2(c: IExContext): Unit = {
    if (p.cfg.syncRF) {
      c.r1 := rs1(c)  // Save rs1 before rf1 is used for rs2
      c.r2 := c.rf1.readReq(rv.rs2(c.ir).resized).asUInt
      p.nextStage(c.id)
    } else {
      // Async: both values available, write immediately
      val data = c.rf1.readReq(rv.rs2(c.ir).resized)
      val address = c.add(c.r1, rv.imm_s(c.ir).asUInt)
      p.dmem.write(address, data, Some(rv.funct3(c.ir)))
      p.finish(c.id)
    }
  }
  
  override def getS3(c: IExContext): Unit = {
    if (p.cfg.syncRF) {
      val data = rs2Rf1(c).asBits
      val address = c.add(c.r1, rv.imm_s(c.ir).asUInt)
      p.dmem.write(address, data, Some(rv.funct3(c.ir)))
      p.finish(c.id)
    }
  }
}

class JalImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jal
  override def getReadsS1(c: IExContext) = 0
  override def getWritesS1(c: IExContext): Bits = regMask(rv.rd(c.ir))
  override val KillFollowing = true
  override def getS1(c: IExContext): Unit = {
    val linkAddr = (c.pc + 4).asBits
    val jumpTarget = c.add(c.pc, rv.imm_j(c.ir).asUInt)
    c.rf1.write(rv.rd(c.ir).resized, linkAddr, guardZero = true)
    p.fetch_jump(c, jumpTarget)
    p.nextStage(c.id)
  }
  override def getS2(c: IExContext): Unit = {
    p.take_jump(c)
  }
}

class JalrImpl(p: Pipeline) extends InstImpl(p) {
  override def opcode: Opcode.C = Opcode.Jalr
  override val KillFollowing = true
  override def getWritesS3(c: IExContext): Bits = regMask(rv.rd(c.ir))
  override def getS2(c: IExContext): Unit = {
    val address = c.add(rs1(c), rv.imm_i(c.ir).asUInt)
    p.fetch_jump(c, address)
    p.nextStage(c.id)
  }
  
  override def getS3(c: IExContext): Unit = {
    val linkAddr = (c.pc + 4).asBits
    c.rf1.write(rv.rd(c.ir).resized, linkAddr, guardZero = true)
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
      c.rf1.write(rv.rd(c.ir).resized, csrVal.asBits, guardZero = true)
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
  override def getReadsS1(c: IExContext) =
    if (p.cfg.enableDualPort) 
      regMask(rv.rs1(c.ir)) | regMask(rv.rs2(c.ir))
    else
      regMask(rv.rs1(c.ir))
  override def getReadsS2(c: IExContext): Bits =
    if (p.cfg.enableDualPort) 0
    else regMask(rv.rs2(c.ir))
  override val KillFollowing = true

  override def getS1(c: IExContext): Unit = {
    c.r1 := c.rf1.readReq(rv.rs1(c.ir).resized).asUInt
    if (p.cfg.enableDualPort) {
      c.r2 := c.rf2.get.readReq(rv.rs2(c.ir).resized).asUInt
      // Pre-compute branch target so ALU is free for compare in S2
      p.pc2 := c.add(c.pc, rv.imm_b(c.ir).asUInt)
    }
    p.nextStage(c.id)
  }

  override def getS2(c: IExContext): Unit = {
    if (p.cfg.enableDualPort) {
      p.fetch_jump(c, p.pc2)
      val jumpCond = c.compute(rs1(c), rs2Rf2(c))._1(0)
      when(!jumpCond) { p.finish(c.id) }
      .otherwise { p.nextStage(c.id) }
    } else {
      c.r1 := rs1(c)  // Save rs1 before rf1 is used for rs2
      c.r2 := c.rf1.readReq(rv.rs2(c.ir).resized).asUInt
      val branchTarget = c.add(c.pc, rv.imm_b(c.ir).asUInt)
      p.fetch_jump(c, branchTarget)
      p.nextStage(c.id)
    }
  }

  override def getS3(c: IExContext): Unit = {
    if (p.cfg.enableDualPort) {
      p.take_jump(c)
    } else {
      val jumpCond = c.compute(c.r1, rs2Rf1(c))._1(0)
      when(jumpCond) { p.take_jump(c) }
      .otherwise { p.finish(c.id) }
    }
  }
}
