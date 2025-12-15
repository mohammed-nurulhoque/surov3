package surov3

import spinal.core._
import spinal.core.sim._
import surov3.Utils._
import surov3.SoftStage.SoftS1

case class SurovConfig(
  regCount: Int = 32,
  enableDualPort: Boolean = false,
  issueWidth: Int = 1,
  enableForward: Boolean = false,
  xlen: Int = 32,
  syncRF: Boolean = true,  // true = sync read (1 cycle latency), false = async read (combinational)
  enableZba: Boolean = false,  // Zba address generation extension (sh1add, sh2add, sh3add)
)

/** Unified synchronous port interface for register file and memory access.
  * @param dataWidth    Width of data bus in bits
  * @param addrWidth    Width of address bus in bits
  * @param writable     Whether write operations are supported
  * @param isByteAddress If true, addresses are byte addresses (shifted for word access)
  * @param extendWidth  Target width for sign extension in readRsp (typically xlen)
  * @param hasReady     Whether to include ready signal (for memory handshake)
  */
class PortIF(
  val dataWidth: Int,
  val addrWidth: Int,
  val writable: Boolean = true,
  val isByteAddress: Boolean = false,
  val extendWidth: Int = 32,
  val hasReady: Boolean = false,
) extends Bundle {
  val addr = out UInt(addrWidth bits)
  val valid = out Bool()
  val readData = in Bits(dataWidth bits)
  val wren = Option.when(writable)(out Bool())
  val writeData = Option.when(writable)(out Bits(dataWidth bits))
  val ready = Option.when(hasReady)(in Bool)

  private def wordBytes = dataWidth / 8
  private def addrShift = if (isByteAddress) log2Up(wordBytes) else 0
  val wmask = Option.when(writable && isByteAddress)(out Bits(wordBytes bits))

  /** Issue read request and return read data.
    * For sync RF: returns data from previous cycle's request (current return value is stale).
    * For async RF: returns data for current address (valid same cycle).
    */
  def readReq(address: UInt): Bits = {
    addr := (if (addrShift > 0) address.drop(addrShift).asUInt else address)
    valid := True
    wren.foreach(_ := False)
    readData
  }

  /** Get read response (alias for readData). */
  def readRsp(): Bits = readData

  /** Read response with RISC-V load alignment and sign extension.
    * @param address  Original byte address (for alignment calculation)
    * @param funct3   RISC-V funct3 for load width/sign
    */
  def readRsp(address: UInt, funct3: Bits): Bits = {
    val byteOffset   = address(0, log2Up(wordBytes) bits)
    val bitOffset    = byteOffset << 3
    val shiftedData  = readData |>> bitOffset
    val logbyteCount = funct3.dropHigh(1).asUInt
    val byteCount    = U(1, log2Up(wordBytes).max(3) bits) |<< logbyteCount

    // Build contiguous bitmask for requested byte/half/word
    val bitmask = ~(~B(0, dataWidth bits) |<< (byteCount << 3))

    // Pick sign bit for requested width
    val msb = Bool
    switch(logbyteCount) {
      is(U(0, 2 bits)) { msb := shiftedData(7) }
      is(U(1, 2 bits)) { msb := shiftedData(15) }
      is(U(2, 2 bits)) { msb := shiftedData(31) }
      default          { msb.assignDontCare() }
    }

    val signBit = funct3.msb ? False | msb // unsigned loads zero-extend
    (shiftedData & bitmask | (~bitmask).andMask(signBit)).resize(extendWidth bits)
  }

  /** Write operation.
    * @param address    Address to write
    * @param data       Data to write
    * @param funct3     If Some and isByteAddress, applies RISC-V store alignment and byte mask
    * @param guardZero  If true, prevents writes to address 0 (for x0 register)
    */
  def write(address: UInt, data: Bits, funct3: Option[Bits] = None, guardZero: Boolean = false): Unit = {
    assert(writable, "write to non-writable port interface")
    valid := True

    if (funct3.isDefined && isByteAddress) {
      // RISC-V Store: alignment and byte mask
      val f3 = funct3.get
      val byteOffset   = address(0, log2Up(wordBytes) bits)
      val bitOffset    = byteOffset << 3
      val logbyteCount = f3.dropHigh(1).asUInt
      val byteCount    = U(1, log2Up(wordBytes) + 1 bits) |<< logbyteCount
      addr          := address.drop(log2Up(wordBytes)).asUInt
      wren.get      := True
      writeData.get := (data << bitOffset).resize(dataWidth bits)
      wmask.foreach(_ := (~(~B(0, wordBytes bits) |<< byteCount)) |<< byteOffset)
    } else {
      // Simple write (register file style)
      addr := address
      wren.get := (if (guardZero) address.orR else True)
      writeData.get := data
    }
  }
}

// Factory methods for common port configurations
object PortIF {
  /** Create a register file port */
  def rf(xlen: Int, regCount: Int, writable: Boolean = true) =
    new PortIF(xlen, log2Up(regCount), writable)

  /** Create a memory port with byte addressing */
  def mem(xlen: Int, wordBytes: Int, writable: Boolean) =
    new PortIF(wordBytes * 8, xlen - log2Up(wordBytes), writable,
      isByteAddress = true, extendWidth = xlen, hasReady = true)
}

class ALU(xlen: Int, regCount: Int) extends BlackBox {  
  val clk = in Bool
  val start = in Bool
  val src_a = in (UInt(xlen bits))
  val src_b = in (UInt(xlen bits))
  val f3 = in Bits(3 bits)
  val arith_bit = in Bool()
  val shadd = in Bool()
  val branch = in Bool()
  val result = out UInt(xlen bits)
  val shamt_out = out UInt(log2Up(regCount) bits)
  val ready = out Bool()

  mapCurrentClockDomain(clock = clk)
  addRTLPath("./hw/verilog/alu.v")
}

// S0 is a special stage that is idle
object Stage extends SpinalEnum {
  val S0, S1, S2, S3 = newElement()
}

sealed trait SoftStage {
  def harden: Stage.C
}

object SoftStage {
  case object SoftS1 extends SoftStage { def harden = Stage.S1 }
  case object SoftS2 extends SoftStage { def harden = Stage.S2 }
  case object SoftS3 extends SoftStage { def harden = Stage.S3 }

  def succ(ss: SoftStage) = ss match {
    case SoftS1 => SoftS2
    case SoftS2 => SoftS3
  }

  def following(ss: SoftStage): Seq[SoftStage] = ss match {
    case SoftS1 => Seq(SoftS2, SoftS3)
    case SoftS2 => Seq(SoftS3)
    case SoftS3 => Seq()
  }
  
  def currentAndFollowing(ss: SoftStage): Seq[SoftStage] = ss +: following(ss)
}

class IExContext(cfg: SurovConfig, val id: Int,
  val pc : UInt,
  val ir : Bits,
  val stage : Stage.C,
  val start : Bool,
  val stall : Bool,
  val kill  : Bool,
) {
  pc.simPublic
  ir.simPublic
  stage.simPublic
  start.simPublic

  val op = rv.opcode(ir).simPublic

  val r1 = Reg(UInt(cfg.xlen bits)).simPublic()
  val r2 = Reg(UInt(cfg.xlen bits)).simPublic()
  val alu = new ALU(cfg.xlen, cfg.regCount)

  val rf1 = PortIF.rf(cfg.xlen, cfg.regCount, writable = true)
  rf1.addr.assignDontCare(); rf1.valid := False; rf1.wren.get := False; rf1.writeData.get.assignDontCare()
  val rf2 = Option.when(cfg.enableDualPort)(PortIF.rf(cfg.xlen, cfg.regCount, writable = false))
  rf2.foreach { r => r.addr.assignDontCare(); r.valid := False }
  alu.f3 := rv.F3_ADDSUB
  List(alu.src_a, alu.src_b, alu.start) map (_.assignDontCare)
  List(alu.arith_bit, alu.shadd, alu.branch) map (_ := False)

  when(!alu.ready) {  // FIXME an instruction can choose to stall for other reasons
    start := False
  }

  def add(a: UInt, b: UInt) = {
    alu.src_a := a
    alu.src_b := b
    alu.f3 := B"000"
    alu.result
  }

  def compute(a: UInt, b: UInt) = {
    alu.src_a  := a
    alu.src_b  := b
    alu.f3 := rv.funct3(ir)
    alu.arith_bit := rv.arith(ir)
    alu.shadd := (if (cfg.enableZba) rv.shadd(ir) else False)
    alu.branch := rv.opcode(ir) === Opcode.Branch
    alu.start   := start
    (alu.result, alu.shamt_out)
  }
}

class Pipeline(val cfg: SurovConfig) {
  // base of the issue group
  val pcBase = Reg(UInt(cfg.xlen bits)) init(0) simPublic
  val pc2 = Reg(UInt(cfg.xlen bits)).simPublic // jump target
  val irBuf = Vec.fill(cfg.issueWidth)(Reg(Bits(cfg.xlen bits))).simPublic
  irBuf(0).init(Opcode.Jal.asBits.resize(32) | 0x1000)
  val jumping = False simPublic  // current cycle issued jump
  val jumped = Reg(Bool).init(False) simPublic  // current group issued jump earlier
  val ir2 = Reg(Bits(cfg.xlen * cfg.issueWidth bits)).simPublic
  val stage = Vec.fill(cfg.issueWidth)(Reg(Stage).init(Stage.S1)) // multicycle stage
  val start = Vec.fill(cfg.issueWidth)(Reg(Bool).init(True)) // 1st cycle of current stage

  // ### Hazard detection logic ###

  // - stage##Op variables indicate if each instruction does Op in the current stage
  // - will##Op variables indicate if each instruction will do Op will do Op (exclusive of
  // current stage for reads, inclusive otherwise).
  // - Op##Scan variables indicate for each instruction if any predecessor will do Op
  val stageReads  = Vec.fill(cfg.issueWidth)(B(0, cfg.regCount bits)).simPublic
  val willRead    = Vec.fill(cfg.issueWidth)(B(0, cfg.regCount bits)).simPublic
  val readScan    = Vec(willRead.scanLeft(B(0, cfg.regCount bits))(_ | _)).simPublic

  val stageWrites = Vec.fill(cfg.issueWidth)(B(0, cfg.regCount bits)).simPublic
  val willWrite   = Vec.fill(cfg.issueWidth)(B(0, cfg.regCount bits)).simPublic
  val writeScan   = Vec(willWrite.scanLeft(B(0, cfg.regCount bits))(_ | _)).simPublic

  // memory access hazards
  val stageMemX  = Vec.fill(cfg.issueWidth)(False).simPublic
  val willMemX   = Vec.fill(cfg.issueWidth)(False).simPublic
  val memScan  = Vec(willMemX.scanLeft(False)(_ | _)).simPublic

  // overlap current stage operation with scan of all predecessors to check hazard
  def checkOverlap[T <: Data with BitwiseOp[T]](access: Vec[T], scan: Vec[T], reduce: T => Bool): Vec[Bool] = {
    Vec(access.zip(scan)
    .map({ case (a, b) => reduce(a & b) }))
  }
  // drop first element because x0 is not hazard dependency
  val raw  = checkOverlap (stageReads,  writeScan, (b: Bits) => b.drop(1).orR).simPublic
  val war  = checkOverlap (stageWrites, readScan,  (b: Bits) => b.drop(1).orR).simPublic
  val waw  = checkOverlap (stageWrites, writeScan, (b: Bits) => b.drop(1).orR).simPublic
  val memDep = checkOverlap (stageMemX,  memScan,   (b: Bool) => b)
  
  val occupied = Vec(stage.map(_ =/= Stage.S0))
  val killAfter = B(False, cfg.issueWidth).simPublic // jumps and branches
  val kill  = killAfter.fillDownUntilLSO.simPublic // successors of jumps/branches
  val alive = occupied & ~kill.asBools
  val stall = occupied & (raw | war | waw | memDep)
  val active = alive & ~stall
  // finished is set in build() after kill is computed
  val finished = ~occupied
  val retiring = Vec.fill(cfg.issueWidth)(False)  // set when instruction retires (finish + not killed)
  stall.simPublic
  active.simPublic
  finished.simPublic

  val pipes: IndexedSeq[IExContext] =
    for (i <- 0 until cfg.issueWidth)
    yield new IExContext(cfg, i, pcBase + 4*i, irBuf(i), stage(i), start(i), stall(i), kill(i))

  val trap = False #* cfg.issueWidth

  val imem = PortIF.mem(cfg.xlen, cfg.xlen / 8 * cfg.issueWidth, writable = false)
  val dmem = PortIF.mem(cfg.xlen, cfg.xlen / 8, writable = true)
  // Initialize at instantiation to avoid overlapping assignments when exported across SoC layers
  imem.addr.assignDontCare(); imem.valid := False
  dmem.addr.assignDontCare(); dmem.valid := False
  dmem.wren.get := False; dmem.writeData.get.assignDontCare(); dmem.wmask.get.assignDontCare()

  val fetchedJump = Reg(Bool) init(False) simPublic
  val jumpPipeIdx = Reg(UInt(log2Up(cfg.issueWidth) bits)) simPublic
  // Combinational view of jumpPipeIdx for same-cycle decisions (before register updates)
  val jumpPipeIdxNow = UInt(log2Up(cfg.issueWidth) bits)
  jumpPipeIdxNow := jumpPipeIdx  // default: use registered value

  // Zicntr counters
  val mcycle = Reg(UInt(64 bits)) init(0) simPublic
  val minstret = Reg(UInt(64 bits)) init(0) simPublic;
  mcycle := mcycle + 1
  // instret incremented in build() after finished is computed

  def build(plugins: Seq[InstImpl]) {
    when (!fetchedJump) { imem.readReq(pcBase + 4*cfg.issueWidth) }
    for ((c, i) <- pipes.zipWithIndex) {
      when (kill(i)) {
        stage(i) := Stage.S0
        finished(i) := True
        // killed instructions don't retire (retiring stays False)
      }
      switch(rv.opcode(c.ir)) {
        for (p <- plugins) {
          is(p.opcode) {
            switch (stage(i)) {
              for (ss <- SoftStage.currentAndFollowing(SoftS1)) {
                is(ss.harden) {
                  when(~c.stall & ~c.kill) {
                    p.getStage(c, ss)
                  }
                  stageReads(i)  := p.getStageReads(c, ss)
                  stageWrites(i) := p.getStageWrites(c, ss)
                  willRead(i)    := p.getWillRead(c, ss)
                  willWrite(i)   := p.getWillWrite(c, ss)
                  stageMemX(i)   := p.getStageMemX(ss)
                  willMemX(i)    := p.getWillMemX(ss)
                }
              }
            }
            if (p.KillFollowing) {
              when(occupied(i)) (killAfter(i) := True)
            }
          }
        }
      }
    }

    // if the last inst in the issue group is at 1st cycle of S1, we didn't get to
    // fetch the next instructions yet.
    val lastPipe = pipes(cfg.issueWidth-1)
    when (finished.asBits.andR & lastPipe.stage =/= Stage.S1) {
      when(jumping | jumped) {
        // jump / branch-taken. start from jump target (possibly in the middle of issue group)
        for (i <- 0 until cfg.issueWidth)
          irBuf(i) := imem.readData(i*cfg.xlen, cfg.xlen bits)
        pcBase := pc2.clearedLow(2 + log2Up(cfg.issueWidth))
        val occupiedMask = if (cfg.issueWidth == 1) B"1"
          else ~U(0, cfg.issueWidth bits) |<< pc2(2, log2Up(cfg.issueWidth) bits)
        reset(occupiedMask.asBits)
      } elsewhen (jumpPipeIdxNow.orR) {
        // a branch that was not taken and is not last in issue group.
        // Revive instructions after branch
        reset(~B(0, cfg.issueWidth bits) |<< jumpPipeIdxNow)
      } otherwise {
        // non-branch/jump or branch not taken at end of group. whole next issue group will be occupied
        val irSrc = fetchedJump ? ir2 | imem.readData
        for (i <- 0 until cfg.issueWidth)
          irBuf(i) := irSrc(i*cfg.xlen, cfg.xlen bits)
        pcBase := pcBase + 4*cfg.issueWidth
        reset(~B(0, cfg.issueWidth bits))
      }
    }

    // Count retired instructions (finished and not killed)
    minstret := minstret + cpop(retiring.asBits)
  }

  def nextStage(i: Int) {
    start(i) := True
    switch(stage(i)) {
      is(Stage.S1) { stage(i) := Stage.S2 }
      is(Stage.S2) { stage(i) := Stage.S3 }
      default { trap(i) := True}
    }
  }

  def reset(occupiedMask: Bits) {
    jumped := False
    fetchedJump := False
    jumpPipeIdx := 0
    for (i <- 0 until cfg.issueWidth) {
      stage(i) := occupiedMask(i) ? Stage.S1 | Stage.S0
      start(i) := True
    }
  }

  def finish(i: Int) {
    stage(i) := Stage.S0
    finished(i) := True
    retiring(i) := True
  }

  def fetch_jump(c: IExContext, target: UInt) = {
    pc2 := target
    jumpPipeIdx := U(c.id + 1).resized
    jumpPipeIdxNow := U(c.id + 1).resized  // same-cycle visibility
    fetchedJump := True
    ir2 := imem.readData
    imem.readReq(target)
  }

  // has to happen on a later cycle than fetch_jump
  def take_jump(c: IExContext) {
    jumping := True
    jumped := True
    finish(c.id)
  }
}

// Hardware definition
case class Surov3Core(cfg: SurovConfig) extends Component {
  val pipeline = new Pipeline(cfg)
  val trap = out (pipeline.trap)
  val plugins = Seq(
    new OpOpImpl(pipeline),
    new OpImmImpl(pipeline),
    new AuipcImpl(pipeline),
    new LoadImpl(pipeline),
    new StoreImpl(pipeline),
    new LuiImpl(pipeline),
    new JalImpl(pipeline),
    new JalrImpl(pipeline),
    new BranchImpl(pipeline),
    new FenceImpl(pipeline),
    new SysImpl(pipeline),
  )
  pipeline.build(plugins)
}

case class Surov3_RF_Top(cfg: SurovConfig) extends Component {
  val core = Surov3Core(cfg)
  val trap = out Bits(cfg.issueWidth bits)
  trap := core.trap
  val imemIF = PortIF.mem(cfg.xlen, cfg.xlen/8 * cfg.issueWidth, writable = false)
  val dmemIF = PortIF.mem(cfg.xlen, cfg.xlen/8, writable = true)
  core.pipeline.imem <> imemIF
  core.pipeline.dmem <> dmemIF
  val rf = Mem(Bits(cfg.xlen bits), wordCount = cfg.regCount).simPublic
  
  // One-shot init: write 0 to x0 on first cycle after reset
  val initX0 = RegNext(False).init(True)
  when(initX0) { rf.write(address = 0, data = B(0, cfg.xlen bits)) }
  
  for (c <- core.pipeline.pipes) {
    if (cfg.syncRF) {
      // Synchronous read/write port for rf1 (x0 initialized above, write guard in PortIF.write)
      c.rf1.readData := rf.readWriteSync(
        address = c.rf1.addr,
        data = c.rf1.writeData.get,
        enable = c.rf1.valid,
        write = c.rf1.wren.get
      )
      if (cfg.enableDualPort) {
        // Synchronous read-only port for rf2
        c.rf2.get.readData := rf.readSync(
          address = c.rf2.get.addr,
          enable = c.rf2.get.valid
        )
      }
    } else {
      // Async read, separate write for rf1
      c.rf1.readData := rf.readAsync(c.rf1.addr)
      rf.write(
        address = c.rf1.addr,
        data = c.rf1.writeData.get,
        enable = c.rf1.wren.get
      )
      if (cfg.enableDualPort) {
        // Async read-only port for rf2
        c.rf2.get.readData := rf.readAsync(c.rf2.get.addr)
      }
    }
  }
}

// wrap it with memory for simulation
case class SimDUT(cfg: SurovConfig) extends Component {
  val top = Surov3_RF_Top(cfg)
  val trap = out (Bits(cfg.issueWidth bits)).simPublic
  trap := top.trap
  val mem = Mem(Bits(cfg.xlen * cfg.issueWidth bits), wordCount = 1 << 14).simPublic
  top.imemIF.readData := mem.readSync(
    address = top.imemIF.addr.resize(14),
    enable = top.imemIF.valid,
  )

  // dmem is xlen-wide, but memory is (xlen * issueWidth)-wide
  // Map the narrow dmem access to the correct lane in the wide memory word
  val laneBits = log2Up(cfg.issueWidth)
  val dmemWideAddr = top.dmemIF.addr.dropLow(laneBits).resize(14).asUInt
  val dmemLane = top.dmemIF.addr.takeLow(laneBits).asUInt
  // Register lane for read response alignment (readWriteSync has 1-cycle latency)
  val dmemLaneReg = RegNextWhen(dmemLane, top.dmemIF.valid)

  // Shift narrow data/mask into the correct lane for writes
  val dmemWideData = (top.dmemIF.writeData.get.resize(cfg.xlen * cfg.issueWidth) |<< (dmemLane * cfg.xlen))
  val dmemWideMask = (top.dmemIF.wmask.get.resize(cfg.xlen / 8 * cfg.issueWidth) |<< (dmemLane * (cfg.xlen / 8)))

  val dmemWideReadData = mem.readWriteSync(
    address = dmemWideAddr,
    enable = top.dmemIF.valid,
    write = top.dmemIF.wren.get,
    data = dmemWideData,
    mask = dmemWideMask
  )

  // Extract the correct lane from the wide read data (use registered lane)
  top.dmemIF.readData := (dmemWideReadData |>> (dmemLaneReg * cfg.xlen)).resize(cfg.xlen)
}

object Surov3CoreVerilog extends App {
  Config.spinal.generateVerilog(Surov3_RF_Top(SurovConfig()))
}

object Surov3CoreVhdl extends App {
  Config.spinal.generateVhdl(Surov3Core(SurovConfig()))
}
