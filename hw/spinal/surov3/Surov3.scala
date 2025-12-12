package surov3

import spinal.core._
import spinal.core.sim._
import surov3.Utils._
import surov3.SoftStage.SoftS1

case class SurovConfig(
  regCount: Int = 32,
  enableDualPort: Boolean = false,
  issueWidth: Int = 8,
  enableForward: Boolean = false,
  xlen: Int = 32,
)

class RFIF(xlen: Int, regCount: Int) extends Bundle {
  val addr = out UInt(log2Up(regCount) bits) assignDontCare
  val wren = out (False)
  val writeData = out Bits(xlen bits) assignDontCare
  val readData = in Bits(xlen bits)

  /** Async read */
  def read(regnum: UInt) = {
    addr := regnum
    readData
  }
  
  def write(c: IExContext, regnum: UInt, value: Bits) {
    addr := regnum
    wren := True
    writeData := value
  }
}

class MemIF(val xlen: Int, val wordBytes: Int, val writable: Boolean) extends Bundle {
  val addr = out UInt(xlen-log2Up(wordBytes) bits) assignDontCare
  val readData = in Bits(wordBytes*8 bits)
  val valid = out Bool
  val ready = in Bool
  val wren      = Option.when(writable)(out (Bool))
  val wmask     = Option.when(writable)(out Bits(wordBytes bits) assignDontCare)
  val writeData = Option.when(writable)(out Bits(wordBytes*8 bits) assignDontCare)

  /**
    * PHASE 1: Read Request
    * Call this in the Execute stage to drive the memory bus.
    * @param address The full byte-address (e.g., 32 bits). 
    * The lower bits are dropped for the bus index.
    */
  def readReq(address: UInt): Unit = {
    // Drop lower log2Up(wordBytes) bits to convert Byte Address -> Word Index
    // For wordBytes=16 (4 words), this drops 4 bits
    addr  := address.drop(log2Up(wordBytes)).asUInt
    valid := True
    wren.foreach(_ := False)
  }

/**
    * PHASE 2: Read Response (after readReq in a prior cycle)
    * @param address The full byte-address used in the Request phase (needed for alignment).
    * @param funct3  Optional RISC-V encoding. If None, returns raw aligned data.
    */
  def readRsp(address: UInt, funct3: Option[Bits] = None): Bits = {
    funct3 match {
      case None => 
        // instruction load: Return data directly from bus
        readData

      case Some(f3) =>
        // RISC-V Load : Handle alignment and extension
        val byteOffset   = address(0, log2Up(wordBytes) bits)
        val bitOffset    = byteOffset << 3
        val shiftedData  = readData |>> bitOffset
        val logbyteCount = f3.dropHigh(1).asUInt
        // +1 bit to avoid overflow (e.g., 1 << 2 = 4 needs 3 bits)
        val byteCount    = U(1, log2Up(wordBytes).max(3) bits) |<< logbyteCount

        // Build a contiguous bitmask (little-endian) for the requested byte/half/word
        val bitmask = ~(~B(0, wordBytes * 8 bits) |<< (byteCount << 3))

        // Pick the sign bit for the requested width
        val msb = Bool
        switch(logbyteCount) {
          is(U(0, 2 bits)) { msb := shiftedData(7) }
          is(U(1, 2 bits)) { msb := shiftedData(15) }
          is(U(2, 2 bits)) { msb := shiftedData(31) }
          default          { msb.assignDontCare() }
        }

        val signBit = f3.msb ? False | msb // unsigned loads zero-extend
        (shiftedData & bitmask | (~bitmask).andMask(signBit)).resize(xlen bits)
    }
  }

  /**
    * Write Operation (Atomic Request)
    * @param address Full byte-address.
    * @param data    Data to write (Right aligned / from Register File).
    * @param funct3  Optional RISC-V encoding. If None, writes full word.
    */
  def write(address: UInt, data: Bits, funct3:Bits): Unit = {
    assert(writable, "write to unwritable memory interface") 
    val byteOffset  = address(0, log2Up(wordBytes) bits)
    val bitOffset   = byteOffset << 3
    val logbyteCount= funct3.dropHigh(1).asUInt
    // +1 bit to avoid overflow (e.g., 1 << 2 = 4 needs 3 bits)
    val byteCount   = U(1, log2Up(wordBytes) + 1 bits) |<< logbyteCount
    valid          := True
    wren.get       := True
    addr           := address.drop(log2Up(wordBytes)).asUInt
    writeData.get  := (data << bitOffset).resize(wordBytes * 8 bits)
    wmask.get      := (~(~B(0, wordBytes bits) |<< byteCount)) |<< byteOffset
  }
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

  val rf1 = new RFIF(cfg.xlen, cfg.regCount)
  val rf2 = Option.when(cfg.enableDualPort)(new RFIF(cfg.xlen, cfg.regCount))
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
    alu.shadd := rv.shadd(ir)
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

  val imem = new MemIF(cfg.xlen, cfg.xlen / 8 * cfg.issueWidth, false)
  val dmem = new MemIF(cfg.xlen, cfg.xlen / 8 * cfg.issueWidth, true)
  // Default: no memory access unless write/readReq sets it
  imem.valid := False
  dmem.valid := False
  dmem.wren.foreach(_ := False)

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
  val imemIF = new MemIF(cfg.xlen, cfg.xlen/8 * cfg.issueWidth, false)
  val dmemIF = new MemIF(cfg.xlen, cfg.xlen/8 * cfg.issueWidth, true)
  core.pipeline.imem <> imemIF
  core.pipeline.dmem <> dmemIF
  val rf = Mem(Bits(32 bits), wordCount = cfg.regCount) simPublic;
  for (c <- core.pipeline.pipes) {
    // Asynchronous read so dependent ops see latest reg value without an extra cycle
    c.rf1.readData := c.rf1.addr.mux(
      U(0, log2Up(cfg.regCount) bits) -> B(0, cfg.xlen bits),
      default -> rf.readAsync(c.rf1.addr)
    )
    rf.write(c.rf1.addr, c.rf1.writeData, c.rf1.wren)
    if (cfg.enableDualPort) {
      c.rf2.get.readData := c.rf2.get.addr.mux(
        U(0, log2Up(cfg.regCount) bits) -> B(0, cfg.xlen bits),
        default -> rf.readAsync(c.rf2.get.addr)
      )
      // rf.write(c.rf2.get.addr, c.rf2.get.writeData, c.rf2.get.wren)
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
  top.dmemIF.readData := mem.readWriteSync(
    address = top.dmemIF.addr.resize(14),
    enable = top.dmemIF.valid,
    write = top.dmemIF.wren.get,
    data = top.dmemIF.writeData.get,
    mask = top.dmemIF.wmask.get
  )
}

object Surov3CoreVerilog extends App {
  Config.spinal.generateVerilog(Surov3_RF_Top(SurovConfig()))
}

object Surov3CoreVhdl extends App {
  Config.spinal.generateVhdl(Surov3Core(SurovConfig()))
}
