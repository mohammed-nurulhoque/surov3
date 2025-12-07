package surov3

import spinal.core._
import spinal.core.sim._
import surov3.Utils._

case class SurovConfig(
  regCount: Int = 32,
  enableDualPort: Boolean = false,
  // enableZba: Boolean = false,
  issueWidth: Int = 4,
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
    wren := ~c.stall & ~c.kill
    writeData := value
  }
}

class MemIF(val xlen: Int, val wordBytes: Int, val writable: Boolean) extends Bundle {
  val addr = out UInt(xlen-log2Up(wordBytes) bits) assignDontCare
  val readData = in Bits(wordBytes*8 bits)
  val valid = out Bool
  val ready = in Bool
  val wren      = Option.when(writable)(out (Bool))
  val wmask     = Option.when(writable)(out Bits(wordBytes*8 bits) assignDontCare)
  val writeData = Option.when(writable)(out Bits(wordBytes*8 bits) assignDontCare)

  /**
    * PHASE 1: Read Request
    * Call this in the Execute stage to drive the memory bus.
    * @param address The full byte-address (e.g., 32 bits). 
    * The lower bits are dropped for the bus index.
    */
  def readReq(address: UInt): Unit = {
    // Drop lower 2 bits to convert Byte Address -> Word Index
    addr  := address(xlen-1 downto log2Up(wordBytes))
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
        // Simple Word Mode: Return data directly from bus
        readData

      case Some(f3) => // assumes wordBytes = 4
        // RISC-V Mode: Handle alignment and extension
        val byteOffset  = address(1 downto 0)
        val bitOffset   = byteOffset << 3
        val shiftedData = readData >> bitOffset
        val result      = Bits(xlen bits)

        switch(f3) {
          is(B"000") { // LB (Sign Extend Byte)
            result := (shiftedData(7 downto 0).asSInt.resize(xlen bits)).asBits
          }
          is(B"001") { // LH (Sign Extend Half)
            result := (shiftedData(15 downto 0).asSInt.resize(xlen bits)).asBits
          }
          is(B"100") { // LBU (Zero Extend Byte)
            result := shiftedData(7 downto 0).resize(xlen bits)
          }
          is(B"101") { // LHU (Zero Extend Half)
            result := shiftedData(15 downto 0).resize(xlen bits)
          }
          default {    // LW (Word)
            result := shiftedData
          }
        }
        result
    }
  }

  /**
    * Write Operation (Atomic Request)
    * @param address Full byte-address.
    * @param data    Data to write (Right aligned / from Register File).
    * @param funct3  Optional RISC-V encoding. If None, writes full word.
    */
  def write(address: UInt, data: Bits, funct3: Option[Bits] = None): Unit = {
    assert(writable, "write to unwritable memory interface") 

    valid := True
    wren.foreach(_ := True)
    addr  := address(xlen-1 downto 2)

    funct3 match {
      case None =>
        // Simple Word Mode: Write full width, Mask all 1s
        writeData.foreach(_ := data)
        wmask.foreach(_ setAll)

      case Some(f3) =>
        // RISC-V Mode: Shift data to lane and generate specific mask
        val byteOffset = address(1 downto 0)
        val bitOffset  = byteOffset << 3
        
        // Shift data to correct byte lane
        writeData.foreach(_ := data |<< bitOffset)

        // Generate Mask
        val baseMask = Bits(xlen bits)
        switch(f3(1 downto 0)) {
          is(B"00") { baseMask := B(0xFF, xlen bits) }   // Byte mask (lower 8 bits 1)
          is(B"01") { baseMask := B(0xFFFF, xlen bits) } // Half mask (lower 16 bits 1)
          default   { baseMask.setAll() }           // Word                                                 // Word
        }
        wmask.foreach(_ := baseMask |<< bitOffset)
    }
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
  val stall : Bool,
  val kill  : Bool,
) {
  val stage     = Reg(Stage).init(Stage.S1).simPublic()
  val finished  = False.simPublic
  val trap      = False

  pc.simPublic
  ir.simPublic
  stall.simPublic
  kill.simPublic

  val start = out (Reg (Bool) init(True))
  val r1 = Reg(UInt(cfg.xlen bits)).simPublic()
  val r2 = Reg(UInt(cfg.xlen bits)).simPublic()
  val alu = new ALU(cfg.xlen, cfg.regCount)

  val rf1 = new RFIF(cfg.xlen, cfg.regCount)
  val rf2 = Option.when(cfg.enableDualPort)(new RFIF(cfg.xlen, cfg.regCount))
  alu.f3 := rv.F3_ADDSUB
  List(alu.src_a, alu.src_b, alu.start) map (_.assignDontCare)
  List(alu.arith_bit, alu.shadd, alu.branch) map (_ := False)

  when (kill) { _finish() }
  when(!alu.ready) {  // FIXME an instruction can choose to stall for other reasons
    start := False
  }
  when(stage === Stage.S0) (finished := True)

  def nextStage() {
    when (~kill & ~stall) {
      start := True
      switch(stage) {
        is(Stage.S1) { stage := Stage.S2 }
        is(Stage.S2) { stage := Stage.S3 }
      }
    }
  }

  def reset(dead: Bool) {
    stage := dead ? Stage.S0 | Stage.S1
    start := True
  }

  def _finish() {
    stage := Stage.S0
    finished := True
  }

  def finish() {
    when(~stall) { _finish() }
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
  val pcBase = Reg(UInt(cfg.xlen bits)) init(0) simPublic
  val pc2 = Reg(UInt(cfg.xlen bits)).assignDontCare()
  val irBuf = Vec.fill(cfg.issueWidth)(Reg(Bits(cfg.xlen bits))).simPublic
  irBuf(0).init(Opcode.Jal.asBits.resize(32) | 0x1000)
  irBuf.drop(1).foreach(_.init(rv.nop))
  val jumping = False  simPublic
  val jumped = Reg(Bool).init(False)  simPublic
  val ir2 = Reg(Bits(cfg.xlen * cfg.issueWidth bits)).assignDontCare
  val reset = B(False, cfg.issueWidth)
  val killAfter = B(False, cfg.issueWidth).simPublic
  val kill  = killAfter.fillDownToLSO.asBools.simPublic // control hazards
  val regReads = Vec.fill(cfg.issueWidth)(B(0, cfg.regCount bits).simPublic)
  val regWrites = Vec.fill(cfg.issueWidth)(B(0, cfg.regCount bits).simPublic)
  val regRemReads = Vec.fill(cfg.issueWidth)(B(0, cfg.regCount bits)).simPublic.addAttribute("keep")
  val regRemWrites = Vec.fill(cfg.issueWidth)(B(0, cfg.regCount bits)).simPublic.addAttribute("keep")
  val readScan = Vec(regRemReads.scanLeft(B(0, cfg.regCount bits))(_ | _)).addAttribute("keep")
  val writeScan = Vec(regRemWrites.scanLeft(B(0, cfg.regCount bits))(_ | _)).simPublic.addAttribute("keep")

  val checkOverlap: (Vec[Bits], Vec[Bits]) => Vec[Bool] = {
    case (access, scan) => Vec(access.zip(scan)
    .map({ case (a, b) => (a & b).asUInt.clearedLow(1).orR }))
  }
  val raw = checkOverlap (regReads, writeScan) simPublic
  val war = checkOverlap (regWrites, readScan) simPublic
  val waw = checkOverlap (regWrites, writeScan) simPublic
  val stall = Vec.fill(cfg.issueWidth)(Bool)
  val pipes: IndexedSeq[IExContext] =
    for (i <- 0 until cfg.issueWidth)
    yield new IExContext(cfg, i, pcBase + 4*i, irBuf(i), stall(i), kill(i))

  val occupied = Vec(pipes.map(_.stage =/= Stage.S0))
  val alive = occupied & ~kill  
  stall := alive & (raw | war | waw)
  val active = alive & ~stall
  alive.simPublic 
  active.simPublic

  // for convenience
  val finished = Vec(pipes.map (_.finished))
  val trap = Vec(pipes.map (_.trap)).asBits

  val imem = new MemIF(cfg.xlen, cfg.xlen / 8 * cfg.issueWidth, false)
  val dmem = new MemIF(cfg.xlen, cfg.xlen / 8, true)
  dmem.valid := False
  dmem.wren.foreach(_ := False)

  val fetchedJump = Reg(Bool) init(False) simPublic

  def build(plugins: Seq[InstImpl]) {
    imem.readReq(pcBase + 4*cfg.issueWidth)
    for ((c, i) <- pipes.zipWithIndex) {
      switch(rv.opcode(c.ir)) {
        for (p <- plugins) {
          is(p.opcode) {
            switch (c.stage) {
              for (ss <- List(SoftStage.SoftS1, SoftStage.SoftS2, SoftStage.SoftS3)) {
                is(ss.harden) {
                  p.getStage(c, ss)
                  regReads(i) := p.getStageReads(c, ss) & (occupied(i) #* cfg.regCount)
                  regWrites(i) := p.getStageWrites(c, ss) & (occupied(i) #* cfg.regCount)
                  regRemReads(i) := p.getReadRem(c, ss) & (occupied(i) #* cfg.regCount)
                  regRemWrites(i) := p.getWriteRem(c, ss) & (occupied(i) #* cfg.regCount)
                }
              }
            }
            if (p.KillFollowing) {
              when(c.stage =/= Stage.S0) (killAfter(i) set)
            }
          }
          default {}
        }
      }
    }
    // if the last inst in the issue group is at S1, we didn't get to
    // fetch the next instructions yet.
    when (finished.asBits.andR & pipes(cfg.issueWidth-1).stage =/= Stage.S1) {
      when(jumping | jumped) {
        for (i <- 0 until cfg.issueWidth)
          irBuf(i) := imem.readData(i*cfg.xlen, cfg.xlen bits)
        pcBase := pc2.clearedLow(2 + log2Up(cfg.issueWidth))
        val dead = (U(1) << pc2(2, log2Up(cfg.issueWidth) bits)) - 1
        for (c <- pipes) {
          c.reset (dead(c.id))
        }
      } elsewhen (kill.last) {
        // a branch that was not taken and is not last in issue group.
        // Revive instructions after branch
        for (c <- pipes)
          c.reset(~kill(c.id))
      } otherwise {
        val irSrc = fetchedJump ? ir2 | imem.readData
        for (i <- 0 until cfg.issueWidth)
          irBuf(i) := irSrc(i*cfg.xlen, cfg.xlen bits)
        pcBase := pcBase + 4*cfg.issueWidth
        pipes.foreach(_.reset(False))
      }
      jumped := False
      fetchedJump := False
    }
  }

  def fetch_jump(c: IExContext, target: UInt) = {
    when (~kill(c.id)) {
      pc2 := target
      ir2 := imem.readData
      fetchedJump := True
      imem.readReq(target)
    }
  }

  // has to happen on a later cycle than fetch_jump
  // thus, doesn't need guarding with c.stall
  def take_jump(c: IExContext) {
    jumping := True
    jumped := True
    c.finish
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

case class Surov3Top(cfg: SurovConfig) extends Component {
  val core = Surov3Core(cfg)
  val trap = out Bits(cfg.issueWidth bits)
  trap := core.trap
  val imemIF = new MemIF(cfg.xlen, cfg.xlen/8 * cfg.issueWidth, false)
  val dmemIF = new MemIF(cfg.xlen, cfg.xlen/8, true)
  core.pipeline.imem <> imemIF
  core.pipeline.dmem <> dmemIF
  val rf = Mem(Bits(32 bits), wordCount = cfg.regCount) simPublic;
  for (c <- core.pipeline.pipes) {
    c.rf1.readData := c.rf1.addr.mux(U(0, log2Up(cfg.regCount) bits) -> B(0, cfg.xlen bits), default -> rf(c.rf1.addr))
    rf.write(c.rf1.addr, c.rf1.writeData, c.rf1.wren)
    if (cfg.enableDualPort) {
      c.rf2.get.readData := c.rf2.get.addr.mux(U(0, log2Up(cfg.regCount) bits) -> B(0, cfg.xlen bits), default -> rf(c.rf2.get.addr))
      // rf.write(c.rf2.get.addr, c.rf2.get.writeData, c.rf2.get.wren)
    }
  }
}

case class SimDUT(cfg: SurovConfig) extends Component {
  val top = Surov3Top(cfg)
  val trap = out (Bits(cfg.issueWidth bits)).simPublic
  trap := top.trap
  val mem = Mem(Bits(cfg.xlen * cfg.issueWidth bits), wordCount = 1 << 14).simPublic
  top.imemIF.readData := mem.readSync(
    address = top.imemIF.addr.resize(14),
    enable = top.imemIF.valid,
  )
  val dmem = Mem(Bits(cfg.xlen bits), wordCount = 1 << 14).simPublic
  top.dmemIF.readData := dmem.readWriteSync(
    address = top.dmemIF.addr.resize(14),
    enable = top.dmemIF.valid,
    write = top.dmemIF.wren.get,
    data = top.dmemIF.writeData.get,
    mask = top.dmemIF.wmask.get
  )
}

object Surov3CoreVerilog extends App {
  Config.spinal.generateVerilog(Surov3Top(SurovConfig()))
}

object Surov3CoreVhdl extends App {
  Config.spinal.generateVhdl(Surov3Core(SurovConfig()))
}
