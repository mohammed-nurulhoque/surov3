package surov3

import spinal.core._
import spinal.core.sim._
import surov3.Utils._

case class SurovConfig(
  regCount: Int = 32,
  enableDualPort: Boolean = false,
  enableZba: Boolean = false,
  issueWidth: Int = 4,
  enableForward: Boolean = false,
  xlen: Int = 32,
)

class RFIF(xlen: Int, regCount: Int) extends Bundle {
  val addr = out UInt(log2Up(regCount) bits) assignDontCare
  val wren = out Bool() assignDontCare
  val writeData = out Bits(xlen bits) assignDontCare
  val readData = in Bits(xlen bits)

  /** Async read */
  def read(regnum: UInt) = {
    addr := regnum
    wren := False
    readData
  }
  
  def write(c: IExContext, regnum: UInt, value: Bits) {
    addr := regnum
    wren := c.active & ~c.kill
    writeData := value
  }
}

class MemIF(val xlen: Int, val wordBytes: Int, val writable: Boolean) extends Bundle {
  val addr = out UInt(xlen-log2Up(wordBytes) bits) assignDontCare
  val readData = in Bits(wordBytes*8 bits)
  val valid = out Bool
  val ready = in Bool
  val wren      = Option.when(writable)(out Bool() assignDontCare)
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
  def toInt: Int
  def harden: Stage.C
}

object SoftStage {
  case object SoftS1 extends SoftStage { def toInt = 1; def harden = Stage.S1 }
  case object SoftS2 extends SoftStage { def toInt = 2; def harden = Stage.S2 }
  case object SoftS3 extends SoftStage { def toInt = 3; def harden = Stage.S3 }

  def fromInt(i: Int): SoftStage = i match {
    case 1 => SoftS1
    case 2 => SoftS2
    case 3 => SoftS3
  }

  def succ(ss: SoftStage) = ss match {
    case SoftS1 => SoftS2
    case SoftS2 => SoftS3
  }
}

class IExContext(cfg: SurovConfig, val id: Int,
  val pc: UInt, val ir: Bits,
  val active: Bool, val kill: Bool, val finished: Bool)
{
  pc.simPublic
  ir.simPublic
  active.simPublic
  val stage = Reg(Stage).init(Stage.S1).simPublic()
  val start = out (Reg(Bool)) init(True)

  val r1 = Reg(UInt(cfg.xlen bits)).simPublic()
  val r2 = Reg(UInt(cfg.xlen bits)).simPublic()
  val alu = new ALU(cfg.xlen, cfg.regCount)

  val rf1 = new RFIF(cfg.xlen, cfg.regCount)
  val rf2 = Option.when(cfg.enableDualPort)(new RFIF(cfg.xlen, cfg.regCount))

  alu.f3 := rv.F3_ADDSUB
  List(alu.src_a, alu.src_b, alu.start) map (_.assignDontCare)
  List(alu.arith_bit, alu.shadd, alu.branch) map (_ := False)


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
  val pc = Reg(UInt(cfg.xlen bits)) init(0) simPublic
  val pc2 = Reg(UInt(cfg.xlen bits)).assignDontCare()
  val irLine = Vec.fill(cfg.issueWidth)(Reg(Bits(cfg.xlen bits))).simPublic
  irLine(0).init(Opcode.Jal.asBits.resize(32) | 0x1000)
  for (i <- 1 until cfg.issueWidth)
    irLine(i).init(rv.nop)
  val active = Reg(UInt(cfg.issueWidth bits)) init(1)
  val finished = Vec.fill(cfg.issueWidth)(Bool)
  val jumping = Bool
  val jumped = Reg(Bool).init(False)
  val ir2 = Reg(Bits(cfg.xlen * cfg.issueWidth bits)).assignDontCare()
  val killAfter = Bits(cfg.issueWidth bits).simPublic
  val killCur   = Bits(cfg.issueWidth bits).simPublic
  val kill = (killAfter | (killCur |>> 1)).asUInt.fillDownToLSO simPublic
  val regReads = Vec.fill(cfg.issueWidth)(Bits(cfg.regCount bits).simPublic)
  val regWrites = Vec.fill(cfg.issueWidth)(Bits(cfg.regCount bits).simPublic)
  val readScan = Vec(regReads.scanLeft(B(0, cfg.regCount bits) simPublic)(_ | _ simPublic)).simPublic
  val writeScan = Vec(regWrites.scanLeft(B(0, cfg.regCount bits) simPublic)(_ | _ simPublic)).simPublic
  val pipes =
    for (i <- 0 until cfg.issueWidth)
    yield new IExContext(cfg, i, pc + 4*i, irLine(i), active(i), kill(i), finished(i))
  val trap = Vec(irLine
    .map(rv.opcode(_) === Opcode.Sys)
    .zip((active & ~kill).asBools)
    .map({case (a, b) => a & b})) asBits

  val imem = new MemIF(cfg.xlen, cfg.xlen / 8 * cfg.issueWidth, false)
  val dmem = new MemIF(cfg.xlen, cfg.xlen / 8, true)

  imem.valid.assignDontCare
  dmem.valid.assignDontCare
  val fetchedJump = Reg(Bool) init(False)
  var ss: SoftStage = SoftStage.SoftS1

  def build(plugins: Seq[InstImpl]) {
    jumping := False
    when (pipes(0).stage === Stage.S1) { // S1 is sync across all pipes anyway
      imem.readReq(pc + 4*cfg.issueWidth)
    }
    killAfter.clearAll
    killCur.clearAll
    finished.clearAll
    for ((c, i) <- pipes.zipWithIndex) {
      when(!c.alu.ready) {  // FIXME an instruction can choose to stall for other reasons
        c.start := False
      }
      when(c.stage === Stage.S0 | !c.active | kill(i)) (c.finished := True)
      switch(rv.opcode(c.ir)) {
        for (p <- plugins) {
          is(p.opcode) {
            switch (c.stage) {
              for (stage_i <- List(1, 2, 3)) {
                ss = SoftStage.fromInt(stage_i)
                is(ss.harden) {
                  p.getStage(c, ss)
                }
              }
            }
            regReads(i) := c.active ? p.getReads(c) | B(0, cfg.regCount bits)
            regWrites(i) := c.active ? p.getWrites(c) | B(0, cfg.regCount bits)
            if (p.KillFollowing) {
              when(c.active) (killAfter(i) set)
            }
            val raw = (regReads(i) & writeScan(i)).asUInt.clearedLow(1).orR
            val war = (regWrites(i) & readScan(i)).asUInt.clearedLow(1).orR // TODO war not issue with dual port?
            val waw = (regWrites(i) & writeScan(i)).asUInt.clearedLow(1).orR // TODO waw shouldn't happen in normal code
            when (c.active & (raw | war | waw)) (killCur(i) set)
          }
          default {}
        }
      }
    }
    for ((c, i) <- pipes.zipWithIndex) {
      when(kill(i)) {
        c.stage := Stage.S0
        c.active := False
      }
    }
    when (finished.asBits.andR) {
      when(jumping | jumped) {
        for (i <- 0 until cfg.issueWidth)
          irLine(i) := imem.readData(i*cfg.xlen, cfg.xlen bits)
        pc := pc2.clearedLow(2 + log2Up(cfg.issueWidth))
        active := ~((U(1) << pc2(2, log2Up(cfg.issueWidth) bits)) - 1)
      } elsewhen ((active & ~kill)(cfg.issueWidth-1)) {
        val irSrc = fetchedJump ? ir2 | imem.readData
        for (i <- 0 until cfg.issueWidth)
          irLine(i) := irSrc(i*cfg.xlen, cfg.xlen bits)
        pc := (pc + 4*cfg.issueWidth).clearedLow(2 + log2Up(cfg.issueWidth))
        active.setAll()
      } otherwise {
        active := ((k: UInt) => ~((k-1) | k))(active & ~kill) // XXX FIXME
      }
      jumped := False
      fetchedJump := False
      for (c <- pipes) {
        c.start := True
        c.stage := Stage.S1
      }
    } otherwise {
      for (c <- pipes)
        when(c.finished) (c.stage := Stage.S0)
    }
  }

  def nextStage(c: IExContext) {
    c.start := True
    switch(c.stage) {
      is(Stage.S1) { c.stage := Stage.S2 }
      is(Stage.S2) { c.stage := Stage.S3 }
    }
  }

  def fetch_jump(c: IExContext, target: UInt) = {
    when (c.active) {
      pc2 := target
      ir2 := imem.readData
      fetchedJump := True
      imem.readReq(target)
    }
  }

  def take_jump(c: IExContext) {
    jumping := True
    jumped := True
    c.finished := True
  }

  def finish(c: IExContext) {
    c.finished := True
    // if (ss == SoftStage.SoftS1) c.stage := Stage.S0
    // else forward(c, fetchedJump)
  }
}

// Hardware definition
case class Surov3Core(cfg: SurovConfig) extends Component {
  val pipeline = new Pipeline(cfg)
  val trap = out Bits(cfg.issueWidth bits)
  trap := pipeline.trap
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
  val trap = out Bits(cfg.issueWidth bits)
  trap := top.trap.simPublic()
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
