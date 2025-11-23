package surov3

import spinal.core._
import spinal.core.sim._
import surov3.Opcode.Jalr

sealed trait XLEN
case object XLEN32 extends XLEN
case object XLEN64 extends XLEN

case class SurovConfig(
  enable32Regs: Boolean = true,
  enableDualPort: Boolean = true,
  enableZba: Boolean = false,
  enableForward: Boolean = true,
  xlen: XLEN = XLEN32,
) {
  def xlenb    = xlen match { case XLEN32 => 32 bits
                              case XLEN64 => 64 bits}
  // def xlenB    = xlenb / (8 bits)
  def regCount = if (enable32Regs) 32 else 16 
  def wrfaddr  = if (enable32Regs) 5 bits else 4 bits
}

class RFIF(xlen: BitCount, wrfaddr: BitCount) extends Bundle {
  val addr = out UInt(wrfaddr) assignDontCare
  val wren = out Bool() assignDontCare
  val writeData = out Bits(xlen) assignDontCare
  val readData = in Bits(xlen)

  /**
    * Async read
    */
  def read(regnum: UInt) = {
    addr := regnum
    wren := False
    readData
  }
  
  def write(regnum: UInt, value: Bits) {
    addr := regnum
    wren := True
    writeData := value
  }
}

class MemIF(val xlen: BitCount, val writable: Boolean) extends Bundle {
  val addr = out UInt(xlen - (2 bits)) assignDontCare// FIXME hardcoded when rv32
  val readData = in Bits(xlen)
  val valid = out Bool(false)
  val ready = in Bool
  val wren      = Option.when(writable)(out Bool() assignDontCare)
  val wmask     = Option.when(writable)(out Bits(xlen) assignDontCare)
  val writeData = Option.when(writable)(out Bits(xlen) assignDontCare)

  /**
    * PHASE 1: Read Request
    * Call this in the Execute stage to drive the memory bus.
    * @param address The full byte-address (e.g., 32 bits). 
    * The lower 2 bits are dropped for the bus index.
    */
  def readReq(address: UInt): Unit = {
    // Drop lower 2 bits to convert Byte Address -> Word Index
    addr  := address(xlen.value - 1 downto 2)
    valid := True
    wren.foreach(_ := False)
  }

/**
    * PHASE 2: Read Response
    * Call this in the WriteBack stage (or whenever data returns).
    * @param address The full byte-address used in the Request phase (needed for alignment).
    * @param funct3  Optional RISC-V encoding. If None, returns raw aligned data.
    */
  def readRsp(address: UInt, funct3: Option[Bits] = None): Bits = {
    funct3 match {
      case None => 
        // Simple Word Mode: Return data directly from bus
        readData

      case Some(f3) =>
        // RISC-V Mode: Handle alignment and extension
        val byteOffset  = address(1 downto 0)
        val bitOffset   = byteOffset << 3
        val shiftedData = readData >> bitOffset
        val result      = Bits(xlen)

        switch(f3) {
          is(B"000") { // LB (Sign Extend Byte)
            result := (shiftedData(7 downto 0).asSInt.resize(xlen)).asBits
          }
          is(B"001") { // LH (Sign Extend Half)
            result := (shiftedData(15 downto 0).asSInt.resize(xlen)).asBits
          }
          is(B"100") { // LBU (Zero Extend Byte)
            result := shiftedData(7 downto 0).resize(xlen)
          }
          is(B"101") { // LHU (Zero Extend Half)
            result := shiftedData(15 downto 0).resize(xlen)
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
    addr  := address(xlen.value - 1 downto 2)

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
        val baseMask = Bits(xlen)
        switch(f3(1 downto 0)) {
          is(B"00") { baseMask := B(0xFF, xlen) }   // Byte mask (lower 8 bits 1)
          is(B"01") { baseMask := B(0xFFFF, xlen) } // Half mask (lower 16 bits 1)
          default   { baseMask.setAll() }           // Word                                                 // Word
        }
        wmask.foreach(_ := baseMask |<< bitOffset)
    }
  }
}

class ALU(xlen: BitCount, wrfaddr: BitCount) extends BlackBox {  
  // addGeneric("wordWidth", wordWidth)
  val clk = in Bool
  val start = in Bool
  val src_a = in (UInt(xlen))
  val src_b = in (UInt(xlen))
  val f3 = in Bits(3 bits)
  val arith_bit = in Bool()
  val shadd = in Bool()
  val branch = in Bool()
  val result = out UInt(xlen)
  val shamt_out = out UInt(wrfaddr)
  val ready = out Bool()

  mapCurrentClockDomain(clock = clk)
  addRTLPath("./hw/verilog/alu.v")
}

// S0 is a special stage the is idle
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

class IExContext(cfg: SurovConfig) {
  val stage = Reg(Stage).init(Stage.S0).simPublic()
  val start = Reg(Bool) init(True)
  val trap = out Bool(false)
  val pc = Reg(UInt(cfg.xlenb)) init(0x1000) simPublic
  val pc2 = Reg(UInt(cfg.xlenb))
  val pcPlus4 = pc + 4;
  val ir = Reg(Bits(cfg.xlenb)).init(Opcode.OpImm.asBits.resize(32 bits)).simPublic()
  val ir2 = Reg(Bits(cfg.xlenb))
  val r1 = Reg(UInt(cfg.xlenb)).simPublic()
  val r2 = Reg(UInt(cfg.xlenb)).simPublic()
  val alu = new ALU(cfg.xlenb, cfg.wrfaddr)
  val imem = new MemIF(cfg.xlenb, false)
  val dmem = new MemIF(cfg.xlenb, true)
  val rf1 = new RFIF(cfg.xlenb, cfg.wrfaddr)
  val rf2 = Option.when(cfg.enableDualPort)(new RFIF(cfg.xlenb, cfg.wrfaddr))

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
  val c = new IExContext(cfg)
  var jumpTarget = UInt(cfg.xlenb) assignDontCare
  var fetchJumpStage: SoftStage = null;
  var (fetchingJump, fetchedJump) = (false, false)
  val opcode = rv.opcode((c.ir))
  var ss: SoftStage = SoftStage.SoftS1

  def build(plugins: Seq[InstImpl]) {
    when (c.stage === Stage.S1 & c.start) { // fetch next
      c.imem.readReq(c.pc+4)
    }
    when(!c.alu.ready) {  // FIXME an instruction can choose to stall for other reasons
      c.start := False
    }
    when(c.stage === Stage.S0) {
      c.imem.readReq(c.pc+4) // only needed for reset TODO: better reset
      forward(false)
    }
    switch(opcode) {
      for (p <- plugins) {
        is(p.opcode) {
          fetchingJump = false; fetchedJump =false
          switch (c.stage) {
            for (i <- List(1, 2, 3)) {
              if (fetchingJump) fetchedJump = true
              ss = SoftStage.fromInt(i)
              is(ss.harden) {
                p.getStage(ss)
              }
            }
          }
        }
        default {}
      }
    }
  }

  def nextStage() {
    c.start := True
    switch(c.stage) {
      is(Stage.S1) { c.stage := Stage.S2 }
      is(Stage.S2) { c.stage := Stage.S3 }
    }
  }

  def fetch_jump(target: UInt) = {
    fetchingJump = true
    fetchJumpStage = ss
    jumpTarget \= target
    c.pc2 := target
    c.ir2 := c.imem.readData
    c.imem.readReq(target)
  }

  def take_jump() {
    if (ss == fetchJumpStage) {
      c.pc := jumpTarget
      c.stage := Stage.S0
      // c.start := True
    } else {
      c.pc := c.pc2
      forward(false)
    }
  }

  def finish() {
    c.pc := c.pc + 4
    if (ss == SoftStage.SoftS1) c.stage := Stage.S0
    else forward(fetchedJump)
  }

  def forward(irSaved: Boolean) {
    c.stage := Stage.S1
    c.start := True
    c.ir := (if (irSaved) c.ir2 else c.imem.readData)
  }
}

// Hardware definition
case class Surov3Core(cfg: SurovConfig) extends Component {
  val pipeline = new Pipeline(cfg)
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

case class SimDUT(cfg: SurovConfig) extends Component {
  val core = Surov3Core(cfg)
  val imem = Mem(Bits(32 bits), wordCount = 4096) simPublic
  val c = core.pipeline.c

  c.imem.readData.simPublic
  c.imem.addr.simPublic
  c.start.simPublic
  core.pipeline.jumpTarget.simPublic
  core.pipeline.opcode.simPublic
  c.trap.simPublic()

  c.imem.readData := imem.readSync(
    address = c.imem.addr.resize(12),
    enable = c.imem.valid,
  )

  c.dmem.readData := imem.readWriteSync(
    address = c.dmem.addr.resize(12),
    enable = c.dmem.valid,
    write = c.dmem.wren.get,
    data = c.dmem.writeData.get,
    mask = c.dmem.wmask.get
  )

  val rf = Mem(Bits(32 bits), wordCount = cfg.regCount) simPublic;
  c.rf1.readData := c.rf1.addr.mux(U(0, cfg.wrfaddr) -> B(0, cfg.xlenb), default -> rf(c.rf1.addr))
  rf.write(c.rf1.addr, c.rf1.writeData, c.rf1.wren)
  if (cfg.enableDualPort) {
    c.rf2.get.readData := c.rf2.get.addr.mux(U(0, cfg.wrfaddr) -> B(0, cfg.xlenb), default -> rf(c.rf2.get.addr))
    rf.write(c.rf2.get.addr, c.rf2.get.writeData, c.rf2.get.wren)
  }
}

object Surov3CoreVerilog extends App {
  Config.spinal.generateVerilog(Surov3Core(SurovConfig()))
}

object Surov3CoreVhdl extends App {
  Config.spinal.generateVhdl(Surov3Core(SurovConfig()))
}
