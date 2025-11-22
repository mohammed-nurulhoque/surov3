package projectname

import spinal.core._
import spinal.core.sim._

sealed trait XLEN
case object XLEN32 extends XLEN
case object XLEN64 extends XLEN

case class SurovConfig(
  enable32Regs: Boolean = true,
  enableDualPort: Boolean = false,
  enableZba: Boolean = false,
  enableForward: Boolean = true,
  xlen: XLEN = XLEN32,
) {
  def xlenb = xlen match { case XLEN32 => 32 bits
                           case XLEN64 => 64 bits}
  def xlenB = xlenb / (8 bits)
  def regCount() = if (enable32Regs) 32 else 16 
  def wrfaddr() = if (enable32Regs) 5 else 4
}

sealed trait Direction {
  def flip(): Direction
  def dir(): IODirection
}
case object Master extends Direction {
  override def flip() = Slave
  override def dir() = out
}
case object Slave extends Direction {
  override def flip() = Master
  override def dir() = in
}

class RFIF(side: Direction, cfg: SurovConfig) extends Bundle {
  val addr = side.dir UInt(cfg.wrfaddr bits)
  val wren = side.dir Bool
  val writeData = side.dir Bits(cfg.xlenb)
  val readData = side.flip.dir Bits(cfg.xlenb)


  def read(regnum: Bits) = {
    addr := regnum.asUInt
    wren := False
    readData
  }

  def write(regnum: Bits, value: Bits) {
    addr := regnum.asUInt
    wren := True
    writeData := value
  }
}

class MemIF(side: Direction, cfg: SurovConfig) extends Bundle {
  val addr = side.dir UInt(cfg.xlenb - (2 bits)) // FIXME hardcoded when rv32
  val wren = side.dir Bool
  val wmask = side.dir Bits(cfg.xlenb)
  val writeData = side.dir Bits(cfg.xlenb)
  val readData = side.flip.dir Bits(cfg.xlenb)
  val valid = side.dir Bool
  val ready = side.flip.dir Bool

  def read(address: UInt): Bits = {
    addr  := address(address.high downto 2)
    wren  := False
    valid := True
    readData
  }

  def write(address: UInt, value: UInt) {
    // FIXME
  }
}

// class AluIF(side: Direction, cfg: SurovConfig) extends Bundle {
//     val start = side.dir Bool
//     val src_a = side.dir() (UInt(cfg.xlenb))
//     val src_b = side.dir() (UInt(cfg.xlenb))
//     val f3 = side.dir Bits(3 bits)
//     val arith_bit = side.dir Bool()
//     val shadd = side.dir Bool()
//     val branch = side.dir Bool()
//     val result = side.flip.dir() UInt(cfg.xlenb)
//     val shamt_out = side.flip.dir UInt(cfg.wrfaddr bits)
//     val ready = side.flip.dir Bool()
// }

class ALU(cfg: SurovConfig ) extends BlackBox {  
  // addGeneric("wordWidth", wordWidth)
  val clk = in Bool
  val start = in Bool
  val src_a = in (UInt(cfg.xlenb))
  val src_b = in (UInt(cfg.xlenb))
  val f3 = in Bits(3 bits)
  val arith_bit = in Bool()
  val shadd = in Bool()
  val branch = in Bool()
  val result = out UInt(cfg.xlenb)
  val shamt_out = out UInt(cfg.wrfaddr bits)
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
  val pc = Reg(UInt(cfg.xlenb)) init(0) simPublic
  val pc2 = Reg(UInt(cfg.xlenb))
  val pcPlus4 = pc + 4;
  val ir = Reg(Bits(cfg.xlenb)).init(rv.OP_IMM.resize(32 bits)).simPublic()
  val ir2 = Reg(Bits(cfg.xlenb))
  val r1 = Reg(UInt(cfg.xlenb)).simPublic()
  val r2 = Reg(UInt(cfg.xlenb)).simPublic()
  val alu = new ALU(cfg)
  val mem = new MemIF(Master, cfg)
  val rf1 = new RFIF(Master, cfg)
  val rf2 = if (cfg.enableDualPort) new RFIF(Master, cfg) else null

  var finished = Bool

  mem.valid := False
  mem.addr.assignDontCare()
  mem.wren.assignDontCare()
  mem.writeData.assignDontCare()
  mem.wmask.assignDontCare()
  
  rf1.addr.assignDontCare()
  rf1.wren.assignDontCare()
  rf1.writeData.assignDontCare()


  List(alu.src_a, alu.src_b, alu.f3) map (_.assignDontCare)
  List(alu.arith_bit, alu.shadd, alu.branch, alu.start) map (_ := False)


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
    alu.start   := start
    (alu.result, alu.shamt_out)
  }
}

class Pipeline(cfg: SurovConfig) {
  val c = new IExContext(cfg)
  var jumpTarget = UInt(cfg.xlenb)
  var (fetchingJump, fetchedJump) = (false, false)
  // var waitForMem: Option[SoftStage] = None
  var ss: SoftStage = SoftStage.SoftS1

  def build(plugins: List[Plugin]) {
    switch(rv.opcode(c.ir)) {
      when (c.stage === Stage.S1 & c.start) {
        c.mem.read(c.pc+4)
      }
      for (p <- plugins) {
        is(p.opcode) {
          // waitForMem = None
          c.finished \= False
          fetchingJump = false; fetchedJump =false
          switch (c.stage) {
            for (i <- List(1, 2, 3)) {
              if (fetchingJump) fetchedJump = true
              ss = SoftStage.fromInt(i)
              val s  = ss.harden
              is(s) {
                p.getStage(ss)
              }
            }
          }
        }
        default {}
      }
    }
    when(!c.alu.ready) {
      c.start := False
    }
    when(c.stage === Stage.S0) {
      c.mem.read(c.pc+4)
      forward(false)
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
    jumpTarget \= target
    c.pc2 := target
    c.ir2 := c.mem.readData
    c.mem.read(target)
    ss
  }

  def take_jump(fetchS: SoftStage) {
    if (ss == fetchS) {
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
    c.ir := (if (irSaved) c.ir2 else c.mem.readData)
  }
}

// Hardware definition
case class MyTopLevel(cfg: SurovConfig) extends Component {
  val pipeline = new Pipeline(cfg)
  val plugins = List(
    new OpOpPlugin(pipeline),
    new OpImmPlugin(pipeline))
  pipeline.build(plugins)
}

case class SimDUT(cfg: SurovConfig) extends Component {
  val core = MyTopLevel(cfg)
  val imem = Mem(Bits(32 bits), wordCount = 256) simPublic
  val c = core.pipeline.c
  c.mem.readData.simPublic
  c.mem.addr.simPublic
  c.start.simPublic
  c.mem.readData := imem.readSync(
    address = c.mem.addr.resize(8),
    enable = c.mem.valid,
  )
  val rf = Mem(Bits(32 bits), wordCount = cfg.regCount) simPublic;
  rf(B"5'h0".asUInt) := B"32'h0"
  c.rf1.readData := rf(c.rf1.addr)
  rf.write(c.rf1.addr, c.rf1.writeData, c.rf1.wren)
  if (cfg.enableDualPort) {
    val rf2 = Mem(Bits(32 bits), wordCount = cfg.regCount)
    c.rf2.readData := rf2(c.rf2.addr)
    rf2.write(c.rf2.addr, c.rf2.writeData, c.rf2.wren & c.rf2.addr =/= 0)
  }
}

object MyTopLevelVerilog extends App {
  Config.spinal.generateVerilog(MyTopLevel(SurovConfig()))
}

object MyTopLevelVhdl extends App {
  Config.spinal.generateVhdl(MyTopLevel(SurovConfig()))
}
