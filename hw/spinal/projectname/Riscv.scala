package projectname
import spinal.core._

// S0 is a special stage the is idle
object Opcode extends SpinalEnum {
  val Op, OpImm, Auipc, Lui = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    Op    -> 0b01_100_11,
    OpImm -> 0b00_100_11,
    Auipc -> 0b00_101_11,
    Lui   -> 0b01_101_11
  )
}

object rv {
  def opcode (ir: Bits): Opcode.C = { val o = Opcode(); o.assignFromBits(ir, 6, 0); o }
  def rd     (ir: Bits): Bits = ir(11 downto 7)
  def funct3 (ir: Bits): Bits = ir(14 downto 12)
  def rs1    (ir: Bits): Bits = ir(19 downto 15)
  def rs2    (ir: Bits): Bits = ir(24 downto 20)
  def funct7 (ir: Bits): Bits = ir(31 downto 25)
  def arith  (ir: Bits): Bool = ((opcode(ir) === Opcode.Op 
                                 | (opcode(ir) === Opcode.OpImm & (funct3(ir) === F3_SLL | funct3(ir) === F3_SR))) 
                                & ir(30))
  def shadd  (ir: Bits): Bool = opcode(ir) === Opcode.Op & ir(29)
  def imm_i  (ir: Bits): SInt = ir(31 downto 20).asSInt.resize(32) // Sign extended to 32 bits
  def imm_s  (ir: Bits): SInt = (ir(31 downto 25) ## ir(11 downto 7)).asSInt.resize(32) // Concatenate [31:25] and [11:7], then sign extend
  def imm_b  (ir: Bits): SInt = (ir(31) ## ir(7) ## ir(30 downto 25) ## ir(11 downto 8) ## B(0)).asSInt.resize(32) // Concatenate bits: [31] [7] [30:25] [11:8] [0]
  def imm_u  (ir: Bits): SInt = (ir(31 downto 12) ## B(0, 12 bits)).asSInt.resize(32) // Concatenate [31:12] and 12 zero bits
  def imm_j  (ir: Bits): SInt = (ir(31) ## ir(19 downto 12) ## ir(20) ## ir(30 downto 21) ## B(0)).asSInt.resize(32) // Concatenate bits: [31] [19:12] [20] [30:21] [0]

  def OP_LOAD     = B"00_000_11"
  def OP_STORE    = B"01_000_11"
  def OP_BRANCH   = B"11_000_11"
  def OP_JAL      = B"11_011_11"
  def OP_JALR     = B"11_001_11"
  def OP_OP       = B"01_100_11"
  def OP_IMM      = B"00_100_11"
  def OP_AUIPC    = B"00_101_11"
  def OP_LUI      = B"01_101_11"
  def OP_FENCE	  = B"00_011_11"
  def OP_SYS      = B"11_100_11"

  def  F3_ADDSUB  = B"000"
  def  F3_SLT     = B"010"
  def  F3_SLTU    = B"011"
  def  F3_XOR     = B"100"
  def  F3_OR      = B"110"
  def  F3_AND     = B"111"
  def  F3_SLL     = B"001"
  def  F3_SR      = B"101"
}