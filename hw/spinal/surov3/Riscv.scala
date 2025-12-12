package surov3
import spinal.core._
import oshi.driver.windows.perfmon.LoadAverage

// S0 is a special stage the is idle
object Opcode extends SpinalEnum {
  val Op, OpImm, Auipc, Lui, Load, Store, Branch, Jal, Jalr, Fence, Sys = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    Op    -> 0b01_100_11,
    OpImm -> 0b00_100_11,
    Auipc -> 0b00_101_11,
    Lui   -> 0b01_101_11,
    Load  -> 0b00_000_11,
    Store -> 0b01_000_11,
    Branch-> 0b11_000_11,
    Jal   -> 0b11_011_11,
    Jalr  -> 0b11_001_11,
    Fence -> 0b00_011_11,
    Sys   -> 0b11_100_11,
  )
}

object rv {
  def opcode (ir: Bits): Opcode.C = { val o = Opcode(); o.assignFromBits(ir(6 downto 0)); o }
  def rd     (ir: Bits): UInt = ir(11 downto 7).asUInt
  def funct3 (ir: Bits): Bits = ir(14 downto 12)
  def rs1    (ir: Bits): UInt = ir(19 downto 15).asUInt
  def rs2    (ir: Bits): UInt = ir(24 downto 20).asUInt
  def funct7 (ir: Bits): Bits = ir(31 downto 25)
  def arith  (ir: Bits): Bool = ((opcode(ir) === Opcode.Op 
                                 | (opcode(ir) === Opcode.OpImm & (funct3(ir) === F3_SLL | funct3(ir) === F3_SR))) 
                                & ir(30))
  def nop: Bits = Opcode.OpImm.asBits.resize(32)
  def shadd  (ir: Bits): Bool = opcode(ir) === Opcode.Op & ir(29)
  def imm_i  (ir: Bits): SInt = ir(31 downto 20).asSInt.resize(32) // Sign extended to 32 bits
  def imm_s  (ir: Bits): SInt = (ir(31 downto 25) ## ir(11 downto 7)).asSInt.resize(32) // Concatenate [31:25] and [11:7], then sign extend
  def imm_b  (ir: Bits): SInt = (ir(31) ## ir(7) ## ir(30 downto 25) ## ir(11 downto 8) ## B"0").asSInt.resize(32) // Concatenate bits: [31] [7] [30:25] [11:8] [0]
  def imm_u  (ir: Bits): SInt = (ir(31 downto 12) ## B(0, 12 bits)).asSInt.resize(32) // Concatenate [31:12] and 12 zero bits
  def imm_j  (ir: Bits): SInt = (ir(31) ## ir(19 downto 12) ## ir(20) ## ir(30 downto 21) ## B"0").asSInt.resize(32) // Concatenate bits: [31] [19:12] [20] [30:21] [0]

  def  F3_ADDSUB  = B"000"
  def  F3_SLT     = B"010"
  def  F3_SLTU    = B"011"
  def  F3_XOR     = B"100"
  def  F3_OR      = B"110"
  def  F3_AND     = B"111"
  def  F3_SLL     = B"001"
  def  F3_SR      = B"101"

  // CSR address (bits 31:20 of I-type immediate)
  def csr(ir: Bits): UInt = ir(31 downto 20).asUInt

  // Zicntr CSR addresses
  val CSR_CYCLE    = 0xC00
  val CSR_TIME     = 0xC01
  val CSR_INSTRET  = 0xC02
  val CSR_CYCLEH   = 0xC80
  val CSR_TIMEH    = 0xC81
  val CSR_INSTRETH = 0xC82
}