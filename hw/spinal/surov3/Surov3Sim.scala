package surov3

import java.io._
import java.io.FileInputStream
import scala.util.Try
import spinal.core._
import spinal.core.sim._
import upickle.default._

case class PipeSnap(
  id: Int,
  opcode: String,
  stage: String,
  start: Boolean,
  stall: Boolean,
  kill: Boolean,
  finished: Boolean,
  trap: Boolean,
  pc: Long,
  ir: Long,
  r1: Long,
  r2: Long
)

case class CycleSnap(
  n: Int,
  fetchedJump: Boolean,
  jumping: Boolean,
  jumped: Boolean,
  jumpPipeIdx: Int,
  pcBase: Long,
  pc2: Long,
  ir2: Long,
  pipes: Seq[PipeSnap],
  var regs: Seq[Long],
  instsRet: Int
)

object CycleSnap {
  implicit val pipeRw: ReadWriter[PipeSnap] = macroRW
  implicit val cycleRw: ReadWriter[CycleSnap] = macroRW
}

object Surov3CoreSim extends App {
  import scala.util.control.Breaks._
  import sys.process._

  val SYS_EXIT  = 93
  val SYS_WRITE = 64
  val maxCycles = 200000
  val baseAddr = 0x1000

  // Parse config from command-line args: --key=value
  // e.g., --dualPort=true --syncRF=false --zba=true --regCount=32 --issueWidth=2
  val argMap = args.filter(_.startsWith("--")).map { arg =>
    val parts = arg.drop(2).split("=", 2)
    parts(0) -> (if (parts.length > 1) parts(1) else "true")
  }.toMap

  val cfg = SurovConfig(
    regCount = argMap.get("regCount").map(_.toInt).getOrElse(32),
    enableDualPort = argMap.get("dualPort").map(_.toBoolean).getOrElse(false),
    issueWidth = argMap.get("issueWidth").map(_.toInt).getOrElse(1),
    syncRF = argMap.get("syncRF").map(_.toBoolean).getOrElse(false),
    enableZba = argMap.get("zba").map(_.toBoolean).getOrElse(false),
  )
  println(s"Config: regCount=${cfg.regCount} dualPort=${cfg.enableDualPort} issueWidth=${cfg.issueWidth} syncRF=${cfg.syncRF} zba=${cfg.enableZba}")

  // Convert ELF to binary, returns path to bin
  def elf2bin(elf: String): String = {
    val bin = "hw/a.bin"
    val cmd = s"riscv64-unknown-elf-objcopy -O binary $elf $bin"
    println(s"Running: $cmd")
    require(cmd.! == 0, s"objcopy failed for $elf")
    bin
  }

  val elfFiles = args.filterNot(_.startsWith("--"))
  val elfFilesOrDefault = if (elfFiles.isEmpty) Array("hw/a.out") else elfFiles

  // Fallback opcode lookup using low 7 bits of the instruction.
  private val opcodeNames: Map[Int, String] = Map(
    0x33 -> "Op",
    0x13 -> "OpImm",
    0x17 -> "Auipc",
    0x37 -> "Lui",
    0x03 -> "Load",
    0x23 -> "Store",
    0x63 -> "Branch",
    0x6f -> "Jal",
    0x67 -> "Jalr",
    0x0f -> "Fence",
    0x73 -> "Sys"
  )

  private def opcodeName(ir: Long): String =
    opcodeNames.getOrElse((ir & 0x7fL).toInt, "???")

  // Result tracking for multiple files
  sealed trait RunResult
  case class Success(file: String, instsRet: Int, cycles: Int, exitCode: Long) extends RunResult
  case class Failure(file: String, reason: String) extends RunResult

  Config.sim.withWave.noOptimisation.compile(SimDUT(cfg)).doSim { dut =>
    val pl = dut.top.core.pipeline
    var results = List.empty[RunResult]

    // Fork a process to generate the clock - runs for entire simulation
    fork {
      while (true) {
        dut.clockDomain.clockToggle()
        sleep(5)
      }
    }

    for (elfPath <- elfFilesOrDefault) {
      val binPath = elf2bin(elfPath)
      val logPath = elfPath.replaceAll("\\.(elf|out)$", "") + ".log"
      val fw = new FileWriter(logPath, false)
      val pw = new PrintWriter(fw)

      // Reset the core first
      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge(2)
      dut.clockDomain.deassertReset()

      val fis = new FileInputStream(binPath)
      val buffer = new Array[Byte](cfg.issueWidth * 4)
      
      // Clear memory and load new program
      for (i <- 0 until 16384) dut.mem.setBigInt(i, 0)
      var wordsLoaded = baseAddr / (4 * cfg.issueWidth)
      println(s"\n=== Running $elfPath ===")
      while (fis.read(buffer) != -1) {
        dut.mem.setBigInt(address = wordsLoaded, data = BigInt(0.toByte +: buffer.reverse))
        wordsLoaded += 1
      }
      fis.close()
      println(s"Instruction memory loaded successfully: $wordsLoaded words.")

      var cycles = 0
      var instsRet = 0
      var runDone = false
      var runResult: RunResult = Failure(elfPath, "Unknown error")

      breakable {
        while (cycles < maxCycles && !runDone) {
          val trapBits = dut.trap.toLong
          println(s"willRead: ${pl.willRead.map(_.toLong)}") // This fails
          val pipesSnap = pl.pipes.map { c =>
            println(s"op: ${c.op.toEnum}")  // This also fails
            PipeSnap(
              id = c.id,
              opcode = opcodeName(c.ir.toLong),
              stage = c.stage.toEnum.toString,
              start = pl.start(c.id).toBoolean,
              stall = pl.stall(c.id).toBoolean,
              kill = (((pl.kill.toLong >> c.id) & 1) == 1),
              finished = pl.finished(c.id).toBoolean,
              trap = ((trapBits >> c.id) & 1) == 1,
              pc = c.pc.toLong,
              ir = c.ir.toLong,
              r1 = c.r1.toLong,
              r2 = c.r2.toLong
            )
          }.toSeq
          
          instsRet += pl.pipes.count(c => pl.stage(c.id).toEnum == Stage.S1 && !pl.stall(c.id).toBoolean && (((pl.kill.toLong >> c.id) & 1) != 1))

          var cycleSnap = CycleSnap(
            n = cycles,
            fetchedJump = pl.fetchedJump.toBoolean,
            jumping = pl.jumping.toBoolean,
            jumped = pl.jumped.toBoolean,
            jumpPipeIdx = pl.jumpPipeIdx.toInt,
            pcBase = pl.pcBase.toLong,
            pc2 = pl.pc2.toLong,
            ir2 = pl.ir2.toBigInt.longValue,
            pipes = pipesSnap,
            regs = Seq.empty,
            instsRet = instsRet
          )

          dut.clockDomain.waitRisingEdge()
          cycles += 1

          cycleSnap.regs = (0 until cfg.regCount).map(i => dut.top.rf.getBigInt(i).toLong).toSeq
          pw.println(write(cycleSnap))

          val trap = dut.trap.toInt
          val c = if (trap != 0) pl.pipes(Integer.numberOfTrailingZeros(trap)) else null
          if (trap != 0 && c.stage.toEnum == Stage.S1 && !c.stall.toBoolean) {
            val pc = c.pc.toLong
            val ir = c.ir.toBigInt

            val a0 = dut.top.rf.getBigInt(10)
            val a1 = dut.top.rf.getBigInt(11)
            val a2 = dut.top.rf.getBigInt(12)
            val a5 = dut.top.rf.getBigInt(15)

            if (ir == 0x73) { // ECALL
              a5.toLong match {
                case SYS_EXIT =>
                  val exitCode = a0.toLong
                  println(f"Completed $instsRet instructions in $cycles Cycles")
                  println(f"EXIT STATUS: $exitCode")
                  runResult = Success(elfPath, instsRet, cycles, exitCode)
                  runDone = true

                case SYS_WRITE =>
                  val sb = new StringBuilder()
                  val lineSize = cfg.issueWidth * 4  // 16 bytes per line
                  for (i <- 0 until a2.toInt) {
                    val currentAddr = (a1 + i).toLong
                    val lineAddr = currentAddr / lineSize
                    val byteOffset = (currentAddr % lineSize).toInt
                    val lineData = dut.mem.getBigInt(lineAddr)
                    val charVal = ((lineData >> (byteOffset * 8)) & 0xFF).toByte.toChar
                    sb.append(charVal)
                  }
                  print(sb.toString)

                case _ =>
                  println(f"Unhandled Syscall #$a5 at PC: $pc%08x")
                  runResult = Failure(elfPath, f"Unhandled Syscall #$a5")
                  runDone = true
              }
            }
            else if ((ir & 0x2073) == 0x2073) {
              val dataToWrite = (ir >> 20 & 0xFFF).toInt match {
                case 0xC00 => cycles
                case 0xC02 => instsRet
                case csr =>
                  println(f"ERROR: Unhandled CSR 0x$csr%03x at PC: $pc%08x")
                  runResult = Failure(elfPath, f"Unhandled CSR 0x$csr%03x")
                  runDone = true
                  0
              }
              if (!runDone) dut.top.rf.setBigInt((ir.toLong >> 7).toInt & (cfg.regCount - 1), dataToWrite)
            }
            else if ((ir & 0x7F) == 0x0F) {
              // FENCE - no-op
            }
            else {
              println(f"Unrecognized TRAP op $ir%08x at PC: $pc%08x")
              runResult = Failure(elfPath, f"Unrecognized TRAP op $ir%08x")
              runDone = true
            }
          }
        }
      }

      pw.close()
      fw.close()

      if (!runDone) {
        runResult = Failure(elfPath, s"Timeout after $maxCycles cycles")
        println(s"TIMEOUT: $elfPath didn't finish after $maxCycles cycles")
      }
      results = results :+ runResult
    }

    // Print summary
    println("\n" + "=" * 60)
    println("SIMULATION SUMMARY")
    println("=" * 60)
    var allSuccess = true
    results.foreach {
      case Success(file, insts, cyc, exit) =>
        println(f"✓ $file: $insts insts, $cyc cycles, exit=$exit")
        if (exit != 0) allSuccess = false
      case Failure(file, reason) =>
        println(f"✗ $file: $reason")
        allSuccess = false
    }
    println("=" * 60)

    if (allSuccess) simSuccess() else simFailure("Some tests failed")
  }
}

// Lightweight interactive TUI to browse the generated log (hw/a.log by default)
object Surov3TraceTui extends App {
  val logPath = args.headOption.getOrElse("hw/a.log")
  // Keep only non-empty JSON-looking lines; ignore trap/status text.
  val lines = scala.io.Source.fromFile(logPath).getLines().filter(_.trim.startsWith("{")).toVector
  println(s"[TUI] Read ${lines.length} JSON-like lines from $logPath")

  def parseCycle(line: String): Option[CycleSnap] =
    Try {
      read[CycleSnap](line)
    }.toOption

  val cycles = lines.zipWithIndex.flatMap { case (ln, idx) =>
    parseCycle(ln).orElse {
      println(s"[TUI] Parse failed at line $idx: $ln")
      None
    }
  }
  println(s"[TUI] Parsed ${cycles.length} cycles")

  val pcFirst = collection.mutable.HashMap.empty[Long, Int]
  cycles.foreach { c =>
    c.pipes.foreach { p =>
      if (!pcFirst.contains(p.pc)) pcFirst(p.pc) = c.n
    }
  }

  var cursor = 0
  def show(i: Int): Unit = {
    if (cycles.isEmpty) { println("No cycles parsed."); return }
    val c = cycles(i)
    val ir2Masked    = BigInt(c.ir2 & 0xffffffffffffffffL)
    val digits       = math.max(8, ((ir2Masked.bitLength max 32) + 3) / 4)
    val ir2HexPadded = {
      val s = ir2Masked.toString(16)
      ("0" * (digits - s.length)) + s
    }
    val ir2Chunks = ir2HexPadded.grouped(8).mkString("[", ", ", "]")

    println(f"[cycle ${c.n}] pcBase=${c.pcBase}%08x pc2=${c.pc2}%08x fetchedJump=${c.fetchedJump} jumping=${c.jumping} jumped=${c.jumped} jumpPipeIdx=${c.jumpPipeIdx} instsRet=${c.instsRet} ir2=${ir2Chunks}")

    def pipeBox(p: PipeSnap): (Seq[String], String) = {
      def b(v: Boolean) = if (v) "✓" else "✗"
      val header   = f"P${p.id} | ${p.stage} | ${p.opcode}"
      val pcLine   = f"pc=${p.pc}%08x"
      val irLine   = f"ir=${p.ir}%08x"
      val status   = f"start=${b(p.start)} stall=${b(p.stall)} kill=${b(p.kill)} fin=${b(p.finished)} trap=${b(p.trap)}"
      val regsLine = f"r1=${p.r1}%08x r2=${p.r2}%08x"
      val lines    = Seq(header, pcLine, irLine, status, regsLine)
      val width    = lines.map(_.length).max
      val border   = "+" + ("-" * (width + 2)) + "+"
      def row(text: String) = "| " + text + (" " * (width - text.length)) + " |"
      (border +: lines.map(row), border)
    }

    val boxes = c.pipes.map(pipeBox)
    val combinedLines = boxes.zipWithIndex.flatMap { case ((boxLines, border), idx) =>
      if (idx == boxes.size - 1) boxLines :+ border else boxLines
    }
    println(combinedLines.mkString("\n"))
  }

  def showRegs(i: Int): Unit = {
    if (cycles.isEmpty) { println("No cycles parsed."); return }
    val regs = cycles(i).regs
    val rows = regs.grouped(4).zipWithIndex.map { case (g, row) =>
      g.zipWithIndex.map { case (v, idx) =>
        val regIdx = row * 4 + idx
        f"x$regIdx%02d=0x${v & 0xffffffffffffffffL}%08x" // assume rv32
      }.mkString(" ")
    }
    println(rows.mkString("\n"))
  }

  def help(): Unit = {
    println("Commands: n/next, p/prev, c <num>, pc <hex>, r/regs, q, h")
  }

  help()
  show(cursor)
  Iterator.continually(scala.io.StdIn.readLine("> ")).takeWhile(_ != null).foreach {
    case null => ()
    case s if s == "q" => System.exit(0)
    case s if s == "n" || s == "next" =>
      cursor = (cursor + 1).min(cycles.size - 1); show(cursor)
    case s if s == "p" || s == "prev" =>
      cursor = (cursor - 1).max(0); show(cursor)
    case s if s.startsWith("c ") =>
      val n = s.split("\\s+")(1).toInt
      cycles.indexWhere(_.n == n) match {
        case -1 => println(s"cycle $n not found")
        case idx => cursor = idx; show(cursor)
      }
    case s if s.startsWith("pc ") =>
      val pc = java.lang.Long.parseLong(s.split("\\s+")(1).replace("0x",""), 16)
      pcFirst.get(pc) match {
        case Some(n) =>
          val idx = cycles.indexWhere(_.n == n)
          if (idx >= 0) { cursor = idx; show(cursor) } else println(s"pc 0x$pc not found in cycles")
        case None => println(s"pc 0x$pc not found")
      }
    case s if s == "r" || s == "regs" =>
      showRegs(cursor)
    case s if s == "h" || s == "help" => help()
    case other => println(s"unrecognized: $other"); help()
  }
}
