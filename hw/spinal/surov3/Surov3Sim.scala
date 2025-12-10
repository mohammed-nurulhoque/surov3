package surov3

import java.io._
import scala.io.Source
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import spinal.core._
import spinal.core.sim._

object Surov3CoreSim extends App {
  // Define Syscall Numbers (Linux/Newlib standard)
  val SYS_EXIT  = 93
  val SYS_WRITE = 64

  val maxCycles = 1000 // Safety timeout
  val baseAddr = 0x1000
  val binFiles = if (args.isEmpty) Array("hw/a.bin") else args
  val cfg = SurovConfig()

    val fw = new FileWriter("hw/a.log", false)
    val pw = new PrintWriter(fw)

  Config.sim.withWave.noOptimisation.compile(SimDUT(cfg)).doSim { dut =>
    val pl = dut.top.core.pipeline
    // --- Load Instructions from program.bin ---
    for (binFilePath <- binFiles) {
      val fis = new FileInputStream(binFilePath)
      val buffer = new Array[Byte](cfg.issueWidth * 4) // Read 4 bytes at a time for 32-bit words
      
      dut.mem.setBigInt(0, 0)
      var wordsLoaded = baseAddr / (4 * cfg.issueWidth)
      println(s"Loading instructions from $binFilePath...")
      while (fis.read(buffer) != -1) {
        dut.mem.setBigInt(address = wordsLoaded, data = BigInt(0.toByte +: buffer.reverse))
        wordsLoaded += 1
      }
      fis.close()
      println(s"Instruction memory loaded successfully: $wordsLoaded words.")
      // --- End Load Instructions ---

      // Fork a process to generate the reset and the clock on the dut
      fork {
        dut.clockDomain.assertReset()
        for (i <- 0 to 10) {
          dut.clockDomain.clockToggle()
          sleep(5)
        }
        dut.clockDomain.deassertReset()
        while(true) {
          dut.clockDomain.clockToggle()
          sleep(5)
        }
      }

      // Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge(6)
      println(f"first word: ${dut.mem.getBigInt(0x100)}%x")
      assert(!pl.pipes.exists(c => c.stage.toEnum != Stage.S1))

      // 4. Main Simulation Loop
      var cycles = 0
      var instsRet = 0
      while(cycles < maxCycles) {
        dut.clockDomain.waitRisingEdge()
        cycles += 1
        pw.println(f"[cycle ${cycles}%03d]")
        val global =
          f"G fetchedJump=${pl.fetchedJump.toBoolean} jumping=${pl.jumping.toBoolean} jumped=${pl.jumped.toBoolean} " +
          f"pcBase=${pl.pcBase.toLong}%08x pc2=${pl.pc2.toLong}%08x " +
          f"irLine=${pl.irBuf.map(_.toLong.toHexString).mkString("[",",","]")} " +
          f"ir2=${pl.ir2.toBigInt.toString(16)} " +
          f"stall=${pl.stall.map(_.toBoolean)} kill=${pl.kill.toInt.toBinaryString} " +
          f"finished=${pl.finished.map(_.toBoolean)}" +
          f"active=${Vec.fill(4)(False)} " //f"active=${pl.active.map(_.toBoolean)} "
        pw.println(global)
        val scans =
          //f"H readScan=${pl.readScan.map(_.toLong.toBinaryString)} "
          //f"writeScan=${pl.writeScan.map(_.toLong.toBinaryString)} "
          // f"willRead=${pl.willRead.map(_.toLong.toBinaryString)} "
          //f"willWrite=${pl.willWrite.map(_.toLong.toBinaryString)} "
          f"stageReads=${pl.stageReads.map(_.toLong.toBinaryString)} stageWrites=${pl.stageWrites.map(_.toLong.toBinaryString)} "
          f"stageLoads=${pl.stageLoads.map(_.toBoolean)} stageStores=${pl.stageStores.map(_.toBoolean)} "
          //f"willLoad=${pl.willLoad.map(_.toBoolean)}
          //f"willStore=${pl.willStore.map(_.toBoolean)} "
        pw.println(scans)
        for (c <- pl.pipes) {
          val line =
            f"P${c.id} stage=${c.stage.toEnum} start=${pl.start(c.id).toBoolean} " +
            f"stall=${pl.stall(c.id).toBoolean} kill=${((pl.kill.toLong >> c.id) & 1)==1} finished=${pl.finished(c.id).toBoolean} " +
            f"pc=${c.pc.toLong}%08x ir=${c.ir.toLong}%08x " +
            f"r1=${c.r1.toLong}%08x r2=${c.r2.toLong}%08x"
          pw.println(line)
        }

        val sb = new StringBuilder()
        sb.append("R ")
        for (i <- 0 until cfg.regCount) {
          val regValue = dut.top.rf.getBigInt(i)
          sb.append(f"x$i%02d:${regValue}%d ")
        }
        pw.println(sb.toString)

        instsRet += pl.pipes.count(c => pl.stage(c.id).toEnum == Stage.S1 && !pl.stall(c.id).toBoolean && (((pl.kill.toLong >> c.id) & 1) != 1))
        pw.println(f"InstsRet=${instsRet}")
        val trap = dut.trap.toInt
        val c = if (trap != 0) pl.pipes(Integer.numberOfTrailingZeros(trap)) else null
        if (trap != 0 && c.stage.toEnum == Stage.S3) {
          val pc = c.pc.toLong
          val ir = c.ir.toBigInt // The instruction causing the trap
          
          // Retrieve register values needed for arguments
          val a0 = dut.top.rf.getBigInt(10)
          val a1 = dut.top.rf.getBigInt(11)
          val a2 = dut.top.rf.getBigInt(12)
          val a5 = dut.top.rf.getBigInt(15) // Syscall ID

          // Match instruction patterns
          if (ir == 0x73) { // ECALL
            a5.toLong match {
              case SYS_EXIT =>
                val exitCode = a0.toLong
                pw.println(f"Completed $instsRet instructions in $cycles Cycles")
                pw.println(f"EXIT STATUS: $exitCode")
                pw.close()
                fw.close()
                simSuccess()

              case SYS_WRITE =>
                val sb = new StringBuilder()

                // Iterate through the bytes we need to print
                for (i <- 0 until a2.toInt) {
                  val currentAddr = a1 + i
                  val wordData = dut.mem.getBigInt(currentAddr / 4 toLong) 
                  val byteOffset = (currentAddr % 4) * 8
                  val charVal    = ((wordData >> byteOffset.toInt) & 0xFF).toByte.toChar
                  sb.append(charVal)
                }

                print(sb.toString)

              case _ =>
                pw.println(f"Unhandled Syscall #$a5 at PC: $pc%08x")
                pw.close()
                fw.close()
                simFailure()
            }
          } 
          else if ((ir & 0x2073) == 0x2073) {

            // Match the specific CSR address
            val dataToWrite = (ir >> 20 & 0xFFF).toInt match {
              case 0xC00 => cycles
              case 0xC02 => instsRet
              case _ =>
                // Default case: Unhandled CSR
                pw.println(f"ERROR: Unhandled CSR read at PC: $pc%08x")
                pw.close()
                fw.close()
                simFailure() // <-- SIMULATION FAILURE ON UNHANDLED CSR
            }
            dut.top.rf.setBigInt(ir.toLong >> 7 & 0x1F, dataToWrite)
          } 
          else if ((ir & 0x7F) == 0x0F) { // Opcode 0x0F is FENCE
            // Fence Logic placeholder
          } 
          else {
            pw.println(f"Unrecognized TRAP op $ir%08x at PC: $pc%08x")
            pw.close()
            fw.close()
            simFailure()
          }
        }
      }
      pw.close()
      fw.close()
      simFailure(s"Didn't finish after $maxCycles steps")
    }
  }
}

// Lightweight interactive TUI to browse the generated log (hw/a.log by default)
object Surov3TraceTui extends App {
  case class PipeRow(
    id: Int,
    stage: String,
    start: Boolean,
    stall: Boolean,
    kill: Boolean,
    finished: Boolean,
    pc: Long,
    ir: Long,
    r1: Long,
    r2: Long
  )
  case class Cycle(
    n: Int,
    fetchedJump: Boolean,
    jumping: Boolean,
    jumped: Boolean,
    pcBase: Long,
    pc2: Long,
    irLine: Seq[String],
    ir2: String,
    stall: Seq[Boolean],
    kill: String,
    pipes: Seq[PipeRow]
  )

  val logPath = args.headOption.getOrElse("hw/a.log")
  val lines = scala.io.Source.fromFile(logPath).getLines().toVector

  // Parse cycles based on the structured log format above
  val cycles = collection.mutable.ArrayBuffer.empty[Cycle]
  var idx = 0
  while (idx < lines.length) {
    val line = lines(idx)
    if (line.startsWith("[cycle")) {
      val cnum = line.drop(7).takeWhile(_.isDigit).toInt
      val g = lines(idx + 1).split("\\s+").toSeq
      def gFlag(k: String) = g.find(_.startsWith(k)).map(_.dropWhile(_ != '=').drop(1)).getOrElse("false").toBoolean
      def gHex(k: String) = java.lang.Long.parseLong(g.find(_.startsWith(k)).map(_.split("=")(1)).getOrElse("0"), 16)
      val irLineStr = g.find(_.startsWith("irLine=")).getOrElse("irLine=[]")
      val irLine = irLineStr.dropWhile(_ != '[').drop(1).takeWhile(_ != ']').split(",").filter(_.nonEmpty)
      val ir2 = g.find(_.startsWith("ir2=")).map(_.split("=")(1)).getOrElse("0")
      val stallStr = g.find(_.startsWith("stall=")).map(_.split("=")(1)).getOrElse("Vector()")
      val stalls = stallStr.dropWhile(_ != '(').drop(1).dropRight(1).split(",").toSeq.filter(_.nonEmpty).map(_.toBoolean)
      val kill = g.find(_.startsWith("kill=")).map(_.split("=")(1)).getOrElse("0")
      val pipes = collection.mutable.ArrayBuffer.empty[PipeRow]
      var j = idx + 2
      while (j < lines.length && lines(j).startsWith("P")) {
        val parts = lines(j).split("\\s+")
        def part(k: String) = parts.find(_.startsWith(k)).map(_.split("=")(1)).getOrElse("")
        pipes += PipeRow(
          id = parts(0).drop(1).toInt,
          stage = part("stage"),
          start = part("start").toBoolean,
          stall = part("stall").toBoolean,
          kill = part("kill").toBoolean,
          finished = part("finished").toBoolean,
          pc = java.lang.Long.parseLong(part("pc"), 16),
          ir = java.lang.Long.parseLong(part("ir"), 16),
          r1 = java.lang.Long.parseLong(part("r1"), 16),
          r2 = java.lang.Long.parseLong(part("r2"), 16)
        )
        j += 1
      }
      cycles += Cycle(
        n = cnum,
        fetchedJump = gFlag("fetchedJump"),
        jumping = gFlag("jumping"),
        jumped = gFlag("jumped"),
        pcBase = gHex("pcBase"),
        pc2 = gHex("pc2"),
        irLine = irLine,
        ir2 = ir2,
        stall = stalls,
        kill = kill,
        pipes = pipes.toSeq
      )
      idx = j
    } else idx += 1
  }

  val pcFirst = collection.mutable.HashMap.empty[Long, Int]
  cycles.foreach { c =>
    c.pipes.foreach { p =>
      if (!pcFirst.contains(p.pc)) pcFirst(p.pc) = c.n
    }
  }

  var cursor = 0
  def show(i: Int): Unit = {
    val c = cycles(i)
    println(f"[cycle ${c.n}] pcBase=${c.pcBase}%08x pc2=${c.pc2}%08x fetchedJump=${c.fetchedJump} jumping=${c.jumping} jumped=${c.jumped}")
    println(f"irLine=${c.irLine.mkString("[", ",", "]")} ir2=${c.ir2}")
    println(f"kill=${c.kill} stall=${c.stall.mkString("[", ",", "]")}")
    c.pipes.foreach { p =>
      println(f"P${p.id} ${p.stage} pc=${p.pc}%08x ir=${p.ir}%08x start=${p.start} stall=${p.stall} kill=${p.kill} fin=${p.finished} r1=${p.r1}%08x r2=${p.r2}%08x")
    }
  }

  def help(): Unit = {
    println("Commands: n/next, p/prev, c <num>, pc <hex>, q, h")
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
    case s if s == "h" || s == "help" => help()
    case other => println(s"unrecognized: $other"); help()
  }
}
