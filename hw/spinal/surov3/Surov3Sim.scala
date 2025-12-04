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
        pw.println(f"[cycle ${cycles}%03d] Pipes state:")
        pw.println(s"fetchedJump: ${pl.fetchedJump.toBoolean} jumping ${pl.jumping.toBoolean} jumped: ${pl.jumped.toBoolean}")
        pw.println(s"irLine:    ${pl.irLine.map(_.toLong.toHexString)}")
        pw.println(s"regReads:  ${pl.regReads.map(_.toLong.toBinaryString)}")
        pw.println(s"regWrites: ${pl.regWrites.map(_.toLong.toBinaryString)}")
        // println(s"readScan:  ${pl.readScan.map(_.toLong.toBinaryString)}")
        // println(s"writeScan: ${pl.writeScan.map(_.toLong.toBinaryString)}")
        pw.println(s"killCurr:  ${pl.killCur.toLong.toBinaryString}")
        pw.println(f"kills: ${pl.kill.toLong.toBinaryString}")
        for (c <- pl.pipes) {
          pw.println(f"(${c.id}) ${if (c.active.toBoolean) "✓" else "✗"} ${if ((pl.kill.toLong >> c.id & 1) == 0) "✓" else "✗"} ${c.stage.toEnum} pc: ${c.pc.toLong}%08x ir: ${c.ir.toLong}%08x")
        }
        if (pl.pipes(0).stage.toEnum == Stage.S1) {
          // println(f"kill ${pl.kill.toInt.toBinaryString} active: ${pl.active.toInt.toBinaryString} (a & ~k): ${(pl.kill.toLong & ~pl.active.toLong).toBinaryString} count: ${java.lang.Long.bitCount(pl.kill.toLong & ~pl.active.toLong)}")
          instsRet += java.lang.Long.bitCount(~pl.kill.toLong & pl.active.toLong)
        }
        pw.println(f"InstsRet after: ${instsRet}")
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
