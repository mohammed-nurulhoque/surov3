package surov3

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

  val maxCycles = 400 // Safety timeout
  val binFiles = if (args.isEmpty) Array("hw/a.bin") else args
  val cfg = SurovConfig()

  Config.sim.withWave.noOptimisation.compile(SimDUT(cfg)).doSim { dut =>
    // --- Load Instructions from program.bin ---
    for (binFilePath <- binFiles) {
      val fis = new FileInputStream(binFilePath)
      val buffer = new Array[Byte](cfg.issueWidth * 4) // Read 4 bytes at a time for 32-bit words
      
      dut.mem.setBigInt(0, 0)
      var wordsLoaded = 0x1000 / (4 * cfg.issueWidth)
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
      assert(!dut.top.core.pipeline.pipes.exists(c => c.stage.toEnum != Stage.S1))

      // 4. Main Simulation Loop
      var cycles = 0
      var instsRet = 0
      while(cycles < maxCycles) {
        dut.clockDomain.waitRisingEdge()
        cycles += 1
        println(f"[cycle ${cycles}%03d Pipes state:")
        println(s"irLine:    ${dut.top.core.pipeline.irLine.map(_.toLong.toHexString)}")
        println(s"regReads:  ${dut.top.core.pipeline.regReads.map(_.toLong.toBinaryString)}")
        println(s"regWrites: ${dut.top.core.pipeline.regWrites.map(_.toLong.toBinaryString)}")
        // println(s"readScan:  ${dut.top.core.pipeline.readScan.map(_.toLong.toBinaryString)}")
        // println(s"writeScan: ${dut.top.core.pipeline.writeScan.map(_.toLong.toBinaryString)}")
        println(s"killCurr:  ${dut.top.core.pipeline.killCur.toLong.toBinaryString}")
        println(f"kills: ${dut.top.core.pipeline.kill.toLong.toBinaryString}")
        for (c <- dut.top.core.pipeline.pipes) {
          println(f"(${c.id}) ${if (c.active.toBoolean) "✓" else "✗"} ${if ((dut.top.core.pipeline.kill.toLong >> c.id & 1) == 0) "✓" else "✗"} ${c.stage.toEnum} pc: ${c.pc.toLong}%08x ir: ${c.ir.toLong}%08x")
        }
        if (dut.top.core.pipeline.pipes(0).stage.toEnum == Stage.S1)
          instsRet += 1
        val trap = dut.trap.toInt
        if (trap != 0 & dut.top.core.pipeline.pipes(0).stage.toEnum == Stage.S1) {
          val c = dut.top.core.pipeline.pipes(Integer.numberOfTrailingZeros(trap))
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
                System.err.println(f"Completed $instsRet instructions in $cycles Cycles")
                System.err.println(f"EXIT STATUS: $exitCode")
                
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
                System.err.println(f"Unhandled Syscall #$a5 at PC: $pc%08x")
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
                System.err.println(f"ERROR: Unhandled CSR read at PC: $pc%08x")
                simFailure() // <-- SIMULATION FAILURE ON UNHANDLED CSR
            }
            dut.top.rf.setBigInt(ir.toLong >> 7 & 0x1F, dataToWrite)
          } 
          else if ((ir & 0x7F) == 0x0F) { // Opcode 0x0F is FENCE
            // Fence Logic placeholder
          } 
          else {
            System.err.println(f"Unrecognized TRAP op $ir%08x at PC: $pc%08x")
            simFailure()
          }
        }
      }

      simFailure(s"Didn't finish after $maxCycles steps")
    }
  }
}
