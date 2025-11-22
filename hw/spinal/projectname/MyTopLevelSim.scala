package projectname

import scala.io.Source
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import spinal.core._
import spinal.core.sim._

object MyTopLevelSim extends App {
  Config.sim.withWave.compile(SimDUT(SurovConfig())).doSim { dut =>
    // --- Load Instructions from program.bin ---
    val binFilePath = "hw/sim.bin"
    val fis = new FileInputStream(binFilePath)
    val buffer = new Array[Byte](4) // Read 4 bytes at a time for 32-bit words
    
    dut.imem.setBigInt(0, 0)
    var wordsLoaded = 1
    println(s"Loading instructions from $binFilePath...")
    while (fis.read(buffer) != -1) {
      val byteBuffer = ByteBuffer.wrap(buffer)
      byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
      // println(f"${byteBuffer.getInt() & 0xFFFFFFFFL}%08x")
      dut.imem.setBigInt(address = wordsLoaded, data = byteBuffer.getInt() & 0xFFFFFFFFL)
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

    val c = dut.core.pipeline.c   
    // Wait a rising edge on the clock
    dut.clockDomain.waitRisingEdge(4)
    println(f"${c.ir.toInt}%x")
    assert(c.stage.toBigInt == 0)
    for (i <- 0 to 120) {
      dut.clockDomain.waitRisingEdge()
      println(f"($i%3d) pc: ${c.pc.toLong}%08x ir: ${c.ir.toInt}%08x Stage: ${c.stage.toBigInt}")
      println(f"r1: ${dut.c.r1.toLong}%8d, r2: ${dut.c.r2.toLong}%8d")
    }

    for (i <- 0 until 32)
      println(f"x$i%2d: ${dut.rf.getBigInt(i)}%8x")
  }
}
