package surov3

import spinal.core._
import spinal.core.formal._

// You need SymbiYosys to be installed.
// See https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Formal%20verification/index.html#installing-requirements
object Surov3CoreFormal extends App {
  FormalConfig
    .withBMC(10)
    .doVerify(new Component {
      val dut = FormalDut(Surov3Core(SurovConfig()))

      // Ensure the formal test start with a reset
      assumeInitial(clockDomain.isResetActive)
      // Provide some stimulus

    })
}
