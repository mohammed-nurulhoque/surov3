package surov3

import spinal.core._
import spinal.core.sim._

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    onlyStdLogicVectorAtTopLevelIo = false,
    genLineComments = true,
    keepAll = true
  )

  def sim = SimConfig.withConfig(spinal).withFstWave
}
