package surov3
import spinal.core._

object Utils {
    implicit class UIntOps(val x: UInt) extends AnyVal {
    def fillDownToLSO(): UInt = ~x ^ (x - 1)
    }
}
