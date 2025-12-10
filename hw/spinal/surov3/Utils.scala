package surov3
import spinal.core._

object Utils {
    implicit class UIntOps(val x: Bits) extends AnyVal {
        def fillDownUntilLSO(): Bits = ~x ^ (x.asUInt - 1).asBits
        def clearLSO(): Bits = x & (x.asUInt-1).asBits
    }
}
