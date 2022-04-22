package currexx.calculations

import scala.annotation.tailrec

object MomentumOscillatorCalculator {

  private val Zero: BigDecimal = BigDecimal(0)

  def rsi(values: List[BigDecimal], length: Int): List[BigDecimal] = {
    val rsis = Array.ofDim[BigDecimal](values.size)
    @tailrec
    def calc(remainingValues: List[BigDecimal], prevValue: BigDecimal, i: Int, gain: BigDecimal, loss: BigDecimal): Array[BigDecimal] =
      if (remainingValues.isEmpty) rsis
      else {
        val diff     = remainingValues.head - prevValue
        val currGain = diff.max(Zero)
        val currLoss = diff.min(Zero).abs
        if (i < length) calc(remainingValues.tail, remainingValues.head, i + 1, gain + currGain, loss + currLoss)
        else {
          val (avgGain, avgLoss) =
            if (i == length) ((gain + currLoss) / length, (loss + currLoss) / length)
            else ((gain * (length - 1) + currGain) / length, (loss * (length - 1) + currLoss) / length)
          val rsi = 100 - (100 / (1 + avgGain / avgLoss))
          rsis(values.length - i - 1) = rsi
          calc(remainingValues.tail, remainingValues.head, i + 1, avgGain, avgLoss)
        }
      }
    val allValues = values.reverse
    calc(allValues.tail, allValues.head, 1, Zero, Zero).toList
  }
}
