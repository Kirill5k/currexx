package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.Queue

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
    calc(allValues.tail, allValues.head, 1, Zero, Zero).take(rsis.length - length).toList
  }

  def stoch(
      closings: List[BigDecimal],
      highs: List[BigDecimal],
      lows: List[BigDecimal],
      length: Int,
      slowKLength: Int,
      slowDLength: Int
  ): (List[BigDecimal], List[BigDecimal]) = {
    val highsArr    = highs.reverse.toArray
    val lowsArr     = lows.reverse.toArray
    val closingsArr = closings.reverse.toArray
    val stochs      = Array.ofDim[BigDecimal](closings.size)
    val lc: Queue[BigDecimal] = Queue.empty
    val hh: Queue[BigDecimal] = Queue.empty
    val ll: Queue[BigDecimal] = Queue.empty
    var i = 0
    while (i < closings.length) {
      lc.enqueue(closingsArr(i))
      hh.enqueue(highsArr(i))
      ll.enqueue(lowsArr(i))
      if (i >= length) {
        stochs(i) = 100 * ((closingsArr(i) - lc.min) / (hh.max - ll.min))
        lc.dequeue()
        hh.dequeue()
        ll.dequeue()
      }
      i += 1
    }
    val k = MovingAverageCalculator.sma(stochs.take(closings.length-length).reverse.toList, slowKLength)
    val d = MovingAverageCalculator.sma(k, slowDLength)
    (k, d)
  }
}
