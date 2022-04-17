package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.Queue

object MovingAverageCalculator {

  val EmaSmoothing: BigDecimal = BigDecimal(2.0D)

  def ema(values: List[BigDecimal], n: Int, smoothing: BigDecimal = EmaSmoothing): List[BigDecimal] = {
    val k = smoothing / (1 + n)
    @tailrec
    def calc(remainingValues: List[BigDecimal], emas: Array[BigDecimal], i: Int): List[BigDecimal] =
      if (remainingValues.isEmpty) emas.toList
      else {
        val ema = remainingValues.head * k + emas(i) * (1-k)
        emas.update(i-1, ema)
        calc(remainingValues.tail, emas, i-1)
      }
    val allValues = values.reverse
    calc(allValues.tail, Array.fill(allValues.size)(allValues.head), allValues.size - 1)
  }

  def sma(values: List[BigDecimal], n: Int): List[BigDecimal] = {
    @tailrec
    def calc(smas: List[BigDecimal], queue: Queue[BigDecimal], remaining: List[BigDecimal]): List[BigDecimal] =
      if (remaining.isEmpty) smas
      else if (queue.size < n) calc(smas, queue.addOne(remaining.head), remaining.tail)
      else {
        val updatedQueue = queue.drop(1).addOne(remaining.head)
        val sma = updatedQueue.sum / n
        calc(sma :: smas, updatedQueue, remaining.tail)
      }
    calc(List.empty, Queue.empty, values.reverse)
  }

  def macd(values: List[BigDecimal]): List[BigDecimal] = {
    val fastMa = ema(values, 12)
    val slowMa = ema(values, 26)
    ???
  }
}
