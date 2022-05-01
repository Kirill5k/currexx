package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.Queue

object MovingAverageCalculator {

  private val EmaSmoothing: BigDecimal = BigDecimal(2.0d)
  private val Zero: BigDecimal         = BigDecimal(0)

  private def emaAsArray(values: List[BigDecimal], n: Int, smoothing: BigDecimal = EmaSmoothing): Array[BigDecimal] = {
    val k         = smoothing / (1 + n)
    val allValues = values.reverse
    val emas      = Array.ofDim[BigDecimal](allValues.size)
    emas(allValues.size - 1) = allValues.head
    @tailrec
    def calc(remainingValues: List[BigDecimal], i: Int): Array[BigDecimal] =
      if (remainingValues.isEmpty) emas
      else {
        val ema = remainingValues.head * k + emas(i) * (1 - k)
        emas.update(i - 1, ema)
        calc(remainingValues.tail, i - 1)
      }
    calc(allValues.tail, allValues.size - 1)
  }

  def ema(values: List[BigDecimal], n: Int, smoothing: BigDecimal = EmaSmoothing): List[BigDecimal] =
    emaAsArray(values, n, smoothing).toList

  def sma(values: List[BigDecimal], n: Int): List[BigDecimal] = {
    val smas = Array.ofDim[BigDecimal](values.size)
    @tailrec
    def calc(queue: Queue[BigDecimal], remaining: List[BigDecimal], i: Int): List[BigDecimal] =
      if (remaining.isEmpty) smas.drop(i).toList
      else if (queue.size < n) calc(queue.addOne(remaining.head), remaining.tail, i)
      else {
        val updatedQueue = queue.drop(1).addOne(remaining.head)
        smas(i - 1) = updatedQueue.sum / n
        calc(updatedQueue, remaining.tail, i - 1)
      }
    calc(Queue.empty, values.reverse, values.size)
  }

  def macd(values: List[BigDecimal], fastLength: Int = 12, slowLength: Int = 26): List[BigDecimal] = {
    val fastMa = emaAsArray(values, fastLength)
    val slowMa = emaAsArray(values, slowLength)
    val macd   = Array.ofDim[BigDecimal](values.size)
    var i      = 0
    while (i < values.size) {
      macd(i) = fastMa(i) - slowMa(i)
      i += 1
    }
    macd.toList
  }

  def macdWithSignal(
      values: List[BigDecimal],
      fastLength: Int = 12,
      slowLength: Int = 26,
      signalSmoothing: Int = 9
  ): (List[BigDecimal], List[BigDecimal]) = {
    val macdLine   = macd(values, fastLength, slowLength)
    val signalLine = sma(macdLine, signalSmoothing)
    (macdLine, signalLine)
  }

  def wmaAsArray(values: List[BigDecimal], n: Int): Array[BigDecimal] = {
    val wmas = Array.ofDim[BigDecimal](values.size)
    val divider = (n * (n + 1)) / 2
    @tailrec
    def calc(queue: Queue[BigDecimal], remaining: List[BigDecimal], i: Int): Array[BigDecimal] =
      if (remaining.isEmpty) wmas.drop(i)
      else if (queue.size < n) calc(queue.addOne(remaining.head), remaining.tail, i)
      else {
        val updatedQueue = queue.drop(1).addOne(remaining.head)
        wmas(i - 1) = updatedQueue.zipWithIndex.foldLeft(Zero) { case (sum, (v, i)) => (sum + (n + i + 1 - n) * v) } / divider
        calc(updatedQueue, remaining.tail, i - 1)
      }
    calc(Queue.empty, values.reverse, values.size)
  }

  def wma(values: List[BigDecimal], n: Int): List[BigDecimal] =
    wmaAsArray(values, n).toList
}
