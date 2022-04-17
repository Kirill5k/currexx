package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.Queue

object MovingAverageCalculator {

  val EmaSmoothing: BigDecimal = BigDecimal(2.0d)

  private def emaAsArray(values: List[BigDecimal], n: Int, smoothing: BigDecimal = EmaSmoothing): Array[BigDecimal] = {
    val k = smoothing / (1 + n)
    @tailrec
    def calc(remainingValues: List[BigDecimal], emas: Array[BigDecimal], i: Int): Array[BigDecimal] =
      if (remainingValues.isEmpty) emas
      else {
        val ema = remainingValues.head * k + emas(i) * (1 - k)
        emas.update(i - 1, ema)
        calc(remainingValues.tail, emas, i - 1)
      }
    val allValues = values.reverse
    val emas = Array.ofDim[BigDecimal](allValues.size)
    emas(allValues.size-1) = allValues.head
    calc(allValues.tail, emas, allValues.size - 1)
  }

  def ema(values: List[BigDecimal], n: Int, smoothing: BigDecimal = EmaSmoothing): List[BigDecimal] =
    emaAsArray(values, n, smoothing).toList

  def sma(values: List[BigDecimal], n: Int): List[BigDecimal] = {
    @tailrec
    def calc(smas: Array[BigDecimal], queue: Queue[BigDecimal], remaining: List[BigDecimal], i: Int): List[BigDecimal] =
      if (remaining.isEmpty) smas.drop(i).toList
      else if (queue.size < n) calc(smas, queue.addOne(remaining.head), remaining.tail, i)
      else {
        val updatedQueue = queue.drop(1).addOne(remaining.head)
        smas(i-1) = updatedQueue.sum / n
        calc(smas, updatedQueue, remaining.tail, i - 1)
      }
    val smas = Array.ofDim[BigDecimal](values.size)
    calc(smas, Queue.empty, values.reverse, values.size)
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
  ): (List[BigDecimal], List[BigDecimal]) = ???
}
