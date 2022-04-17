package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.Queue

object MovingAverageCalculator {

  val EmaSmoothing: BigDecimal = BigDecimal(2.0D)

  def ema(values: List[BigDecimal], n: Int, smoothing: BigDecimal = EmaSmoothing): List[BigDecimal] = {
    val k = smoothing / (1 + n)
    @tailrec
    def calc(remainingValues: List[BigDecimal], emas: List[BigDecimal]): List[BigDecimal] =
      if (remainingValues.isEmpty) emas
      else {
        val ema = remainingValues.head * k + emas.head * (1-k)
        calc(remainingValues.tail, ema :: emas)
      }
    val allValues = values.reverse
    calc(allValues.tail, allValues.head :: Nil)
  }

  def sma(values: List[BigDecimal], n: Int): List[BigDecimal] = {
    @tailrec
    def go(smas: List[BigDecimal], queue: Queue[BigDecimal], remaining: List[BigDecimal]): List[BigDecimal] =
      if (remaining.isEmpty) smas
      else if (queue.size < n) go(smas, queue.addOne(remaining.head), remaining.tail)
      else {
        val updatedQueue = queue.drop(1).addOne(remaining.head)
        val sma = updatedQueue.sum / n
        go(sma :: smas, updatedQueue, remaining.tail)
      }
    go(List.empty, Queue.empty, values.reverse)
  }
}
