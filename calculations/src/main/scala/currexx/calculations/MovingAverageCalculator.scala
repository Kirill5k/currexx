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
    values
      .reverse
      .foldLeft((List.empty[BigDecimal], Queue.empty[BigDecimal])) { case ((smas, queue), v) =>
        if (queue.size < n) (smas, queue.addOne(v))
        else {
          val updatedQueue = queue.drop(1).addOne(v)
          val sma = updatedQueue.sum / n
          (sma :: smas, updatedQueue)
        }
      }
      ._1
  }
}
