package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MovingAverageCalculator {

  private val EmaSmoothing: BigDecimal = BigDecimal(2.0D)

  def ema(values: List[BigDecimal], n: Int): List[BigDecimal] = {
    val k = EmaSmoothing / (1 + n)
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
}
