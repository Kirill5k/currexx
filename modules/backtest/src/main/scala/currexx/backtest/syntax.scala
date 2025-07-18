package currexx.backtest

import scala.math.BigDecimal.RoundingMode
import scala.util.Random

object syntax {
  extension (r: Random)
    def pickOne[A](element1: A, element2: A, elements: A*): A =
      r.nextInt(2 + elements.size) match
        case 0 => element1
        case 1 => element2
        case n => elements(n-2)  // Direct access instead of toIndexedSeq

  extension (bd: BigDecimal)
    def roundTo(scale: Int): BigDecimal = bd.setScale(scale, RoundingMode.HALF_UP)

  extension (list: List[BigDecimal])
    def mean: BigDecimal = if (list.isEmpty) BigDecimal(0) else list.sum / list.size
    def median: BigDecimal =
      if (list.isEmpty) BigDecimal(0)
      else if (list.size == 1) list.head
      else {
        val sorted = list.sorted
        val size = list.size
        if (size % 2 == 1) sorted(size / 2)
        else {
          val mid = size / 2
          (sorted(mid - 1) + sorted(mid)) / 2
        }
      }
}
