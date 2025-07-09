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

  extension (array: Array[Int])
    def toInt: Int = {
      var res = 0
      var i   = 0
      while (i < array.length) {
        if (array(i) == 1)
          res = res | (1 << (array.length - i - 1))
        i = i + 1
      }
      res
    }

  extension (num: Int)
    def toBinaryArray(maxValue: Int): Array[Int] = {
      val bitLength = math.max(Integer.SIZE - Integer.numberOfLeadingZeros(maxValue), 1)
      val array = Array.fill(bitLength)(0)
      
      if (num == 0) array
      else {
        var i = 0
        var j = num
        while (i < array.length) {
          if (j % 2 == 1)
            array(array.length - i - 1) = 1
          j = j / 2
          i = i + 1
        }
        array
      }
    }
}
