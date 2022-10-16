package currexx.backtest.optimizer

import scala.annotation.tailrec

object syntax {
  extension (list: List[BigDecimal])
    def median: BigDecimal = {
      val sorted = list.sorted
      val size   = list.size
      if (size % 2 == 1) sorted(size / 2)
      else {
        val partial       = sorted.drop((size - 1) / 2)
        val (left, right) = (partial.head, partial.tail.head)
        (left + right) / 2
      }
    }

  extension (array: Array[Int])
    def toInt: Int = {
      var res = 0
      var i   = 0
      while (i < array.length) {
        if (array(i) == 1)
          res = res + math.pow(2, array.length - i - 1).toInt
        i = i + 1
      }
      res
    }

  extension (num: Int)
    def toBinaryArray(maxValue: Int): Array[Int] = {
      val bitLength = Integer.SIZE - Integer.numberOfLeadingZeros(maxValue)
      val array     = Array.fill(bitLength)(0)
      var i         = 0
      var j         = num
      while (i < array.length) {
        if (j % 2 == 1)
          array(array.length - i - 1) = 1
        j = j / 2
        i = i + 1
      }
      array
    }
}
