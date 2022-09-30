package currexx.backtest.optimizer

object syntax {
  extension (s: String)
    def prePadTo(maxLength: Int, char: Char): String =
      if (s.length >= maxLength) s
      else {
        val remainingLength = maxLength - s.length
        char.toString * remainingLength + s
      }

  extension (array: Array[Int]) def toInt: Int = Integer.parseInt(array.mkString(""), 2)

  extension (i: Int)
    def toBinaryArray(maxValue: Int): Array[Int] =
      i.toBinaryString.prePadTo(maxValue.toBinaryString.length, '0').map(_.toString.toInt).toArray
}
