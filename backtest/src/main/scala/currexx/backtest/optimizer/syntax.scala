package currexx.backtest.optimizer

object syntax {
  extension (array: Array[Int]) def toInt: Int = Integer.parseInt(array.mkString(""), 2)

  extension (i: Int)
    def toBinaryArray(maxValue: Int): Array[Int] =
      i.toBinaryString.reverse.padTo(maxValue.toBinaryString.length, '0').reverse.map(_.toString.toInt).toArray
}
