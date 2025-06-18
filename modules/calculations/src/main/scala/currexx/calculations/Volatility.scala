package currexx.calculations

import scala.collection.immutable.List
import scala.collection.mutable.{ListBuffer, Queue as MQueue}

object Volatility {

  def averageTrueRange(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] = {
    val highsArr    = highs.toArray
    val lowsArr     = lows.toArray
    val closingsArr = closings.toArray
    val trs         = MQueue.empty[Double]
    val atrs        = ListBuffer.empty[Double]
    var i           = closings.size - 2
    while (i >= 0) {
      val prevClose = closingsArr(i + 1)
      val high      = highsArr(i)
      val low       = lowsArr(i)
      val tr        = math.max(math.max(high - low, math.abs(high - prevClose)), math.abs(low - prevClose))
      trs.addOne(tr)
      if (trs.size == length && atrs.isEmpty) {
        val atr = trs.sum / length
        atrs.addOne(atr)
        val _ = trs.dequeue()
        ()
      }
      if (trs.size == length && atrs.nonEmpty) {
        val atr = (atrs.last * (length - 1) + tr) / length
        atrs.addOne(atr)
        val _ = trs.dequeue()
        ()
      }
      i -= 1
    }
    atrs.reverse.toList
  }
}
