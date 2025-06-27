package currexx.calculations

import scala.collection.mutable.Queue as MQueue

object Volatility {

  /** Calculates the Average True Range (ATR).
    *
    * @param closings
    *   List of closing prices, from oldest to earliest.
    * @param highs
    *   List of high prices, from oldest to earliest.
    * @param lows
    *   List of low prices, from oldest to earliest.
    * @param length
    *   The smoothing period for the ATR.
    * @return
    *   A list of ATR values in reverse chronological order (earliest to latest), same size as input.
    */
  def averageTrueRange(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] =
    if (closings.size < 2) {
      List.fill(closings.size)(0.0)
    } else {
      val closeIt = closings.reverseIterator
      val highIt  = highs.reverseIterator
      val lowIt   = lows.reverseIterator

      var result  = List.empty[Double]
      val trQueue = MQueue.empty[Double]
      var prevAtr = 0.0

      var prevClose = closeIt.next()
      val firstHigh = highIt.next()
      val firstLow  = lowIt.next()
      trQueue.enqueue(firstHigh - firstLow)
      result = 0.0 :: result

      while (closeIt.hasNext && highIt.hasNext && lowIt.hasNext) {
        val high  = highIt.next()
        val low   = lowIt.next()
        val close = closeIt.next()

        val tr = math.max(high - low, math.max(math.abs(high - prevClose), math.abs(low - prevClose)))
        trQueue.enqueue(tr)

        if (trQueue.size > length) {
          val _ = trQueue.dequeue()
        }

        var currentAtr = 0.0
        if (trQueue.size == length) {
          currentAtr = if (prevAtr == 0.0) {
            trQueue.sum / length
          } else {
            (prevAtr * (length - 1) + tr) / length
          }
          prevAtr = currentAtr
        }

        result = currentAtr :: result
        prevClose = close
      }

      result
    }
}
