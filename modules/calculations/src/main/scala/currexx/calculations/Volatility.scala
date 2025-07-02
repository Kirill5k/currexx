package currexx.calculations

import scala.collection.mutable.{ListBuffer, Queue as MQueue}

object Volatility {

  /** Calculates the Average True Range (ATR).
    *
    * It takes lists sorted from latest to earliest and returns a result in the same order.
    *
    * @param closings
    *   List of closing prices, from latest to earliest.
    * @param highs
    *   List of high prices, from latest to earliest.
    * @param lows
    *   List of low prices, from latest to earliest.
    * @param length
    *   The smoothing period for the ATR.
    * @return
    *   A list of ATR values, sorted from latest to earliest, same size as input.
    */
  def averageTrueRange(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] =
    // ATR requires at least one price change, so 2 data points.
    if (closings.size < 2) List.fill(closings.size)(0.0)
    else {

      val resultBuffer = new ListBuffer[Double]
      val trQueue      = MQueue.empty[Double]
      var prevAtr      = 0.0

      val data = closings.lazyZip(highs).lazyZip(lows).toList
      val it   = data.reverseIterator

      val (firstClose, firstHigh, firstLow) = it.next()

      val firstTr   = firstHigh - firstLow
      var prevClose = firstClose
      trQueue.enqueue(firstTr)
      resultBuffer += 0.0 // Pad the result for the first point.

      while (it.hasNext) {
        val (close, high, low) = it.next()

        // Calculate the current True Range using the *previous* close.
        val tr = math.max(high - low, math.max(math.abs(high - prevClose), math.abs(low - prevClose)))
        trQueue.enqueue(tr)

        if (trQueue.size > length) {
          val _ = trQueue.dequeue()
        }

        var currentAtr = 0.0 // Default to 0 during warm-up period
        if (trQueue.size == length) {
          currentAtr = if (prevAtr == 0.0) {
            // First ATR calculation: a Simple Moving Average of the TRs.
            trQueue.sum / length
          } else {
            // Subsequent ATRs: use Wilder's Smoothing.
            (prevAtr * (length - 1) + tr) / length
          }
          prevAtr = currentAtr
        }

        resultBuffer += currentAtr

        // Update the previous close for the next iteration.
        prevClose = close
      }

      // --- 3. Reverse Final Output ---
      resultBuffer.toList.reverse
    }
}
