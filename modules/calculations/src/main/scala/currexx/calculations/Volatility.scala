package currexx.calculations

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
  ): List[Double] = averageTrueRange(closings.toArray, highs.toArray, lows.toArray, length).toList

  /** Returns newest-first ATR values up to the shortest input length. The period must be positive; inputs are not mutated. */
  def averageTrueRange(
      closings: Array[Double],
      highs: Array[Double],
      lows: Array[Double],
      length: Int
  ): Array[Double] = {
    require(length > 0, "ATR period must be positive")
    val size   = math.min(closings.length, math.min(highs.length, lows.length))
    val result = new Array[Double](size)
    if (size > 1 && length <= size) {
      val window = new Array[Double](length)
      val last   = size - 1
      window(0) = highs(last) - lows(last)
      var windowStart = 0
      var windowSize  = 1
      var prevClose   = closings(last)
      var prevAtr     = 0.0
      var i           = last - 1

      while (i >= 0) {
        val tr = math.max(highs(i) - lows(i), math.max(math.abs(highs(i) - prevClose), math.abs(lows(i) - prevClose)))
        if (windowSize < length) {
          window(windowSize) = tr
          windowSize += 1
        } else {
          window(windowStart) = tr
          windowStart += 1
          if (windowStart == length) windowStart = 0
        }

        if (windowSize == length) {
          val currentAtr = if (prevAtr == 0.0) {
            // Sum oldest to newest to preserve floating-point rounding.
            var sum   = window(windowStart)
            var index = windowStart
            var count = 1
            while (count < length) {
              index += 1
              if (index == length) index = 0
              sum += window(index)
              count += 1
            }
            sum / length
          } else {
            (prevAtr * (length - 1) + tr) / length
          }
          result(i) = currentAtr
          prevAtr = currentAtr
        }
        prevClose = closings(i)
        i -= 1
      }
    }
    result
  }
}
