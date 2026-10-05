package currexx.calculations

object Statistics {

  /** Calculates the rolling standard deviation for a given list of values.
    *
    * @param values
    *   A list of values, sorted from latest to earliest.
    * @param n
    *   The rolling window period.
    * @return
    *   A list of standard deviation values, sorted from latest to earliest.
    */
  def standardDeviation(values: List[Double], n: Int): List[Double] = standardDeviation(values.toArray, n).toList

  /** Returns newest-first sample standard deviations without mutating the input. The period must be positive. */
  def standardDeviation(values: Array[Double], n: Int): Array[Double] = {
    require(n > 0, "Standard-deviation period must be positive")
    val result = new Array[Double](values.length)
    if (n > 1 && n <= values.length) {
      var i = values.length - n
      while (i >= 0) {
        val oldest = i + n - 1
        var sum    = values(oldest)
        var j      = oldest - 1
        while (j >= i) {
          sum += values(j)
          j -= 1
        }
        val mean           = sum / n
        var sumSquaredDiff = 0.0
        j = oldest
        while (j >= i) {
          val diff = values(j) - mean
          sumSquaredDiff += diff * diff
          j -= 1
        }
        result(i) = math.sqrt(sumSquaredDiff / (n - 1))
        i -= 1
      }
    }
    result
  }
}
