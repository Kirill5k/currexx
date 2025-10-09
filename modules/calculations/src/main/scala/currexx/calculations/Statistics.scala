package currexx.calculations

import scala.collection.mutable.{ListBuffer, Queue as MQueue}

object Statistics {
  /**
   * Calculates the rolling standard deviation for a given list of values.
   *
   * @param values A list of values, sorted from latest to earliest.
   * @param n      The rolling window period.
   * @return A list of standard deviation values, sorted from latest to earliest.
   */
  def standardDeviation(values: List[Double], n: Int): List[Double] = {
    val chronologicalValues = values.reverse
    val window = MQueue.empty[Double]
    val resultBuffer = new ListBuffer[Double]

    chronologicalValues.foreach { value =>
      window.enqueue(value)
      if (window.size > n) {
        val _ = window.dequeue()
      }

      var stdDev = 0.0 // Default to 0 during warm-up
      if (window.size == n) {
        val mean = window.sum / n
        // Use sample standard deviation (Bessel's correction: divide by n-1)
        // Calculate variance without creating intermediate collections
        var sumSquaredDiff = 0.0
        window.foreach { x =>
          val diff = x - mean
          sumSquaredDiff += diff * diff
        }
        val variance = if (n > 1) sumSquaredDiff / (n - 1) else 0.0
        stdDev = math.sqrt(variance)
      }
      resultBuffer += stdDev
    }
    resultBuffer.toList.reverse
  }
}
