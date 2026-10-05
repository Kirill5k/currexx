package currexx.calculations

object Filters {

  /** Provides a simplified interface to the full 1D Kalman filter.
    *
    * @param values
    *   The list of prices/measurements, sorted from latest to earliest.
    * @param gain
    *   A value controlling the filter's responsiveness, mapped to process noise.
    * @return
    *   A list of smoothed prices, sorted from latest to earliest.
    */
  def kalman(values: List[Double], gain: Double, measurementNoise: Double = 1.0): List[Double] =
    kalman(values.toArray, gain, measurementNoise).toList

  def kalmanVelocity(values: List[Double], gain: Double, measurementNoise: Double = 1.0): List[Double] =
    kalmanVelocity(values.toArray, gain, measurementNoise).toList

  def kalman(values: Array[Double], gain: Double, measurementNoise: Double): Array[Double] =
    runKalman(values, gain, measurementNoise, velocity = false)

  def kalmanVelocity(values: Array[Double], gain: Double, measurementNoise: Double): Array[Double] =
    runKalman(values, gain, measurementNoise, velocity = true)

  private def runKalman(values: Array[Double], processNoise: Double, measurementNoise: Double, velocity: Boolean): Array[Double] =
    if (values.isEmpty) Array.emptyDoubleArray
    else {
      val q00    = processNoise * 0.25
      val q01    = processNoise * 0.5
      val result = new Array[Double](values.length)
      val last   = values.length - 1

      var x0  = values(last)
      var x1  = 0.0
      var p00 = 500.0
      var p01 = 0.0
      var p11 = 500.0

      result(last) = if (velocity) x1 else x0
      var i = last - 1

      while (i >= 0) {
        val z = values(i)

        // Predict (dt = 1.0)
        val x0p  = x0 + x1
        val p00p = p00 + p01 + p01 + p11 + q00
        val p01p = p01 + p11 + q01
        val p11p = p11 + processNoise

        // Update
        val s  = p00p + measurementNoise
        val k0 = p00p / s
        val k1 = p01p / s
        val y  = z - x0p

        x0 = x0p + k0 * y
        x1 = x1 + k1 * y
        p00 = (1.0 - k0) * p00p
        p01 = (1.0 - k0) * p01p
        p11 = p11p - k1 * p01p

        result(i) = if (velocity) x1 else x0
        i -= 1
      }

      result
    }

}
