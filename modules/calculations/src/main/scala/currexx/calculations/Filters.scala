package currexx.calculations

import breeze.linalg.{DenseMatrix, DenseVector, inv}

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
    runKalman(values, gain, measurementNoise)((x0, _) => x0)

  def kalmanVelocity(values: List[Double], gain: Double, measurementNoise: Double = 1.0): List[Double] =
    runKalman(values, gain, measurementNoise)((_, x1) => x1)

  private def runKalman(values: List[Double], processNoise: Double, measurementNoise: Double)(extract: (Double, Double) => Double): List[Double] =
    if (values.isEmpty) Nil
    else {
      val q00 = processNoise * 0.25
      val q01 = processNoise * 0.5

      val it  = values.reverseIterator
      var x0  = it.next()
      var x1  = 0.0
      var p00 = 500.0
      var p01 = 0.0
      var p11 = 500.0

      var result: List[Double] = List(extract(x0, x1))

      while (it.hasNext) {
        val z = it.next()

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

        x0  = x0p + k0 * y
        x1  = x1  + k1 * y
        p00 = (1.0 - k0) * p00p
        p01 = (1.0 - k0) * p01p
        p11 = p11p - k1 * p01p

        result = extract(x0, x1) :: result
      }

      result
    }

  /** Represents the state of the Kalman filter at a single point in time.
    *
    * @param x
    *   The state vector [position; velocity].
    * @param p
    *   The state covariance matrix, representing the uncertainty of the estimate.
    */
  final case class KalmanState(x: DenseVector[Double], p: DenseMatrix[Double])

  /** A 1-D Kalman filter for tracking an object's position and velocity. It takes a list sorted from latest to earliest and returns a
    * result in the same order.
    *
    * @param measurements
    *   List of position measurements, from latest to earliest.
    * @param dt
    *   The time step between measurements (e.g., 1.0 period).
    * @param processNoise
    *   The uncertainty in the model's physics.
    * @param measurementNoise
    *   The uncertainty of the sensor providing the measurements.
    * @return
    *   A list of filtered state estimates, from latest to earliest.
    */
  def kalmanFilter1D(
      measurements: List[Double],
      dt: Double = 1.0,
      processNoise: Double = 1e-4,
      measurementNoise: Double = 0.1
  ): List[KalmanState] =
    if (measurements.isEmpty) Nil
    else {
      val F = DenseMatrix((1.0, dt), (0.0, 1.0))
      val H = DenseMatrix((1.0, 0.0))
      val Q = DenseMatrix((math.pow(dt, 4) / 4, math.pow(dt, 3) / 2), (math.pow(dt, 3) / 2, math.pow(dt, 2))) * processNoise
      val R = DenseMatrix(measurementNoise)
      val I = DenseMatrix.eye[Double](2)

      val it = measurements.reverseIterator

      val firstMeasurement = it.next()
      val initialX         = DenseVector(firstMeasurement, 0.0)
      val initialP         = DenseMatrix.eye[Double](2) * 500.0

      var currentState              = KalmanState(initialX, initialP)
      var result: List[KalmanState] = List(currentState)

      while (it.hasNext) {
        val z = it.next()

        // Predict
        val x_predicted = F * currentState.x
        val p_predicted = F * currentState.p * F.t + Q

        // Update
        val predictedMeasurementVector = H * x_predicted
        val y                          = z - predictedMeasurementVector(0)
        val S                          = H * p_predicted * H.t + R
        val K                          = p_predicted * H.t * inv(S)
        val x_new                      = x_predicted + (K.toDenseVector * y)
        val p_new                      = (I - (K * H)) * p_predicted

        currentState = KalmanState(x_new, p_new)
        result = currentState :: result
      }

      result
    }
}
