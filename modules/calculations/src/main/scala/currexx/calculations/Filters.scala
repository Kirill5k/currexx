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
  def kalman(values: List[Double], gain: Double, measurementNoise: Double = 1.0): List[Double] = {
    val filteredStates = kalmanFilter1D(
      measurements = values,
      processNoise = gain,
      measurementNoise = measurementNoise
    )

    filteredStates.map(_.x(0))
  }

  /**
   * NEW METHOD
   * Provides a simplified interface to the full 1D Kalman filter for extracting the calculated velocity.
   *
   * In a financial context, the velocity can be interpreted as the momentum or rate-of-change of the price.
   *
   * @param values The list of prices/measurements, sorted from latest to earliest.
   * @param gain   A value controlling the filter's responsiveness, mapped to process noise.
   * @return A list of calculated velocities, sorted from latest to earliest.
   */
  def kalmanVelocity(values: List[Double], gain: Double, measurementNoise: Double = 1.0): List[Double] = {
    val filteredStates = kalmanFilter1D(
      measurements = values,
      processNoise = gain,
      measurementNoise = measurementNoise
    )
    // Extract the velocity component, which is at index 1 of the state vector.
    filteredStates.map(_.x(1))
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
