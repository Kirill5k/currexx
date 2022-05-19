package currexx.calculations

import scala.annotation.tailrec

object Filters {

  def kalman(
      values: List[Double],
      measurementError: Double,           // (0.1)
      noiseVariance: Double,              // q (0.00011)
      initialGuess: Double,               // ^x0,0
      initializationEstimateError: Double // (100)
  ): List[Double] = {
    val measurementUncertainty = math.pow(measurementError, 2)
    @tailrec
    def calc(
        measurements: List[Double],
        currentEstimate: Double,                 // Xn,n
        extrapolatedEstimateUncertainty: Double, // Pn,n
        predictions: List[Double]
    ): List[Double] =
      if (measurements.isEmpty) predictions
      else {
        val measurement             = measurements.head
        val kalmanGain              = extrapolatedEstimateUncertainty / (extrapolatedEstimateUncertainty + measurementUncertainty)
        val currentState            = currentEstimate + kalmanGain * (measurement - currentEstimate)
        val currentStateUncertainty = (1 - kalmanGain) * extrapolatedEstimateUncertainty
        calc(measurements.tail, currentState, currentStateUncertainty + noiseVariance, currentState :: predictions)
      }
    calc(values.reverse, initialGuess, math.pow(initializationEstimateError, 2) + noiseVariance, Nil)
  }

  def ghKalman(
      values: List[Double],
      initialGuess: Double,
      initialVelocity: Double,
      time: Int,
      alpha: Double,
      beta: Double
  ): List[Double] = {
    @tailrec
    def calc(
        measurements: List[Double],
        prevEstimate: Double,
        prevVelocity: Double,
        predictions: List[Double]
    ): List[Double] =
      if (measurements.isEmpty) predictions
      else {
        val currentEstimate = prevEstimate + alpha * (measurements.head - prevEstimate)
        val currentVelocity = prevVelocity + beta * ((measurements.head - prevEstimate) / time)
        val nextEstimate    = currentEstimate + time * currentVelocity
        calc(measurements.tail, nextEstimate, currentVelocity, currentEstimate :: predictions)
      }
    calc(values.reverse, initialGuess + time * initialVelocity, initialVelocity, Nil)
  }

  def ghkKalman(
      values: List[Double],
      initialGuess: Double,
      initialVelocity: Double,
      initialAcceleration: Double,
      time: Int,
      alpha: Double,
      beta: Double,
      gamma: Double
  ): List[Double] = {
    @tailrec
    def calc(
        measurements: List[Double],
        prevEstimate: Double,
        prevVelocity: Double,
        prevAcceleration: Double,
        predictions: List[Double]
    ): List[Double] =
      if (measurements.isEmpty) predictions
      else {
        val currentEstimate     = prevEstimate + alpha * (measurements.head - prevEstimate)
        val currentVelocity     = prevVelocity + beta * ((measurements.head - prevEstimate) / time)
        val currentAcceleration = prevAcceleration + gamma * ((measurements.head - prevEstimate) / (math.pow(time, 2) / 2))
        val nextEstimate        = currentEstimate + time * currentVelocity + currentAcceleration * (math.pow(time, 2) / 2)
        val nextVelocity        = currentVelocity + currentAcceleration * time
        calc(measurements.tail, nextEstimate, nextVelocity, currentAcceleration, currentEstimate :: predictions)
      }
    val initialEstimate = initialGuess + time * initialVelocity + initialAcceleration * math.pow(time, 2) / 2
    calc(values.reverse, initialEstimate, initialVelocity + initialAcceleration * time, initialAcceleration, Nil)
  }
}
