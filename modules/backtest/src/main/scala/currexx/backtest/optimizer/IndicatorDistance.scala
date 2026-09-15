package currexx.backtest.optimizer

import cats.syntax.traverse.*
import currexx.algorithms.operators.species.Distance
import currexx.backtest.optimizer.bounds.GeneBounds.{DoubleRange, IntRange}
import currexx.backtest.optimizer.bounds.{GeneBounds, ThresholdBounds}
import currexx.domain.signal.{Indicator, ValueTransformation as VT}

/** Distance in a round's searchable parameter space. Fixed leaves never contribute a coordinate, including automatically frozen trackers.
  * Bounds define coordinate scales, not repairs: finite values outside a search range retain their actual distance from that range.
  */
object IndicatorDistance {
  def make(space: IndicatorSearchSpace): Distance[Indicator] = new Distance[Indicator] {
    private def project(indicator: Indicator): Either[IllegalArgumentException, Option[Indicator]] =
      space.project(indicator).left.map {
        case error: IllegalArgumentException => error
        case error                           => new IllegalArgumentException(error.getMessage, error)
      }

    override def between(a: Indicator, b: Indicator): Either[IllegalArgumentException, Double] =
      for
        first  <- project(a)
        second <- project(b)
        left   <- coordinates(first).sequence
        right  <- coordinates(second).sequence
        _ <- Either.cond(left.size == right.size, (), new IllegalArgumentException("Indicator distance needs matching numeric schemas"))
        distance =
          if (left.isEmpty) 0.0
          else left.zip(right).foldLeft(0.0) { case (norm, (x, y)) => math.hypot(norm, x - y) } / math.sqrt(left.size.toDouble)
        result <- Either.cond(distance.isFinite, distance, new IllegalArgumentException("Indicator distance must be finite"))
      yield result
  }

  private def normalise(value: Double, min: Double, max: Double): Either[IllegalArgumentException, Double] =
    if (!value.isFinite) Left(new IllegalArgumentException("Indicator distance needs finite gene values"))
    else {
      val coordinate = (value - min) / (max - min)
      Either.cond(coordinate.isFinite, coordinate, new IllegalArgumentException("Indicator distance needs finite normalised coordinates"))
    }

  private def normalise(value: Int, range: IntRange): Either[IllegalArgumentException, Double] =
    if (range.isLogarithmic)
      if (value <= 0) Left(new IllegalArgumentException("Indicator distance needs positive logarithmic gene values"))
      else normalise(math.log(value.toDouble), math.log(range.min.toDouble), math.log(range.max.toDouble))
    else normalise(value.toDouble, range.min.toDouble, range.max.toDouble)

  private def normalise(value: Double, range: DoubleRange): Either[IllegalArgumentException, Double] =
    normalise(value, range.min, range.max)

  private def coordinates(projected: Option[Indicator]): Vector[Either[IllegalArgumentException, Double]] = {
    def transformation(vt: VT): Vector[Either[IllegalArgumentException, Double]] = vt match
      case VT.Sequenced(sequence)       => sequence.toVector.flatMap(transformation)
      case VT.StandardDeviation(length) => Vector(normalise(length, GeneBounds.standardDeviation))
      case VT.Kalman(gain, noise)       =>
        Vector(normalise(gain, GeneBounds.kalmanGain), normalise(noise, GeneBounds.kalmanNoise))
      case VT.KalmanVelocity(gain, noise) =>
        Vector(normalise(gain, GeneBounds.kalmanGain), normalise(noise, GeneBounds.kalmanNoise))
      case VT.ATR(length)                          => Vector(normalise(length, GeneBounds.oscillatorLength))
      case VT.RSX(length)                          => Vector(normalise(length, GeneBounds.oscillatorLength))
      case VT.JRSX(length)                         => Vector(normalise(length, GeneBounds.oscillatorLength))
      case VT.WMA(length)                          => Vector(normalise(length, GeneBounds.maLength))
      case VT.SMA(length)                          => Vector(normalise(length, GeneBounds.maLength))
      case VT.EMA(length)                          => Vector(normalise(length, GeneBounds.maLength))
      case VT.HMA(length)                          => Vector(normalise(length, GeneBounds.maLength))
      case VT.NMA(length, signalLength, lambda, _) =>
        Vector(
          normalise(length, GeneBounds.nmaLength),
          normalise(signalLength, GeneBounds.nmaSignalLength),
          normalise(lambda, GeneBounds.nmaLambda)
        )
      case VT.JMA(length, phase, power) =>
        Vector(normalise(length, GeneBounds.jmaLength), normalise(phase, GeneBounds.jmaPhase), normalise(power, GeneBounds.jmaPower))
      case VT.STOCH(length)                  => Vector(normalise(length, GeneBounds.oscillatorLength))
      case VT.ADX(length)                    => Vector(normalise(length, GeneBounds.adxLength))
      case VT.WilliamsR(length)              => Vector(normalise(length, GeneBounds.oscillatorLength))
      case VT.CCI(length)                    => Vector(normalise(length, GeneBounds.cciLength))
      case VT.IchimokuKijunSen(length)       => Vector(normalise(length, GeneBounds.ichimokuLength))
      case VT.ParabolicSAR(start, max, step) =>
        Vector(normalise(start, GeneBounds.sarAfStart), normalise(max, GeneBounds.sarAfMax), normalise(step, GeneBounds.sarAfStep))
      case VT.CMF(length) => Vector(normalise(length, GeneBounds.cmfLength))

    def indicator(current: Indicator): Vector[Either[IllegalArgumentException, Double]] = current match
      case Indicator.Composite(children, _)                 => children.toList.toVector.flatMap(indicator)
      case Indicator.TrendChangeDetection(_, vt)            => transformation(vt)
      case Indicator.ThresholdCrossing(_, vt, upper, lower) =>
        val band = ThresholdBounds.of(vt)
        transformation(vt) ++ Vector(normalise(upper, band.upperMin, band.upperMax), normalise(lower, band.lowerMin, band.lowerMax))
      case Indicator.LinesCrossing(_, first, second) =>
        transformation(first) ++ transformation(second)
      case Indicator.KeltnerChannel(_, middle, length, multiplier) =>
        transformation(middle) ++ Vector(normalise(length, GeneBounds.atrLength), normalise(multiplier, GeneBounds.keltnerMultiplier))
      case Indicator.VolatilityRegimeDetection(length, smoothing) =>
        Vector(normalise(length, GeneBounds.atrLength)) ++ transformation(smoothing)
      case Indicator.ValueTracking(_, _, vt)                       => transformation(vt)
      case Indicator.PriceLineCrossing(_, _, vt)                   => transformation(vt)
      case Indicator.BollingerBands(_, middle, length, multiplier) =>
        transformation(middle) ++ Vector(normalise(length, GeneBounds.stdDevLength), normalise(multiplier, GeneBounds.bollingerMultiplier))

    projected.toVector.flatMap(indicator)
  }
}
