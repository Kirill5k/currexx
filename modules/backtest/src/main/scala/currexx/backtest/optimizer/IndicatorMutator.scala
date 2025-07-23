package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Mutator
import currexx.domain.signal.{Indicator, ValueTransformation as VT}

import scala.util.Random

object IndicatorMutator {
  def make[F[_]](using F: Sync[F]): F[Mutator[F, Indicator]] = F.pure {
    new Mutator[F, Indicator] {

      override def mutate(ind: Indicator, mutationProbability: Double)(using r: Random): F[Indicator] = {

        /** Mutates an integer parameter using a Gaussian distribution. */
        def mutInt(value: Int, minValue: Int, maxValue: Int): Int =
          if (r.nextDouble() < mutationProbability) {
            val stdDev   = (maxValue - minValue) * 0.1 // 10% of range as standard deviation
            val mutation = (r.nextGaussian() * stdDev).round.toInt
            val result   = value + mutation
            math.max(minValue, math.min(result, maxValue))
          } else {
            value
          }

        /** Mutates a double parameter using a Gaussian distribution, with rounding to a step size. */
        def mutDouble(value: Double, minValue: Double, maxValue: Double, stepSize: Double): Double =
          if (r.nextDouble() < mutationProbability) {
            val stdDev        = (maxValue - minValue) * 0.1 // 10% of range
            val mutation      = r.nextGaussian() * stdDev
            val mutated       = value + mutation
            val roundedToStep = math.round(mutated / stepSize) * stepSize
            val finalValue    = math.max(minValue, math.min(roundedToStep, maxValue))
            math.round(finalValue * 10000.0) / 10000.0
          } else {
            value
          }

        def mutVt(vt: VT): VT = vt match
          case VT.Sequenced(sequence) =>
            VT.Sequenced(sequence.map(mutVt))
          case VT.Kalman(gain, measurementNoise) =>
            VT.Kalman(mutDouble(gain, 0.01, 0.5, 0.01), mutDouble(measurementNoise, 0.01, 1.0, 0.01))
          case VT.KalmanVelocity(gain, measurementNoise) =>
            VT.KalmanVelocity(mutDouble(gain, 0.01, 0.5, 0.01), mutDouble(measurementNoise, 0.01, 1.0, 0.01))
          case VT.STOCH(length) =>
            VT.STOCH(mutInt(length, 5, 50))
          case VT.RSX(length) =>
            VT.RSX(mutInt(length, 5, 50))
          case VT.JRSX(length) =>
            VT.JRSX(mutInt(length, 5, 50))
          case VT.WMA(length) =>
            VT.WMA(mutInt(length, 5, 100))
          case VT.SMA(length) =>
            VT.SMA(mutInt(length, 5, 100))
          case VT.EMA(length) =>
            VT.EMA(mutInt(length, 5, 100))
          case VT.HMA(length) =>
            VT.HMA(mutInt(length, 5, 100))
          case VT.JMA(length, phase, power) =>
            VT.JMA(mutInt(length, 5, 50), mutInt(phase, -100, 100), mutInt(power, 1, 10)) // JMA phase can be negative
          case VT.NMA(length, signalLength, lambda, maCalc) =>
            VT.NMA(mutInt(length, 5, 50), mutInt(signalLength, 5, 50), mutDouble(lambda, 0.5, 4.0, 0.25), maCalc)

        def mutInd(indicator: Indicator): Indicator = indicator match
          case Indicator.Composite(is) =>
            Indicator.Composite(is.map(mutInd))
          case Indicator.TrendChangeDetection(vs, vt) =>
            Indicator.TrendChangeDetection(vs, mutVt(vt))
          case Indicator.ThresholdCrossing(vs, vt, ub, lb) =>
            val mutatedUb = mutDouble(ub, 50.0, 95.0, 1.0)
            val mutatedLb = mutDouble(lb, 5.0, mutatedUb, 1.0)
            Indicator.ThresholdCrossing(vs, mutVt(vt), mutatedUb, mutatedLb)
          case Indicator.LinesCrossing(vs, vt1, vt2) =>
            Indicator.LinesCrossing(vs, mutVt(vt1), mutVt(vt2))
          case Indicator.KeltnerChannel(vs, vt1, vt2, atrL, atrM) =>
            Indicator.KeltnerChannel(vs, mutVt(vt1), mutVt(vt2), mutInt(atrL, 5, 50), mutDouble(atrM, 0.5, 5.0, 0.1))
          case Indicator.VolatilityRegimeDetection(atrL, smoothing, smoothingL) =>
            Indicator.VolatilityRegimeDetection(mutInt(atrL, 5, 50), mutVt(smoothing), mutInt(smoothingL, 5, 50))
          case Indicator.ValueTracking(vr, vs, vt) =>
            Indicator.ValueTracking(vr, vs, mutVt(vt))

        F.delay(mutInd(ind))
      }
    }
  }
}
