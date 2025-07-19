package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Mutator
import currexx.domain.signal.{Indicator, MovingAverage, ValueTransformation as VT}

import scala.util.Random

object IndicatorMutator {
  def make[F[_]](using F: Sync[F]): F[Mutator[F, Indicator]] = F.pure {
    new Mutator[F, Indicator] {

      override def mutate(ind: Indicator, mutationProbability: Double)(using r: Random): F[Indicator] = {

        /** Mutates an integer parameter using a Gaussian distribution. */
        def mutateInt(value: Int, minValue: Int, maxValue: Int): Int =
          if (r.nextDouble() < mutationProbability) {
            val stdDev   = (maxValue - minValue) * 0.1 // 10% of range as standard deviation
            val mutation = (r.nextGaussian() * stdDev).round.toInt
            val result   = value + mutation
            math.max(minValue, math.min(result, maxValue))
          } else {
            value
          }

        /** Mutates a double parameter using a Gaussian distribution, with rounding to a step size. */
        def mutateDouble(value: Double, minValue: Double, maxValue: Double, stepSize: Double): Double =
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

        def mutateVt(vt: VT): VT = vt match {
          case VT.Sequenced(sequence)       => VT.Sequenced(sequence.map(mutateVt))
          case VT.Kalman(gain)              => VT.Kalman(mutateDouble(gain, 0.01, 0.5, 0.01))
          case VT.STOCH(length)             => VT.STOCH(mutateInt(length, 5, 50))
          case VT.RSX(length)               => VT.RSX(mutateInt(length, 5, 50))
          case VT.WMA(length)               => VT.WMA(mutateInt(length, 5, 100))
          case VT.SMA(length)               => VT.SMA(mutateInt(length, 5, 100))
          case VT.EMA(length)               => VT.EMA(mutateInt(length, 5, 100))
          case VT.HMA(length)               => VT.HMA(mutateInt(length, 5, 100))
          case VT.JMA(length, phase, power) =>
            VT.JMA(
              mutateInt(length, 5, 50),
              mutateInt(phase, -100, 100), // JMA phase can be negative
              mutateInt(power, 1, 10)
            )
          case VT.NMA(length, signalLength, lambda, maCalc) =>
            VT.NMA(
              mutateInt(length, 5, 50),
              mutateInt(signalLength, 5, 50),
              mutateDouble(lambda, 0.5, 4.0, 0.25),
              maCalc
            )
        }

        def mutateInd(indicator: Indicator): Indicator = indicator match {
          case Indicator.Composite(is) =>
            Indicator.Composite(is.map(mutateInd))
          case Indicator.TrendChangeDetection(vs, vt) =>
            Indicator.TrendChangeDetection(vs, mutateVt(vt))
          case Indicator.ThresholdCrossing(vs, vt, ub, lb) =>
            val mutatedUb = mutateDouble(ub, 50.0, 95.0, 1.0)
            val mutatedLb = mutateDouble(lb, 5.0, mutatedUb, 1.0)
            Indicator.ThresholdCrossing(vs, mutateVt(vt), mutatedUb, mutatedLb)
          case Indicator.LinesCrossing(vs, vt1, vt2) =>
            Indicator.LinesCrossing(vs, mutateVt(vt1), mutateVt(vt2))
          case Indicator.KeltnerChannel(vs, vt1, vt2, atrL, atrM) =>
            Indicator.KeltnerChannel(
              vs,
              mutateVt(vt1),
              mutateVt(vt2),
              mutateInt(atrL, 5, 50),
              mutateDouble(atrM, 0.5, 5.0, 0.1) // Using a finer step for the multiplier
            )
          case Indicator.VolatilityRegimeDetection(atrL, smoothing, smoothingL) =>
            Indicator.VolatilityRegimeDetection(
              mutateInt(atrL, 5, 50),
              mutateVt(smoothing),
              mutateInt(smoothingL, 5, 50)
            )
          case Indicator.ValueTracking(vr, vs, vt) =>
            Indicator.ValueTracking(vr, vs, mutateVt(vt))
        }

        F.delay(mutateInd(ind))
      }
    }
  }
}
