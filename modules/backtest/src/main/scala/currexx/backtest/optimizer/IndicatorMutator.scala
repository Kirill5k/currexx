package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Mutator
import currexx.domain.signal.{Indicator, ValueTransformation as VT}

import scala.util.Random

object IndicatorMutator:
  def make[F[_]](using F: Sync[F]): F[Mutator[F, Indicator]] = F.pure {
    new Mutator[F, Indicator] {

      override def mutate(ind: Indicator, mutationProbability: Double)(using r: Random): F[Indicator] = {

        def mutateInt(int: Int, maxValue: Int, minValue: Int): Int =
          if (r.nextDouble() < mutationProbability) {
            // Use gaussian mutation for better locality
            val stdDev = (maxValue - minValue) * 0.1 // 10% of range as standard deviation
            val mutation = (r.nextGaussian() * stdDev).round.toInt
            val result = int + mutation
            math.max(minValue, math.min(result, maxValue))
          } else {
            int
          }

        def mutateDouble(dbl: Double, maxValue: Double, minValue: Double, stepSize: Double): Double =
          if (r.nextDouble() < mutationProbability) {
            // Use gaussian mutation for continuous values
            val stdDev = (maxValue - minValue) * 0.1 // 10% of range
            val mutation = r.nextGaussian() * stdDev
            val mutated = dbl + mutation

            // Clamp to bounds and snap to step size
            val clamped = math.max(minValue, math.min(mutated, maxValue))
            val steps = math.round(clamped / stepSize)
            val result = math.max(minValue, steps * stepSize)
            
            // Round to 4 decimal places for clean precision
            math.round(result * 10000.0) / 10000.0
          } else {
            dbl
          }

        def mutateVt(vt: VT): VT = vt match
          case VT.Sequenced(sequence)       => VT.Sequenced(sequence.map(mutateVt))
          case VT.Kalman(gain)              => VT.Kalman(mutateDouble(gain, 1.0, 0.025, 0.025))
          case VT.STOCH(length)             => VT.STOCH(mutateInt(length, 50, 5))
          case VT.RSX(length)               => VT.RSX(mutateInt(length, 50, 5))
          case VT.WMA(length)               => VT.WMA(mutateInt(length, 50, 5))
          case VT.SMA(length)               => VT.SMA(mutateInt(length, 50, 5))
          case VT.EMA(length)               => VT.EMA(mutateInt(length, 50, 5))
          case VT.HMA(length)               => VT.HMA(mutateInt(length, 50, 5))
          case VT.JMA(length, phase, power) =>
            // Fix JMA phase handling - phase should be in [-100, 100] range
            val mutatedLength = mutateInt(length, 50, 5)
            val mutatedPhase = mutateInt(phase, 100, -100)
            val mutatedPower = mutateInt(power, 10, 1)
            VT.JMA(mutatedLength, mutatedPhase, mutatedPower)
          case VT.NMA(length, signalLength, lambda, maCalc) =>
            VT.NMA(
              mutateInt(length, 50, 5),
              mutateInt(signalLength, 50, 5),
              mutateDouble(lambda, 15.0, 0.25, 0.25),
              maCalc
            )

        def mutateInd(indicator: Indicator): Indicator = indicator match
          case Indicator.Composite(is) =>
            Indicator.Composite(is.map(mutateInd))
          case Indicator.TrendChangeDetection(vs, vt) =>
            Indicator.TrendChangeDetection(vs, mutateVt(vt))
          case Indicator.ThresholdCrossing(vs, vt, ub, lb) =>
            val mutatedVt = mutateVt(vt)
            val mutatedUb = mutateInt(ub.toInt, 100, 0)
            val mutatedLb = mutateInt(lb.toInt, 100, 0)
            // Ensure lb <= ub
            val (finalLb, finalUb) = if (mutatedLb > mutatedUb) (mutatedUb, mutatedLb) else (mutatedLb, mutatedUb)
            Indicator.ThresholdCrossing(vs, mutatedVt, finalUb, finalLb)
          case Indicator.LinesCrossing(vs, vt1, vt2) =>
            Indicator.LinesCrossing(vs, mutateVt(vt1), mutateVt(vt2))
          case Indicator.KeltnerChannel(vs, vt1, vt2, atrL, atrR) =>
            Indicator.KeltnerChannel(vs, mutateVt(vt1), mutateVt(vt2), atrL, atrR)
          case Indicator.VolatilityRegimeDetection(atr, st, sl) =>
            Indicator.VolatilityRegimeDetection(mutateInt(atr, 50, 1), mutateVt(st), mutateInt(sl, 50, 1))
          case Indicator.ValueTracking(vr, vs, vt) =>
            Indicator.ValueTracking(vr, vs, mutateVt(vt))

        F.delay(mutateInd(ind))
      }
    }
  }
