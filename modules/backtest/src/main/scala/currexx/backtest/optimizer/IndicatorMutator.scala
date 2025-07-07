package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Mutator
import currexx.domain.signal.{Indicator, ValueTransformation as VT}
import currexx.backtest.syntax.*

import scala.util.Random

object IndicatorMutator:
  def make[F[_]](using F: Sync[F]): F[Mutator[F, Indicator]] = F.pure {
    new Mutator[F, Indicator] {
      private val bitFlitMutator = Mutator.pureBitFlip
      override def mutate(ind: Indicator, mutationProbability: Double)(using r: Random): F[Indicator] = {
        def mutateInt(int: Int, maxValue: Int = 100, minValue: Int = 1): Int = {
          val binArray = int.toBinaryArray(maxValue)
          val res      = bitFlitMutator.mutate(binArray, mutationProbability).toInt
          math.max(minValue, math.min(res, maxValue))
        }

        def mutateDouble(dbl: Double, maxValue: Double = 1d, stepSize: Double = 0.05d): Double = {
          val max      = (maxValue / stepSize).toInt
          val int      = (dbl / stepSize).toInt
          val binArray = int.toBinaryArray(max)
          val mutated  = bitFlitMutator.mutate(binArray, mutationProbability)
          mutated.toInt * stepSize
        }

        def mutateVt(vt: VT): VT = vt match
          case VT.Sequenced(sequence) => VT.Sequenced(sequence.map(mutateVt))
          case VT.Kalman(gain)        => VT.Kalman(math.max(mutateDouble(gain, stepSize = 0.025), 0.025d))
          case VT.STOCH(length)       => VT.STOCH(mutateInt(length, 45))
          case VT.RSX(length)         => VT.RSX(mutateInt(length, 45))
          case VT.WMA(length)         => VT.WMA(mutateInt(length, 45))
          case VT.SMA(length)         => VT.SMA(mutateInt(length, 45))
          case VT.EMA(length)         => VT.EMA(mutateInt(length, 45))
          case VT.HMA(length)         => VT.HMA(mutateInt(length, 45, 5))
          case VT.JMA(length, phase, power) =>
            VT.JMA(mutateInt(length, 45, 5), mutateInt((phase + 100) / 5, 40) * 5 - 100, mutateInt(power - 2, 1, 0) + 2)
          case VT.NMA(length, signalLength, lambda, maCalc) =>
            VT.NMA(mutateInt(length, 50), mutateInt(signalLength, 31), math.max(mutateDouble(lambda, 15d, 0.5d), 0.25), maCalc)

        F.delay {
          ind match
            case Indicator.TrendChangeDetection(vs, vt) =>
              Indicator.TrendChangeDetection(vs, mutateVt(vt))
            case Indicator.ThresholdCrossing(vs, vt, ub, lb) =>
              Indicator.ThresholdCrossing(vs, mutateVt(vt), mutateInt(ub.toInt, 100), mutateInt(lb.toInt, 100))
            case Indicator.LinesCrossing(vs, vt1, vt2) =>
              Indicator.LinesCrossing(vs, mutateVt(vt1), mutateVt(vt2))
            case Indicator.KeltnerChannel(vs, vt1, vt2, atrL, atrR) =>
              Indicator.KeltnerChannel(vs, mutateVt(vt1), mutateVt(vt2), atrL, atrR)
        }
      }
    }
  }
