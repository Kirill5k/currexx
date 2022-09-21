package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Mutator
import currexx.domain.market.ValueTransformation.{DoubleOutput, SingleOutput}
import currexx.domain.market.{Indicator, ValueTransformation as VT}
import currexx.backtest.optimizer.syntax.*

import scala.util.Random

object IndicatorMutator {

  def make[F[_]](using F: Sync[F]): F[Mutator[F, Indicator]] = F.pure {
    new Mutator[F, Indicator] {
      val bitFlitMutator = Mutator.pureBitFlip
      override def mutate(ind: Indicator, mutationProbability: Double)(using r: Random): F[Indicator] = {
        def mutateInt(int: Int, maxValue: Int = 100, minValue: Int = 1): Int = {
          val res = math.min(bitFlitMutator.mutate(int.toBinaryArray(maxValue), mutationProbability).toInt, maxValue)
          math.max(minValue, res)
        }

        def mutateDouble(dbl: Double, maxValue: Double = 1d, stepSize: Double = 0.05d): Double = {
          val max      = (maxValue / stepSize).toInt
          val int      = (dbl / stepSize).toInt
          val binArray = int.toBinaryArray(max)
          val mutated  = bitFlitMutator.mutate(binArray, mutationProbability)
          mutated.toInt * stepSize
        }

        def mutateVtSo(vt: VT.SingleOutput): VT.SingleOutput = vt match
          case SingleOutput.Sequenced(sequence) => SingleOutput.Sequenced(sequence.map(mutateVtSo))
          case SingleOutput.Kalman(gain)        => SingleOutput.Kalman(mutateDouble(gain, stepSize = 0.025))
          case SingleOutput.WMA(length)         => SingleOutput.WMA(mutateInt(length, 45))
          case SingleOutput.SMA(length)         => SingleOutput.SMA(mutateInt(length, 45))
          case SingleOutput.EMA(length)         => SingleOutput.EMA(mutateInt(length, 45))
          case SingleOutput.HMA(length)         => SingleOutput.HMA(mutateInt(length, 45))
          case SingleOutput.NMA(length, signalLength, lambda, maCalc) =>
            SingleOutput.NMA(mutateInt(length, 50), mutateInt(signalLength, 31), mutateDouble(lambda, 15d, 0.5d), maCalc)

        def mutateVtDo(vt: VT.DoubleOutput): VT.DoubleOutput = vt match
          case DoubleOutput.STOCH(length, slowKLength, slowDLength) =>
            DoubleOutput.STOCH(mutateInt(length, 45), mutateInt(slowKLength, 11), mutateInt(slowDLength, 11))

        def mutateVt(vt: VT): VT = vt match
          case vtso: SingleOutput => mutateVtSo(vtso)
          case vtdo: DoubleOutput => mutateVtDo(vtdo)

        F.delay {
          ind match
            case Indicator.TrendChangeDetection(vs, vt) =>
              Indicator.TrendChangeDetection(vs, mutateVtSo(vt))
            case Indicator.ThresholdCrossing(vs, vt, ub, lb) =>
              Indicator.ThresholdCrossing(
                vs,
                mutateVt(vt),
                mutateInt(ub.toInt - 50, 49) + 50,
                mutateInt(lb.toInt, 49)
              )
        }
      }
    }
  }
}
