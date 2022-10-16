package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Initialiser
import currexx.domain.market.{Indicator, MovingAverage, ValueTransformation as VT}

import scala.util.Random

object IndicatorInitialiser:
  def make[F[_]](using F: Sync[F], rand: Random): F[Initialiser[F, Indicator]] =
    Initialiser.simple[F, Indicator] { ind =>

      def randomiseSo(transformation: VT.SingleOutput): VT.SingleOutput = transformation match
        case VT.SingleOutput.Sequenced(sequence) => VT.SingleOutput.Sequenced(sequence.map(randomiseSo))
        case VT.SingleOutput.Kalman(_)           => VT.SingleOutput.Kalman(rand.nextInt(20) * 0.05d)
        case VT.SingleOutput.WMA(_)              => VT.SingleOutput.WMA(rand.nextInt(41) + 2)
        case VT.SingleOutput.SMA(_)              => VT.SingleOutput.SMA(rand.nextInt(41) + 2)
        case VT.SingleOutput.EMA(_)              => VT.SingleOutput.EMA(rand.nextInt(41) + 2)
        case VT.SingleOutput.HMA(_)              => VT.SingleOutput.HMA(rand.nextInt(41) + 2)
        case VT.SingleOutput.JMA(_, _, _) => VT.SingleOutput.JMA(rand.nextInt(41) + 2, rand.nextInt(40) * 50 - 100, rand.nextInt(6) + 1)
        case VT.SingleOutput.NMA(_, _, _, _) =>
          VT.SingleOutput.NMA(rand.nextInt(48) + 2, rand.nextInt(28) + 2, rand.nextInt(80) * 0.25d, MovingAverage.Weighted)

      def randomiseDo(transformation: VT.DoubleOutput): VT.DoubleOutput = transformation match
        case VT.DoubleOutput.STOCH(_, _, _) => VT.DoubleOutput.STOCH(rand.nextInt(60) + 3, rand.nextInt(7) + 1, rand.nextInt(7) + 1)

      def randomise(transformation: VT): VT = transformation match
        case vtso: VT.SingleOutput => randomiseSo(vtso)
        case vtdo: VT.DoubleOutput => randomiseDo(vtdo)

      F.delay {
        ind match
          case Indicator.TrendChangeDetection(source, transformation) =>
            Indicator.TrendChangeDetection(source, randomiseSo(transformation))
          case Indicator.ThresholdCrossing(source, transformation, _, _) =>
            Indicator.ThresholdCrossing(
              source,
              randomise(transformation),
              rand.nextInt(49) + 50,
              rand.nextInt(49) + 1
            )
          case Indicator.LinesCrossing(source, slow, fast) => Indicator.LinesCrossing(source, randomiseSo(slow), randomiseSo(fast))
      }
    }
