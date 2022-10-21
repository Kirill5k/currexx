package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Initialiser
import currexx.domain.market.{Indicator, MovingAverage, ValueTransformation as VT}

import scala.util.Random

object IndicatorInitialiser:
  def make[F[_]](using F: Sync[F], rand: Random): F[Initialiser[F, Indicator]] =
    Initialiser.simple[F, Indicator] { ind =>

      def randomiseVt(transformation: VT): VT = transformation match
        case VT.Sequenced(sequence) => VT.Sequenced(sequence.map(randomiseVt))
        case _: VT.Kalman           => VT.Kalman(rand.nextInt(20) * 0.05d)
        case _: VT.STOCH            => VT.STOCH(rand.nextInt(41) + 2)
        case _: VT.RSX              => VT.RSX(rand.nextInt(41) + 2)
        case _: VT.WMA              => VT.WMA(rand.nextInt(41) + 2)
        case _: VT.SMA              => VT.SMA(rand.nextInt(41) + 2)
        case _: VT.EMA              => VT.EMA(rand.nextInt(41) + 2)
        case _: VT.HMA              => VT.HMA(rand.nextInt(41) + 2)
        case _: VT.JMA              => VT.JMA(rand.nextInt(41) + 2, rand.nextInt(40) * 5 - 100, rand.nextInt(6) + 1)
        case _: VT.NMA              => VT.NMA(rand.nextInt(48) + 2, rand.nextInt(28) + 2, rand.nextInt(80) * 0.25d, MovingAverage.Weighted)

      F.delay {
        ind match
          case Indicator.TrendChangeDetection(source, transformation) =>
            Indicator.TrendChangeDetection(source, randomiseVt(transformation))
          case Indicator.ThresholdCrossing(source, transformation, _, _) =>
            Indicator.ThresholdCrossing(
              source,
              randomiseVt(transformation),
              rand.nextInt(49) + 50,
              rand.nextInt(49) + 1
            )
          case Indicator.LinesCrossing(source, slow, fast) =>
            Indicator.LinesCrossing(source, randomiseVt(slow), randomiseVt(fast))
      }
    }
