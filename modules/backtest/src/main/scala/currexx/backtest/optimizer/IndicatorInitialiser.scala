package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Initialiser
import currexx.domain.market.{Indicator, ValueTransformation as VT}

import scala.util.Random

object IndicatorInitialiser:
  def make[F[_]](using F: Sync[F], rand: Random): F[Initialiser[F, Indicator]] =
    Initialiser.simple[F, Indicator] { ind =>

      def randomiseVt(transformation: VT): VT = transformation match
        case VT.Sequenced(sequence) => VT.Sequenced(sequence.map(randomiseVt))
        case _: VT.Kalman           => VT.Kalman(rand.nextInt(19) * 0.05d + 0.05d)
        case _: VT.STOCH            => VT.STOCH(rand.nextInt(41) + 2)
        case _: VT.RSX              => VT.RSX(rand.nextInt(41) + 2)
        case _: VT.WMA              => VT.WMA(rand.nextInt(41) + 2)
        case _: VT.SMA              => VT.SMA(rand.nextInt(41) + 2)
        case _: VT.EMA              => VT.EMA(rand.nextInt(41) + 2)
        case _: VT.HMA              => VT.HMA(rand.nextInt(41) + 2)
        case _: VT.JMA              => VT.JMA(rand.nextInt(41) + 2, rand.nextInt(40) * 5 - 100, rand.nextInt(2) + 2)
        case nma: VT.NMA            => VT.NMA(rand.nextInt(48) + 2, rand.nextInt(28) + 2, (1 + rand.nextInt(80)) * 0.25d, nma.maCalc)

      F.delay {
        ind match
          case Indicator.TrendChangeDetection(vs, vt) =>
            Indicator.TrendChangeDetection(vs, randomiseVt(vt))
          case Indicator.ThresholdCrossing(vs, vt, _, _) =>
            Indicator.ThresholdCrossing(vs, randomiseVt(vt), rand.nextInt(49) + 50, rand.nextInt(49) + 1)
          case Indicator.LinesCrossing(vs, vt1, vt2) =>
            Indicator.LinesCrossing(vs, randomiseVt(vt1), randomiseVt(vt2))
          case Indicator.KeltnerChannel(vs, vt1, vt2, atrL, atrR) =>
            Indicator.KeltnerChannel(vs, randomiseVt(vt1), randomiseVt(vt2), atrL, atrR)
      }
    }
