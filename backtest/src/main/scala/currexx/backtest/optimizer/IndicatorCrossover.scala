package currexx.backtest.optimizer

import cats.effect.Sync
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.applicativeError.*
import currexx.algorithms.operators.Crossover
import currexx.domain.market.{Indicator, ValueTransformation as VS}
import currexx.backtest.optimizer.syntax.*

import scala.util.Random

object IndicatorCrossover {

  def make[F[_]](using F: Sync[F]): F[Crossover[F, Indicator]] = F.pure {
    new Crossover[F, Indicator] {
      val threeWaySplitCrossover = Crossover.pureThreeWaySplit[Int]

      override def cross(par1: Indicator, par2: Indicator)(using r: Random): F[Indicator] = {

        def crossDouble(d1: Double, d2: Double, stepSize: Double): Double = {
          val i1 = d1 * 100
          val i2 = d2 * 100
          val result = crossInt(i1.toInt, i2.toInt).toDouble / 100
          result - (result % stepSize)
        }

        def crossInt(i1: Int, i2: Int): Int = {
          val max = math.max(i1, i2)
          threeWaySplitCrossover.cross(i1.toBinaryArray(max), i2.toBinaryArray(max)).toInt
        }

        def crossSo(so1: VS.SingleOutput, so2: VS.SingleOutput): Either[Throwable, VS.SingleOutput] = (so1, so2) match
          case (VS.SingleOutput.HMA(l1), VS.SingleOutput.HMA(l2))       => Right(VS.SingleOutput.HMA(crossInt(l1, l2)))
          case (VS.SingleOutput.SMA(l1), VS.SingleOutput.SMA(l2))       => Right(VS.SingleOutput.SMA(crossInt(l1, l2)))
          case (VS.SingleOutput.WMA(l1), VS.SingleOutput.WMA(l2))       => Right(VS.SingleOutput.WMA(crossInt(l1, l2)))
          case (VS.SingleOutput.EMA(l1), VS.SingleOutput.EMA(l2))       => Right(VS.SingleOutput.EMA(crossInt(l1, l2)))
          case (VS.SingleOutput.Kalman(g1), VS.SingleOutput.Kalman(g2)) => Right(VS.SingleOutput.Kalman(crossDouble(g1, g2, 0.05)))
          case (VS.SingleOutput.NMA(l1, sl1, d1, ma1), VS.SingleOutput.NMA(l2, sl2, d2, _)) =>
            Right(VS.SingleOutput.NMA(crossInt(l1, l2), crossInt(sl1, sl2), crossDouble(d1, d2, 0.5), ma1))
          case (VS.SingleOutput.Sequenced(s1), VS.SingleOutput.Sequenced(s2)) =>
            s1.zip(s2).traverse(crossSo(_, _)).map(VS.SingleOutput.Sequenced(_))
          case _ => Left(new IllegalArgumentException("both parents must be of the same type"))

        F.defer {
          (par1, par2) match
            case (i1: Indicator.TrendChangeDetection, i2: Indicator.TrendChangeDetection) =>
              F.fromEither(crossSo(i1.transformation, i2.transformation)).map(t => Indicator.TrendChangeDetection(i1.source, t))
            case (i1: Indicator.ThresholdCrossing, i2: Indicator.ThresholdCrossing) =>
              ???
            case _ =>
              F.raiseError(new IllegalArgumentException("both parents must be of the same type"))
        }
      }

      override def cross(par1: Indicator, par2: Indicator, crossoverProbability: Double)(using r: Random): F[Option[Indicator]] =
        maybeCrossSync(par1, par2, crossoverProbability)
    }
  }
}
