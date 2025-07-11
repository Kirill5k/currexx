package currexx.backtest.optimizer

import cats.effect.Sync
import cats.syntax.traverse.*
import cats.syntax.apply.*
import cats.syntax.applicativeError.*
import currexx.algorithms.operators.Crossover
import currexx.domain.signal.{Indicator, ValueTransformation as VT}
import currexx.backtest.syntax.*

import scala.util.Random

object IndicatorCrossover:
  def make[F[_]](using F: Sync[F]): F[Crossover[F, Indicator]] = F.pure {
    new Crossover[F, Indicator] {
      private val threeWaySplitCrossover = Crossover.pureThreeWaySplit[Int]

      override def cross(par1: Indicator, par2: Indicator)(using r: Random): F[Indicator] = {

        def crossDouble(d1: Double, d2: Double, stepSize: Double): Double = {
          val i1     = d1 * 100
          val i2     = d2 * 100
          val result = crossInt(i1.toInt, i2.toInt).toDouble / 100
          result - (result % stepSize)
        }

        def crossInt(i1: Int, i2: Int, minValue: Option[Int] = None): Int = {
          val max    = math.max(i1, i2)
          val result = threeWaySplitCrossover.cross(i1.toBinaryArray(max), i2.toBinaryArray(max)).toInt
          minValue.fold(result)(math.max(_, result))
        }

        def crossVt(so1: VT, so2: VT): Either[Throwable, VT] = (so1, so2) match
          case (VT.RSX(l1), VT.RSX(l2))                       => Right(VT.RSX(crossInt(l1, l2, Some(5))))
          case (VT.STOCH(l1), VT.STOCH(l2))                   => Right(VT.STOCH(crossInt(l1, l2, Some(5))))
          case (VT.HMA(l1), VT.HMA(l2))                       => Right(VT.HMA(crossInt(l1, l2, Some(5))))
          case (VT.SMA(l1), VT.SMA(l2))                       => Right(VT.SMA(crossInt(l1, l2, Some(5))))
          case (VT.WMA(l1), VT.WMA(l2))                       => Right(VT.WMA(crossInt(l1, l2, Some(5))))
          case (VT.EMA(l1), VT.EMA(l2))                       => Right(VT.EMA(crossInt(l1, l2, Some(5))))
          case (VT.Kalman(g1), VT.Kalman(g2))                 => Right(VT.Kalman(crossDouble(g1, g2, 0.05)))
          case (VT.JMA(l1, ph1, pow1), VT.JMA(l2, ph2, pow2)) =>
            Right(VT.JMA(crossInt(l1, l2, Some(5)), crossInt((ph1 + 100) / 5, (ph2 + 100) / 5) * 5 - 100, r.pickOne(pow1, pow2)))
          case (VT.NMA(l1, sl1, d1, ma1), VT.NMA(l2, sl2, d2, _)) =>
            Right(VT.NMA(crossInt(l1, l2), crossInt(sl1, sl2), crossDouble(d1, d2, 0.5), ma1))
          case (VT.Sequenced(s1), VT.Sequenced(s2)) => s1.zip(s2).traverse((v1, v2) => crossVt(v1, v2)).map(VT.Sequenced(_))
          case _                                    => Left(new IllegalArgumentException("both parents must be of the same type"))

        def crossInd(ind1: Indicator, ind2: Indicator): Either[Throwable, Indicator] = (ind1, ind2) match
          case (Indicator.VolatilityRegimeDetection(atr1, vt1, sl1), Indicator.VolatilityRegimeDetection(atr2, vt2, sl2)) =>
            crossVt(vt1, vt2).map(vt => Indicator.VolatilityRegimeDetection(crossInt(atr1, atr2), vt, crossInt(sl1, sl2)))
          case (Indicator.ValueTracking(vr1, vs1, vt1), Indicator.ValueTracking(vr2, vs2, vt2)) =>
            if (vr1 == vr2 && vs1 == vs2) {
              crossVt(vt1, vt2).map(vt => Indicator.ValueTracking(vr1, vs1, vt))
            } else {
              Left(new IllegalArgumentException("both ValueTracking indicators must have the same value range and value source"))
            }
          case (Indicator.Composite(is1), Indicator.Composite(is2)) =>
            is1.zip(is2).traverse((i1, i2) => crossInd(i1, i2)).map(Indicator.Composite(_))
          case (Indicator.LinesCrossing(s, st1, ft1), Indicator.LinesCrossing(_, st2, ft2)) =>
            (crossVt(st1, st2), crossVt(ft1, ft2)).mapN((st, ft) => Indicator.LinesCrossing(s, st, ft))
          case (Indicator.TrendChangeDetection(s, t1), Indicator.TrendChangeDetection(_, t2)) =>
            crossVt(t1, t2).map(t => Indicator.TrendChangeDetection(s, t))
          case (Indicator.ThresholdCrossing(s, t1, ub1, lb1), Indicator.ThresholdCrossing(_, t2, ub2, lb2)) =>
            crossVt(t1, t2)
              .map { t =>
                val ub = math.min(crossInt(ub1.toInt, ub2.toInt), 100)
                val lb = math.min(crossInt(lb1.toInt, lb2.toInt), 100)
                Indicator.ThresholdCrossing(s, t, ub, lb)
              }
          case (Indicator.KeltnerChannel(vs, st1, ft1, al, ar), Indicator.KeltnerChannel(_, st2, ft2, _, _)) =>
            (crossVt(st1, st2), crossVt(ft1, ft2)).mapN((st, ft) => Indicator.KeltnerChannel(vs, st, ft, al, ar))
          case _ =>
            Left(new IllegalArgumentException("both parents must be of the same type"))

        F.fromEither(crossInd(par1, par2))
          .handleErrorWith { e =>
            F.raiseError(new IllegalArgumentException(s"failed to cross $par1 and $par2 together: ${e.getMessage}"))
          }
      }

      override def cross(par1: Indicator, par2: Indicator, crossoverProbability: Double)(using r: Random): F[Indicator] =
        maybeCrossSync(par1, par2, crossoverProbability)
    }
  }
