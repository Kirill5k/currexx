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
      override def cross(par1: Indicator, par2: Indicator)(using r: Random): F[Indicator] = {

        def crossDouble(d1: Double, d2: Double, stepSize: Double): Double = {
          // Use a more direct approach for double crossover
          // Instead of converting to int, use weighted random selection or interpolation
          val alpha = r.nextDouble() // Random weight between 0 and 1
          val interpolated = d1 * alpha + d2 * (1.0 - alpha)

          // Properly snap to step size grid by rounding to nearest step
          val steps = math.round(interpolated / stepSize)
          val result = steps * stepSize

          // Ensure the result is within the bounds of the original values
          val minVal = math.min(d1, d2)
          val maxVal = math.max(d1, d2)
          math.max(minVal, math.min(maxVal, result))
        }

        def crossInt(i1: Int, i2: Int, minValue: Option[Int] = None): Int = {
          // Proper genetic crossover for integers
          // Use uniform crossover or arithmetic crossover with bounds
          val result = r.nextInt(4) match {
            case 0 => i1 // Take from parent 1 (25%)
            case 1 => i2 // Take from parent 2 (25%)
            case 2 => // Arithmetic crossover with random weight (25%)
              val alpha = r.nextDouble()
              math.round(i1 * alpha + i2 * (1.0 - alpha)).toInt
            case 3 => // Random value within the range of both parents (25%)
              val minVal = math.min(i1, i2)
              val maxVal = math.max(i1, i2)
              if (minVal == maxVal) minVal else minVal + r.nextInt(maxVal - minVal + 1)
          }
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
            // Improve JMA phase handling - phase should be in [-100, 100] range
            val crossedPhase = crossInt(ph1, ph2, Some(-100))
            val clampedPhase = math.max(-100, math.min(100, crossedPhase))
            Right(VT.JMA(crossInt(l1, l2, Some(5)), clampedPhase, r.pickOne(pow1, pow2)))
          case (VT.NMA(l1, sl1, d1, ma1), VT.NMA(l2, sl2, d2, _)) =>
            Right(VT.NMA(crossInt(l1, l2), crossInt(sl1, sl2), crossDouble(d1, d2, 0.5), ma1))
          case (VT.Sequenced(s1), VT.Sequenced(s2)) => 
            // Ensure sequences have same length before crossing
            if (s1.length != s2.length) {
              Left(new IllegalArgumentException(s"Sequenced transformations must have same length: ${s1.length} vs ${s2.length}"))
            } else {
              s1.zip(s2).traverse((v1, v2) => crossVt(v1, v2)).map(VT.Sequenced(_))
            }
          case _                                    => Left(new IllegalArgumentException("both parents must be of the same type"))

        def crossInd(ind1: Indicator, ind2: Indicator): Either[Throwable, Indicator] = (ind1, ind2) match
          case (Indicator.VolatilityRegimeDetection(atr1, vt1, sl1), Indicator.VolatilityRegimeDetection(atr2, vt2, sl2)) =>
            crossVt(vt1, vt2).map(vt => Indicator.VolatilityRegimeDetection(crossInt(atr1, atr2, Some(1)), vt, crossInt(sl1, sl2, Some(1))))
          case (Indicator.ValueTracking(vr1, vs1, vt1), Indicator.ValueTracking(vr2, vs2, vt2)) =>
            if (vr1 == vr2 && vs1 == vs2) {
              crossVt(vt1, vt2).map(vt => Indicator.ValueTracking(vr1, vs1, vt))
            } else {
              Left(new IllegalArgumentException("both ValueTracking indicators must have the same value range and value source"))
            }
          case (Indicator.Composite(is1), Indicator.Composite(is2)) =>
            // Ensure composite indicators have same length before crossing
            if (is1.length != is2.length) {
              Left(new IllegalArgumentException(s"Composite indicators must have same length: ${is1.length} vs ${is2.length}"))
            } else {
              is1.zip(is2).traverse((i1, i2) => crossInd(i1, i2)).map(Indicator.Composite(_))
            }
          case (Indicator.LinesCrossing(s, st1, ft1), Indicator.LinesCrossing(_, st2, ft2)) =>
            (crossVt(st1, st2), crossVt(ft1, ft2)).mapN((st, ft) => Indicator.LinesCrossing(s, st, ft))
          case (Indicator.TrendChangeDetection(s, t1), Indicator.TrendChangeDetection(_, t2)) =>
            crossVt(t1, t2).map(t => Indicator.TrendChangeDetection(s, t))
          case (Indicator.ThresholdCrossing(s, t1, ub1, lb1), Indicator.ThresholdCrossing(_, t2, ub2, lb2)) =>
            crossVt(t1, t2)
              .map { t =>
                // Ensure proper threshold bounds: lb <= ub and both in [0, 100]
                val crossedUb = math.max(0, math.min(100, crossInt(ub1.toInt, ub2.toInt)))
                val crossedLb = math.max(0, math.min(100, crossInt(lb1.toInt, lb2.toInt)))
                val (finalLb, finalUb) = if (crossedLb > crossedUb) (crossedUb, crossedLb) else (crossedLb, crossedUb)
                Indicator.ThresholdCrossing(s, t, finalUb, finalLb)
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
