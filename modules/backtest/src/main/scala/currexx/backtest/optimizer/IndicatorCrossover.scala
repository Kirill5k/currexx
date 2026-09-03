package currexx.backtest.optimizer

import cats.effect.Sync
import cats.syntax.traverse.*
import cats.syntax.apply.*
import cats.syntax.applicativeError.*
import currexx.algorithms.operators.Crossover
import currexx.domain.signal.{Indicator, ValueTransformation as VT}

import scala.util.Random

object IndicatorCrossover:

  /** One convex weight, shared by every gene of the indicator being crossed.
    *
    * A constraint of the form `min <= b/a <= max` is a convex cone, so a blend of two valid parents at a single weight is valid without the
    * operator knowing the constraint exists. A weight drawn per gene is what breaks that: it takes the ATR from one parent and its
    * smoothing from the other, and the pair it lands on need not satisfy a relationship both parents satisfied. Blending at one weight per
    * indicator costs nothing and makes the whole class of ratio constraints survive crossover for free.
    *
    * A quarter of the time this is a straight copy of one parent's genes, and the two extreme weights return that parent's value untouched
    * rather than computing it. Rounding an interpolation of one parent with itself is not the identity: a CMF threshold of 0.17 lands on a
    * 0.02 grid at 0.18, and where the other parent is above it the clamp to the parental interval does not catch that. Off-grid values are
    * legal - `IndicatorBounds.repair` clamps but does not regrid - so a copy that quietly regrids is a copy that lies.
    */
  final case class Blend(alpha: Double):
    def int(i1: Int, i2: Int): Int =
      if (alpha == 1.0) i1
      else if (alpha == 0.0) i2
      else hold(i1, i2, math.round(i1 * alpha + i2 * (1 - alpha)).toInt)

    def double(d1: Double, d2: Double, step: Double): Double =
      if (alpha == 1.0) d1
      else if (alpha == 0.0) d2
      else
        val interpolated = d1 * alpha + d2 * (1 - alpha)
        val snapped      = math.round(math.round(interpolated / step) * step * 10000.0) / 10000.0
        hold(d1, d2, snapped)

    private def hold[N](n1: N, n2: N, value: N)(using ord: Ordering[N]): N =
      ord.max(ord.min(n1, n2), ord.min(ord.max(n1, n2), value))

  object Blend:
    def draw(using r: Random): Blend = r.nextInt(4) match
      case 0 => Blend(1.0) // every gene from parent 1
      case 1 => Blend(0.0) // every gene from parent 2
      case _ => Blend(r.nextDouble())

  def make[F[_]](using F: Sync[F]): F[Crossover[F, Indicator]] = F.pure {
    new Crossover[F, Indicator] {
      override def cross(par1: Indicator, par2: Indicator)(using r: Random): F[Indicator] = {

        def crossVt(blend: Blend)(so1: VT, so2: VT): Either[Throwable, VT] =
          (so1, so2) match
            case (vt1, vt2) if !sameVtShape(vt1, vt2) =>
              Left(new IllegalArgumentException(s"both value transformations must be of the same shape: $vt1 vs $vt2"))
            case (VT.StandardDeviation(l1), VT.StandardDeviation(l2)) => Right(VT.StandardDeviation(blend.int(l1, l2)))
            case (VT.RSX(l1), VT.RSX(l2))                             => Right(VT.RSX(blend.int(l1, l2)))
            case (VT.JRSX(l1), VT.JRSX(l2))                           => Right(VT.JRSX(blend.int(l1, l2)))
            case (VT.STOCH(l1), VT.STOCH(l2))                         => Right(VT.STOCH(blend.int(l1, l2)))
            case (VT.HMA(l1), VT.HMA(l2))                             => Right(VT.HMA(blend.int(l1, l2)))
            case (VT.SMA(l1), VT.SMA(l2))                             => Right(VT.SMA(blend.int(l1, l2)))
            case (VT.WMA(l1), VT.WMA(l2))                             => Right(VT.WMA(blend.int(l1, l2)))
            case (VT.EMA(l1), VT.EMA(l2))                             => Right(VT.EMA(blend.int(l1, l2)))
            case (VT.WilliamsR(l1), VT.WilliamsR(l2))                 => Right(VT.WilliamsR(blend.int(l1, l2)))
            case (VT.ATR(l1), VT.ATR(l2))                             => Right(VT.ATR(blend.int(l1, l2)))
            case (VT.ADX(l1), VT.ADX(l2))                             => Right(VT.ADX(blend.int(l1, l2)))
            case (VT.CCI(l1), VT.CCI(l2))                             => Right(VT.CCI(blend.int(l1, l2)))
            case (VT.CMF(l1), VT.CMF(l2))                             => Right(VT.CMF(blend.int(l1, l2)))
            case (VT.IchimokuKijunSen(l1), VT.IchimokuKijunSen(l2))   => Right(VT.IchimokuKijunSen(blend.int(l1, l2)))
            case (VT.Kalman(g1, mn1), VT.Kalman(g2, mn2))             =>
              Right(VT.Kalman(blend.double(g1, g2, GeneBounds.kalmanGain.step), blend.double(mn1, mn2, GeneBounds.kalmanNoise.step)))
            case (VT.KalmanVelocity(g1, mn1), VT.KalmanVelocity(g2, mn2)) =>
              Right(
                VT.KalmanVelocity(
                  blend.double(g1, g2, GeneBounds.kalmanGain.step),
                  blend.double(mn1, mn2, GeneBounds.kalmanNoise.step)
                )
              )
            case (VT.JMA(l1, ph1, pow1), VT.JMA(l2, ph2, pow2)) =>
              Right(VT.JMA(blend.int(l1, l2), blend.int(ph1, ph2), blend.int(pow1, pow2)))
            case (VT.NMA(l1, sl1, d1, ma1), VT.NMA(l2, sl2, d2, _)) =>
              Right(VT.NMA(blend.int(l1, l2), blend.int(sl1, sl2), blend.double(d1, d2, GeneBounds.nmaLambda.step), ma1))
            case (VT.ParabolicSAR(start1, max1, step1), VT.ParabolicSAR(start2, max2, step2)) =>
              Right(
                VT.ParabolicSAR(
                  blend.double(start1, start2, GeneBounds.sarAfStart.step),
                  blend.double(max1, max2, GeneBounds.sarAfMax.step),
                  blend.double(step1, step2, GeneBounds.sarAfStep.step)
                )
              )
            case (VT.Sequenced(s1), VT.Sequenced(s2)) =>
              s1.zip(s2).traverse((v1, v2) => crossVt(blend)(v1, v2)).map(VT.Sequenced(_))
            case (vt1, vt2) =>
              Left(new IllegalStateException(s"unhandled value transformation types: $vt1 vs $vt2"))

        def crossInd(ind1: Indicator, ind2: Indicator): Either[Throwable, Indicator] =
          val blend = Blend.draw
          val cross = crossVt(blend)
          (ind1, ind2) match
            case (i1, i2) if !sameShape(i1, i2) =>
              Left(new IllegalArgumentException(s"both parent indicators must be of the same shape: $i1 vs $i2"))
            case (Indicator.VolatilityRegimeDetection(atr1, vt1), Indicator.VolatilityRegimeDetection(atr2, vt2)) =>
              cross(vt1, vt2).map(vt => Indicator.VolatilityRegimeDetection(blend.int(atr1, atr2), vt))
            case (Indicator.ValueTracking(vr, vs, vt1), Indicator.ValueTracking(_, _, vt2)) =>
              cross(vt1, vt2).map(vt => Indicator.ValueTracking(vr, vs, vt))
            case (Indicator.Composite(is1, comb), Indicator.Composite(is2, _)) =>
              // Each member draws its own blend, because a composite is a bag of independent indicators rather than one relationship.
              is1.zip(is2).traverse((i1, i2) => crossInd(i1, i2)).map(inds => Indicator.Composite(inds, comb))
            case (Indicator.LinesCrossing(s, st1, ft1), Indicator.LinesCrossing(_, st2, ft2)) =>
              linePairing(st1, ft1, st2, ft2).fold(
                Left(new IllegalStateException(s"linePairing missing for same-shape parents: ($st1, $ft1) vs ($st2, $ft2)"))
              ) { case (other1, other2) =>
                (cross(st1, other1), cross(ft1, other2)).mapN((st, ft) => Indicator.LinesCrossing(s, st, ft))
              }
            case (Indicator.TrendChangeDetection(s, t1), Indicator.TrendChangeDetection(_, t2)) =>
              cross(t1, t2).map(t => Indicator.TrendChangeDetection(s, t))
            case (Indicator.ThresholdCrossing(s, t1, ub1, lb1), Indicator.ThresholdCrossing(_, t2, ub2, lb2)) =>
              cross(t1, t2).map { t =>
                val band = ThresholdBounds.of(t)
                Indicator.ThresholdCrossing(s, t, blend.double(ub1, ub2, band.step), blend.double(lb1, lb2, band.step))
              }
            case (Indicator.KeltnerChannel(vs, md1, al1, am1), Indicator.KeltnerChannel(_, md2, al2, am2)) =>
              cross(md1, md2).map { md =>
                Indicator.KeltnerChannel(vs, md, blend.int(al1, al2), blend.double(am1, am2, GeneBounds.keltnerMultiplier.step))
              }
            case (Indicator.BollingerBands(vs, md1, sdl1, sdm1), Indicator.BollingerBands(_, md2, sdl2, sdm2)) =>
              cross(md1, md2).map { md =>
                Indicator.BollingerBands(vs, md, blend.int(sdl1, sdl2), blend.double(sdm1, sdm2, GeneBounds.bollingerMultiplier.step))
              }
            case (Indicator.PriceLineCrossing(s, r, vt1), Indicator.PriceLineCrossing(_, _, vt2)) =>
              cross(vt1, vt2).map(Indicator.PriceLineCrossing(s, r, _))
            case (i1, i2) =>
              Left(new IllegalStateException(s"unhandled indicator types: $i1 vs $i2"))

        F.fromEither(crossInd(par1, par2).map(IndicatorBounds.repair))
          .handleErrorWith {
            case e: IllegalStateException => F.raiseError(e)
            case e => F.raiseError(new IllegalArgumentException(s"failed to cross $par1 and $par2 together: ${e.getMessage}"))
          }
      }

      override def cross(par1: Indicator, par2: Indicator, crossoverProbability: Double)(using r: Random): F[Indicator] =
        maybeCrossSync(par1, par2, crossoverProbability)
    }
  }

  /** The second parent's two lines, ordered so each is crossed with the line of the first parent playing the same role, or `None` if the
    * two parents cannot be paired at all.
    *
    * The valid region for a crossover pair is a union of two cones rather than one, because orientation flips which way the crossover reads
    * and the catalogue holds both. Blending a fast-first parent with a slow-first parent therefore passes straight through the near-equal
    * region where two lines cross on noise, however correlated the weight is; pairing fast with fast puts the blend back inside a cone.
    *
    * This is also the answer `sameShape` gives for a pair of `LinesCrossing`, so the seeds the initialiser admits and the pairings the
    * crossover can actually perform are the same set by construction. Asking positionally, as it used to, rejected an SMA/EMA parent
    * against a reversed EMA/SMA one that this handles.
    */
  private def linePairing(first1: VT, second1: VT, first2: VT, second2: VT): Option[(VT, VT)] =
    val positional = sameVtShape(first1, first2) && sameVtShape(second1, second2)
    val swapped    = sameVtShape(first1, second2) && sameVtShape(second1, first2)
    val aligned    =
      (GeneBounds.lengthOf(first1), GeneBounds.lengthOf(second1), GeneBounds.lengthOf(first2), GeneBounds.lengthOf(second2)) match
        case (Some(f1), Some(s1), Some(f2), Some(s2)) => (f1 <= s1) == (f2 <= s2)
        case _                                        => true
    if (positional && (aligned || !swapped)) Some((first2, second2))
    else if (swapped) Some((second2, first2))
    else None

  /** Whether two indicators can be crossed, which is the same question as whether they have the same shape: a structural mismatch fails a
    * whole run, so a hall-of-fame seed has to be checked before it enters the population rather than after.
    */
  def sameShape(a: Indicator, b: Indicator): Boolean = (a, b) match
    case (Indicator.Composite(is1, _), Indicator.Composite(is2, _)) =>
      is1.length == is2.length && is1.toList.zip(is2.toList).forall(sameShape)
    case (Indicator.TrendChangeDetection(_, t1), Indicator.TrendChangeDetection(_, t2))           => sameVtShape(t1, t2)
    case (Indicator.ThresholdCrossing(_, t1, _, _), Indicator.ThresholdCrossing(_, t2, _, _))     => sameVtShape(t1, t2)
    case (Indicator.LinesCrossing(_, a1, b1), Indicator.LinesCrossing(_, a2, b2))                 => linePairing(a1, b1, a2, b2).isDefined
    case (Indicator.KeltnerChannel(_, m1, _, _), Indicator.KeltnerChannel(_, m2, _, _))           => sameVtShape(m1, m2)
    case (Indicator.BollingerBands(_, m1, _, _), Indicator.BollingerBands(_, m2, _, _))           => sameVtShape(m1, m2)
    case (Indicator.VolatilityRegimeDetection(_, s1), Indicator.VolatilityRegimeDetection(_, s2)) => sameVtShape(s1, s2)
    // Crossover additionally requires these two to agree, because a tracked value read under the wrong role is not the same gene.
    case (Indicator.ValueTracking(r1, s1, t1), Indicator.ValueTracking(r2, s2, t2))       => r1 == r2 && s1 == s2 && sameVtShape(t1, t2)
    case (Indicator.PriceLineCrossing(_, r1, t1), Indicator.PriceLineCrossing(_, r2, t2)) => r1 == r2 && sameVtShape(t1, t2)
    case _                                                                                => false

  private def sameVtShape(a: VT, b: VT): Boolean = (a, b) match
    case (VT.Sequenced(s1), VT.Sequenced(s2))        => s1.length == s2.length && s1.zip(s2).forall(sameVtShape)
    case (VT.Sequenced(_), _) | (_, VT.Sequenced(_)) => false
    case _                                           => a.getClass == b.getClass
