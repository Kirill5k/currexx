package currexx.backtest.optimizer

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Population
import currexx.algorithms.operators.{Initialiser, Mutator}
import currexx.backtest.optimizer.GeneBounds.{DoubleRange, IntRange}
import currexx.domain.signal.{Indicator, ValueTransformation as VT}

import scala.util.Random

object IndicatorInitialiser:

  private val CloneShare  = 0.15
  private val JitterShare = 0.55

  /** Multiples of the mutator's own 10%-of-range step: near neighbours, middle distance, and most of the way to an independent draw. */
  private val JitterRadii = List(1.0, 3.0, 6.0)

  def make[F[_]](using F: Sync[F], rand: Random): F[Initialiser[F, Indicator]] = seeded(Nil)

  /** The same population, with champions of the same shape mixed in alongside the round's own indicator. See `OptimisationRound.extraSeeds`
    * for what belongs here and why an unshuffled round ignores it.
    */
  def seeded[F[_]](extraSeeds: List[Indicator])(using F: Sync[F], rand: Random): F[Initialiser[F, Indicator]] =
    JitterRadii.traverse(IndicatorMutator.scaled[F]).flatMap { jitters =>
      Initialiser.custom[F, Indicator] { (seed, size, shuffle) =>
        if (!shuffle) F.pure(Vector.fill(size)(seed))
        else buildMixed[F](seed, extraSeeds, size, jitters)
      }
    }

  private def buildMixed[F[_]](
      seed: Indicator,
      extraSeeds: List[Indicator],
      size: Int,
      jitters: List[Mutator[F, Indicator]]
  )(using
      F: Sync[F],
      rand: Random
  ): F[Population[Indicator]] = {
    // A seed that cannot be crossed with the target is a seed that fails the run the first time selection pairs it, so incompatible ones
    // are dropped rather than trusted to the caller.
    val seeds = (seed +: extraSeeds.filter(sameShape(seed, _))).toVector
    // Every seed earns a place of its own, which is what makes the share a floor rather than the count. Both shares are then held under
    // what is left, because a caller that hands over more seeds than the population has room for is still asking for a population of `size`.
    val clones     = math.min(size, math.max(seeds.size, (size * CloneShare).toInt))
    val jittered   = math.min(size - clones, (size * JitterShare).toInt)
    val immigrants = size - clones - jittered
    val clonePop   = Vector.tabulate(clones)(i => seeds(i % seeds.size))
    for jitterPop <- Vector.range(0, jittered).traverse(i => jitters(i % jitters.size).mutate(seeds(i % seeds.size), 1.0))
    // The draws are relationally correct by construction; repairing them anyway means the invariant is enforced in one place and a future
    // change to a draw cannot quietly reintroduce a candidate the operators would have been forbidden to produce.
    yield clonePop ++ jitterPop ++ Vector.fill(immigrants)(IndicatorBounds.repair(randomiseInd(seed)))
  }

  /** Whether two indicators can be crossed. The question belongs to `IndicatorCrossover`, whose precondition it is, and is asked here
    * because a structural mismatch fails a whole run - so a hall-of-fame seed has to be checked before it enters the population.
    */
  def sameShape(a: Indicator, b: Indicator): Boolean = IndicatorCrossover.sameShape(a, b)

  private def logUniform(range: IntRange)(using rand: Random): Int =
    val low  = math.log(range.min.toDouble)
    val high = math.log(range.max.toDouble)
    range.clamp(math.exp(low + rand.nextDouble() * (high - low)).round.toInt)

  private def uniform(range: DoubleRange)(using rand: Random): Double =
    range.snap(range.min + rand.nextDouble() * range.span)

  private def uniformInt(range: IntRange)(using rand: Random): Int =
    range.min + rand.nextInt(range.span + 1)

  private def randomiseVt(transformation: VT)(using rand: Random): VT = transformation match
    case VT.Sequenced(sequence)  => VT.Sequenced(sequence.map(randomiseVt))
    case _: VT.StandardDeviation => VT.StandardDeviation(logUniform(GeneBounds.standardDeviation))
    case _: VT.Kalman            => VT.Kalman(uniform(GeneBounds.kalmanGain), uniform(GeneBounds.kalmanNoise))
    case _: VT.KalmanVelocity    => VT.KalmanVelocity(uniform(GeneBounds.kalmanGain), uniform(GeneBounds.kalmanNoise))
    case _: VT.STOCH             => VT.STOCH(logUniform(GeneBounds.oscillatorLength))
    case _: VT.ATR               => VT.ATR(logUniform(GeneBounds.oscillatorLength))
    case _: VT.RSX               => VT.RSX(logUniform(GeneBounds.oscillatorLength))
    case _: VT.JRSX              => VT.JRSX(logUniform(GeneBounds.oscillatorLength))
    case _: VT.WMA               => VT.WMA(logUniform(GeneBounds.maLength))
    case _: VT.SMA               => VT.SMA(logUniform(GeneBounds.maLength))
    case _: VT.EMA               => VT.EMA(logUniform(GeneBounds.maLength))
    case _: VT.HMA               => VT.HMA(logUniform(GeneBounds.maLength))
    // Power moves the curve more than length does and the catalogue lives at the bottom of its range, so it is drawn log-uniformly too:
    // a uniform draw over [1, 10] spends most of the population on shapes nothing has ever kept.
    case _: VT.JMA   => VT.JMA(logUniform(GeneBounds.jmaLength), uniformInt(GeneBounds.jmaPhase), logUniform(GeneBounds.jmaPower))
    case nma: VT.NMA =>
      VT.NMA(logUniform(GeneBounds.nmaLength), logUniform(GeneBounds.nmaSignalLength), uniform(GeneBounds.nmaLambda), nma.maCalc)
    case _: VT.ADX              => VT.ADX(logUniform(GeneBounds.adxLength))
    case _: VT.WilliamsR        => VT.WilliamsR(logUniform(GeneBounds.oscillatorLength))
    case _: VT.CCI              => VT.CCI(logUniform(GeneBounds.cciLength))
    case _: VT.IchimokuKijunSen => VT.IchimokuKijunSen(logUniform(GeneBounds.ichimokuLength))
    case _: VT.ParabolicSAR     =>
      VT.ParabolicSAR(uniform(GeneBounds.sarAfStart), uniform(GeneBounds.sarAfMax), uniform(GeneBounds.sarAfStep))
    case _: VT.CMF => VT.CMF(logUniform(GeneBounds.cmfLength))

  private def randomiseInd(indicator: Indicator)(using rand: Random): Indicator = indicator match
    case Indicator.TrendChangeDetection(vs, vt) =>
      Indicator.TrendChangeDetection(vs, randomiseVt(vt))
    case Indicator.ThresholdCrossing(vs, vt, _, _) =>
      val randomisedVt = randomiseVt(vt)
      val band         = ThresholdBounds.of(randomisedVt)
      val ub           = band.upperMin + rand.nextDouble() * (band.upperMax - band.upperMin)
      val lb           = band.lowerMin + rand.nextDouble() * (band.lowerMax - band.lowerMin)
      Indicator.ThresholdCrossing(vs, randomisedVt, band.snap(ub), band.snap(lb))
    case Indicator.LinesCrossing(vs, vt1, vt2) =>
      // Two lines of near-equal length cross on noise, which is how a shuffled round arrives at a candidate that trades constantly and
      // earns nothing. What matters is the ratio between them and not which side of it is longer - an inverted pair is the same crossover
      // read the other way round, and the catalogue holds both - so the separation is drawn and the orientation is not.
      val (first, second) = (randomiseVt(vt1), randomiseVt(vt2))
      val ratio           = IndicatorBounds.linesSeparation.drawRatio
      val fastRange       = GeneBounds.lengthRange(first).leavingRoomFor(ratio, GeneBounds.lengthRange(second))
      val fast            = logUniform(fastRange)
      val slow            = math.round(fast * ratio).toInt
      val (l1, l2)        = if (rand.nextBoolean()) (fast, slow) else (slow, fast)
      Indicator.LinesCrossing(vs, GeneBounds.withLength(first, l1), GeneBounds.withLength(second, l2))
    case Indicator.KeltnerChannel(vs, md, _, _) =>
      // The channel is the middle band plus a multiple of ATR, and an ATR measured over a longer window than the band it widens is
      // measuring a different market than the one being banded.
      val middle = randomiseVt(md)
      val atr    = GeneBounds.atrLength.clamp(math.round(lengthOr(middle, 20) * IndicatorBounds.keltnerAtr.drawRatio).toInt)
      Indicator.KeltnerChannel(vs, middle, atr, uniform(GeneBounds.keltnerMultiplier))
    case Indicator.BollingerBands(vs, md, _, _) =>
      // The deviation is meant to describe the spread of the same stretch of price the middle band averages, so its window is drawn near
      // the band's rather than independently of it.
      val middle = randomiseVt(md)
      val stdDev = GeneBounds.stdDevLength.clamp(math.round(lengthOr(middle, 20) * IndicatorBounds.bollingerStdDev.drawRatio).toInt)
      Indicator.BollingerBands(vs, middle, stdDev, uniform(GeneBounds.bollingerMultiplier))
    case Indicator.VolatilityRegimeDetection(_, vt) =>
      // "Low volatility" means ATR below its own longer average. Drawn independently, half of these come back with the smoothing shorter
      // than the ATR it smooths, which inverts the regime the rules then read - the s5 shuffled round of 2026-08-31 drew (29, SMA(6)) and
      // closed 49 trades. The catalogue's own squeezes sit between 1.2 and 4 times: s6 at 20/50, s5_optimized_v2 at 28/63.
      val smoothing = randomiseVt(vt)
      val ratio     = IndicatorBounds.volatilityRegime.drawRatio
      val atrRange  = GeneBounds.atrLength.leavingRoomFor(ratio, GeneBounds.lengthRange(smoothing))
      val atr       = logUniform(atrRange)
      Indicator.VolatilityRegimeDetection(atr, GeneBounds.withLength(smoothing, math.round(atr * ratio).toInt))
    case Indicator.Composite(is, combinator) =>
      Indicator.Composite(is.map(randomiseInd), combinator)
    case Indicator.ValueTracking(vr, vs, vt) =>
      Indicator.ValueTracking(vr, vs, randomiseVt(vt))
    case Indicator.PriceLineCrossing(vs, role, vt) =>
      Indicator.PriceLineCrossing(vs, role, randomiseVt(vt))

  private def lengthOr(vt: VT, fallback: Int): Int = GeneBounds.lengthOf(vt).getOrElse(fallback)
