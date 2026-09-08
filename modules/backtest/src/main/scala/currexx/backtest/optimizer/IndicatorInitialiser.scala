package currexx.backtest.optimizer

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Population
import currexx.algorithms.operators.Initialiser
import currexx.backtest.optimizer.GeneBounds.{DoubleRange, IntRange}
import currexx.domain.signal.{Indicator, ValueTransformation as VT}

import scala.util.Random

object IndicatorInitialiser:

  /** What a starting population is made of: copies of the seeds, jittered neighbours of them, and independent draws for the remainder.
    *
    * The radii are multiples of the mutator's own 10%-of-range step, so a jitter is a mutation held at a chosen distance rather than a
    * separate kind of draw: one step out is a near neighbour, six is most of the way to an immigrant.
    *
    * @param cloneShare
    *   a floor rather than the count. Every seed earns a place of its own however many are handed over.
    * @param jitterShare
    *   taken from what the clones leave, so a caller with more seeds than the population has room for still gets a population of the size
    *   it asked for.
    * @param jitterRadii
    *   spread evenly over the jittered share, in whatever order they are given.
    */
  final private case class PopulationMix(cloneShare: Double, jitterShare: Double, jitterRadii: List[Double]):
    require(cloneShare + jitterShare <= 1.0, s"clones and jitter cannot claim more than the whole population: $cloneShare + $jitterShare")
    require(jitterRadii.nonEmpty, "a mix with a jitter share needs at least one radius to jitter by")

  /** Searching for a shape the round does not have yet, so most of the population sits away from the seed. */
  private val Exploring = PopulationMix(cloneShare = 0.15, jitterShare = 0.55, jitterRadii = List(1.0, 3.0, 6.0))

  /** Refining a shape the round already believes in, so the population stays within a step of it - but not entirely, because a population
    * that is nothing but its seed can only move as fast as mutation alone, and crossover between near-identical parents does nothing.
    */
  private val Refining = PopulationMix(cloneShare = 0.70, jitterShare = 0.20, jitterRadii = List(1.0))

  def make[F[_]](using F: Sync[F], rand: Random): F[Initialiser[F, Indicator]] = seeded(Nil)

  /** The same population, with champions of the same shape mixed in alongside the round's own indicator. See
    * `OptimisationRound.extraSeeds`.
    *
    * `shuffle` chooses between the two mixes rather than between a random population and a copied one, so neither setting returns the seed
    * repeated - `Initialiser.simple` is the one that does that.
    */
  def seeded[F[_]](extraSeeds: List[Indicator])(using F: Sync[F], rand: Random): F[Initialiser[F, Indicator]] =
    Initialiser.custom[F, Indicator] { (seed, size, shuffle) =>
      buildPopulation(seed, extraSeeds, size, if (shuffle) Exploring else Refining)
    }

  private def buildPopulation[F[_]](
      seed: Indicator,
      extraSeeds: List[Indicator],
      size: Int,
      mix: PopulationMix
  )(using
      F: Sync[F],
      rand: Random
  ): F[Population[Indicator]] = {
    // A seed that cannot be crossed with the target is a seed that fails the run the first time selection pairs it, so incompatible ones
    // are dropped rather than trusted to the caller.
    val seeds      = (seed +: extraSeeds.filter(sameShape(seed, _))).toVector
    val clones     = math.min(size, math.max(seeds.size, (size * mix.cloneShare).toInt))
    val jittered   = math.min(size - clones, (size * mix.jitterShare).toInt)
    val immigrants = size - clones - jittered
    val clonePop   = Vector.tabulate(clones)(i => seeds(i % seeds.size))
    for
      jitters   <- mix.jitterRadii.traverse(IndicatorMutator.scaled[F])
      jitterPop <- Vector.range(0, jittered).traverse(i => jitters(i % jitters.size).mutate(seeds(i % seeds.size), 1.0))
    // The draws are relationally correct by construction; repairing them anyway means the invariant is enforced in one place and a future
    // change to a draw cannot quietly reintroduce a candidate the operators would have been forbidden to produce.
    yield clonePop ++ jitterPop ++ Vector.fill(immigrants)(IndicatorBounds.repair(randomiseInd(seed)))
  }

  /** Whether two indicators can be crossed. The question belongs to `IndicatorCrossover`, whose precondition it is, and is asked here
    * because a structural mismatch fails a whole run - so a hall-of-fame seed has to be checked before it enters the population.
    */
  private def sameShape(a: Indicator, b: Indicator): Boolean = IndicatorCrossover.sameShape(a, b)

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
      val ratio      = IndicatorBounds.keltnerAtr.drawRatio
      val middleBase = randomiseVt(md)
      val bandRange  = GeneBounds.lengthRange(middleBase).leavingRoomFor(ratio, GeneBounds.atrLength)
      val bandLength = logUniform(bandRange)
      val middle     = GeneBounds.withLength(middleBase, bandLength)
      val atr        = GeneBounds.atrLength.clamp(math.round(bandLength * ratio).toInt)
      Indicator.KeltnerChannel(vs, middle, atr, uniform(GeneBounds.keltnerMultiplier))
    case Indicator.BollingerBands(vs, md, _, _) =>
      // The deviation is meant to describe the spread of the same stretch of price the middle band averages, so its window is drawn near
      // the band's rather than independently of it.
      val ratio      = IndicatorBounds.bollingerStdDev.drawRatio
      val middleBase = randomiseVt(md)
      val bandRange  = GeneBounds.lengthRange(middleBase).leavingRoomFor(ratio, GeneBounds.stdDevLength)
      val bandLength = logUniform(bandRange)
      val middle     = GeneBounds.withLength(middleBase, bandLength)
      val stdDev     = GeneBounds.stdDevLength.clamp(math.round(bandLength * ratio).toInt)
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
