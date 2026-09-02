package currexx.backtest.optimizer

import currexx.backtest.optimizer.GeneBounds.{DoubleRange, IntRange}
import currexx.domain.signal.{Indicator, ValueTransformation as VT}

import scala.util.Random

/** The constraints that hold *between* two genes of one indicator, and what the operators do about them.
  *
  * `GeneBounds` says what a single gene may be and `ThresholdBounds` says what a boundary may be given the transformation feeding it. Both
  * describe one gene at a time, which is all a per-gene operator can honour. These are relationships, and the region they carve out is not a
  * box, so an operator that moves each gene inside its own range can still leave it - which is how the champion of
  * ga-optimisation-2026-09-02-0722 came to be `LinesCrossing(JMA(35,..), JMA(38,..))` with `VolatilityRegimeDetection(22, SMA(6))`: two
  * lines a whisker apart and a squeeze smoothed over a quarter of the window it smooths, neither of which could have been drawn.
  *
  * What is guaranteed, and what is not:
  *   - `isValid` is the definition: every gene inside its own range, and every related pair inside its ratio band.
  *   - crossover and mutation preserve it, so a valid parent cannot produce an invalid child. Mutation additionally holds the anchor inside
  *     `feasibleAnchor`, so it cannot walk a pair somewhere the dependent's range is unable to follow.
  *   - `repair` does not guarantee it, and cannot. It never moves the anchor, so a pair whose anchor has no legal partner at all - a fast
  *     line at 95 against a `jmaLength` ceiling of 100 - comes back with the dependent as close as its range allows and the ratio still
  *     broken. Repair makes an arbitrary indicator legal where that is possible without rewriting the gene the pair hangs on; `isValid` is
  *     what says whether it worked. Do not read either as the other.
  */
object IndicatorBounds:

  /** A relationship between an anchor gene and a dependent one, as the ratio of the second to the first, and every operation on it.
    *
    * The two bands are not the same question. `draw` is where a fresh random indicator should start and is a preference. `valid` is what an
    * operator may not produce, and is set by measurement: wide enough to hold every strategy in `TestStrategy`, narrow enough to exclude the
    * failures above. They differ by more than a safety margin - s2_optimized and s2_optimized_v3, the best two vals by holdout net, both run
    * `VolatilityRegimeDetection(37, SMA(35))` at 0.946, so a repair enforcing the draw band would rewrite the two strategies the search
    * exists to find. Drawing a ratio below 1.2 is a bad bet; forbidding one is a false claim about what works.
    */
  final case class Relation(draw: DoubleRange, valid: DoubleRange):

    def ratioOf(anchor: Int, dependent: Int): Double = dependent.toDouble / math.max(1, anchor)

    def holds(anchor: Int, dependent: Int): Boolean =
      val ratio = ratioOf(anchor, dependent)
      ratio >= valid.min && ratio <= valid.max

    /** A ratio for a fresh draw, from the narrower of the two bands. */
    def drawRatio(using rand: Random): Double = draw.min + rand.nextDouble() * draw.span

    /** The anchors that leave room for some legal dependent, which is where mutation is allowed to put one.
      *
      * Only `linesSeparation` is narrowed by this today - a fast line above 83 has no slow partner under a ceiling of 100, so the top sixth
      * of `jmaLength` holds no usable crossover pair. For the other three the whole anchor range is feasible and this is the identity. Never
      * empty: where the ranges cannot be reconciled it collapses to a point at the bottom of the anchor's range.
      */
    def feasibleAnchor(anchorRange: IntRange, dependentRange: IntRange): IntRange =
      val lo = math.max(anchorRange.min, math.ceil(dependentRange.min / valid.max).toInt)
      val hi = math.min(anchorRange.max, math.floor(dependentRange.max / valid.min).toInt)
      IntRange(lo, math.max(lo, hi))

    /** The dependent an anchor and a ratio imply, held inside the band and inside the dependent's own range. */
    def dependentFor(anchor: Int, ratio: Double, dependentRange: IntRange): Int =
      project(anchor, math.round(anchor * ratio).toInt, dependentRange)

    /** The legal dependent nearest the one given, without moving the anchor. See the note on `repair` above for when this cannot succeed. */
    def project(anchor: Int, dependent: Int, dependentRange: IntRange): Int =
      val wanted = math.ceil(valid.min * anchor).toInt
      val lo     = math.max(dependentRange.min, wanted)
      val hi     = math.min(dependentRange.max, math.floor(valid.max * anchor).toInt)
      if (lo > hi) then if (wanted > dependentRange.max) dependentRange.max else dependentRange.min
      else math.max(lo, math.min(hi, dependent))

  /** Smoothing over ATR. "Low volatility" means ATR below its own longer average, so a smoothing shorter than the ATR it smooths inverts the
    * regime the rules read. Catalogue range 0.778 (s2_optimized_v2) to 2.500 (s6); the failure above sat at 0.27.
    */
  val volatilityRegime: Relation = Relation(DoubleRange(1.2, 4.0, 0.05), DoubleRange(0.70, 5.0, 0.05))

  /** Slow line over fast line. Two lines of near-equal length cross on noise and trade constantly for nothing. Catalogue range 1.550
    * (s2_optimized_v3) to 2.211 (s1_v2_optimized); the failure above sat at 1.086.
    */
  val linesSeparation: Relation = Relation(DoubleRange(1.3, 4.0, 0.05), DoubleRange(1.20, 5.0, 0.05))

  /** ATR over the middle band it widens. An ATR measured over a longer window than its band is measuring a different market. Catalogue range
    * 0.476 to 0.731.
    */
  val keltnerAtr: Relation = Relation(DoubleRange(0.4, 1.0, 0.05), DoubleRange(0.30, 1.50, 0.05))

  /** Deviation window over the middle band it describes. Catalogue holds 1.171 in both vals that use one; the floor allows for a long band's
    * deviation being clamped to `stdDevLength.max`, which bottoms out at exactly 0.5.
    */
  val bollingerStdDev: Relation = Relation(DoubleRange(0.7, 1.5, 0.05), DoubleRange(0.45, 2.50, 0.05))

  /** The related pair inside an indicator, where it has one: the relation, the anchor, the dependent, and the range the dependent lives in.
    *
    * One description, read by `repair`, by `isValid` and by the mutator, so the three cannot drift apart on which gene anchors which. For a
    * crossover pair the anchor is the faster line whichever side it sits on: an inverted pair is the same crossover read the other way round
    * and the catalogue holds both, so the separation is the constraint and the orientation is not.
    */
  def relationOf(indicator: Indicator): Option[(Relation, Int, Int, IntRange)] = indicator match
    case Indicator.VolatilityRegimeDetection(atrLength, smoothing) =>
      GeneBounds.lengthOf(smoothing).map((volatilityRegime, atrLength, _, GeneBounds.lengthRange(smoothing)))
    case Indicator.LinesCrossing(_, vt1, vt2) =>
      for
        l1 <- GeneBounds.lengthOf(vt1)
        l2 <- GeneBounds.lengthOf(vt2)
      yield
        if (l1 <= l2) (linesSeparation, l1, l2, GeneBounds.lengthRange(vt2))
        else (linesSeparation, l2, l1, GeneBounds.lengthRange(vt1))
    case Indicator.KeltnerChannel(_, middleBand, atrLength, _) =>
      GeneBounds.lengthOf(middleBand).map((keltnerAtr, _, atrLength, GeneBounds.atrLength))
    case Indicator.BollingerBands(_, middleBand, stdDevLength, _) =>
      GeneBounds.lengthOf(middleBand).map((bollingerStdDev, _, stdDevLength, GeneBounds.stdDevLength))
    case _ => None

  /** Whether an indicator is one the search is allowed to hold: every gene in its own range, every related pair in its band. */
  def isValid(indicator: Indicator): Boolean = indicator match
    case Indicator.Composite(is, _) => is.forall(isValid)
    case other                      => genesInRange(other) && relationOf(other).forall((r, a, d, _) => r.holds(a, d))

  private def genesInRange(indicator: Indicator): Boolean = indicator match
    case Indicator.Composite(is, _)            => is.forall(genesInRange)
    case Indicator.TrendChangeDetection(_, vt) => normalise(vt) == vt
    case Indicator.ValueTracking(_, _, vt)     => normalise(vt) == vt
    case Indicator.PriceLineCrossing(_, _, vt) => normalise(vt) == vt
    case Indicator.LinesCrossing(_, vt1, vt2)  => normalise(vt1) == vt1 && normalise(vt2) == vt2
    case Indicator.ThresholdCrossing(_, vt, ub, lb) =>
      val band = ThresholdBounds.of(vt)
      normalise(vt) == vt && band.clampUpper(ub) == ub && band.clampLower(lb) == lb
    case Indicator.VolatilityRegimeDetection(atrLength, smoothing) =>
      normalise(smoothing) == smoothing && GeneBounds.atrLength.clamp(atrLength) == atrLength
    case Indicator.KeltnerChannel(_, middleBand, atrLength, atrMultiplier) =>
      normalise(middleBand) == middleBand && GeneBounds.atrLength.clamp(atrLength) == atrLength &&
      GeneBounds.keltnerMultiplier.clamp(atrMultiplier) == atrMultiplier
    case Indicator.BollingerBands(_, middleBand, stdDevLength, stdDevMultiplier) =>
      normalise(middleBand) == middleBand && GeneBounds.stdDevLength.clamp(stdDevLength) == stdDevLength &&
      GeneBounds.bollingerMultiplier.clamp(stdDevMultiplier) == stdDevMultiplier

  /** Every gene back inside its own range, and every related pair as close to its band as the anchor allows. Idempotent. */
  def repair(indicator: Indicator): Indicator = indicator match
    case Indicator.Composite(is, combinator) =>
      Indicator.Composite(is.map(repair), combinator)
    case Indicator.TrendChangeDetection(vs, vt) =>
      Indicator.TrendChangeDetection(vs, normalise(vt))
    case Indicator.ValueTracking(role, vs, vt) =>
      Indicator.ValueTracking(role, vs, normalise(vt))
    case Indicator.PriceLineCrossing(vs, role, vt) =>
      Indicator.PriceLineCrossing(vs, role, normalise(vt))
    case Indicator.ThresholdCrossing(vs, vt, ub, lb) =>
      val transformation = normalise(vt)
      val band           = ThresholdBounds.of(transformation)
      Indicator.ThresholdCrossing(vs, transformation, band.clampUpper(ub), band.clampLower(lb))
    case Indicator.VolatilityRegimeDetection(atrLength, smoothing) =>
      val atr      = GeneBounds.atrLength.clamp(atrLength)
      val smoothed = normalise(smoothing)
      val length   = GeneBounds.lengthOf(smoothed).map(volatilityRegime.project(atr, _, GeneBounds.lengthRange(smoothed)))
      Indicator.VolatilityRegimeDetection(atr, length.fold(smoothed)(GeneBounds.withLength(smoothed, _)))
    case Indicator.LinesCrossing(vs, vt1, vt2) =>
      val (line1, line2) = (normalise(vt1), normalise(vt2))
      (GeneBounds.lengthOf(line1), GeneBounds.lengthOf(line2)) match
        case (Some(l1), Some(l2)) if l1 <= l2 =>
          Indicator.LinesCrossing(vs, line1, GeneBounds.withLength(line2, linesSeparation.project(l1, l2, GeneBounds.lengthRange(line2))))
        case (Some(l1), Some(l2)) =>
          Indicator.LinesCrossing(vs, GeneBounds.withLength(line1, linesSeparation.project(l2, l1, GeneBounds.lengthRange(line1))), line2)
        case _ => Indicator.LinesCrossing(vs, line1, line2)
    case Indicator.KeltnerChannel(vs, middleBand, atrLength, atrMultiplier) =>
      val middle = normalise(middleBand)
      val atr    = GeneBounds
        .lengthOf(middle)
        .fold(GeneBounds.atrLength.clamp(atrLength))(keltnerAtr.project(_, atrLength, GeneBounds.atrLength))
      Indicator.KeltnerChannel(vs, middle, atr, GeneBounds.keltnerMultiplier.clamp(atrMultiplier))
    case Indicator.BollingerBands(vs, middleBand, stdDevLength, stdDevMultiplier) =>
      val middle = normalise(middleBand)
      val stdDev = GeneBounds
        .lengthOf(middle)
        .fold(GeneBounds.stdDevLength.clamp(stdDevLength))(bollingerStdDev.project(_, stdDevLength, GeneBounds.stdDevLength))
      Indicator.BollingerBands(vs, middle, stdDev, GeneBounds.bollingerMultiplier.clamp(stdDevMultiplier))

  /** Every gene of a transformation, back inside the range the search is allowed to hold it in. Clamps rather than snapping to the step: the
    * step is where an operator may place a new value, not a property a legal value has to have - see `DoubleRange.clamp`.
    */
  def normalise(transformation: VT): VT = transformation match
    case VT.Sequenced(sequence)  => VT.Sequenced(sequence.map(normalise))
    case VT.Kalman(g, n)         => VT.Kalman(GeneBounds.kalmanGain.clamp(g), GeneBounds.kalmanNoise.clamp(n))
    case VT.KalmanVelocity(g, n) => VT.KalmanVelocity(GeneBounds.kalmanGain.clamp(g), GeneBounds.kalmanNoise.clamp(n))
    case VT.JMA(length, phase, power) =>
      VT.JMA(GeneBounds.jmaLength.clamp(length), GeneBounds.jmaPhase.clamp(phase), GeneBounds.jmaPower.clamp(power))
    case VT.NMA(length, signalLength, lambda, maCalc) =>
      VT.NMA(
        GeneBounds.nmaLength.clamp(length),
        GeneBounds.nmaSignalLength.clamp(signalLength),
        GeneBounds.nmaLambda.clamp(lambda),
        maCalc
      )
    case VT.ParabolicSAR(afStart, afMax, afStep) =>
      VT.ParabolicSAR(GeneBounds.sarAfStart.clamp(afStart), GeneBounds.sarAfMax.clamp(afMax), GeneBounds.sarAfStep.clamp(afStep))
    case other => GeneBounds.lengthOf(other).fold(other)(l => GeneBounds.withLength(other, GeneBounds.lengthRange(other).clamp(l)))
