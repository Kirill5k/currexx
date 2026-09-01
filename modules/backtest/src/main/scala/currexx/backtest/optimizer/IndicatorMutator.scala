package currexx.backtest.optimizer

import cats.effect.Sync
import currexx.algorithms.operators.Mutator
import currexx.backtest.optimizer.GeneBounds.{DoubleRange, IntRange}
import currexx.domain.signal.{Indicator, ValueTransformation as VT}

import scala.util.Random

object IndicatorMutator {

  def make[F[_]](using F: Sync[F]): F[Mutator[F, Indicator]] = scaled(1.0)

  /** The same walk with a wider step. A scale of 1.0 is the search's own mutation, a standard deviation of a tenth of each gene's range;
    * larger scales are how `IndicatorInitialiser` builds a population that sits at a chosen distance from its seed rather than on top of
    * it. Nothing in the search itself should pass anything but 1.0.
    */
  def scaled[F[_]](sigmaScale: Double)(using F: Sync[F]): F[Mutator[F, Indicator]] = F.pure {
    new Mutator[F, Indicator] {

      override def mutate(ind: Indicator, mutationProbability: Double)(using r: Random): F[Indicator] = {

        /** Mutates an integer parameter using a Gaussian distribution. */
        def mutInt(value: Int, range: IntRange): Int =
          if (r.nextDouble() < mutationProbability) {
            val stdDev   = range.span * 0.1 * sigmaScale // 10% of range as standard deviation
            val mutation = (r.nextGaussian() * stdDev).round.toInt
            range.clamp(value + mutation)
          } else {
            value
          }

        /** Mutates a double parameter using a Gaussian distribution, with rounding to a step size. */
        def mutDouble(value: Double, range: DoubleRange): Double =
          if (r.nextDouble() < mutationProbability) {
            val stdDev = range.span * 0.1 * sigmaScale // 10% of range
            range.snap(value + r.nextGaussian() * stdDev)
          } else {
            value
          }

        def mutVt(vt: VT): VT = vt match
          case VT.Sequenced(sequence) =>
            VT.Sequenced(sequence.map(mutVt))
          case VT.StandardDeviation(length) =>
            VT.StandardDeviation(mutInt(length, GeneBounds.standardDeviation))
          case VT.Kalman(gain, measurementNoise) =>
            VT.Kalman(mutDouble(gain, GeneBounds.kalmanGain), mutDouble(measurementNoise, GeneBounds.kalmanNoise))
          case VT.KalmanVelocity(gain, measurementNoise) =>
            VT.KalmanVelocity(mutDouble(gain, GeneBounds.kalmanGain), mutDouble(measurementNoise, GeneBounds.kalmanNoise))
          case VT.STOCH(length) =>
            VT.STOCH(mutInt(length, GeneBounds.oscillatorLength))
          case VT.ATR(length) =>
            VT.ATR(mutInt(length, GeneBounds.oscillatorLength))
          case VT.RSX(length) =>
            VT.RSX(mutInt(length, GeneBounds.oscillatorLength))
          case VT.JRSX(length) =>
            VT.JRSX(mutInt(length, GeneBounds.oscillatorLength))
          case VT.WMA(length) =>
            VT.WMA(mutInt(length, GeneBounds.maLength))
          case VT.SMA(length) =>
            VT.SMA(mutInt(length, GeneBounds.maLength))
          case VT.EMA(length) =>
            VT.EMA(mutInt(length, GeneBounds.maLength))
          case VT.HMA(length) =>
            VT.HMA(mutInt(length, GeneBounds.maLength))
          case VT.JMA(length, phase, power) =>
            // JMA phase can be negative
            VT.JMA(mutInt(length, GeneBounds.jmaLength), mutInt(phase, GeneBounds.jmaPhase), mutInt(power, GeneBounds.jmaPower))
          case VT.NMA(length, signalLength, lambda, maCalc) =>
            VT.NMA(
              mutInt(length, GeneBounds.nmaLength),
              mutInt(signalLength, GeneBounds.nmaSignalLength),
              mutDouble(lambda, GeneBounds.nmaLambda),
              maCalc
            )
          case VT.ADX(length) =>
            VT.ADX(mutInt(length, GeneBounds.adxLength))
          case VT.WilliamsR(length) =>
            VT.WilliamsR(mutInt(length, GeneBounds.oscillatorLength))
          case VT.CCI(length) =>
            VT.CCI(mutInt(length, GeneBounds.cciLength))
          case VT.IchimokuKijunSen(length) =>
            VT.IchimokuKijunSen(mutInt(length, GeneBounds.ichimokuLength))
          case VT.ParabolicSAR(afStart, afMax, afStep) =>
            VT.ParabolicSAR(
              mutDouble(afStart, GeneBounds.sarAfStart),
              mutDouble(afMax, GeneBounds.sarAfMax),
              mutDouble(afStep, GeneBounds.sarAfStep)
            )
          case VT.CMF(length) =>
            VT.CMF(mutInt(length, GeneBounds.cmfLength))

        def mutInd(indicator: Indicator): Indicator = indicator match
          case Indicator.Composite(is, comb) =>
            Indicator.Composite(is.map(mutInd), comb)
          case Indicator.TrendChangeDetection(vs, vt) =>
            Indicator.TrendChangeDetection(vs, mutVt(vt))
          case Indicator.ThresholdCrossing(vs, vt, ub, lb) =>
            val mutatedVt = mutVt(vt)
            val band      = ThresholdBounds.of(mutatedVt)
            val mutatedUb = band.clampUpper(mutDouble(ub, DoubleRange(band.upperMin, band.upperMax, band.step)))
            val mutatedLb = band.clampLower(mutDouble(lb, DoubleRange(band.lowerMin, band.lowerMax, band.step)))
            Indicator.ThresholdCrossing(vs, mutatedVt, mutatedUb, mutatedLb)
          case Indicator.LinesCrossing(vs, vt1, vt2) =>
            Indicator.LinesCrossing(vs, mutVt(vt1), mutVt(vt2))
          case Indicator.KeltnerChannel(vs, md, atrL, atrM) =>
            Indicator.KeltnerChannel(vs, mutVt(md), mutInt(atrL, GeneBounds.atrLength), mutDouble(atrM, GeneBounds.keltnerMultiplier))
          case Indicator.BollingerBands(vs, md, stdDevL, stdDevM) =>
            Indicator.BollingerBands(
              vs,
              mutVt(md),
              mutInt(stdDevL, GeneBounds.stdDevLength),
              mutDouble(stdDevM, GeneBounds.bollingerMultiplier)
            )
          case Indicator.VolatilityRegimeDetection(atrL, smoothing) =>
            Indicator.VolatilityRegimeDetection(mutInt(atrL, GeneBounds.atrLength), mutVt(smoothing))
          case Indicator.ValueTracking(vr, vs, vt) =>
            Indicator.ValueTracking(vr, vs, mutVt(vt))
          case Indicator.PriceLineCrossing(vs, role, vt) =>
            Indicator.PriceLineCrossing(vs, role, mutVt(vt))
        F.delay(mutInd(ind))
      }
    }
  }
}
