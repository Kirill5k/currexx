package currexx.core.signal

import currexx.calculations.{Filters, MomentumOscillators, MovingAverages, Statistics, Volatility}
import currexx.domain.signal.{MovingAverage, ValueSource as VS, ValueTransformation as VT}

private[signal] object ValueTransformer {
  def extractFrom(window: NumericalWindow, vs: VS): Array[Double] =
    window.source(vs)

  def averageTrueRange(values: Array[Double], window: NumericalWindow, length: Int): Array[Double] =
    Volatility.averageTrueRange(values, window.highs, window.lows, length)

  def transformTo(values: Array[Double], window: NumericalWindow, vt: VT, outputCount: Int = Int.MaxValue): Array[Double] =
    vt match {
      case VT.Sequenced(Nil)             => values
      case VT.Sequenced(transformations) =>
        val intermediate = transformations.init.foldLeft(values)((current, transformation) => transformTo(current, window, transformation))
        transformTo(intermediate, window, transformations.last, outputCount)
      case VT.StandardDeviation(length)              => Statistics.standardDeviation(values, length, outputCount)
      case VT.ATR(length)                            => Volatility.averageTrueRange(values, window.highs, window.lows, length, outputCount)
      case VT.RSX(length)                            => MomentumOscillators.relativeStrengthIndex(values, length, outputCount)
      case VT.WMA(length)                            => MovingAverages.weighted(values, length)
      case VT.SMA(length)                            => MovingAverages.simple(values, length, outputCount)
      case VT.EMA(length)                            => MovingAverages.exponential(values, length)
      case VT.HMA(length)                            => MovingAverages.hull(values, length)
      case VT.JMA(length, phase, power)              => MovingAverages.jurikSimplified(values, length, phase, power, outputCount)
      case VT.Kalman(gain, measurementNoise)         => Filters.kalman(values, gain, measurementNoise)
      case VT.KalmanVelocity(gain, measurementNoise) => Filters.kalmanVelocity(values, gain, measurementNoise)
      case VT.JRSX(length)                           => MomentumOscillators.jurikRelativeStrengthIndex(values, length)
      case VT.STOCH(length)                          => MomentumOscillators.stochastic(values, window.highs, window.lows, length)
      case VT.ADX(length)                            =>
        MomentumOscillators.averageDirectionalIndex(window.closings, window.highs, window.lows, length)
      case VT.WilliamsR(length) => MomentumOscillators.williamsR(window.closings, window.highs, window.lows, length)
      case VT.CCI(length)       =>
        MomentumOscillators.commodityChannelIndex(window.closings, window.highs, window.lows, length)
      case VT.IchimokuKijunSen(length)             => MomentumOscillators.ichimokuKijunSen(window.highs, window.lows, length)
      case VT.ParabolicSAR(afStart, afMax, afStep) =>
        MomentumOscillators.parabolicSAR(window.highs, window.lows, afStart, afMax, afStep)
      case VT.CMF(length) =>
        MomentumOscillators.chaikinMoneyFlow(window.closings, window.highs, window.lows, window.volumes, length, outputCount)
      case VT.NMA(length, signalLength, lambda, ma) =>
        MovingAverages.nyquist(values, length, signalLength, lambda, ma.calculation)
    }

  extension (ma: MovingAverage)
    private def calculation: (Array[Double], Int) => Array[Double] =
      ma match
        case MovingAverage.Exponential => MovingAverages.exponential
        case MovingAverage.Simple      => MovingAverages.simple
        case MovingAverage.Weighted    => MovingAverages.weighted
        case MovingAverage.Hull        => MovingAverages.hull
}
