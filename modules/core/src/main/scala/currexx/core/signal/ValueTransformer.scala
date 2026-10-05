package currexx.core.signal

import currexx.calculations.{Filters, MomentumOscillators, MovingAverages, Statistics, Volatility}
import currexx.domain.signal.{MovingAverage, ValueSource as VS, ValueTransformation as VT}

private[signal] object ValueTransformer {
  def extractFrom(window: NumericalWindow, vs: VS): Array[Double] =
    window.source(vs)

  def averageTrueRange(values: Array[Double], window: NumericalWindow, length: Int): Array[Double] =
    Volatility.averageTrueRange(values, window.highs, window.lows, length)

  def transformTo(values: Array[Double], window: NumericalWindow, vt: VT): Array[Double] =
    vt match {
      case VT.Sequenced(transformations) =>
        transformations.foldLeft(values)((current, transformation) => transformTo(current, window, transformation))
      case VT.StandardDeviation(length) => Statistics.standardDeviation(values, length)
      case VT.ATR(length)               => averageTrueRange(values, window, length)
      case VT.RSX(length)               => MomentumOscillators.relativeStrengthIndex(values, length)
      case VT.WMA(length)               => MovingAverages.weighted(values, length)
      case VT.SMA(length)               => MovingAverages.simple(values, length)
      case VT.EMA(length)               => MovingAverages.exponential(values, length)
      case VT.HMA(length)               => MovingAverages.hull(values, length)
      case VT.JMA(length, phase, power) => MovingAverages.jurikSimplified(values, length, phase, power)
      // List-based kernels use the original data for their OHLC inputs.
      case VT.Kalman(gain, measurementNoise)         => Filters.kalman(values.toList, gain, measurementNoise).toArray
      case VT.KalmanVelocity(gain, measurementNoise) => Filters.kalmanVelocity(values.toList, gain, measurementNoise).toArray
      case VT.JRSX(length)                           => MomentumOscillators.jurikRelativeStrengthIndex(values.toList, length).toArray
      case VT.STOCH(length) => MomentumOscillators.stochastic(values.toList, window.data.highs, window.data.lows, length).toArray
      case VT.ADX(length)   =>
        MomentumOscillators.averageDirectionalIndex(window.data.closings, window.data.highs, window.data.lows, length).toArray
      case VT.WilliamsR(length) => MomentumOscillators.williamsR(window.data.closings, window.data.highs, window.data.lows, length).toArray
      case VT.CCI(length)       =>
        MomentumOscillators.commodityChannelIndex(window.data.closings, window.data.highs, window.data.lows, length).toArray
      case VT.IchimokuKijunSen(length) => MomentumOscillators.ichimokuKijunSen(window.data.highs, window.data.lows, length).toArray
      case VT.ParabolicSAR(afStart, afMax, afStep) =>
        MomentumOscillators.parabolicSAR(window.data.highs, window.data.lows, afStart, afMax, afStep).toArray
      case VT.CMF(length) =>
        MomentumOscillators.chaikinMoneyFlow(window.data.closings, window.data.highs, window.data.lows, window.data.volumes, length).toArray
      case VT.NMA(length, signalLength, lambda, ma) =>
        MovingAverages.nyquist(values.toList, length, signalLength, lambda, ma.calculation).toArray
    }

  extension (ma: MovingAverage)
    private def calculation: (Array[Double], Int) => Array[Double] =
      ma match
        case MovingAverage.Exponential => MovingAverages.exponential
        case MovingAverage.Simple      => MovingAverages.simple
        case MovingAverage.Weighted    => MovingAverages.weighted
        case MovingAverage.Hull        => MovingAverages.hull
}
