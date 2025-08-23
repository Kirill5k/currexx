package currexx.core.signal

import currexx.calculations.{Filters, MomentumOscillators, MovingAverages, Volatility}
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.signal.{MovingAverage, ValueSource as VS, ValueTransformation as VT}

import scala.collection.concurrent.TrieMap

trait ValueTransformer:
  def extractFrom(data: MarketTimeSeriesData, vs: VS): List[Double]
  def transformTo(values: List[Double], data: MarketTimeSeriesData, vt: VT): List[Double]
  def averageTrueRange(values: List[Double], data: MarketTimeSeriesData, length: Int): List[Double]

final private class PureValueTransformer() extends ValueTransformer {
  extension (vs: VS)
    private def extract(data: MarketTimeSeriesData): List[Double] = {
      val prices = data.prices.toList
      vs match
        case VS.Close => prices.map(_.close)
        case VS.Open  => prices.map(_.open)
        case VS.HL2   => prices.map(p => (p.high + p.low) / 2)
        case VS.HLC3  => prices.map(p => (p.high + p.low + p.close) / 3)
    }

  extension (vt: VT)
    private def transform(data: List[Double], ref: MarketTimeSeriesData): List[Double] =
      vt match
        case VT.Sequenced(transformations)             => transformations.foldLeft(data)((d, t) => t.transform(d, ref))
        case VT.Kalman(gain, measurementNoise)         => Filters.kalman(data, gain, measurementNoise)
        case VT.KalmanVelocity(gain, measurementNoise) => Filters.kalmanVelocity(data, gain, measurementNoise)
        case VT.RSX(length)                            => MomentumOscillators.relativeStrengthIndex(data, length)
        case VT.JRSX(length)                           => MomentumOscillators.jurikRelativeStrengthIndex(data, length)
        case VT.STOCH(length)                          => MomentumOscillators.stochastic(data, ref.highs, ref.lows, length)
        case VT.WMA(length)                            => MovingAverages.weighted(data, length)
        case VT.SMA(length)                            => MovingAverages.simple(data, length)
        case VT.EMA(length)                            => MovingAverages.exponential(data, length)
        case VT.HMA(length)                            => MovingAverages.hull(data, length)
        case VT.JMA(length, phase, power)              => MovingAverages.jurikSimplified(data, length, phase, power)
        case VT.NMA(length, signalLength, lambda, ma)  => MovingAverages.nyquist(data, length, signalLength, lambda, ma.calculation)

  extension (ma: MovingAverage)
    private def calculation: (List[Double], Int) => List[Double] =
      ma match
        case MovingAverage.Exponential => (values, length) => MovingAverages.exponential(values, length)
        case MovingAverage.Simple      => MovingAverages.simple
        case MovingAverage.Weighted    => MovingAverages.weighted
        case MovingAverage.Hull        => MovingAverages.hull

  override def extractFrom(data: MarketTimeSeriesData, vs: VS): List[Double] =
    vs.extract(data)

  override def transformTo(values: List[Double], data: MarketTimeSeriesData, vt: VT): List[Double] =
    vt.transform(values, data)

  override def averageTrueRange(values: List[Double], data: MarketTimeSeriesData, length: Int): List[Double] =
    Volatility.averageTrueRange(values, data.highs, data.lows, length)
}

final private class CachedValueTransformer(
    private val cache: collection.mutable.Map[String, List[Double]]
) extends ValueTransformer {
  private val transformer = new PureValueTransformer()

  private def extractKey(data: MarketTimeSeriesData, vs: VS): String =
    s"${data.currencyPair}-${data.interval}-${data.prices.head.time}-$vs"

//  private def transformKey(values: List[Double], data: MarketTimeSeriesData, vt: VT): String =
//    s"${data.currencyPair}-${data.interval}-${data.prices.head.time}-$vt-${values.head}"
//
//  private def averageTrueRangeKey(values: List[Double], data: MarketTimeSeriesData, length: Int): String =
//    s"${data.currencyPair}-${data.interval}-${data.prices.head.time}-atr-$length-${values.head}"

  override def extractFrom(data: MarketTimeSeriesData, vs: VS): List[Double] =
    cache.getOrElseUpdate(extractKey(data, vs), transformer.extractFrom(data, vs))

  override def transformTo(values: List[Double], data: MarketTimeSeriesData, vt: VT): List[Double] =
    transformer.transformTo(values, data, vt)

  override def averageTrueRange(values: List[Double], data: MarketTimeSeriesData, length: Int): List[Double] =
    transformer.averageTrueRange(values, data, length)
}

object ValueTransformer:
  def pure: ValueTransformer =
    new PureValueTransformer()

  def cached: ValueTransformer =
    CachedValueTransformer(TrieMap.empty)
