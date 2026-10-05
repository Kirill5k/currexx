package currexx.core.signal

import currexx.domain.market.{MarketTimeSeriesData, PriceRange}
import currexx.domain.signal.ValueSource

/** Numerical inputs for one detector call. Arrays are read-only and shared only within its composite evaluation. */
final private[signal] class NumericalWindow(val data: MarketTimeSeriesData) {
  private val size = data.prices.length

  private inline def extract(inline select: PriceRange => Double): Array[Double] = {
    val result = new Array[Double](size)
    val prices = data.prices.iterator
    var index  = 0
    while (prices.hasNext) {
      result(index) = select(prices.next())
      index += 1
    }
    result
  }

  lazy val closings: Array[Double]     = extract(_.close)
  lazy val openings: Array[Double]     = extract(_.open)
  lazy val highs: Array[Double]        = extract(_.high)
  lazy val lows: Array[Double]         = extract(_.low)
  lazy val volumes: Array[Double]      = extract(_.volume)
  private lazy val hl2: Array[Double]  = extract(p => (p.high + p.low) / 2)
  private lazy val hlc3: Array[Double] = extract(p => (p.high + p.low + p.close) / 3)

  def source(valueSource: ValueSource): Array[Double] = valueSource match {
    case ValueSource.Close => closings
    case ValueSource.Open  => openings
    case ValueSource.HL2   => hl2
    case ValueSource.HLC3  => hlc3
  }
}
