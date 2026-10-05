package currexx.core.signal

import cats.data.NonEmptyList
import currexx.calculations.{Statistics, Volatility}
import currexx.core.fixtures.{Markets, Users}
import currexx.domain.market.{MarketTimeSeriesData, PriceRange}
import currexx.domain.signal.{Condition, Direction, Indicator, ValueSource, ValueTransformation as VT}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class BandSignalDetectorSpec extends AnyWordSpec with Matchers {

  private val detector    = SignalDetector.pure
  private val transformer = ValueTransformer.pure

  "Band signal detection" should {
    "match full-history Keltner bands on short and complete windows" in
      checkAgainstFullBands((source, middle, length, multiplier) => Indicator.KeltnerChannel(source, middle, length, multiplier))

    "match full-history Bollinger bands on short and complete windows" in
      checkAgainstFullBands((source, middle, length, multiplier) => Indicator.BollingerBands(source, middle, length, multiplier))

    "preserve equality at the current band and upper-band priority when both bands cross" in {
      val cases = List(
        (List(1.5, 1.0, 2.0), 0.0, Direction.Upward),
        (List(1.5, 2.0, 1.0), 0.0, Direction.Downward),
        (List(3.0, 1.0, 2.0), 0.1, Direction.Upward),
        (List(0.0, 2.0, 1.0), 0.1, Direction.Downward)
      )
      cases.foreach { (closes, multiplier, direction) =>
        val data       = dataFor(closes)
        val indicators = List(
          Indicator.KeltnerChannel(ValueSource.Close, VT.EMA(3), 2, multiplier),
          Indicator.BollingerBands(ValueSource.Close, VT.EMA(3), 3, multiplier)
        )
        indicators.foreach { indicator =>
          withClue(s"$indicator on $closes: ") {
            detector.detect(Users.uid, data)(indicator) mustBe Some(signal(data, indicator, Condition.UpperBandCrossing(direction)))
          }
        }
      }
    }
  }

  private def checkAgainstFullBands(makeIndicator: (ValueSource, VT, Int, Double) => Indicator): Unit = {
    val closes      = List.tabulate(120)(i => 1.2 + math.sin(i * 0.7) * 0.02 + math.cos(i * 0.13) * 0.01)
    val middleBands = List(VT.SMA(20), VT.EMA(20), VT.JMA(20, 50, 2), VT.Sequenced(List(VT.EMA(5), VT.SMA(20))))
    for {
      windowSize <- List(1, 2, 7, 19, 100)
      offset     <- List(0, 1, 3, 7, 12, 20)
      source     <- List(ValueSource.Close, ValueSource.HLC3)
      middle     <- middleBands
      length     <- List(1, 14, 41)
      multiplier <- List(0.5, 1.0, 2.0)
    } {
      val data      = dataFor(closes.drop(offset).take(windowSize))
      val indicator = makeIndicator(source, middle, length, multiplier)
      withClue(s"$indicator, windowSize=$windowSize, offset=$offset: ") {
        detector.detect(Users.uid, data)(indicator) mustBe fullBandSignal(data, indicator)
      }
    }
  }

  // Retain the original full-band calculation as an oracle: trimming inputs or intermediate transformations changes these results.
  private def fullBandSignal(data: MarketTimeSeriesData, indicator: Indicator): Option[Signal] = {
    val (source, middle, width, multiplier) = indicator match {
      case Indicator.KeltnerChannel(source, middle, length, multiplier) =>
        (source, middle, Volatility.averageTrueRange(data.closings, data.highs, data.lows, length), multiplier)
      case Indicator.BollingerBands(source, middle, length, multiplier) =>
        (source, middle, Statistics.standardDeviation(transformer.extractFrom(data, source), length), multiplier)
      case _ => fail(s"Expected a band indicator, got $indicator")
    }
    val prices     = transformer.extractFrom(data, source)
    val middleLine = transformer.transformTo(prices, data, middle)
    val upper      = middleLine.lazyZip(width).map((mid, spread) => mid + (spread * multiplier))
    val lower      = middleLine.lazyZip(width).map((mid, spread) => mid - (spread * multiplier))
    Condition.bandCrossing(prices, upper, lower).map(signal(data, indicator, _))
  }

  private def signal(data: MarketTimeSeriesData, indicator: Indicator, condition: Condition): Signal =
    Signal(Users.uid, data.currencyPair, data.interval, condition, indicator, data.latestTime)

  private def dataFor(closes: List[Double]): MarketTimeSeriesData = {
    val prices = closes.zipWithIndex.map { (close, index) =>
      PriceRange(close - 0.002, close + 0.004, close - 0.003, close, 1000.0, Instant.EPOCH.minusSeconds(index.toLong * 3600))
    }
    Markets.timeSeriesData.copy(prices = NonEmptyList.fromListUnsafe(prices))
  }
}
