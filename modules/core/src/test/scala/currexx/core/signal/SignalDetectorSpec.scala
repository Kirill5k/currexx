package currexx.core.signal

import cats.data.NonEmptyList
import currexx.core.fixtures.{Markets, Users}
import currexx.domain.market.{MarketTimeSeriesData, PriceRange}
import currexx.domain.signal.{
  Boundary,
  Condition,
  Direction,
  Indicator,
  ValueRole,
  ValueSource,
  ValueTransformation as VT,
  VolatilityRegime
}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class SignalDetectorSpec extends AnyWordSpec with Matchers {

  "A SignalDetector" when {

    "detectTrendChange" should {
      val indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.HMA(16))

      "create signal when trend direction changes" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

        signal mustBe Some(
          Signal(
            userId = Users.uid,
            interval = Markets.timeSeriesData.interval,
            currencyPair = Markets.gbpeur,
            condition = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(13)),
            triggeredBy = indicator,
            time = timeSeriesData.prices.head.time
          )
        )
      }

      "not do anything when trend hasn't changed" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

        signal mustBe None
      }
    }

    "detectThresholdCrossing" should {
      val indicator = Indicator.ThresholdCrossing(ValueSource.Close, VT.STOCH(14), 80d, 20d)
      "create signal when current value is below threshold" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

        signal mustBe Some(
          Signal(
            userId = Users.uid,
            interval = Markets.timeSeriesData.interval,
            currencyPair = Markets.gbpeur,
            condition = Condition.ThresholdCrossing(20d, BigDecimal(16.294773928361835), Direction.Downward, Boundary.Lower),
            triggeredBy = indicator,
            time = timeSeriesData.prices.head.time
          )
        )
      }

      "not do anything when current value is within limits" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(10))
        val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

        signal mustBe None
      }
    }

    "detectComposite with All combinator" should {
      val indicator = Indicator.compositeAllOf(
        Indicator.ThresholdCrossing(ValueSource.Close, VT.STOCH(14), 80d, 20d),
        Indicator.TrendChangeDetection(ValueSource.Close, VT.HMA(16))
      )

      "return composite condition when all indicators generate signals" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

        signal mustBe Some(
          Signal(
            userId = Users.uid,
            currencyPair = Markets.gbpeur,
            interval = Markets.timeSeriesData.interval,
            condition = Condition.Composite(
              NonEmptyList.of(
                Condition.ThresholdCrossing(20d, BigDecimal(16.294773928361835), Direction.Downward, Boundary.Lower),
                Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(13))
              )
            ),
            triggeredBy = indicator,
            time = timeSeriesData.prices.head.time
          )
        )
      }

      "not return anything when only one indicator generated signal" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

        signal mustBe None
      }
    }

    "detectComposite with Any combinator" should {
      val indicator = Indicator.compositeAnyOf(
        Indicator.ThresholdCrossing(ValueSource.Close, VT.STOCH(14), 80d, 20d),
        Indicator.TrendChangeDetection(ValueSource.Close, VT.HMA(16))
      )

      "return composite condition when any of indicators generate signals" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(1))
        val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

        signal mustBe Some(
          Signal(
            userId = Users.uid,
            currencyPair = Markets.gbpeur,
            interval = Markets.timeSeriesData.interval,
            condition = Condition.Composite(
              NonEmptyList.of(
                Condition.ThresholdCrossing(20d, BigDecimal(32.868467410452254), Direction.Upward, Boundary.Lower)
              )
            ),
            triggeredBy = indicator,
            time = timeSeriesData.prices.head.time
          )
        )
      }
    }
  }

  "detectLinesCrossing" should {
    val indicator = Indicator.LinesCrossing(ValueSource.Close, VT.SMA(5), VT.SMA(2))

    "create signal when lines cross" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(1))
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe Some(
        Signal(
          userId = Users.uid,
          interval = Markets.timeSeriesData.interval,
          currencyPair = Markets.gbpeur,
          condition = Condition.LinesCrossing(Direction.Downward),
          triggeredBy = indicator,
          time = timeSeriesData.prices.head.time
        )
      )
    }

    "not create signal when lines have not crossed" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(4))
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe None
    }
  }

  "detectKeltnerChannel" should {
    val indicator = Indicator.KeltnerChannel(ValueSource.Close, VT.EMA(20), 14, 1.0)

    "create signal when price crosses a channel band" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(9))
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe Some(
        Signal(
          userId = Users.uid,
          interval = Markets.timeSeriesData.interval,
          currencyPair = Markets.gbpeur,
          condition = Condition.LowerBandCrossing(Direction.Downward),
          triggeredBy = indicator,
          time = timeSeriesData.prices.head.time
        )
      )
    }

    "not create signal when price is within the channel" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(10))
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe None
    }
  }

  "detectVolatilityRegimeDetection" should {
    val indicator = Indicator.VolatilityRegimeDetection(5, VT.SMA(5))

    "create signal when volatility regime changes" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe Some(
        Signal(
          userId = Users.uid,
          interval = Markets.timeSeriesData.interval,
          currencyPair = Markets.gbpeur,
          condition = Condition.VolatilityRegimeChange(Some(VolatilityRegime.High), VolatilityRegime.Low),
          triggeredBy = indicator,
          time = timeSeriesData.prices.head.time
        )
      )
    }

    "not create signal when volatility regime has not changed" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(3))
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe None
    }
  }

  "detectPriceLineCrossing" should {
    val indicator = Indicator.PriceLineCrossing(ValueSource.Close, ValueRole.Momentum, VT.SMA(5))

    "create signal when price crosses the line" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe Some(
        Signal(
          userId = Users.uid,
          interval = Markets.timeSeriesData.interval,
          currencyPair = Markets.gbpeur,
          condition = Condition.PriceCrossedLine(ValueRole.Momentum, Direction.Downward),
          triggeredBy = indicator,
          time = timeSeriesData.prices.head.time
        )
      )
    }

    "not create signal when price has not crossed the line" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe None
    }
  }

  "detectBollingerBands" should {
    val indicator = Indicator.BollingerBands(ValueSource.Close, VT.SMA(20), 20, 1.0)

    "create signal when price crosses a Bollinger band" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe Some(
        Signal(
          userId = Users.uid,
          interval = Markets.timeSeriesData.interval,
          currencyPair = Markets.gbpeur,
          condition = Condition.LowerBandCrossing(Direction.Downward),
          triggeredBy = indicator,
          time = timeSeriesData.prices.head.time
        )
      )
    }

    "not create signal when price is within the Bollinger bands" in {
      val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
      val signal         = SignalDetector.pure.detect(Users.uid, timeSeriesData)(indicator)

      signal mustBe None
    }
  }

  "band boundaries" should {
    "include equality at the current band and prefer the upper band when both bands cross" in {
      val cases = List(
        (List(1.5, 1.0, 2.0), 0.0, Direction.Upward),
        (List(1.5, 2.0, 1.0), 0.0, Direction.Downward),
        (List(3.0, 1.0, 2.0), 0.1, Direction.Upward),
        (List(0.0, 2.0, 1.0), 0.1, Direction.Downward)
      )
      cases.foreach { (closes, multiplier, direction) =>
        val data = dataFor(closes)
        // EMA(3) of the first two cases is [1.5, 1.5, oldest]: the current price is exactly on both zero-width bands.
        bandIndicators(multiplier).foreach { indicator =>
          withClue(s"$indicator on $closes: ") {
            SignalDetector.pure.detect(Users.uid, data)(indicator) mustBe
              Some(expectedSignal(data, indicator, Condition.UpperBandCrossing(direction)))
          }
        }
      }
    }

    "use older prices to calculate the current middle band" in {
      val data = dataFor(List(4.0, 2.0, 10.0, 0.0))
      // The full EMA(3) is [3.75, 3.5, 5, 0], so price crosses upward from 2 < 3.5 to 4 > 3.75.
      bandIndicators(0.0).foreach { indicator =>
        SignalDetector.pure.detect(Users.uid, data)(indicator) mustBe
          Some(expectedSignal(data, indicator, Condition.UpperBandCrossing(Direction.Upward)))
        SignalDetector.pure.detect(Users.uid, dataFor(List(4.0, 2.0)))(indicator) mustBe None
      }
    }

    "produce no crossing from a single price or from equality at the preceding band" in {
      for {
        closes    <- List(List(2.0), List(2.0, 1.0))
        indicator <- bandIndicators(0.0)
      } SignalDetector.pure.detect(Users.uid, dataFor(closes))(indicator) mustBe None
    }
  }

  "crossing boundaries" should {
    "include equality at the latest line and keep the price-line role" in
      List((List(1.5, 1.0, 2.0), Direction.Upward), (List(1.5, 2.0, 1.0), Direction.Downward)).foreach { (closes, direction) =>
        val data  = dataFor(closes)
        val lines = Indicator.LinesCrossing(ValueSource.Close, VT.SMA(1), VT.EMA(3))
        val price = Indicator.PriceLineCrossing(ValueSource.Close, ValueRole.ChannelMiddleBand, VT.EMA(3))
        SignalDetector.pure.detect(Users.uid, data)(lines) mustBe
          Some(expectedSignal(data, lines, Condition.LinesCrossing(direction)))
        SignalDetector.pure.detect(Users.uid, data)(price) mustBe
          Some(expectedSignal(data, price, Condition.PriceCrossedLine(ValueRole.ChannelMiddleBand, direction)))
      }
  }

  "trend duration" should {
    "count the preceding trend through the oldest observation and stop at a plateau" in {
      val indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(1))
      val cases     = List(
        (List(4.0, 5.0, 3.0, 2.0, 1.0), Direction.Upward, Direction.Downward, 4),
        (List(4.0, 5.0, 3.0, 3.0, 1.0), Direction.Upward, Direction.Downward, 2),
        (List(2.0, 1.0, 3.0, 4.0, 5.0), Direction.Downward, Direction.Upward, 4)
      )
      cases.foreach { (closes, from, to, duration) =>
        val data = dataFor(closes)
        SignalDetector.pure.detect(Users.uid, data)(indicator) mustBe
          Some(expectedSignal(data, indicator, Condition.TrendDirectionChange(from, to, Some(duration))))
      }
      List(List(4.0, 3.0), List(4.0, 4.0, 3.0)).foreach { closes =>
        SignalDetector.pure.detect(Users.uid, dataFor(closes))(indicator) mustBe None
      }
    }
  }

  "nested composites" should {
    "retain child order and nesting while omitting children without a signal" in {
      val data      = dataFor(List(4.0, 2.0, 10.0, 0.0))
      val indicator = Indicator.compositeAnyOf(
        Indicator.ThresholdCrossing(ValueSource.Close, VT.SMA(1), 20.0, -20.0),
        Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1)),
        Indicator.compositeAllOf(
          Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.SMA(2)),
          Indicator.ThresholdCrossing(ValueSource.Close, VT.SMA(1), 3.0, 1.0)
        ),
        Indicator.ValueTracking(ValueRole.Velocity, ValueSource.Close, VT.EMA(3))
      )
      val condition = Condition.Composite(
        NonEmptyList.of(
          Condition.ValueUpdated(ValueRole.Price, BigDecimal(4)),
          Condition.Composite(
            NonEmptyList.of(
              Condition.ValueUpdated(ValueRole.Momentum, BigDecimal(3)),
              Condition.ThresholdCrossing(BigDecimal(3), BigDecimal(4), Direction.Upward, Boundary.Upper)
            )
          ),
          Condition.ValueUpdated(ValueRole.Velocity, BigDecimal(3.75))
        )
      )
      SignalDetector.pure.detect(Users.uid, data)(indicator) mustBe Some(expectedSignal(data, indicator, condition))
    }
  }

  private def bandIndicators(multiplier: Double): List[Indicator] = List(
    Indicator.KeltnerChannel(ValueSource.Close, VT.EMA(3), 2, multiplier),
    Indicator.BollingerBands(ValueSource.Close, VT.EMA(3), 3, multiplier)
  )

  private def dataFor(closes: List[Double]): MarketTimeSeriesData = {
    val prices = closes.zipWithIndex.map { (close, index) =>
      PriceRange(close - 0.002, close + 0.004, close - 0.003, close, 1000.0, Instant.EPOCH.minusSeconds(index.toLong * 3600))
    }
    Markets.timeSeriesData.copy(prices = NonEmptyList.fromListUnsafe(prices))
  }

  private def expectedSignal(data: MarketTimeSeriesData, indicator: Indicator, condition: Condition): Signal =
    Signal(Users.uid, data.currencyPair, data.interval, condition, indicator, data.latestTime)

  extension [A](nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
