package currexx.core.signal

import cats.data.NonEmptyList
import currexx.core.fixtures.{Markets, Users}
import currexx.domain.signal.{Boundary, Condition, Direction, Indicator, ValueRole, ValueSource, ValueTransformation as VT, VolatilityRegime}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

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

  extension [A](nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
