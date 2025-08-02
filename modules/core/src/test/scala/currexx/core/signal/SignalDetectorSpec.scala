package currexx.core.signal

import cats.data.NonEmptyList
import currexx.core.fixtures.{Markets, Users}
import currexx.domain.signal.{Boundary, Condition, Direction, Indicator, ValueSource, ValueTransformation as VT}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SignalDetectorSpec extends AnyWordSpec with Matchers {

  "A SignalDetector" when {

    "detectTrendChange" should {
      val indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.HMA(16))

      "create signal when trend direction changes" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val tcdInd         = indicator.asInstanceOf[Indicator.TrendChangeDetection]
        val signal         = SignalDetector.pure.detectTrendChange(Users.uid, timeSeriesData, tcdInd)

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
        val tcdInd         = indicator.asInstanceOf[Indicator.TrendChangeDetection]
        val signal         = SignalDetector.pure.detectTrendChange(Users.uid, timeSeriesData, tcdInd)

        signal mustBe None
      }
    }

    "detectThresholdCrossing" should {
      val indicator = Indicator.ThresholdCrossing(ValueSource.Close, VT.STOCH(14), 80d, 20d)
      "create signal when current value is below threshold" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)
        val tcInd          = indicator.asInstanceOf[Indicator.ThresholdCrossing]
        val signal         = SignalDetector.pure.detectThresholdCrossing(Users.uid, timeSeriesData, tcInd)

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
        val tcInd          = indicator.asInstanceOf[Indicator.ThresholdCrossing]
        val signal         = SignalDetector.pure.detectThresholdCrossing(Users.uid, timeSeriesData, tcInd)

        signal mustBe None
      }
    }

    "detectComposite with All combinator" should {
      val indicator = Indicator
        .compositeAllOf(
          Indicator.ThresholdCrossing(ValueSource.Close, VT.STOCH(14), 80d, 20d),
          Indicator.TrendChangeDetection(ValueSource.Close, VT.HMA(16))
        )
        .asInstanceOf[Indicator.Composite]

      "return composite condition when all indicators generate signals" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges)

        val signal = SignalDetector.pure.detectComposite(Users.uid, timeSeriesData, indicator)

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

        val signal = SignalDetector.pure.detectComposite(Users.uid, timeSeriesData, indicator)

        signal mustBe None
      }
    }

    "detectComposite with Any combinator" should {
      val indicator = Indicator
        .compositeAnyOf(
          Indicator.ThresholdCrossing(ValueSource.Close, VT.STOCH(14), 80d, 20d),
          Indicator.TrendChangeDetection(ValueSource.Close, VT.HMA(16))
        )
        .asInstanceOf[Indicator.Composite]

      "return composite condition when any of indicators generate signals" in {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(1))

        val signal = SignalDetector.pure.detectComposite(Users.uid, timeSeriesData, indicator)

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

  extension [A](nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
