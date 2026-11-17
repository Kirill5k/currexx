package currexx.core.market

import currexx.core.market.LegacyMarketProfile.*

import cats.data.NonEmptyList
import currexx.core.fixtures.{Markets, Processing, Users}
import currexx.calculations.MomentumOscillators
import currexx.core.signal.SignalDetector
import currexx.core.trade.{Rule, TradeAction}
import currexx.domain.market.{PriceRange, TradeOrder}
import currexx.domain.signal.{Boundary, Condition, Direction, Indicator, ValueRole, ValueSource, ValueTransformation}
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import scala.concurrent.duration.*

class MarketStateSpec extends AnyWordSpec with Matchers with OptionValues {

  val ts: Instant         = Instant.parse("2026-01-02T20:00:00Z")
  val reopenedAt: Instant = ts.plusSeconds(72 * 3600)
  val gap: FiniteDuration = 71.hours

  val state: MarketState = MarketState
    .initial(Users.uid, Markets.gbpeur, ts)
    .copy(
      profile = MarketProfile.fromLegacy(LegacyMarketProfile(trend = Some(TrendState(Direction.Upward, ts)))),
      lastCandleTime = Some(ts)
    )

  "A guarded candle" should {
    "enter oversold and trigger a short exit when the oscillator jumps across both thresholds" in {
      val indicator: Indicator.ThresholdCrossing =
        Indicator.ThresholdCrossing(ValueSource.Close, ValueTransformation.RSX(14), 70.0, 30.0)
      val candleTime = ts.plusSeconds(3600)
      // Alternate small gains and losses to establish overbought momentum, then drop 20 pips in one candle.
      val history = (1 to 40)
        .scanLeft(1.25) { (price, index) =>
          price + (if (index % 2 == 1) 0.00002 else -0.000002)
        }
        .toList
      val closes = history :+ (history.last - 0.002)
      val prices = closes.zipWithIndex.map { (close, index) =>
        val open = if (index == 0) close else closes(index - 1)
        PriceRange(open, open.max(close), open.min(close), close, 1000.0, candleTime.minusSeconds((closes.size - 1 - index) * 3600L))
      }.reverse
      val data        = Markets.timeSeriesData.copy(prices = NonEmptyList.fromListUnsafe(prices))
      val transformed = MomentumOscillators.relativeStrengthIndex(data.closings, 14)

      transformed(1) must be >= indicator.upperBoundary
      transformed.head must be <= indicator.lowerBoundary

      val previous = state.copy(
        currentPosition = Some(PositionState(TradeOrder.Position.Sell, ts)),
        profile = MarketProfile.fromLegacy(
          MarketProfile
            .toLegacy(state.profile)
            .copy(
              momentum = Some(MomentumState(MomentumZone.Overbought, ts)),
              lastMomentumValue = Some(BigDecimal.valueOf(transformed(1)))
            )
        )
      )
      val signal = SignalDetector.pure.detect(Users.uid, data)(indicator).head
      signal.condition mustBe Condition.ThresholdCrossing(30.0, BigDecimal.valueOf(transformed.head), Direction.Downward, Boundary.Lower)

      val batch   = Processing.batch(List(signal)).copy(sources = Processing.sources.updated(indicator.resolvedId.get, indicator))
      val updated = ProfileTransition(previous, batch).state
      updated.profile.momentum() mustBe Some(Reading.Zone(MomentumZone.Oversold, candleTime))
      MarketProfile.toLegacy(updated.profile).lastMomentumValue mustBe None
      updated.previousProfile.flatMap(_.momentum()) mustBe previous.profile.momentum()

      val exitRule = Rule(
        TradeAction.ClosePosition,
        Rule.Condition.allOf(Rule.Condition.positionIsSell, Rule.Condition.momentumEnteredOversold)
      )
      val neutralRule = Rule(TradeAction.OpenShort, Rule.Condition.MomentumEntered(MomentumZone.Neutral))

      Rule.findTriggeredAction(List(exitRule), updated, updated.previousProfile.value) mustBe Some(TradeAction.ClosePosition)
      Rule.findTriggeredAction(List(neutralRule), updated, updated.previousProfile.value) mustBe None
    }

  }

  "MarketState.adjustForMarketClosure" should {
    val profile = MarketProfile.fromLegacy(
      LegacyMarketProfile(
        trend = Some(TrendState(Direction.Upward, ts)),
        momentum = Some(MomentumState(MomentumZone.Overbought, ts)),
        crossover = Some(CrossoverState(Direction.Upward, ts)),
        lastBandCrossing = Some(BandCrossingState(Boundary.Upper, Direction.Upward, ts)),
        lastPriceLineCrossing = Some(PriceLineCrossingState(ValueRole.Price, Direction.Upward, ts)),
        lastClosePrice = Some(BigDecimal("1.25"))
      )
    )
    val withPosition = state.copy(profile = profile, currentPosition = Some(PositionState(TradeOrder.Position.Buy, ts)))

    "shift timestamps, clear crossings and record the candle after a market closure" in {
      val shiftedAt = ts.plusSeconds(71 * 3600)

      withPosition.adjustForMarketClosure(gap, reopenedAt) mustBe (
        withPosition.copy(
          profile = MarketProfile.fromLegacy(
            MarketProfile
              .toLegacy(profile)
              .copy(
                trend = Some(TrendState(Direction.Upward, shiftedAt)),
                momentum = Some(MomentumState(MomentumZone.Overbought, shiftedAt)),
                crossover = None,
                lastBandCrossing = None,
                lastPriceLineCrossing = None
              )
          ),
          currentPosition = Some(PositionState(TradeOrder.Position.Buy, shiftedAt))
        )
      )
    }

    "leave state confirmed after the market reopened untouched" in {
      val later = reopenedAt.plusSeconds(60)
      val fresh = withPosition.copy(
        profile = MarketProfile.fromLegacy(
          MarketProfile
            .toLegacy(profile)
            .copy(
              trend = Some(TrendState(Direction.Downward, later)),
              crossover = Some(CrossoverState(Direction.Downward, later))
            )
        ),
        currentPosition = Some(PositionState(TradeOrder.Position.Sell, later))
      )

      val updated = fresh.adjustForMarketClosure(gap, reopenedAt)

      updated.profile.trend() mustBe Some(Reading.Trend(Direction.Downward, later))
      updated.profile.crossover() mustBe Some(Reading.Crossing(Direction.Downward, later))
      updated.currentPosition mustBe Some(PositionState(TradeOrder.Position.Sell, later))
    }

  }
}
