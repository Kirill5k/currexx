package currexx.core.market

import currexx.core.market.LegacyMarketProfile.*

import currexx.core.fixtures.{Indicators, Signals}
import currexx.domain.signal.{Boundary, Direction, Indicator, IndicatorId, ValueRole, ValueSource, ValueTransformation}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class MarketProfileSpec extends AnyWordSpec with Matchers {

  "MarketProfile legacy compatibility" should {
    "mirror only configurations representable by the old model" in {
      val cmf      = Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, ValueTransformation.CMF(20))
      val momentum = IndicatorId.forValue(ValueRole.Momentum)
      MarketProfile.legacyCompatible(Map(momentum -> cmf)) mustBe true
      MarketProfile.legacyCompatible(Map(momentum -> Indicator.Named(momentum, cmf))) mustBe false
      val lines = List(ValueRole.Momentum, ValueRole.Volatility).map(role =>
        Indicator.PriceLineCrossing(ValueSource.Close, role, ValueTransformation.SMA(20))
      )
      MarketProfile.legacyCompatible(lines.flatMap(i => i.resolvedId.map(_ -> i)).toMap) mustBe false
    }
  }

  "A MarketProfileUpdater" when {
    "update" should {
      "update trend" in {
        val signal  = Signals.trend(Direction.Upward)
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        profile.trend() mustBe Some(Reading.Trend(Direction.Upward, signal.time))
      }

      "not update anything when trend is the same" in {
        val signal  = Signals.trend(Direction.Upward)
        val profile =
          MarketProfile.fromLegacy(LegacyMarketProfile(trend = Some(TrendState(Direction.Upward, Instant.parse("2023-10-01T00:00:00Z")))))
        val updatedProfile = MarketProfileUpdater.update(profile, signal)
        profile mustBe updatedProfile
      }

      "update crossover" in {
        val signal  = Signals.crossover(Direction.Downward)
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        profile.crossover() mustBe Some(Reading.Crossing(Direction.Downward, signal.time))
      }

      "leave historical composite signals unapplied" in {
        val signal = Signals.composite(
          conditions = List(
            Signals.linesCrossing,
            Signals.trendDirectionChange
          ),
          triggers = List(
            Indicators.linesCrossing,
            Indicators.trendChangeDetection
          )
        )
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)

        profile mustBe MarketProfile()
      }

      "update momentum state for AboveThreshold condition" in {
        val signal  = Signals.aboveThreshold()
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        profile.momentum() mustBe Some(Reading.Zone(MomentumZone.Overbought, signal.time))
        MarketProfile.toLegacy(profile).lastMomentumValue mustBe None
      }

      "update momentum state for BelowThreshold condition" in {
        val signal  = Signals.belowThreshold()
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        profile.momentum() mustBe Some(Reading.Zone(MomentumZone.Oversold, signal.time))
        MarketProfile.toLegacy(profile).lastMomentumValue mustBe None
      }

      "update band crossing state for UpperBandCrossing with upward direction" in {
        val signal  = Signals.upperBandCrossing(Direction.Upward)
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        profile.band() mustBe Some(Reading.BandCrossing(Boundary.Upper, Direction.Upward, signal.time))
      }

      "update band crossing state for UpperBandCrossing with downward direction" in {
        val signal  = Signals.upperBandCrossing(Direction.Downward)
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        profile.band() mustBe Some(Reading.BandCrossing(Boundary.Upper, Direction.Downward, signal.time))
      }

      "update band crossing state for LowerBandCrossing with upward direction" in {
        val signal  = Signals.lowerBandCrossing(Direction.Upward)
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        profile.band() mustBe Some(Reading.BandCrossing(Boundary.Lower, Direction.Upward, signal.time))
      }

      "update band crossing state for LowerBandCrossing with downward direction" in {
        val signal  = Signals.lowerBandCrossing(Direction.Downward)
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        profile.band() mustBe Some(Reading.BandCrossing(Boundary.Lower, Direction.Downward, signal.time))
      }

      "replace previous band crossing state when a new band crossing occurs" in {
        val signal1 = Signals.upperBandCrossing(Direction.Upward)
        val signal2 = Signals.lowerBandCrossing(Direction.Downward)

        val profile1 = MarketProfileUpdater.update(MarketProfile(), signal1)
        profile1.band() mustBe Some(Reading.BandCrossing(Boundary.Upper, Direction.Upward, signal1.time))

        val profile2 = MarketProfileUpdater.update(profile1, signal2)
        profile2.band() mustBe Some(
          Reading.BandCrossing(Boundary.Lower, Direction.Downward, signal2.time)
        )
      }

      "update price line crossing state for PriceCrossedLine with upward direction" in {
        val signal  = Signals.priceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward)
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        MarketProfile.toLegacy(profile).lastPriceLineCrossing mustBe Some(
          PriceLineCrossingState(ValueRole.ChannelMiddleBand, Direction.Upward, signal.time)
        )
      }

      "update price line crossing state for PriceCrossedLine with downward direction" in {
        val signal  = Signals.priceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Downward)
        val profile = MarketProfileUpdater.update(MarketProfile(), signal)
        MarketProfile.toLegacy(profile).lastPriceLineCrossing mustBe Some(
          PriceLineCrossingState(ValueRole.ChannelMiddleBand, Direction.Downward, signal.time)
        )
      }

      "update price line crossing state for different ValueRole types" in {
        val signal1  = Signals.priceCrossedLine(ValueRole.Momentum, Direction.Upward)
        val profile1 = MarketProfileUpdater.update(MarketProfile(), signal1)
        MarketProfile.toLegacy(profile1).lastPriceLineCrossing mustBe Some(
          PriceLineCrossingState(ValueRole.Momentum, Direction.Upward, signal1.time)
        )

        val signal2  = Signals.priceCrossedLine(ValueRole.Volatility, Direction.Downward, time = signal1.time.plusSeconds(1))
        val profile2 = MarketProfileUpdater.update(MarketProfile(), signal2)
        MarketProfile.toLegacy(profile2).lastPriceLineCrossing mustBe Some(
          PriceLineCrossingState(ValueRole.Volatility, Direction.Downward, signal2.time)
        )
      }

      "replace previous price line crossing state when a new crossing occurs" in {
        val signal1 = Signals.priceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Upward)
        val signal2 = Signals.priceCrossedLine(ValueRole.ChannelMiddleBand, Direction.Downward)

        val profile1 = MarketProfileUpdater.update(MarketProfile(), signal1)
        MarketProfile.toLegacy(profile1).lastPriceLineCrossing mustBe Some(
          PriceLineCrossingState(ValueRole.ChannelMiddleBand, Direction.Upward, signal1.time)
        )

        val profile2 = MarketProfileUpdater.update(profile1, signal2)
        MarketProfile.toLegacy(profile2).lastPriceLineCrossing mustBe Some(
          PriceLineCrossingState(ValueRole.ChannelMiddleBand, Direction.Downward, signal2.time)
        )
      }
    }
  }
}
