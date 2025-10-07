package currexx.core.market

import currexx.core.fixtures.{Indicators, Signals}
import currexx.domain.signal.{Boundary, Direction}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class MarketProfileSpec extends AnyWordSpec with Matchers {

  "A MarketProfile" when {
    "update" should {
      "update trend" in {
        val signal  = Signals.trend(Direction.Upward)
        val profile = MarketProfile().update(signal)
        profile.trend mustBe Some(TrendState(Direction.Upward, signal.time))
      }

      "not update anything when trend is the same" in {
        val signal         = Signals.trend(Direction.Upward)
        val profile        = MarketProfile(trend = Some(TrendState(Direction.Upward, Instant.parse("2023-10-01T00:00:00Z"))))
        val updatedProfile = profile.update(signal)
        profile mustBe updatedProfile
      }

      "update crossover" in {
        val signal  = Signals.crossover(Direction.Downward)
        val profile = MarketProfile().update(signal)
        profile.crossover mustBe Some(CrossoverState(Direction.Downward, signal.time))
      }

      "process composite signal" in {
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
        val profile = MarketProfile().update(signal)

        profile.trend mustBe Some(TrendState(Direction.Upward, signal.time))
        profile.crossover mustBe Some(CrossoverState(Direction.Upward, signal.time))
      }

      "update momentum state for AboveThreshold condition" in {
        val signal  = Signals.aboveThreshold()
        val profile = MarketProfile().update(signal)
        profile.momentum mustBe Some(MomentumState(MomentumZone.Overbought, signal.time))
        profile.lastMomentumValue mustBe Some(85.0)
      }

      "update momentum state for BelowThreshold condition" in {
        val signal  = Signals.belowThreshold()
        val profile = MarketProfile().update(signal)
        profile.momentum mustBe Some(MomentumState(MomentumZone.Oversold, signal.time))
        profile.lastMomentumValue mustBe Some(15.0)
      }

      "update band crossing state for UpperBandCrossing with upward direction" in {
        val signal  = Signals.upperBandCrossing(Direction.Upward)
        val profile = MarketProfile().update(signal)
        profile.lastBandCrossing mustBe Some(BandCrossingState(Boundary.Upper, Direction.Upward, signal.time))
      }

      "update band crossing state for UpperBandCrossing with downward direction" in {
        val signal  = Signals.upperBandCrossing(Direction.Downward)
        val profile = MarketProfile().update(signal)
        profile.lastBandCrossing mustBe Some(BandCrossingState(Boundary.Upper, Direction.Downward, signal.time))
      }

      "update band crossing state for LowerBandCrossing with upward direction" in {
        val signal  = Signals.lowerBandCrossing(Direction.Upward)
        val profile = MarketProfile().update(signal)
        profile.lastBandCrossing mustBe Some(BandCrossingState(Boundary.Lower, Direction.Upward, signal.time))
      }

      "update band crossing state for LowerBandCrossing with downward direction" in {
        val signal  = Signals.lowerBandCrossing(Direction.Downward)
        val profile = MarketProfile().update(signal)
        profile.lastBandCrossing mustBe Some(BandCrossingState(Boundary.Lower, Direction.Downward, signal.time))
      }

      "replace previous band crossing state when a new band crossing occurs" in {
        val signal1 = Signals.upperBandCrossing(Direction.Upward)
        val signal2 = Signals.lowerBandCrossing(Direction.Downward)
        
        val profile1 = MarketProfile().update(signal1)
        profile1.lastBandCrossing mustBe Some(BandCrossingState(Boundary.Upper, Direction.Upward, signal1.time))
        
        val profile2 = profile1.update(signal2)
        profile2.lastBandCrossing mustBe Some(BandCrossingState(Boundary.Lower, Direction.Downward, signal2.time))
      }
    }
  }
}
