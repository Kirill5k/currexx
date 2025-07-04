package currexx.core.market

import currexx.core.fixtures.{Indicators, Signals}
import currexx.domain.signal.Direction
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MarketProfileSpec extends AnyWordSpec with Matchers {

  "A MarketProfile" when {
    "update" should {
      "update trend" in {
        val signal  = Signals.trend(Direction.Upward)
        val profile = MarketProfile().update(signal)
        profile.trend mustBe Some(TrendState(Direction.Upward, signal.time))
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
    }
  }
}
