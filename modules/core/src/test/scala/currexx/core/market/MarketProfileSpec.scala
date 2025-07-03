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
    }
  }
}
