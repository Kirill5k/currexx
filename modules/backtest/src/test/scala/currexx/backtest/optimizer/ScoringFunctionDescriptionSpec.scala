package currexx.backtest.optimizer

import currexx.backtest.OrderStats
import currexx.backtest.types.given
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ScoringFunctionDescriptionSpec extends AnyWordSpec with Matchers {
  "Scoring descriptions" should {
    "record the scorer identity and every named configuration value" in {
      val robustConfig     = ScoringFunction.Robust.Config(minTradesPerMonth = 7)
      val consistentConfig = ScoringFunction.Consistent.Config(periodMonths = 2)
      val scorers          = List(
        ("Robust", robustConfig, ScoringFunction.Robust(robustConfig)),
        ("Consistent", consistentConfig, ScoringFunction.Consistent(consistentConfig))
      )

      scorers.foreach { case (name, config, scoring) =>
        scoring.description must startWith(s"$name(")
        config.productElementNames.zip(config.productIterator).foreach { case (key, value) =>
          scoring.description must include(s"$key=$value")
        }
      }
      ScoringFunction.Robust().description must not be ScoringFunction.Robust(robustConfig).description
      ScoringFunction.Consistent().description must not be ScoringFunction.Consistent(consistentConfig).description
    }

    "remain compatible with custom scorers that do not supply a description" in {
      val scoring = new ScoringFunction {
        override def score(stats: List[OrderStats]): Double                               = 1.0
        override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = Nil
      }

      scoring.description mustBe scoring.getClass.getName
    }
  }
}
