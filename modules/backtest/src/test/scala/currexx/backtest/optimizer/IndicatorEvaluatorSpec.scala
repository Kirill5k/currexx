package currexx.backtest.optimizer

import cats.effect.IO
import currexx.backtest.optimizer.IndicatorEvaluator.ScoringFunction
import currexx.backtest.{MarketDataProvider, TestStrategy}
import kirill5k.common.cats.test.IOWordSpec

class IndicatorEvaluatorSpec extends IOWordSpec {

  private val strategy = TestStrategy.s1_v2_optimized_v2
  private val scoring  = ScoringFunction.robust()

  "IndicatorEvaluator.make" should {

    "hand back a backtest that reproduces the run its fitness came from" in {
      val result = for
        evaluation <- IndicatorEvaluator.make[IO](
          testFilePaths = List(MarketDataProvider.majors1h.head),
          strategy = strategy.rules,
          poolSize = 1,
          scoringFunction = scoring
        )
        scored <- evaluation.evaluator.evaluateIndividual(strategy.indicator)
        stats  <- evaluation.backtest(strategy.indicator)
      yield (scored._2.value, scoring.score(stats))

      // The champion report re-runs the winner through this backtest to say whether it satisfies its constraints. If
      // the replay were configured even slightly differently from the search it would be describing a different run,
      // and the report would be confidently wrong about the candidate that is actually about to be used.
      result.asserting { case (searched, replayed) =>
        replayed mustBe searched
      }
    }
  }
}
