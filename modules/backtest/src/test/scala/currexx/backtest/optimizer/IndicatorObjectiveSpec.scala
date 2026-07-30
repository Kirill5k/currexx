package currexx.backtest.optimizer

import cats.effect.IO
import currexx.backtest.{MarketDataProvider, TestStrategy}
import kirill5k.common.cats.test.IOWordSpec

class IndicatorObjectiveSpec extends IOWordSpec {

  private val strategy = TestStrategy.s1_v2_optimized_v2
  private val scoring  = ScoringFunction.Robust()

  "IndicatorObjective.make" should {

    "hand back a backtest that reproduces the run its fitness came from" in {
      val result = for
        objective <- IndicatorObjective.make[IO](
          trainingData = List(MarketDataProvider.majors1h.head),
          strategy = strategy.rules,
          poolSize = 1,
          shortlistSize = 25,
          scoringFunction = scoring
        )
        scored <- objective.evaluator.evaluateIndividual(strategy.indicator)
        stats  <- objective.backtest(strategy.indicator)
      yield (scored._2.value, scoring.score(stats))

      // The champion report re-runs the winner through this backtest to say whether it satisfies its constraints. If
      // the replay were configured even slightly differently from the search it would be describing a different run,
      // and the report would be confidently wrong about the candidate that is actually about to be used.
      result.asserting { case (searched, replayed) =>
        replayed mustBe searched
      }
    }

    "run the searched and the validated backtest over their own halves of the data" in {
      // The point of the split is that a champion is ranked on months no candidate was ever scored against, so these
      // two have to disagree about what period they are describing. Sharing a services pool is what makes that worth
      // asserting: were the validation run to inherit any of the training run's data the two would silently converge
      // on the same answer, and selection would be ranking finalists on the sample that produced them.
      val result = for
        objective <- IndicatorObjective.make[IO](
          trainingData = List(MarketDataProvider.majors1hTraining.head),
          strategy = strategy.rules,
          poolSize = 1,
          shortlistSize = 25,
          validationData = List(MarketDataProvider.majors1hValidation.head),
          scoringFunction = scoring
        )
        trained   <- objective.backtest(strategy.indicator)
        validated <- objective.validate(strategy.indicator)
      yield (trained.flatMap(_.dataWindow), validated.flatMap(_.dataWindow))

      result.asserting { case (trained, validated) =>
        trained must have size 1
        validated must have size 1
        trained.head.to.isBefore(validated.head.from) mustBe true
      }
    }
  }
}
