package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*
import currexx.backtest.optimizer.SearchObjectiveConfig
import currexx.backtest.walkforward.*

/** Compares objective/seed presets on one fixed chronological development schedule. */
object WalkForwardEvaluator extends IOApp.Simple {
  val roundName         = "s13_ga_refine"
  val objectives        = List(SearchObjectiveConfig.Current)
  val masterSeeds       = List(42L)
  val history           = MarketDataProvider.majors1hHistory
  val plan              = WalkForwardPlan.default
  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors()

  override def run: IO[Unit] =
    for
      round         <- IO.fromEither(OptimisationRounds.findByName(roundName))
      validatedPlan <- IO.fromEither(plan)
      _             <- IO.raiseUnless(objectives.nonEmpty && masterSeeds.nonEmpty)(
        new IllegalArgumentException("Choose an objective and a master seed")
      )
      runs <- objectives.traverse { objective =>
        masterSeeds.traverse { seed =>
          for
            experiment <- IO(WalkForwardExperiment.create(round.copy(searchObjective = objective), seed, history, validatedPlan))
            store      <- WalkForwardReportStore.make[IO](experiment)
            _          <- IO.println(s"Walk-forward evaluation (${WalkForwardReportRenderer.evidenceLabel}): ${store.directory}")
            results    <- WalkForwardRunner[IO](
              WindowSearch.make[IO](evaluatorPoolSize),
              ForwardEvaluator.make[IO](evaluatorPoolSize),
              store.events
            )
              .run(experiment)
            _ <- IO.println(s"Completed. Report: ${store.directory / "report.md"}")
          yield WalkForwardComparison.Run(experiment, WalkForwardSummary.from(results))
        }
      }
      comparison <- WalkForwardReportStore.writeComparison[IO](runs.flatten)
      _          <- IO.println(s"Comparison: ${comparison / "report.md"}")
    yield ()
}
