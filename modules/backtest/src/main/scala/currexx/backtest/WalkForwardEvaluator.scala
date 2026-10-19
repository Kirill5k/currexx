package currexx.backtest

import currexx.backtest.optimizer.{OptimisationRounds, SearchObjectiveConfig}
import currexx.backtest.types.given
import currexx.backtest.walkforward.*
import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*

/** Compares objective/seed presets on one fixed chronological development schedule. */
object WalkForwardEvaluator extends IOApp.Simple {
  // Paired objective comparison: preserve the round's base, indicator seeds, budget, costs and upgrade policy.
  val roundName  = "s5_optimized_v2_ga_refine"
  val objectives = List(
    SearchObjectiveConfig.Current,
    SearchObjectiveConfig.BaselineRelative(weight = 0.5)
  )
  val masterSeeds       = List(42L, 43L, 44L)
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
      _ <- IO.println(
        s"$roundName: ${objectives.size * masterSeeds.size} experiments, " +
          s"${objectives.size * masterSeeds.size * validatedPlan.windows.size} window searches"
      )
      runs <- objectives.traverse { objective =>
        masterSeeds.traverse { seed =>
          for
            experiment <- IO(WalkForwardExperiment.create(round.copy(searchObjective = objective), seed, history, validatedPlan))
            store      <- WalkForwardReportStore.make[IO](experiment)
            _          <- IO.println(s"Walk-forward evaluation (${WalkForwardReportRenderer.evidenceLabel}): ${store.directory}")
            results    <- WalkForwardRunner[IO](
              WindowSearch.make[IO](evaluatorPoolSize, outputDirectory = store.directory / "search"),
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
