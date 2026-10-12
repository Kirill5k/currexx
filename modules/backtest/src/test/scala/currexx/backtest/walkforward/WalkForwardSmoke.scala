package currexx.backtest.walkforward

import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*
import currexx.algorithms.Parameters
import currexx.backtest.{OptimisationRounds, WalkForwardEvaluator}
import fs2.io.file.Path

/** Preflight the configured history, then check its objective/seed matrix with a small separate search budget. */
object WalkForwardSmoke extends IOApp.Simple:
  override def run: IO[Unit] =
    for
      base <- IO.fromEither(OptimisationRounds.findByName(WalkForwardEvaluator.roundName))
      _    <- IO.raiseUnless(WalkForwardEvaluator.objectives.nonEmpty && WalkForwardEvaluator.masterSeeds.nonEmpty)(
        new IllegalArgumentException("Choose an objective and a master seed")
      )
      defaultPlan <- IO.fromEither(WalkForwardEvaluator.plan)
      ready       <- WalkForwardPreflight.inspect[IO](WalkForwardEvaluator.history, defaultPlan)
      _           <- IO.println(s"Full history preflight passed: ${WalkForwardEvaluator.history.size} pairs, ${ready.size} windows")
      plan        <- IO.fromEither(WalkForwardPlan.validate(defaultPlan.windows.take(2)))
      round = base.copy(parameters = Parameters.GA(6, 1, 0.7, 0.1, 0.5, shuffle = false), shortlistSize = 6)
      root  = Path("target/walk-forward-smoke")
      runs <- WalkForwardEvaluator.objectives.traverse { objective =>
        WalkForwardEvaluator.masterSeeds.traverse { seed =>
          for
            experiment <- IO(
              WalkForwardExperiment.create(round.copy(searchObjective = objective), seed, WalkForwardEvaluator.history.take(1), plan)
            )
            store   <- WalkForwardReportStore.make[IO](experiment, root)
            results <- new WalkForwardRunner(WindowSearch.make[IO](2), ForwardEvaluator.make[IO](2), store.events).run(experiment)
            _       <- IO.raiseUnless(results.size == plan.windows.size && results.forall(_.forward.coverage.size == 1))(
              new IllegalStateException("Smoke experiment did not complete every paired test")
            )
            _ <- IO.raiseUnless(results.forall { result =>
              result.selection.outcome != SelectionOutcome.BaseRetained || result.forward.netDifference == 0
            })(new IllegalStateException("A retained base produced a different paired result"))
            _ <- IO.println(s"Smoke experiment completed: ${store.directory / "report.md"}")
          yield WalkForwardComparison.Run(experiment, WalkForwardSummary.from(results))
        }
      }
      comparison <- WalkForwardReportStore.writeComparison[IO](runs.flatten, root)
      _          <- IO.println(s"Smoke comparison completed: ${comparison / "report.md"}")
    yield ()
