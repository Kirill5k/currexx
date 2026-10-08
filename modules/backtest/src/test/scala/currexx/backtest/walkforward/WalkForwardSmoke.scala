package currexx.backtest.walkforward

import cats.effect.{IO, IOApp}
import currexx.algorithms.Parameters
import currexx.backtest.{MarketDataProvider, OptimisationRounds}
import fs2.io.file.Path

/** Small real-data wiring check, deliberately separate from the production experiment presets. */
object WalkForwardSmoke extends IOApp.Simple:
  override def run: IO[Unit] =
    for
      base <- IO.fromEither(OptimisationRounds.findByName("s13_ga_refine"))
      round = base.copy(parameters = Parameters.GA(2, 1, 0.7, 0.1, 0.5, shuffle = false), extraSeeds = Nil, shortlistSize = 2)
      defaultPlan <- IO.fromEither(WalkForwardPlan.default)
      plan        <- IO.fromEither(WalkForwardPlan.validate(defaultPlan.windows.take(2)))
      experiment  <- IO(WalkForwardExperiment.create(round, 42L, MarketDataProvider.majors1hHistory.take(1), plan))
      store       <- WalkForwardReportStore.make[IO](experiment, Path("target/walk-forward-smoke"))
      results     <- new WalkForwardRunner(WindowSearch.make[IO](2), ForwardEvaluator.make[IO](2), store.events).run(experiment)
      _           <- IO.raiseUnless(results.size == 2 && results.forall(_.forward.coverage.size == 1))(
        new IllegalStateException("Smoke experiment did not complete both paired tests")
      )
      _ <- IO.println(s"Smoke experiment completed: ${store.directory / "report.md"}")
    yield ()
