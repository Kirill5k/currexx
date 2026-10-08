package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.backtest.walkforward.*

/** Runs one fixed named search preset across the chronological development schedule. */
object WalkForwardEvaluator extends IOApp.Simple {
  val roundName         = "s13_ga_refine"
  val masterSeed        = 42L
  val history           = MarketDataProvider.majors1hHistory
  val plan              = WalkForwardPlan.default
  val evaluatorPoolSize = Runtime.getRuntime.availableProcessors()

  override def run: IO[Unit] =
    for
      round         <- IO.fromEither(OptimisationRounds.findByName(roundName))
      validatedPlan <- IO.fromEither(plan)
      experiment    <- IO(WalkForwardExperiment.create(round, masterSeed, history, validatedPlan))
      store         <- WalkForwardReportStore.make[IO](experiment)
      _             <- IO.println(s"Walk-forward evaluation (${WalkForwardReportRenderer.evidenceLabel}): ${store.directory}")
      _ <- WalkForwardRunner[IO](WindowSearch.make[IO](evaluatorPoolSize), ForwardEvaluator.make[IO](evaluatorPoolSize), store.events)
        .run(experiment)
      _ <- IO.println(s"Completed. Report: ${store.directory / "report.md"}")
    yield ()
}
