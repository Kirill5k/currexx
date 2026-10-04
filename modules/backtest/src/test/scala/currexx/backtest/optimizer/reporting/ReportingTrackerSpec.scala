package currexx.backtest.optimizer.reporting

import cats.effect.{IO, Ref}
import currexx.algorithms.{EvaluationPhase, Fitness, Parameters, ValidatedPopulation}
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

class ReportingTrackerSpec extends IOWordSpec {
  private val indicator: Indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(10))

  "ReportingTracker" should {
    "forward every progress event unchanged while emitting stable fitness at the configured cadence" in {
      val result = for {
        diagnostics <- RunDiagnostics.make[IO]
        events      <- Ref.of[IO, List[Progress[Indicator]]](Nil)
        notes       <- Ref.of[IO, List[(String, List[String])]](Nil)
        delegate = new Tracker[IO, Indicator] {
          def displayInitial(target: Indicator, params: Parameters[?]): IO[Unit] = IO.unit
          def displayProgress(progress: Progress[Indicator]): IO[Unit]           = events.update(_ :+ progress)
          def displayFinal(population: ValidatedPopulation[Indicator]): IO[Unit] = IO.unit
          def displayNote(title: String, lines: List[String]): IO[Unit]          = notes.update(_ :+ (title -> lines))
        }
        _ <- diagnostics.observed(indicator, EvaluationPhase.Search(3), List(1.0, 1.0, 1.0))
        tracker = new ReportingTracker(delegate, diagnostics, foldCount = 6)
        ninth   = Progress.Population(9, 150, Vector(indicator -> Fitness(0.7)))
        tenth   = Progress.Population(10, 150, Vector(indicator -> Fitness(0.2)))
        _         <- tracker.displayProgress(ninth)
        _         <- tracker.displayProgress(tenth)
        forwarded <- events.get
        written   <- notes.get
      } yield (forwarded, written, ninth, tenth)

      result.asserting { case (forwarded, written, ninth, tenth) =>
        forwarded mustBe List(ninth, tenth)
        written.map(_._1) mustBe List("Stable progress: generation 10")
        written.head._2 must contain("Excluded search fold for selection: 5")
        written.head._2 must contain("Best all-fold fitness seen: 1.000000; first seen generation 3.")
      }
    }

    "describe one-fold progress without claiming a fold was withheld" in {
      OptimisationReportRenderer.progress(RunDiagnostics.Snapshot(), 10, 1) must contain("Excluded search fold for selection: none")
    }
  }
}
