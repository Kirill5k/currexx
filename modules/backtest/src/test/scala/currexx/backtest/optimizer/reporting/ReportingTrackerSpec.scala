package currexx.backtest.optimizer.reporting

import cats.effect.{IO, Ref}
import currexx.algorithms.{EvaluationPhase, Fitness, Parameters, ValidatedPopulation}
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.backtest.MarketDataProvider.Corpus
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.concurrent.duration.*

class ReportingTrackerSpec extends IOWordSpec {
  private val indicator: Indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(10))
  private val report               = OptimisationReport(
    "report-test",
    Corpus(Nil),
    Vector.empty,
    Nil,
    Map.empty,
    Map.empty,
    RunDiagnostics.Snapshot(),
    RunDiagnostics.Workload(),
    1.second,
    20.millis
  )

  private def noteTracker(write: (String, List[String]) => IO[Unit]): Tracker[IO, Indicator] = new Tracker[IO, Indicator] {
    def displayInitial(target: Indicator, params: Parameters[?]): IO[Unit] = IO.unit
    def displayProgress(progress: Progress[Indicator]): IO[Unit]           = IO.unit
    def displayFinal(population: ValidatedPopulation[Indicator]): IO[Unit] = IO.unit
    def displayNote(title: String, lines: List[String]): IO[Unit]          = write(title, lines)
  }

  "ReportingTracker" should {
    "forward completed report sections in order" in {
      val result = for {
        diagnostics <- RunDiagnostics.make[IO]
        notes       <- Ref.of[IO, List[(String, List[String])]](Nil)
        tracker = new ReportingTracker(noteTracker((title, lines) => notes.update(_ :+ (title -> lines))), diagnostics, foldCount = 1)
        _       <- tracker.displayReport(report)
        written <- notes.get
      } yield written

      result.asserting { written =>
        written mustBe OptimisationReportRenderer.sections(report)
      }
    }

    "write an incomplete diagnostics note when asked to report a failure" in {
      val failure = new RuntimeException("replay failed")
      val result  = for {
        diagnostics <- RunDiagnostics.make[IO]
        notes       <- Ref.of[IO, List[(String, List[String])]](Nil)
        tracker = new ReportingTracker(noteTracker((title, lines) => notes.update(_ :+ (title -> lines))), diagnostics, foldCount = 1)
        _       <- tracker.displayReportFailure(report.roundName, failure)
        written <- notes.get
      } yield written

      result.asserting { written =>
        written.map(_._1) mustBe List("Incomplete diagnostics: report-test")
        written.head._2 must contain("Optimisation results above are complete; diagnostic reporting failed. Remaining rounds can continue.")
        written.head._2 must contain(failure.toString)
      }
    }

    "propagate a failed section write without writing subsequent sections or a failure note" in {
      val failure  = new RuntimeException("section write failed")
      val sections = OptimisationReportRenderer.sections(report)
      val result   = for {
        diagnostics <- RunDiagnostics.make[IO]
        notes       <- Ref.of[IO, List[(String, List[String])]](Nil)
        delegate = noteTracker { (title, lines) =>
          notes.update(_ :+ (title -> lines)) >> (if (title == sections(1)._1) IO.raiseError(failure) else IO.unit)
        }
        tracker = new ReportingTracker(delegate, diagnostics, foldCount = 1)
        outcome <- tracker.displayReport(report).attempt
        written <- notes.get
      } yield (outcome, written)

      result.asserting { case (outcome, written) =>
        outcome mustBe Left(failure)
        written mustBe sections.take(2)
      }
    }

    "propagate a failed incomplete diagnostics note write" in {
      val reportFailure = new RuntimeException("replay failed")
      val sinkFailure   = new RuntimeException("sink unavailable")
      val result        = for {
        diagnostics <- RunDiagnostics.make[IO]
        notes       <- Ref.of[IO, List[(String, List[String])]](Nil)
        delegate = noteTracker((title, lines) => notes.update(_ :+ (title -> lines)) >> IO.raiseError(sinkFailure))
        tracker  = new ReportingTracker(delegate, diagnostics, foldCount = 1)
        outcome <- tracker.displayReportFailure(report.roundName, reportFailure).attempt
        written <- notes.get
      } yield (outcome, written)

      result.asserting { case (outcome, written) =>
        outcome mustBe Left(sinkFailure)
        written.map(_._1) mustBe List("Incomplete diagnostics: report-test")
        written.head._2 must contain(reportFailure.toString)
      }
    }

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
