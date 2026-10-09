package currexx.backtest.optimizer.reporting

import cats.effect.{IO, Ref}
import currexx.algorithms.{EvaluationPhase, Fitness, Parameters, ValidatedPopulation}
import currexx.algorithms.progress.{Progress, Tracker}
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.SearchObjectiveConfig
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

  private def noteTracker(write: (String, List[String]) => IO[Unit], finalCall: IO[Unit] = IO.unit): Tracker[IO, Indicator] =
    new Tracker[IO, Indicator] {
      def displayInitial(target: Indicator, params: Parameters[?]): IO[Unit] = IO.unit
      def displayProgress(progress: Progress[Indicator]): IO[Unit]           = IO.unit
      def displayFinal(population: ValidatedPopulation[Indicator]): IO[Unit] = finalCall
      def displayNote(title: String, lines: List[String]): IO[Unit]          = write(title, lines)
    }

  "ReportingTracker" should {
    "omit the generic retention table when search and validation use different objectives" in {
      val result = for {
        diagnostics <- RunDiagnostics.make[IO]
        notes       <- Ref.of[IO, List[(String, List[String])]](Nil)
        delegate = noteTracker(
          (title, lines) => notes.update(_ :+ (title -> lines)),
          IO.raiseError(new IllegalStateException("Generic retention table must not be rendered"))
        )
        tracker   = new ReportingTracker(delegate, diagnostics, 1, searchObjective = SearchObjectiveConfig.BaselineRelative())
        finalists = Vector((indicator, Fitness(2.0), Fitness(1.0)))
        _       <- tracker.displayFinal(finalists)
        before  <- notes.get
        _       <- tracker.displayCompletedRankings(finalists)
        written <- notes.get
      } yield (before, written)
      result.asserting { case (before, written) =>
        before mustBe Nil
        written.map(_._1) mustBe List("Final rankings (separate objectives)")
        val text = written.head._2.mkString("\n")
        text must include("search=2.000000; absolute validation=1.000000")
        (text must not).include("retained")
        (text must not).include("50.0%")
      }
    }

    "delay a failing final sink until completed rankings are displayed after the decision" in {
      val failure = new RuntimeException("final sink unavailable")
      val result  = for {
        diagnostics <- RunDiagnostics.make[IO]
        calls       <- Ref.of[IO, Int](0)
        notes       <- Ref.of[IO, List[String]](Nil)
        delegate = noteTracker(
          (title, _) => notes.update(_ :+ title),
          calls.update(_ + 1) >> IO.raiseError(failure)
        )
        tracker = new ReportingTracker(delegate, diagnostics, foldCount = 1)
        beforeDecision <- tracker.displayFinal(report.finalists).attempt
        callsBefore    <- calls.get
        afterDecision  <- tracker.displayCompletedRankings(report.finalists).attempt
        callsAfter     <- calls.get
        written        <- notes.get
      } yield (beforeDecision, callsBefore, afterDecision, callsAfter, written)

      result.asserting { case (beforeDecision, callsBefore, afterDecision, callsAfter, written) =>
        beforeDecision mustBe Right(())
        callsBefore mustBe 0
        afterDecision mustBe Left(failure)
        callsAfter mustBe 1
        written mustBe Nil
      }
    }

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
        written.head._2 must contain("Optimisation and upgrade decision are complete; reporting failed. Remaining rounds can continue.")
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
