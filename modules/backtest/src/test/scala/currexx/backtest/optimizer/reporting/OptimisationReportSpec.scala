package currexx.backtest.optimizer.reporting

import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import currexx.algorithms.{EvaluationPhase, Fitness, Parameters}
import currexx.backtest.{NamedIndicator, OptimisationRound, StrategyCatalogue, TestStrategy}
import currexx.backtest.optimizer.{IndicatorSearchSpace, ScoringFunction}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

import scala.concurrent.duration.*

class OptimisationReportSpec extends IOWordSpec {
  private def indicator(length: Int, tracker: Int = 8): Indicator = Indicator.compositeAnyOf(
    Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(length)),
    Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(tracker))
  )
  private val rules = TradeStrategy(Nil, Nil)
  private val round = OptimisationRound(
    "report-test",
    TestStrategy(indicator(10), rules),
    Parameters.GA(4, 1, 0.7, 0.1, 0.02, false),
    ScoringFunction.Consistent()
  )
  private def fold(score: Double): FoldDiagnostics = FoldDiagnostics(score, BigDecimal(100), 20, 1, BigDecimal(3), BigDecimal("0.5"), Nil)
  private def measured(ind: Indicator, training: Double = 0.5, validation: Option[Double] = Some(0.2)): CandidateDiagnostics =
    CandidateDiagnostics(ind, List(fold(training)), validation.map(fold))

  "OptimisationReportBuilder" should {
    "replay canonical baselines and both leaders once, but discover catalogue aliases for every finalist" in {
      val incompatible = Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(12), 70, 30)
      val configured   = round.copy(extraSeeds =
        List(
          NamedIndicator("seed", indicator(20, 12)),
          NamedIndicator("seed alias", indicator(20, 24)),
          NamedIndicator("literal seed", indicator(20)),
          NamedIndicator("target alias", indicator(10, 16)),
          NamedIndicator("incompatible", incompatible)
        )
      )
      val finalists = Vector(
        (indicator(30), Fitness(0.8), Fitness(0.3)),
        (indicator(40), Fitness(1.0), Fitness(0.2)),
        (indicator(50), Fitness(0.6), Fitness(0.1))
      )
      val otherRules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil)
      val catalogue  = List(
        StrategyCatalogue.Entry("exact", TestStrategy(indicator(30), rules), true),
        StrategyCatalogue.Entry("restored", TestStrategy(indicator(30, 40), rules), true),
        StrategyCatalogue.Entry("different rules", TestStrategy(indicator(30), otherRules), true),
        StrategyCatalogue.Entry("unreplayed finalist", TestStrategy(indicator(50), rules), false)
      )
      val result = for {
        space       <- IO.fromEither(IndicatorSearchSpace.forStrategy(configured.strategy))
        diagnostics <- RunDiagnostics.make[IO]
        calls       <- Ref.of[IO, List[Indicator]](Nil)
        inspect = (ind: Indicator) => calls.update(_ :+ ind).as(measured(ind))
        report <- new OptimisationReportBuilder(configured, space, inspect, diagnostics, catalogue)
          .build(finalists, RunDiagnostics.Snapshot(), 2.seconds)
        called <- calls.get
      } yield (report, called)
      result.asserting { case (report, called) =>
        called mustBe List(indicator(10), indicator(20), indicator(30), indicator(40))
        report.baselines.map(_.name) mustBe List("target", "seed", "seed alias", "literal seed", "target alias", "incompatible")
        report.baselines.find(_.name == "seed").map(_.fixedInputsRestored) mustBe Some(true)
        report.baselines.find(_.name == "literal seed").map(_.fixedInputsRestored) mustBe Some(false)
        report.baselines.last.effective mustBe None
        report.catalogueMatches(indicator(30)) mustBe List(
          CatalogueMatch("exact", DuplicateKind.Exact),
          CatalogueMatch("restored", DuplicateKind.FixedInputsRestored)
        )
        report.catalogueMatches(indicator(50)) mustBe List(CatalogueMatch("unreplayed finalist", DuplicateKind.Exact))
        report.candidates.contains(indicator(50)) mustBe false
        report.diagnostics.firstSeen mustBe empty
        report.optimisationDuration mustBe 2.seconds
        report.reportingWorkload mustBe RunDiagnostics.Workload()
      }
    }
    "count only this report's deduplicated replay work and preserve the frozen search snapshot" in {
      val configured = round.copy(extraSeeds = List(NamedIndicator("seed", indicator(20)), NamedIndicator("alias", indicator(20, 40))))
      val finalists  = Vector((indicator(20), Fitness(1), Fitness(0.5)))
      val stage      = RunDiagnostics.Stage.Reporting
      val result     = for {
        space       <- IO.fromEither(IndicatorSearchSpace.forStrategy(configured.strategy))
        diagnostics <- RunDiagnostics.make[IO]
        _           <- diagnostics.requested(EvaluationPhase.Search(0))
        _           <- diagnostics.observed(indicator(10), EvaluationPhase.Search(0), List(0.5))
        frozen      <- diagnostics.snapshot
        replayWork = diagnostics.candidateRequested(stage) >> diagnostics.foldStarted(stage) >>
          List.fill(2)(()).traverse_(_ => diagnostics.pairStarted(stage) >> diagnostics.pairCompleted(stage)) >>
          diagnostics.foldCompleted(stage)
        _ <- replayWork
        inspect = (ind: Indicator) => replayWork.as(measured(ind))
        report <- new OptimisationReportBuilder(configured, space, inspect, diagnostics, Nil).build(finalists, frozen, 1.second)
        latest <- diagnostics.snapshot
      } yield (report, frozen, latest)
      result.asserting { case (report, frozen, latest) =>
        report.reportingWorkload mustBe RunDiagnostics.Workload(2, 2, 2, 4, 4)
        latest.workloads(stage) mustBe RunDiagnostics.Workload(3, 3, 3, 6, 6)
        report.diagnostics mustBe frozen
        report.diagnostics.searchRequests mustBe 1L
        report.diagnostics.firstSeen mustBe Map(indicator(10) -> 0)
        report.diagnostics.workloads(stage) mustBe RunDiagnostics.Workload()
      }
    }
  }

  "OptimisationReportRenderer" should {
    "compare each fitness against its own strongest seed and show provenance without inventing percentage gain from zero" in {
      val target         = indicator(10)
      val firstSeed      = indicator(20)
      val secondSeed     = indicator(30)
      val champion       = indicator(40)
      val trainingLeader = indicator(50)
      val report         = OptimisationReport(
        round.name,
        round.corpus,
        Vector((champion, Fitness(1.5), Fitness(0.5)), (trainingLeader, Fitness(3), Fitness(0.4))),
        List(
          BaselineReport("target", Some(target), None),
          BaselineReport("validation seed", Some(firstSeed), Some(IndicatorSearchSpace.SeedDisposition.Accepted)),
          BaselineReport("training seed", Some(secondSeed), Some(IndicatorSearchSpace.SeedDisposition.Accepted))
        ),
        Map(
          target         -> measured(target, 0, Some(0)),
          firstSeed      -> measured(firstSeed, 1, Some(0.9)),
          secondSeed     -> measured(secondSeed, 2, Some(0.2)),
          champion       -> measured(champion, 1.5, Some(0.5)),
          trainingLeader -> measured(trainingLeader, 3, Some(0.4))
        ),
        Map(champion -> List(CatalogueMatch("retained", DuplicateKind.Exact))),
        RunDiagnostics.Snapshot(firstSeen = Map(champion -> 7), searchRequests = 10, rescoreRequests = 2, computationAttempts = 3),
        RunDiagnostics.Workload(),
        1.second,
        20.millis
      )
      val text = OptimisationReportRenderer.sections(report).flatMap { case (heading, lines) => heading :: lines }.mkString("\n")
      text must include("Champion selection: report-test")
      text must include("SELECTED (from 2 after validation")
      text must include("strongest seed on training (training seed): -0.500000")
      text must include("strongest seed on validation (validation seed): -0.400000")
      text must include("Final training leader vs strongest seed on training (training seed): +1.000000")
      text must include("Final training leader vs strongest seed on validation (validation seed): -0.500000")
      text must include("n/a (baseline is zero)")
      text must include("#1: first seen=7")
      text must include("retained (exact)")
      text must include("closed=20; forced=1; costs=3")
      text must include("portfolio drawdown=0.5%")
      text must include("Search cache reuses: 9 (includes waiting on an in-flight computation).")
      text must include("reporting duration: 20 ms")
    }

    "include seeds equivalent to the target in comparisons and distinguish restored aliases from literal matches" in {
      val candidate = indicator(10)
      val report    = OptimisationReport(
        round.name,
        round.corpus,
        Vector((candidate, Fitness(0.5), Fitness(0.2))),
        List(
          BaselineReport("target", Some(candidate), None),
          BaselineReport("literal", Some(candidate), Some(IndicatorSearchSpace.SeedDisposition.TargetDuplicate)),
          BaselineReport(
            "restored",
            Some(candidate),
            Some(IndicatorSearchSpace.SeedDisposition.TargetDuplicate),
            fixedInputsRestored = true
          )
        ),
        Map(candidate -> measured(candidate)),
        Map.empty,
        RunDiagnostics.Snapshot(firstSeen = Map(candidate -> 0)),
        RunDiagnostics.Workload(),
        1.second,
        10.millis
      )
      val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
      text must include("strongest seed on training (literal): +0.000000")
      text must include("restored (fixed inputs restored): duplicate of target")
      text must include("baselines=target, literal, restored (fixed inputs restored)")
    }

    "report missing validation without claiming that constraints passed" in {
      val candidate = indicator(10)
      val report    = OptimisationReport(
        round.name,
        round.corpus.copy(validationFold = Nil),
        Vector((candidate, Fitness(0.5), Fitness(0))),
        List(BaselineReport("target", Some(candidate), None)),
        Map(candidate -> measured(candidate, validation = None)),
        Map.empty,
        RunDiagnostics.Snapshot(),
        RunDiagnostics.Workload(),
        1.second,
        10.millis
      )
      val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
      text must include("NOTHING SELECTED: validation measurements are unavailable.")
      text must include("Validation: unavailable.")
      (text must not).include("Satisfies every constraint")
    }

    "handle an empty final population" in {
      OptimisationReportRenderer.verdict(round, Vector.empty, Nil) must contain("No candidates were evaluated.")
    }
  }
}
