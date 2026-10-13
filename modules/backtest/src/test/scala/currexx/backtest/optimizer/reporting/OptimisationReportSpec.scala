package currexx.backtest.optimizer.reporting

import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import currexx.algorithms.{EvaluationPhase, Fitness, Parameters, ValidatedPopulation}
import currexx.backtest.{DataWindow, StrategyCatalogue, TestStrategy}
import currexx.backtest.optimizer.ScoringFunction
import currexx.backtest.optimizer.{
  NamedStrategy,
  OptimisationResult,
  OptimisationRound,
  OptimisationSearchSpace,
  OptimisationTarget,
  SearchObjectiveConfig,
  SelectionCoverage,
  SelectionEvidence,
  UpgradeDecision,
  UpgradePolicy
}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.backtest.optimizer.strategy.{RuleList, RuleParameter, RuleParameterBounds, RuleParameterPath, RulePath, RuleTarget}
import currexx.domain.market.CurrencyPair
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.syntax.*
import kirill5k.common.cats.test.IOWordSpec

import scala.concurrent.duration.*
import java.time.{Instant, YearMonth}

class OptimisationReportSpec extends IOWordSpec {
  private def indicator(length: Int, tracker: Int = 8): TestStrategy = TestStrategy(
    Indicator.compositeAnyOf(
      Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(length)),
      Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.RSX(tracker))
    ),
    rules
  )
  private val rules = TradeStrategy(Nil, Nil)
  private val round = OptimisationRound(
    "report-test",
    indicator(10),
    Parameters.GA(4, 1, 0.7, 0.1, 0.02, false),
    ScoringFunction.Consistent(),
    target = OptimisationTarget.Indicator
  )
  private def fold(score: Double): FoldDiagnostics = FoldDiagnostics(score, BigDecimal(100), 20, 1, BigDecimal(3), BigDecimal("0.5"), Nil)
  private def measured(ind: TestStrategy, training: Double = 0.5, validation: Option[Double] = Some(0.2)): CandidateDiagnostics =
    CandidateDiagnostics(ind, List(fold(training)), validation.map(fold))

  private def evidence(candidate: TestStrategy, netProfit: BigDecimal = 100): SelectionEvidence =
    SelectionEvidence
      .from(
        candidate,
        Map(
          CurrencyPair.fromUnsafe("EUR/USD") -> SelectionCoverage(
            DataWindow(Instant.parse("2025-01-01T00:00:00Z"), Instant.parse("2025-04-30T23:59:59Z")),
            PosBigDecimal.unsafeFrom(BigDecimal(10000))
          )
        ),
        BigDecimal(10000),
        netProfit,
        BigDecimal(3),
        BigDecimal("0.5"),
        (1 to 4).map(month => YearMonth.of(2025, month) -> (netProfit / 4)).toMap,
        0.5,
        Nil,
        20,
        1
      )
      .toOption
      .get

  private def completed(finalists: ValidatedPopulation[TestStrategy]): OptimisationResult = OptimisationResult(
    finalists,
    UpgradeDecision.RetainBase(Nil),
    evidence(round.strategy)
  )

  "OptimisationReportBuilder" should {
    "replay canonical baselines, both leaders, and a lost best candidate once, but discover catalogue aliases for every finalist" in {
      val incompatible = TestStrategy(Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(12), 70, 30), rules)
      val configured   = round.copy(extraSeeds =
        List(
          NamedStrategy("seed", indicator(20, 12)),
          NamedStrategy("seed alias", indicator(20, 24)),
          NamedStrategy("literal seed", indicator(20)),
          NamedStrategy("target alias", indicator(10, 16)),
          NamedStrategy("incompatible", incompatible)
        )
      )
      val finalists = Vector(
        (indicator(30), Fitness(0.8), Fitness(0.3)),
        (indicator(40), Fitness(1.0), Fitness(0.2)),
        (indicator(50), Fitness(0.6), Fitness(0.1))
      )
      val otherRules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil)
      val catalogue  = List(
        StrategyCatalogue.Entry("exact", indicator(30), true),
        StrategyCatalogue.Entry("restored", indicator(30, 40), true),
        StrategyCatalogue.Entry("different rules", indicator(30).copy(rules = otherRules), true),
        StrategyCatalogue.Entry("unreplayed finalist", indicator(50), false)
      )
      val frozen = RunDiagnostics.Snapshot(
        firstSeen = Map(indicator(60) -> 2),
        bestSeen = Some(RunDiagnostics.Discovery(indicator(60), 1.1, 2))
      )
      val result = for {
        space       <- IO.fromEither(OptimisationSearchSpace.forRound(configured))
        diagnostics <- RunDiagnostics.make[IO]
        calls       <- Ref.of[IO, List[TestStrategy]](Nil)
        inspect = (ind: TestStrategy) => calls.update(_ :+ ind).as(measured(ind))
        report <- new OptimisationReportBuilder(configured, space, inspect, diagnostics, catalogue)
          .build(completed(finalists), frozen, 2.seconds)
        called <- calls.get
      } yield (report, called)
      result.asserting { case (report, called) =>
        called mustBe List(indicator(10), indicator(20), indicator(30), indicator(40), indicator(60))
        report.baselines.map(_.name) mustBe List("target", "seed", "seed alias", "literal seed", "target alias", "incompatible")
        report.baselines.find(_.name == "seed").map(_.fixedInputsRestored) mustBe Some(true)
        report.baselines.find(_.name == "literal seed").map(_.fixedInputsRestored) mustBe Some(false)
        report.baselines.last.effective mustBe None
        report.baselines.last.reason.exists(_.nonEmpty) mustBe true
        report.catalogueMatches(indicator(30)) mustBe List(
          CatalogueMatch("exact", DuplicateKind.Exact),
          CatalogueMatch("restored", DuplicateKind.FixedInputsRestored),
          CatalogueMatch("different rules", DuplicateKind.Exact, scope = CatalogueScope.IndicatorsOnly)
        )
        report.catalogueMatches(indicator(50)) mustBe List(CatalogueMatch("unreplayed finalist", DuplicateKind.Exact))
        report.candidates.contains(indicator(50)) mustBe false
        report.candidates.contains(indicator(60)) mustBe true
        report.finalists mustBe finalists
        report.diagnostics mustBe frozen
        report.optimisationDuration mustBe 2.seconds
        report.reportingWorkload mustBe RunDiagnostics.Workload()
        val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
        text must include("seed alias (fixed inputs restored): duplicate of seed 'seed'")
        text must include(
          "catalogue strategies=exact (exact), restored (after restoring fixed inputs); " +
            "parameter-only catalogue matches (different rules)=different rules (exact)"
        )
      }
    }
    "label component-only catalogue matches symmetrically for each target" in {
      val base =
        round.strategy.copy(rules = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(4.hours)))))
      val differentIndicator = base.copy(indicator = indicator(30).indicator)
      val restoredIndicator  = base.copy(indicator = indicator(10, 20).indicator)
      val differentRules     =
        base.copy(rules = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(8.hours)))))
      val catalogue = List(
        StrategyCatalogue.Entry("exact", base, true),
        StrategyCatalogue.Entry("different indicator", differentIndicator, true),
        StrategyCatalogue.Entry("fixed tracker", restoredIndicator, true),
        StrategyCatalogue.Entry("different rules", differentRules, true),
        StrategyCatalogue.Entry("both different", differentRules.copy(indicator = differentIndicator.indicator), true)
      )
      val durationBounds =
        RuleParameterBounds(durations = Map(RuleTarget.HoldingTime -> RuleParameterBounds.DurationRange(1.hour, 12.hours, 1.hour)))
      val result = OptimisationTarget.values.toList.traverse { mode =>
        val configured = round.copy(
          strategy = base,
          target = mode,
          ruleBounds = if (mode == OptimisationTarget.Indicator) RuleParameterBounds.default else durationBounds
        )
        for {
          space       <- IO.fromEither(OptimisationSearchSpace.forRound(configured))
          diagnostics <- RunDiagnostics.make[IO]
          inspect = (candidate: TestStrategy) => IO.pure(measured(candidate))
          report <- new OptimisationReportBuilder(configured, space, inspect, diagnostics, catalogue).build(
            OptimisationResult(Vector((base, Fitness(1), Fitness(1))), UpgradeDecision.RetainBase(Nil), evidence(base)),
            RunDiagnostics.Snapshot(),
            1.second
          )
        } yield mode -> report
      }
      result.asserting { results =>
        results.foreach { case (mode, report) =>
          report.ruleBounds mustBe (if (mode == OptimisationTarget.Indicator) RuleParameterBounds.default else durationBounds)
        }
        val reports = results.toMap
        reports(OptimisationTarget.Indicator).catalogueMatches(base) mustBe List(
          CatalogueMatch("exact", DuplicateKind.Exact),
          CatalogueMatch("fixed tracker", DuplicateKind.FixedInputsRestored),
          CatalogueMatch("different rules", DuplicateKind.Exact, CatalogueScope.IndicatorsOnly)
        )
        reports(OptimisationTarget.Strategy).catalogueMatches(base) mustBe List(
          CatalogueMatch("exact", DuplicateKind.Exact),
          CatalogueMatch("different indicator", DuplicateKind.Exact, CatalogueScope.RulesOnly),
          CatalogueMatch("fixed tracker", DuplicateKind.Exact, CatalogueScope.RulesOnly)
        )
        reports(OptimisationTarget.Both).catalogueMatches(base) mustBe List(
          CatalogueMatch("exact", DuplicateKind.Exact),
          CatalogueMatch("fixed tracker", DuplicateKind.FixedInputsRestored)
        )
        val strategyText = OptimisationReportRenderer.sections(reports(OptimisationTarget.Strategy)).flatMap(_._2).mkString("\n")
        strategyText must include("catalogue strategies=exact (exact);")
        strategyText must include(
          "parameter-only catalogue matches (different indicators)=different indicator (exact), fixed tracker (exact)"
        )
        val baseLines = strategyText.linesIterator.filter(_.startsWith("Base candidate: ")).toList
        baseLines must have size 1
        io.circe.parser.decode[TestStrategy](baseLines.head.stripPrefix("Base candidate: ")) mustBe Right(base)
        (strategyText must not).include(base.indicator.toString)
      }
    }

    "restore fixed rule numbers and label rule-schema fallbacks as indicator-only catalogue matches" in {
      def candidate(
          atr: Double,
          holdingTime: FiniteDuration = 4.hours,
          position: Rule.Condition = Rule.Condition.positionIsBuy
      ): TestStrategy =
        TestStrategy(
          Indicator.compositeAnyOf(
            Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1)),
            Indicator.ValueTracking(ValueRole.Volatility, ValueSource.Close, VT.ATR(10))
          ),
          TradeStrategy(
            Nil,
            List(
              Rule(
                TradeAction.ClosePosition,
                Rule.Condition.allOf(
                  Rule.Condition.PositionOpenFor(holdingTime),
                  Rule.Condition.PriceMovedAgainstEntry(atr),
                  position
                )
              )
            )
          )
        )
      val base      = candidate(4.0)
      val catalogue = List(
        StrategyCatalogue.Entry("exact", base, true),
        StrategyCatalogue.Entry("fixed ATR alias", candidate(8.0), true),
        StrategyCatalogue.Entry("different active time", candidate(8.0, 6.hours), true),
        StrategyCatalogue.Entry("different position", candidate(8.0, position = Rule.Condition.positionIsSell), true)
      )
      val cases = for {
        mode     <- List(OptimisationTarget.Strategy, OptimisationTarget.Both)
        fixedAtr <- List(false, true)
      } yield mode -> fixedAtr
      val result = cases.traverse { case (mode, fixedAtr) =>
        val bounds = RuleParameterBounds(
          numeric = if (fixedAtr) Map(RuleTarget.AtrStop -> RuleParameterBounds.DoubleRange(4.0, 4.0, 0.25)) else Map.empty,
          durations = Map(RuleTarget.HoldingTime -> RuleParameterBounds.DurationRange(1.hour, 12.hours, 1.hour))
        )
        val configured = round.copy(strategy = base, target = mode, ruleBounds = bounds)
        for {
          space       <- IO.fromEither(OptimisationSearchSpace.forRound(configured))
          diagnostics <- RunDiagnostics.make[IO]
          inspect = (candidate: TestStrategy) => IO.pure(measured(candidate))
          report <- new OptimisationReportBuilder(configured, space, inspect, diagnostics, catalogue).build(
            OptimisationResult(Vector((base, Fitness(1), Fitness(1))), UpgradeDecision.RetainBase(Nil), evidence(base)),
            RunDiagnostics.Snapshot(),
            1.second
          )
        } yield report
      }
      result.asserting { reports =>
        reports must have size 4
        reports.foreach { report =>
          val fallbackMatches =
            if (report.target == OptimisationTarget.Both)
              List(CatalogueMatch("different position", DuplicateKind.FixedInputsRestored, CatalogueScope.IndicatorsOnly))
            else Nil
          report.catalogueMatches(base) mustBe List(
            CatalogueMatch("exact", DuplicateKind.Exact),
            CatalogueMatch("fixed ATR alias", DuplicateKind.FixedInputsRestored)
          ) ::: fallbackMatches
          val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
          text must include("fixed ATR alias (after restoring fixed inputs)")
          (text must not).include("different active time")
          if (report.target == OptimisationTarget.Both)
            text must include("parameter-only catalogue matches (different rules)=different position (after restoring fixed inputs)")
          else (text must not).include("different position")
        }
        succeed
      }
    }

    "report accepted joint seeds that use base rules without claiming a complete catalogue match" in {
      val base =
        indicator(10).copy(rules = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(4.hours)))))
      val seed       = indicator(20).copy(rules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil))
      val effective  = seed.copy(rules = base.rules)
      val configured = round.copy(
        strategy = base,
        target = OptimisationTarget.Both,
        extraSeeds = List(NamedStrategy("indicator seed", seed))
      )
      val result = for {
        space       <- IO.fromEither(OptimisationSearchSpace.forRound(configured))
        diagnostics <- RunDiagnostics.make[IO]
        inspect = (candidate: TestStrategy) => IO.pure(measured(candidate))
        report <- new OptimisationReportBuilder(
          configured,
          space,
          inspect,
          diagnostics,
          List(StrategyCatalogue.Entry("seed catalogue entry", seed, true))
        ).build(
          OptimisationResult(Vector((effective, Fitness(1), Fitness(1))), UpgradeDecision.RetainBase(Nil), evidence(base)),
          RunDiagnostics.Snapshot(),
          1.second
        )
      } yield report
      result.asserting { report =>
        val resolved = report.baselines.find(_.name == "indicator seed").get
        resolved.effective mustBe Some(effective)
        resolved.disposition mustBe Some(OptimisationSearchSpace.SeedDisposition.Accepted)
        resolved.fixedInputsRestored mustBe true
        resolved.reason.exists(_.contains("base rules")) mustBe true
        val baselineText = OptimisationReportRenderer.sections(report).toMap.apply("Baseline measurements").mkString("\n")
        baselineText must include("indicator seed (fixed inputs restored): accepted seed")
        baselineText must include(s"Reason: ${resolved.reason.get}")
        report.catalogueMatches(effective) mustBe List(
          CatalogueMatch("seed catalogue entry", DuplicateKind.FixedInputsRestored, CatalogueScope.IndicatorsOnly)
        )
      }
    }

    "leave the fixed-input seed label clear when only inactive rules are replaced" in {
      val seed       = indicator(20).copy(rules = TradeStrategy(List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)), Nil))
      val configured = round.copy(extraSeeds = List(NamedStrategy("different rules", seed)))
      val result     = for {
        space       <- IO.fromEither(OptimisationSearchSpace.forRound(configured))
        diagnostics <- RunDiagnostics.make[IO]
        inspect = (candidate: TestStrategy) => IO.pure(measured(candidate))
        report <- new OptimisationReportBuilder(configured, space, inspect, diagnostics, Nil).build(
          completed(Vector((indicator(20), Fitness(1), Fitness(1)))),
          RunDiagnostics.Snapshot(),
          1.second
        )
      } yield report
      result.asserting { report =>
        report.baselines.find(_.name == "different rules").map(_.fixedInputsRestored) mustBe Some(false)
        val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
        text must include("different rules: accepted seed")
        (text must not).include("different rules (fixed inputs restored)")
      }
    }

    "count only this report's deduplicated replay work and preserve the frozen search snapshot" in {
      val configured = round.copy(extraSeeds = List(NamedStrategy("seed", indicator(20)), NamedStrategy("alias", indicator(20, 40))))
      val finalists  = Vector((indicator(20), Fitness(1), Fitness(0.5)))
      val stage      = RunDiagnostics.Stage.Reporting
      val result     = for {
        space       <- IO.fromEither(OptimisationSearchSpace.forRound(configured))
        diagnostics <- RunDiagnostics.make[IO]
        _           <- diagnostics.requested(EvaluationPhase.Search(0))
        _           <- diagnostics.observed(indicator(10), EvaluationPhase.Search(0), List(0.5))
        frozen      <- diagnostics.snapshot
        replayWork = diagnostics.candidateRequested(stage) >> diagnostics.foldStarted(stage) >>
          List.fill(2)(()).traverse_(_ => diagnostics.pairStarted(stage) >> diagnostics.pairCompleted(stage)) >>
          diagnostics.foldCompleted(stage)
        _ <- replayWork
        inspect = (ind: TestStrategy) => replayWork.as(measured(ind))
        report <- new OptimisationReportBuilder(configured, space, inspect, diagnostics, Nil).build(completed(finalists), frozen, 1.second)
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

  "Diagnostic selection cache misses" should {
    "count report-time selection simulations without changing frozen validation counts" in {
      val target = round.strategy
      val lost   = indicator(99)
      val stage  = RunDiagnostics.Stage.Validation
      val run    = for {
        space       <- IO.fromEither(OptimisationSearchSpace.forRound(round))
        diagnostics <- RunDiagnostics.make[IO]
        frozen      <- diagnostics.snapshot
        inspect = (candidate: TestStrategy) => {
          val validation =
            if (candidate == lost)
              diagnostics.foldStarted(stage) >> diagnostics.pairStarted(stage) >>
                diagnostics.pairCompleted(stage) >> diagnostics.foldCompleted(stage)
            else IO.unit
          validation.as(measured(candidate))
        }
        report <- new OptimisationReportBuilder(round, space, inspect, diagnostics, Nil).build(
          completed(Vector((target, Fitness(1), Fitness(1)))),
          frozen.copy(bestSeen = Some(RunDiagnostics.Discovery(lost, 2.0, 1))),
          1.second
        )
      } yield report
      run.asserting { report =>
        report.reportingWorkload mustBe RunDiagnostics.Workload(0, 1, 1, 1, 1)
        report.diagnostics.workloads.getOrElse(stage, RunDiagnostics.Workload()) mustBe RunDiagnostics.Workload()
      }
    }
  }

  "OptimisationReportRenderer" should {
    "record the target, fixed inputs, full candidates and changed leaf parameters" in {
      val base =
        indicator(10).copy(rules = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(4.hours)))))
      val changed =
        indicator(20).copy(rules = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(8.hours)))))
      val bounds = RuleParameterBounds(durations =
        Map(
          RuleTarget
            .At(RuleParameterPath(RulePath(RuleList.Close, 0), RuleParameter.Duration), base.rules) -> RuleParameterBounds.DurationRange(
            1.hour,
            12.hours,
            1.hour
          )
        )
      )
      val report = OptimisationReport(
        round.name,
        round.corpus,
        Vector((changed, Fitness(1), Fitness(1))),
        Nil,
        Map.empty,
        Map.empty,
        RunDiagnostics.Snapshot(),
        RunDiagnostics.Workload(),
        1.second,
        1.second,
        target = OptimisationTarget.Both,
        baseCandidate = base,
        fixedIndicators = Set(base.indicator),
        ruleBounds = bounds
      )
      val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
      text must include("Optimisation target: Both")
      text must include(s"Rule parameter bounds: ${RuleBoundsRenderer.json(bounds).noSpaces}")
      text must include("Fixed indicator inputs:")
      text must include(s"Base candidate: ${base.asJson.noSpaces}")
      val changes = CandidateParameters.changes(base, changed)
      changes must have size 2
      changes.exists(_.startsWith("indicator.")) mustBe true
      changes.exists(_.startsWith("rules.")) mustBe true
      CandidateParameters.changes(base, base) mustBe Nil
    }

    OptimisationTarget.values.foreach { target =>
      s"record decodable complete base and decision candidates for $target" in {
        def rules(hours: Int): TradeStrategy =
          TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(hours.hours))))
        val base                                             = indicator(10).copy(rules = rules(4))
        def candidate(length: Int, hours: Int): TestStrategy = target match
          case OptimisationTarget.Indicator => base.copy(indicator = indicator(length).indicator)
          case OptimisationTarget.Strategy  => base.copy(rules = rules(hours))
          case OptimisationTarget.Both      => TestStrategy(indicator(length).indicator, rules(hours))
        val approved = candidate(20, 6)
        val rejected = candidate(30, 8)
        val baseline = evidence(base, 100)
        val decision = UpgradePolicy.decide(baseline, List(evidence(rejected, 50), evidence(approved, 200))).toOption.get
        val result   = OptimisationResult(
          Vector((rejected, Fitness(2), Fitness(1)), (approved, Fitness(1), Fitness(0.5))),
          decision,
          baseline
        )
        val report = OptimisationReport(
          round.name,
          round.corpus,
          result.finalists,
          Nil,
          Map.empty,
          Map.empty,
          RunDiagnostics.Snapshot(),
          RunDiagnostics.Workload(),
          1.second,
          1.second,
          upgrade = Some(result),
          target = target,
          baseCandidate = base,
          fixedIndicators = Set.empty,
          ruleBounds = RuleParameterBounds()
        )
        val lines         = OptimisationReportRenderer.sections(report).flatMap(_._2)
        val baseLines     = lines.filter(_.startsWith("Base candidate: "))
        val decisionLines = lines.filter(_.startsWith("Recorded decision: "))
        baseLines must have size 1
        decisionLines must have size 1
        io.circe.parser.decode[TestStrategy](baseLines.head.stripPrefix("Base candidate: ")) mustBe Right(base)
        val recorded = io.circe.parser.parse(decisionLines.head.stripPrefix("Recorded decision: ")).toOption.get
        recorded.hcursor.get[String]("outcome") mustBe Right("UPGRADE APPROVED")
        recorded.hcursor.get[TestStrategy]("candidate") mustBe Right(approved)
        recorded.hcursor.downField("comparison").downField("base").get[TestStrategy]("candidate") mustBe Right(base)
        recorded.hcursor.downField("comparison").downField("candidate").get[TestStrategy]("candidate") mustBe Right(approved)
        recorded.hcursor.downField("rejections").downArray.get[TestStrategy]("candidate") mustBe Right(rejected)
        recorded.hcursor
          .downField("rejections")
          .downArray
          .downField("comparison")
          .downField("candidate")
          .get[TestStrategy]("candidate") mustBe Right(rejected)

        val retained      = UpgradePolicy.decide(baseline, List(evidence(rejected, 50))).toOption.get
        val retainedLines = OptimisationReportRenderer
          .sections(report.copy(upgrade = Some(result.copy(decision = retained))))
          .flatMap(_._2)
          .filter(_.startsWith("Recorded decision: "))
        retainedLines must have size 1
        val retainedJson = io.circe.parser.parse(retainedLines.head.stripPrefix("Recorded decision: ")).toOption.get
        retainedJson.hcursor.get[String]("outcome") mustBe Right("BASE RETAINED")
        retainedJson.hcursor.downField("rejections").downArray.get[TestStrategy]("candidate") mustBe Right(rejected)
        retainedJson.hcursor
          .downField("rejections")
          .downArray
          .downField("comparison")
          .downField("base")
          .get[TestStrategy]("candidate") mustBe Right(base)
      }
    }

    "record the completed upgrade decision and omit ratios across different objectives" in {
      val candidate  = indicator(20)
      val base       = completed(Vector.empty).base
      val challenger = evidence(candidate, 200)
      val decision   = UpgradePolicy.decide(base, List(challenger)).toOption.get
      val finalists  = Vector((base.candidate, Fitness(2), Fitness(1)), (candidate, Fitness(1), Fitness(0.5)))
      val result     = OptimisationResult(finalists, decision, base)
      val report     = OptimisationReport(
        round.name,
        round.corpus,
        finalists,
        List(BaselineReport("target", Some(base.candidate), None)),
        Map(base.candidate -> measured(base.candidate), candidate -> measured(candidate)),
        Map.empty,
        RunDiagnostics.Snapshot(),
        RunDiagnostics.Workload(),
        1.second,
        1.second,
        upgrade = Some(result),
        searchObjective = SearchObjectiveConfig.BaselineRelative(),
        target = OptimisationTarget.Indicator,
        baseCandidate = base.candidate,
        fixedIndicators = Set.empty,
        ruleBounds = RuleParameterBounds()
      )
      val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
      text must include("UPGRADE APPROVED")
      text must include("Format version: 3")
      text must include("BaselineRelative")
      text must include("trial-v1")
      text must include("Net improvement:")
      (text must not).include("Validation/training fitness ratio")
      // The completed decision, not the first finalist's identity, determines approval.
      text must include(candidate.indicator.toString)
      text must include("Approved upgrade vs target")
    }

    "retain monthly financial evidence and coverage alongside complete candidate records" in {
      def strategy(hours: Int): TestStrategy =
        round.strategy.copy(rules = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(hours.hours)))))
      val base     = evidence(strategy(4), 100)
      val approved = evidence(strategy(6), 200)
      val rejected = evidence(strategy(8), 50)
      val decision = UpgradePolicy.decide(base, List(rejected, approved)).toOption.get
      val result   = OptimisationResult(
        Vector((rejected.candidate, Fitness(1), Fitness(1)), (approved.candidate, Fitness(0.5), Fitness(0.5))),
        decision,
        base
      )
      val report = OptimisationReport(
        round.name,
        round.corpus,
        result.finalists,
        Nil,
        Map.empty,
        Map.empty,
        RunDiagnostics.Snapshot(),
        RunDiagnostics.Workload(),
        1.second,
        1.second,
        upgrade = Some(result),
        target = OptimisationTarget.Strategy,
        baseCandidate = base.candidate,
        fixedIndicators = Set(base.candidate.indicator),
        ruleBounds = RuleParameterBounds()
      )
      val sections              = OptimisationReportRenderer.sections(report)
      val lines                 = sections.toMap.apply("Upgrade decision")
      def record(label: String) = io.circe.parser.parse(lines.find(_.startsWith(s"$label: ")).get.stripPrefix(s"$label: ")).toOption.get
      val baseline              = record("Selection baseline")
      val approvedRecord        = record("Approved candidate selection evidence")
      val rejectedRecord        = record("Rejected challenger 1 selection evidence")
      val approvedComparison    = record("Approved candidate financial comparison")
      val rejectedComparison    = record("Rejected challenger 1 financial comparison")
      baseline.hcursor.get[BigDecimal]("initialBalance") mustBe Right(BigDecimal(10000))
      baseline.hcursor.downField("coverage").downArray.get[String]("from") mustBe Right("2025-01-01T00:00:00Z")
      baseline.hcursor.downField("coverage").downArray.get[String]("to") mustBe Right("2025-04-30T23:59:59Z")
      baseline.hcursor.downField("monthlyProfits").get[BigDecimal]("2025-01") mustBe Right(BigDecimal(25))
      approvedRecord.hcursor.downField("monthlyProfits").get[BigDecimal]("2025-01") mustBe Right(BigDecimal(50))
      rejectedRecord.hcursor.downField("monthlyProfits").get[BigDecimal]("2025-01") mustBe Right(BigDecimal("12.5"))
      approvedComparison.hcursor.downField("monthlyNetDifferences").get[BigDecimal]("2025-01") mustBe Right(BigDecimal(25))
      rejectedComparison.hcursor.downField("monthlyNetDifferences").get[BigDecimal]("2025-01") mustBe Right(BigDecimal("-12.5"))
      val financialFields = UpgradeDecisionRenderer.evidence(base).asObject.get.keys.toSet - "candidate"
      List(baseline, approvedRecord, rejectedRecord).foreach { record =>
        record.asObject.get.keys.toSet mustBe financialFields
      }
      List(approvedComparison, rejectedComparison).foreach { record =>
        record.hcursor.downField("candidate").focus mustBe None
        record.hcursor.downField("base").focus mustBe None
        record.hcursor.downField("stressedNetImprovement").focus.isDefined mustBe true
      }
      val text      = sections.flatMap(_._2).mkString("\n")
      val baseLines = text.linesIterator.filter(_.startsWith("Base candidate: ")).toList
      baseLines must have size 1
      io.circe.parser.decode[TestStrategy](baseLines.head.stripPrefix("Base candidate: ")) mustBe Right(base.candidate)
      (text must not).include(base.candidate.indicator.toString)
    }

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
          BaselineReport("validation seed", Some(firstSeed), Some(OptimisationSearchSpace.SeedDisposition.Accepted)),
          BaselineReport("training seed", Some(secondSeed), Some(OptimisationSearchSpace.SeedDisposition.Accepted))
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
        20.millis,
        target = OptimisationTarget.Indicator,
        baseCandidate = target,
        fixedIndicators = Set.empty,
        ruleBounds = RuleParameterBounds()
      )
      val text = OptimisationReportRenderer.sections(report).flatMap { case (heading, lines) => heading :: lines }.mkString("\n")
      text must include("Search ranking: report-test")
      text must include("SEARCH LEADER:")
      text must include("Leading finalist source: searched candidate.")
      text must include("strongest seed on training (training seed): -0.500000")
      text must include("strongest seed on validation (validation seed): -0.400000")
      text must include("Shortlist training leader vs strongest seed on training (training seed): +1.000000")
      text must include("Shortlist training leader vs strongest seed on validation (validation seed): -0.500000")
      text must include("n/a (baseline is zero)")
      text must include("#1: first seen=7")
      text must include("retained (exact)")
      text must include("closed=20; forced=1; costs=3")
      text must include("portfolio drawdown=0.50%")
      text must include("Search + rescore: candidate requests=")
      text must include("Final rescore requests: 2 (includes missing baseline evaluations).")
      text must include("Search cache reuses: 9 (includes waiting on an in-flight computation).")
      text must include("reporting duration: 20 ms")
    }

    "compare a lost best candidate's segment net and trading activity while keeping its validation result separate" in {
      val target     = indicator(10)
      val champion   = indicator(20)
      val lost       = indicator(30)
      val breach     = ScoringFunction.Violation("trade count", "20", "at least 24")
      val targetFold = fold(0.5).copy(
        netProfit = BigDecimal("100.1234567"),
        costs = BigDecimal("3.1234567"),
        maxDrawdownPercent = BigDecimal("0.55555"),
        violations = List(breach)
      )
      val lostFold = fold(1.0).copy(
        netProfit = BigDecimal("125.1234567"),
        closedTrades = 24,
        forcedClosures = 2,
        costs = BigDecimal(4),
        maxDrawdownPercent = BigDecimal("1.236")
      )
      val report = OptimisationReport(
        round.name,
        round.corpus,
        Vector((champion, Fitness(0.5), Fitness(0.2))),
        List(BaselineReport("target", Some(target), None)),
        Map(
          target   -> CandidateDiagnostics(target, List(targetFold, fold(0.5)), Some(fold(0.2))),
          champion -> CandidateDiagnostics(champion, List(fold(0.5), fold(0.5)), Some(fold(0.2))),
          lost     -> CandidateDiagnostics(
            lost,
            List(lostFold, fold(1).copy(netProfit = BigDecimal(95))),
            Some(fold(0.1).copy(netProfit = BigDecimal(80)))
          )
        ),
        Map.empty,
        RunDiagnostics.Snapshot(bestSeen = Some(RunDiagnostics.Discovery(lost, 1.0, 0))),
        RunDiagnostics.Workload(),
        1.second,
        10.millis,
        target = OptimisationTarget.Indicator,
        baseCandidate = target,
        fixedIndicators = Set.empty,
        ruleBounds = RuleParameterBounds()
      )
      val sections    = OptimisationReportRenderer.sections(report).toMap
      val comparisons = sections("Baseline comparisons").mkString("\n")
      val details     = sections("Leader fold diagnostics").mkString("\n")
      val label       = "Best all-fold search candidate (diagnostic only; absent from final shortlist)"
      comparisons must include(
        s"$label vs target, search fold 1: net=+25.00000; closed=+4; forced=+1; " +
          "costs=+0.87654; portfolio drawdown=+0.68 percentage points; constraint breaches=1 -> 0"
      )
      comparisons must include(s"$label vs target, search fold 2: net=-5.00000")
      comparisons must include(s"$label vs target, validation period: net=-20.00000")
      comparisons must include(s"$label vs target, all-fold training fitness: +0.500000")
      comparisons must include(s"$label vs target, validation fitness: -0.100000")
      comparisons.indexOf("net=+25.00000") must be < comparisons.indexOf(s"$label vs target, all-fold training fitness")
      details must include("state resets between folds and open positions are liquidated at each fold's end")
      details must include("not a continuous multi-year backtest")
      details must include("net=125.12346; closed=24; forced=2; costs=4.00000; portfolio drawdown=1.24%")
    }

    "include seeds equivalent to the target in comparisons and distinguish restored aliases from literal matches" in {
      val candidate = indicator(10)
      val report    = OptimisationReport(
        round.name,
        round.corpus,
        Vector((candidate, Fitness(0.5), Fitness(0.2))),
        List(
          BaselineReport("target", Some(candidate), None),
          BaselineReport("literal", Some(candidate), Some(OptimisationSearchSpace.SeedDisposition.TargetDuplicate)),
          BaselineReport(
            "restored",
            Some(candidate),
            Some(OptimisationSearchSpace.SeedDisposition.TargetDuplicate),
            fixedInputsRestored = true
          )
        ),
        Map(candidate -> measured(candidate)),
        Map.empty,
        RunDiagnostics.Snapshot(firstSeen = Map(candidate -> 0)),
        RunDiagnostics.Workload(),
        1.second,
        10.millis,
        target = OptimisationTarget.Indicator,
        baseCandidate = candidate,
        fixedIndicators = Set.empty,
        ruleBounds = RuleParameterBounds()
      )
      val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
      text must include("strongest seed on training (literal): +0.000000")
      text must include("restored (fixed inputs restored): duplicate of target")
      text must include("baselines=target, literal, restored (fixed inputs restored)")
      text must include("Leading finalist source: target parameters.")
      (text must not).include("Leading finalist source: supplied seed parameters")
    }

    "identify selected seed parameters and their aliases without claiming they are the original seed strategy" in {
      val target    = indicator(10)
      val candidate = indicator(20)
      val report    = OptimisationReport(
        round.name,
        round.corpus,
        Vector((candidate, Fitness(0.5), Fitness(0.2))),
        List(
          BaselineReport("target", Some(target), None),
          BaselineReport("seed", Some(candidate), Some(OptimisationSearchSpace.SeedDisposition.Accepted)),
          BaselineReport(
            "alias",
            Some(candidate),
            Some(OptimisationSearchSpace.SeedDisposition.SeedDuplicate(0)),
            fixedInputsRestored = true
          )
        ),
        Map(target -> measured(target), candidate -> measured(candidate)),
        Map.empty,
        RunDiagnostics.Snapshot(),
        RunDiagnostics.Workload(),
        1.second,
        10.millis,
        target = OptimisationTarget.Indicator,
        baseCandidate = target,
        fixedIndicators = Set.empty,
        ruleBounds = RuleParameterBounds()
      )
      val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
      text must include(
        "Leading finalist source: supplied seed parameters (seed, alias (fixed inputs restored)); " +
          "evaluated with this round's fixed inputs."
      )
      text must include("#1: first seen=not observed")
      (text must not).include("no improvement")

      List(0.0, -0.1, Double.NaN).foreach { validation =>
        val rejected     = report.copy(finalists = Vector((candidate, Fitness(0.5), Fitness(validation))))
        val rejectedText = OptimisationReportRenderer.sections(rejected).flatMap(_._2).mkString("\n")
        rejectedText must include("Upgrade decision unavailable")
        (rejectedText must not).include("Leading finalist source:")
      }
      succeed
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
        10.millis,
        target = OptimisationTarget.Indicator,
        baseCandidate = candidate,
        fixedIndicators = Set.empty,
        ruleBounds = RuleParameterBounds()
      )
      val text = OptimisationReportRenderer.sections(report).flatMap(_._2).mkString("\n")
      text must include("Validation diagnostics unavailable: no validation measurements.")
      text must include("Validation: unavailable.")
      (text must not).include("Satisfies every constraint")
      (text must not).include("Leading finalist source:")
    }

    "handle an empty final population" in {
      OptimisationReportRenderer.verdict(round, Vector.empty, Nil) must contain("No candidates were evaluated.")
    }
  }
}
