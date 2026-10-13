package currexx.backtest.optimizer.reporting

import currexx.algorithms.ValidatedPopulation
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.strategy.RuleParameterBounds
import currexx.domain.signal.Indicator
import currexx.backtest.optimizer.{
  FoldAggregation,
  OptimisationResult,
  OptimisationSearchSpace,
  OptimisationTarget,
  ScoringFunction,
  SearchObjectiveConfig,
  UpgradePolicyConfig
}
import currexx.backtest.TestStrategy

import scala.concurrent.duration.FiniteDuration

/** Compact measurements only: reports never retain completed trades or equity histories. */
final case class FoldDiagnostics(
    score: Double,
    netProfit: BigDecimal,
    closedTrades: Int,
    forcedClosures: Int,
    costs: BigDecimal,
    maxDrawdownPercent: BigDecimal,
    violations: List[ScoringFunction.Violation]
)

final case class CandidateDiagnostics(
    candidate: TestStrategy,
    searchFolds: List[FoldDiagnostics],
    validation: Option[FoldDiagnostics]
):
  def trainingFitness: Double = FoldAggregation.combine(searchFolds.map(_.score))

/** No disposition denotes the target; seeds retain their resolution even when they alias another baseline. */
final case class BaselineReport(
    name: String,
    effective: Option[TestStrategy],
    disposition: Option[OptimisationSearchSpace.SeedDisposition],
    fixedInputsRestored: Boolean = false,
    reason: Option[String] = None
)

enum DuplicateKind:
  case Exact, FixedInputsRestored

enum CatalogueScope:
  case FullCandidate, IndicatorsOnly, RulesOnly

/** Component matches do not identify the same complete strategy. */
final case class CatalogueMatch(name: String, kind: DuplicateKind, scope: CatalogueScope = CatalogueScope.FullCandidate)

final case class OptimisationReport(
    roundName: String,
    corpus: Corpus,
    finalists: ValidatedPopulation[TestStrategy],
    baselines: List[BaselineReport],
    candidates: Map[TestStrategy, CandidateDiagnostics],
    catalogueMatches: Map[TestStrategy, List[CatalogueMatch]],
    diagnostics: RunDiagnostics.Snapshot,
    reportingWorkload: RunDiagnostics.Workload,
    optimisationDuration: FiniteDuration,
    reportingDuration: FiniteDuration,
    upgrade: Option[OptimisationResult] = None,
    searchObjective: SearchObjectiveConfig = SearchObjectiveConfig.Current,
    upgradePolicy: UpgradePolicyConfig = UpgradePolicyConfig(),
    scoringDescription: String = "unspecified",
    target: OptimisationTarget,
    baseCandidate: TestStrategy,
    fixedIndicators: Set[Indicator],
    ruleBounds: RuleParameterBounds
)
