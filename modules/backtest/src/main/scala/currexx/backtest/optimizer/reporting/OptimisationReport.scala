package currexx.backtest.optimizer.reporting

import currexx.algorithms.ValidatedPopulation
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.{
  IndicatorObjective,
  IndicatorSearchSpace,
  OptimisationResult,
  ScoringFunction,
  SearchObjectiveConfig,
  UpgradePolicyConfig
}
import currexx.domain.signal.Indicator

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
    indicator: Indicator,
    searchFolds: List[FoldDiagnostics],
    validation: Option[FoldDiagnostics]
):
  def trainingFitness: Double = IndicatorObjective.FoldAggregation.combine(searchFolds.map(_.score))

/** No disposition denotes the target; seeds retain their resolution even when they alias another baseline. */
final case class BaselineReport(
    name: String,
    effective: Option[Indicator],
    disposition: Option[IndicatorSearchSpace.SeedDisposition],
    fixedInputsRestored: Boolean = false
)

enum DuplicateKind:
  case Exact, FixedInputsRestored

/** Matching parameters identify the same strategy only when the trading rules also match. */
final case class CatalogueMatch(name: String, kind: DuplicateKind, sameRules: Boolean = true)

final case class OptimisationReport(
    roundName: String,
    corpus: Corpus,
    finalists: ValidatedPopulation[Indicator],
    baselines: List[BaselineReport],
    candidates: Map[Indicator, CandidateDiagnostics],
    catalogueMatches: Map[Indicator, List[CatalogueMatch]],
    diagnostics: RunDiagnostics.Snapshot,
    reportingWorkload: RunDiagnostics.Workload,
    optimisationDuration: FiniteDuration,
    reportingDuration: FiniteDuration,
    upgrade: Option[OptimisationResult] = None,
    searchObjective: SearchObjectiveConfig = SearchObjectiveConfig.Current,
    upgradePolicy: UpgradePolicyConfig = UpgradePolicyConfig(),
    scoringDescription: String = "unspecified"
)
