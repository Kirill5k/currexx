package currexx.backtest.optimizer

import currexx.backtest.types.OpenUnitInterval
import currexx.backtest.types.given

/** Search scores can change without changing the absolute score used to rank the validation shortlist. */
enum SearchObjectiveConfig {
  case Current
  case BaselineRelative(weight: OpenUnitInterval = 0.5)

  def version: String = "search-objective-v1"

  /** The baseline and candidate use the same fold and execution settings. */
  def adjust(
      qualityScore: Double,
      candidateNet: BigDecimal,
      baseline: SelectionEvidence,
      policyConfig: UpgradePolicyConfig
  ): Double = this match
    case Current                  => qualityScore
    case BaselineRelative(weight) =>
      if (qualityScore <= 0) 0.0
      else {
        val margin = policyConfig.minimumImprovement(baseline.netProfit, baseline.initialBalance.value, baseline.monthsCovered)
        qualityScore * (1 + weight.value * math.tanh(((candidateNet - baseline.netProfit) / margin).toDouble))
      }
}
