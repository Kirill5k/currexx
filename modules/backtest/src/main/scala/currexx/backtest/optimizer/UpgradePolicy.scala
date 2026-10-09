package currexx.backtest.optimizer

import cats.syntax.foldable.*
import currexx.backtest.types.{AtLeastOneBigDecimal, PositiveUnitInterval}
import currexx.backtest.types.given
import currexx.domain.signal.Indicator
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, PosBigDecimal, PosInt}

import java.time.YearMonth

/** Trial settings for research. These values have not been calibrated as a claim of reliability. */
final case class UpgradePolicyConfig(
    minNetImprovementFraction: NonNegBigDecimal = BigDecimal("0.05"),
    minCapitalImprovementPerMonth: PosBigDecimal = BigDecimal("0.0005"),
    maxDrawdownIncreasePercentagePoints: NonNegBigDecimal = BigDecimal("0.10"),
    minMonths: PosInt = 3,
    minWinningMonthRatio: PositiveUnitInterval = 2.0 / 3.0,
    maxMonthlyNetDeclineFraction: NonNegBigDecimal = BigDecimal("0.001"),
    costMultiplier: AtLeastOneBigDecimal = BigDecimal("1.5")
) {
  def version: String = "trial-v1"

  def minimumImprovement(baseNet: BigDecimal, capital: BigDecimal, months: Int): BigDecimal =
    (minNetImprovementFraction.value * baseNet.max(BigDecimal(0))).max(minCapitalImprovementPerMonth.value * capital * months)
}

final case class UpgradeFailure(code: String, actual: String, required: String)

final case class UpgradeComparison(
    base: SelectionEvidence,
    candidate: SelectionEvidence,
    minimumNetImprovement: BigDecimal,
    netImprovement: BigDecimal,
    drawdownIncreasePercentagePoints: BigDecimal,
    monthlyNetDifferences: Map[YearMonth, BigDecimal],
    winningMonths: Int,
    stressedBaseNet: BigDecimal,
    stressedCandidateNet: BigDecimal
):
  def stressedNetImprovement: BigDecimal = stressedCandidateNet - stressedBaseNet

final case class UpgradeRejection(candidate: Indicator, comparison: UpgradeComparison, failures: List[UpgradeFailure])

enum UpgradeDecision:
  case Approved(candidate: Indicator, comparison: UpgradeComparison, rejections: List[UpgradeRejection] = Nil)
  case RetainBase(rejections: List[UpgradeRejection])

  def rejections: List[UpgradeRejection]

/** The policy reads fixed evidence only. It cannot run simulations, change ranking, or inspect forward-period results. */
object UpgradePolicy {

  def decide(
      base: SelectionEvidence,
      evidenceInValidationOrder: List[SelectionEvidence],
      config: UpgradePolicyConfig = UpgradePolicyConfig()
  ): Either[IllegalArgumentException, UpgradeDecision] =
    evidenceInValidationOrder
      .traverse_ { candidate =>
        Either.cond(
          candidate.coverage == base.coverage,
          (),
          new IllegalArgumentException("Candidate selection coverage or capital differs from the base")
        )
      }
      .map { _ =>
        val challengers = evidenceInValidationOrder.filterNot(_.candidate == base.candidate).distinctBy(_.candidate)
        challengers.foldLeft[UpgradeDecision](UpgradeDecision.RetainBase(Nil)) {
          case (approved: UpgradeDecision.Approved, _)             => approved
          case (UpgradeDecision.RetainBase(rejections), candidate) =>
            val comparison = compare(base, candidate, config)
            failures(comparison, config) match
              case Nil    => UpgradeDecision.Approved(candidate.candidate, comparison, rejections)
              case failed => UpgradeDecision.RetainBase(rejections :+ UpgradeRejection(candidate.candidate, comparison, failed))
        }
      }

  private def compare(base: SelectionEvidence, candidate: SelectionEvidence, config: UpgradePolicyConfig): UpgradeComparison = {
    val differences = base.monthlyProfits.value.map { case (month, profit) => month -> (candidate.monthlyProfits.value(month) - profit) }
    UpgradeComparison(
      base = base,
      candidate = candidate,
      minimumNetImprovement = config.minimumImprovement(base.netProfit, base.initialBalance.value, base.monthsCovered),
      netImprovement = candidate.netProfit - base.netProfit,
      drawdownIncreasePercentagePoints = candidate.maxDrawdownPercent.value - base.maxDrawdownPercent.value,
      monthlyNetDifferences = differences,
      winningMonths = differences.values.count(_ > 0),
      stressedBaseNet = base.netProfit - (config.costMultiplier.value - 1) * base.costs.value,
      stressedCandidateNet = candidate.netProfit - (config.costMultiplier.value - 1) * candidate.costs.value
    )
  }

  private def failures(comparison: UpgradeComparison, config: UpgradePolicyConfig): List[UpgradeFailure] = {
    val candidate  = comparison.candidate
    val months     = candidate.monthsCovered
    val worstMonth = comparison.monthlyNetDifferences.values.min
    val worstLimit = -config.maxMonthlyNetDeclineFraction.value * comparison.base.initialBalance.value
    def failure(passed: Boolean, code: String, actual: => String, required: => String): Option[UpgradeFailure] =
      Option.unless(passed)(UpgradeFailure(code, actual, required))

    List(
      failure(candidate.score.value > 0, "absolute-fitness", candidate.score.value.toString, "> 0"),
      failure(candidate.netProfit > 0, "positive-net-profit", candidate.netProfit.toString, "> 0"),
      failure(
        comparison.netImprovement >= comparison.minimumNetImprovement,
        "net-improvement",
        comparison.netImprovement.toString,
        s">= ${comparison.minimumNetImprovement}"
      ),
      failure(
        comparison.drawdownIncreasePercentagePoints <= config.maxDrawdownIncreasePercentagePoints.value,
        "drawdown-increase",
        comparison.drawdownIncreasePercentagePoints.toString,
        s"<= ${config.maxDrawdownIncreasePercentagePoints.value} percentage points"
      ),
      failure(months >= config.minMonths.value, "months-covered", months.toString, s">= ${config.minMonths.value}"),
      failure(
        comparison.winningMonths.toDouble / months >= config.minWinningMonthRatio.value,
        "winning-months",
        s"${comparison.winningMonths}/$months",
        s">= ${config.minWinningMonthRatio.value}"
      ),
      failure(worstMonth >= worstLimit, "worst-month", worstMonth.toString, s">= $worstLimit"),
      failure(comparison.stressedCandidateNet > 0, "stressed-net-profit", comparison.stressedCandidateNet.toString, "> 0"),
      failure(
        comparison.stressedNetImprovement >= comparison.minimumNetImprovement,
        "stressed-net-improvement",
        comparison.stressedNetImprovement.toString,
        s">= ${comparison.minimumNetImprovement}"
      )
    ).flatten ++ candidate.violations.map(v => UpgradeFailure("absolute-constraint", s"${v.constraint}: ${v.actual}", v.required))
  }

}
