package currexx.backtest.optimizer.reporting

import currexx.backtest.optimizer.*
import io.circe.Json
import io.circe.syntax.*

/** Renders the same immutable selection evidence for optimisation and walk-forward reports. */
object UpgradeDecisionRenderer:
  val formatVersion: Int = 2

  def objective(config: SearchObjectiveConfig): Json =
    val settings = config match
      case SearchObjectiveConfig.Current                  => Json.obj("mode" -> "Current".asJson)
      case SearchObjectiveConfig.BaselineRelative(weight) => Json.obj("mode" -> "BaselineRelative".asJson, "weight" -> weight.value.asJson)
    settings.deepMerge(Json.obj("version" -> config.version.asJson))

  def policy(config: UpgradePolicyConfig): Json = Json.obj(
    "version"                             -> config.version.asJson,
    "presetStatus"                        -> "research setting; not calibrated".asJson,
    "minNetImprovementFraction"           -> config.minNetImprovementFraction.value.asJson,
    "minCapitalImprovementPerMonth"       -> config.minCapitalImprovementPerMonth.value.asJson,
    "maxDrawdownIncreasePercentagePoints" -> config.maxDrawdownIncreasePercentagePoints.value.asJson,
    "minMonths"                           -> config.minMonths.value.asJson,
    "minWinningMonthRatio"                -> config.minWinningMonthRatio.value.asJson,
    "maxMonthlyNetDeclineFraction"        -> config.maxMonthlyNetDeclineFraction.value.asJson,
    "costMultiplier"                      -> config.costMultiplier.value.asJson,
    "costSensitivityMethod"               -> "fixed-trade accounting; not changed execution".asJson
  )

  def evidence(value: SelectionEvidence): Json = Json.obj(
    "candidate" -> value.candidate.asJson,
    "coverage"  -> value.coverage.value.toList
      .sortBy(_._1.toString)
      .map { case (pair, covered) =>
        Json.obj(
          "currencyPair"   -> pair.toString.asJson,
          "from"           -> covered.dataWindow.from.toString.asJson,
          "to"             -> covered.dataWindow.to.toString.asJson,
          "initialBalance" -> covered.initialBalance.value.asJson
        )
      }
      .asJson,
    "initialBalance"     -> value.initialBalance.value.asJson,
    "netProfit"          -> value.netProfit.asJson,
    "costs"              -> value.costs.value.asJson,
    "maxDrawdownPercent" -> value.maxDrawdownPercent.value.asJson,
    "monthlyProfits"     -> months(value.monthlyProfits.value),
    "monthsCovered"      -> value.monthsCovered.asJson,
    "absoluteScore"      -> value.score.value.asJson,
    "closedTrades"       -> value.closedTrades.value.asJson,
    "forcedClosures"     -> value.forcedClosures.value.asJson,
    "violations"         -> value.violations.map { violation =>
      Json.obj("constraint" -> violation.constraint.asJson, "actual" -> violation.actual.asJson, "required" -> violation.required.asJson)
    }.asJson
  )

  def decision(value: UpgradeDecision): Json = value match
    case UpgradeDecision.Approved(candidate, comparison, rejections) =>
      Json.obj(
        "outcome"    -> "UPGRADE APPROVED".asJson,
        "candidate"  -> candidate.asJson,
        "comparison" -> comparisonJson(comparison),
        "rejections" -> rejections.map(rejection).asJson
      )
    case UpgradeDecision.RetainBase(rejections) =>
      Json.obj("outcome" -> "BASE RETAINED".asJson, "rejections" -> rejections.map(rejection).asJson)

  def markdown(value: UpgradeDecision): List[String] =
    val selected = value match
      case UpgradeDecision.Approved(candidate, comparison, _) =>
        List(
          "**UPGRADE APPROVED** for further evaluation.",
          s"Approved indicator: `${escape(candidate.toString)}`",
          ""
        ) ++ comparisonMarkdown(comparison)
      case UpgradeDecision.RetainBase(_) => List("**BASE RETAINED**. No assessed challenger met all upgrade requirements.", "")
    val rejected = value.rejections.zipWithIndex.flatMap { case (item, index) =>
      List(s"Rejected challenger ${index + 1}: `${escape(item.candidate.toString)}`", "") ++ comparisonMarkdown(item.comparison) ++
        List("| Reason | Measured | Required |", "|---|---|---|") ++ item.failures.map { failure =>
          s"| ${escape(failure.code)} | ${escape(failure.actual)} | ${escape(failure.required)} |"
        } ++ List("")
    }
    selected ++ rejected

  private def comparisonJson(value: UpgradeComparison): Json = Json.obj(
    "base"                             -> evidence(value.base),
    "candidate"                        -> evidence(value.candidate),
    "minimumNetImprovement"            -> value.minimumNetImprovement.asJson,
    "netImprovement"                   -> value.netImprovement.asJson,
    "drawdownIncreasePercentagePoints" -> value.drawdownIncreasePercentagePoints.asJson,
    "monthlyNetDifferences"            -> months(value.monthlyNetDifferences),
    "winningMonths"                    -> value.winningMonths.asJson,
    "stressedBaseNet"                  -> value.stressedBaseNet.asJson,
    "stressedCandidateNet"             -> value.stressedCandidateNet.asJson,
    "stressedNetImprovement"           -> value.stressedNetImprovement.asJson
  )

  private def rejection(value: UpgradeRejection): Json = Json.obj(
    "candidate"  -> value.candidate.asJson,
    "comparison" -> comparisonJson(value.comparison),
    "failures"   -> value.failures.map { failure =>
      Json.obj("code" -> failure.code.asJson, "actual" -> failure.actual.asJson, "required" -> failure.required.asJson)
    }.asJson
  )

  private def comparisonMarkdown(value: UpgradeComparison): List[String] = List(
    "| Selection measurement | Challenger | Base |",
    "|---|---:|---:|",
    s"| Net profit | ${value.candidate.netProfit} | ${value.base.netProfit} |",
    s"| Costs | ${value.candidate.costs.value} | ${value.base.costs.value} |",
    s"| Maximum drawdown (%) | ${value.candidate.maxDrawdownPercent.value} | ${value.base.maxDrawdownPercent.value} |",
    s"| Net after cost stress | ${value.stressedCandidateNet} | ${value.stressedBaseNet} |",
    "",
    s"Net improvement: ${value.netImprovement}; required: ${value.minimumNetImprovement}. " +
      s"Stressed improvement: ${value.stressedNetImprovement}. Winning months: ${value.winningMonths}/${value.base.monthsCovered}.",
    "Cost stress uses fixed-trade accounting. It does not simulate different fills or trading behaviour.",
    ""
  )

  private def months(values: Map[java.time.YearMonth, BigDecimal]): Json =
    Json.obj(values.toList.sortBy(_._1).map { case (month, amount) => month.toString -> amount.asJson }*)

  private def escape(value: String): String = value.replace("\n", " ").replace("\r", " ").replace("|", "\\|")
