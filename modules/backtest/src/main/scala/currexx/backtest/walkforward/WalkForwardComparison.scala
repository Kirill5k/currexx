package currexx.backtest.walkforward

import currexx.backtest.optimizer.reporting.UpgradeDecisionRenderer
import io.circe.Json
import io.circe.syntax.*

/** Compares independent runs without pooling their accounts or treating reused history as unseen evidence. */
object WalkForwardComparison:
  final case class Run(experiment: WalkForwardExperiment, summary: WalkForwardSummary)

  def json(runs: List[Run]): Json = Json.obj(
    "formatVersion" -> UpgradeDecisionRenderer.formatVersion.asJson,
    "evidence"      -> WalkForwardReportRenderer.evidenceLabel.asJson,
    "runs"          -> runs.map { run =>
      Json.obj(
        "experimentId"  -> run.experiment.id.asJson,
        "masterSeed"    -> run.experiment.masterSeed.asJson,
        "objective"     -> UpgradeDecisionRenderer.objective(run.experiment.round.searchObjective),
        "upgradePolicy" -> UpgradeDecisionRenderer.policy(run.experiment.round.upgradePolicy),
        "summary"       -> WalkForwardReportRenderer.summary(run.summary)
      )
    }.asJson
  )

  def markdown(runs: List[Run]): String =
    val rows = runs.map { run =>
      val totals = run.summary
      s"| [${run.experiment.id}](../${run.experiment.id}/report.md) | ${run.experiment.round.searchObjective} | " +
        s"${run.experiment.masterSeed} | ${totals.totalNetDifference} | ${display(totals.medianNetDifference)} | " +
        s"${display(totals.worstNetDifference)} | ${totals.positiveWindows}/${totals.completedWindows} | ${totals.upgradeApprovedWindows} |"
    }
    (
      List(
        "# Search objective comparison",
        "",
        s"**${WalkForwardReportRenderer.evidenceLabel}**. Reused history is development evidence, not independent confirmation.",
        "Each row uses separate search runs and fresh accounts. Differences compare the frozen strategy with the fixed original base.",
        "Freeze settings before a final test on new data. Approval qualifies a strategy for further evaluation; it does not deploy it.",
        "",
        "| Experiment | Objective | Master seed | Total net difference | Median | Worst | Improved windows | Upgrades approved |",
        "|---|---|---:|---:|---:|---:|---:|---:|"
      ) ++ rows ++ List("")
    ).mkString("\n")

  private def display(value: Option[BigDecimal]): String = value.fold("n/a")(_.toString)
