package currexx.backtest.walkforward

import currexx.algorithms.Parameters
import currexx.algorithms.operators.Validator
import currexx.backtest.{RiskSettings, TestSettings}
import currexx.backtest.MarketDataProvider.DateRange
import io.circe.Json
import io.circe.syntax.*

/** Pure rendering shared by persisted research records and their human-readable report. */
object WalkForwardReportRenderer:
  val evidenceLabel: String = "retrospective development evidence"

  final case class Failure(window: WalkForwardWindow, errorType: String, message: String)

  def manifest(experiment: WalkForwardExperiment, fingerprints: List[(String, String)]): Json =
    val round = experiment.round
    val risk  = RiskSettings()
    Json.obj(
      "experimentId"   -> experiment.id.asJson,
      "evidence"       -> evidenceLabel.asJson,
      "roundName"      -> round.name.asJson,
      "masterSeed"     -> experiment.masterSeed.asJson,
      "seedDerivation" -> WalkForwardWindow.seedVersion.asJson,
      "baseStrategy"   -> round.strategy.asJson,
      "search"         -> Json.obj(
        "parameters"    -> parameters(round.parameters),
        "scoring"       -> round.scoringFunction.description.asJson,
        "shortlistSize" -> round.shortlistSize.asJson,
        "extraSeeds"    -> round.extraSeeds.map(seed => Json.obj("name" -> seed.name.asJson, "indicator" -> seed.indicator.asJson)).asJson,
        "fixedIndicators"          -> round.fixedIndicators.toList.sortBy(_.toString).asJson,
        "selectionGate"            -> "selection fitness > 0".asJson,
        "selectionTieBandRelative" -> Validator.defaultTieBand.relative.asJson,
        "selectionOrdering" -> "Existing Validator consensus ordering; take the first passing finalist, otherwise retain the original base".asJson
      ),
      "simulation" -> Json.obj(
        "initialBalancePerPair" -> risk.initialBalance.value.asJson,
        "accountCurrency"       -> risk.accountCurrency.toString.asJson,
        "tradingParameters"     -> TestSettings.tradingParameters.asJson,
        "unitsPerLot"           -> risk.unitsPerLot.value.asJson,
        "spreadPips"            -> risk.transactionCosts.spreadPips.value.asJson,
        "slippagePipsPerSide"   -> risk.transactionCosts.slippagePipsPerSide.value.asJson,
        "commissionPerTrade"    -> risk.transactionCosts.commissionPerTrade.value.asJson,
        "quoteToAccountRates"   -> risk.quoteToAccountRates.toList
          .map { case (currency, rate) => currency.toString -> rate.value.asJson }
          .toMap
          .asJson,
        "conventions" -> executionConventions.asJson
      ),
      "schedule" -> experiment.plan.windows.map(window => windowJson(window, window.seed(experiment.masterSeed))).asJson,
      "history"  -> experiment.history.map { dataset =>
        Json.obj(
          "currencyPair" -> dataset.currencyPair.toString.asJson,
          "interval"     -> dataset.interval.toString.asJson,
          "files"        -> dataset.filePaths.toList.asJson,
          "range"        -> dataset.range.map(rangeJson).asJson
        )
      }.asJson,
      "sourceFiles" -> fingerprints.map { case (path, hash) => Json.obj("path" -> path.asJson, "sha256" -> hash.asJson) }.asJson
    )

  def preflight(readiness: List[WindowReadiness]): Json = readiness.map { ready =>
    Json.obj(
      "windowIndex" -> ready.window.index.asJson,
      "periods"     -> ready.periods.map { period =>
        Json.obj(
          "stage"          -> period.stage.asJson,
          "range"          -> rangeJson(period.range),
          "currencyPair"   -> period.currencyPair.asJson,
          "priceBars"      -> period.priceBars.asJson,
          "priorBars"      -> period.priorBars.asJson,
          "warmupBarsLost" -> period.warmupBarsLost.asJson,
          "executableBars" -> period.executableBars.asJson
        )
      }.asJson
    )
  }.asJson

  def frozen(window: WalkForwardWindow, seed: Long, selection: FrozenSelection): Json =
    Json.obj("evidence" -> evidenceLabel.asJson, "window" -> windowJson(window, seed), "selection" -> selectionJson(selection))

  def completed(result: WindowResult): Json =
    frozen(result.window, result.seed, result.selection).deepMerge(Json.obj("forward" -> forwardJson(result.forward)))

  def failed(failure: Failure): Json = Json.obj(
    "windowIndex" -> failure.window.index.asJson,
    "testPeriod"  -> rangeJson(failure.window.test),
    "errorType"   -> failure.errorType.asJson,
    "message"     -> failure.message.asJson
  )

  def summary(value: WalkForwardSummary): Json = Json.obj(
    "completedWindows"         -> value.completedWindows.asJson,
    "totalNetDifference"       -> value.totalNetDifference.asJson,
    "medianNetDifference"      -> value.medianNetDifference.asJson,
    "worstNetDifference"       -> value.worstNetDifference.asJson,
    "positiveWindows"          -> value.positiveWindows.asJson,
    "tiedWindows"              -> value.tiedWindows.asJson,
    "negativeWindows"          -> value.negativeWindows.asJson,
    "candidateSelectedWindows" -> value.candidateSelectedWindows.asJson,
    "baseSelectedWindows"      -> value.baseSelectedWindows.asJson,
    "noCandidatePassedWindows" -> value.noCandidatePassedWindows.asJson,
    "baseRetainedWindows"      -> value.baseRetainedWindows.asJson
  )

  def progress(experiment: WalkForwardExperiment, totals: WalkForwardSummary, failure: Option[Failure]): Json = Json.obj(
    "experimentId"   -> experiment.id.asJson,
    "evidence"       -> evidenceLabel.asJson,
    "status"         -> status(experiment, totals.completedWindows, failure).asJson,
    "plannedWindows" -> experiment.plan.windows.size.asJson,
    "summary"        -> summary(totals),
    "failure"        -> failure.map(failed).asJson
  )

  def markdown(
      experiment: WalkForwardExperiment,
      results: List[WindowResult],
      totals: WalkForwardSummary,
      failure: Option[Failure],
      readiness: List[WindowReadiness] = Nil
  ): String =
    val rows = results.map { result =>
      val forward = result.forward
      s"| ${result.window.index} | ${result.window.test} | ${outcomeLabel(result.selection.outcome)} | " +
        s"${forward.candidate.netProfit} | ${forward.base.netProfit} | ${forward.netDifference} | " +
        s"${forward.candidate.maxDrawdownPercent} / ${forward.base.maxDrawdownPercent} |"
    }
    val details = results.flatMap { result =>
      val candidate = result.forward.candidate
      val base      = result.forward.base
      List(
        s"## Window ${result.window.index}",
        "",
        s"Training: ${result.window.training}; selection: ${result.window.selection}; test: ${result.window.test}; seed: ${result.seed}.",
        s"Decision: ${outcomeLabel(result.selection.outcome)}. Training fitness: ${result.selection.trainingFitness.fold("n/a")(_.toString)}; " +
          s"selection fitness: ${result.selection.selectionFitness.fold("n/a")(_.toString)}.",
        "",
        "| Measurement | Frozen strategy | Original base | Difference |",
        "|---|---:|---:|---:|",
        s"| Net profit | ${candidate.netProfit} | ${base.netProfit} | ${result.forward.netDifference} |",
        s"| Costs | ${candidate.costs} | ${base.costs} | ${candidate.costs - base.costs} |",
        s"| Closed trades | ${candidate.closedTrades} | ${base.closedTrades} | ${candidate.closedTrades - base.closedTrades} |",
        s"| Forced closures | ${candidate.forcedClosures} | ${base.forcedClosures} | ${candidate.forcedClosures - base.forcedClosures} |",
        s"| Profit factor | ${display(candidate.profitFactor)} | ${display(base.profitFactor)} | — |",
        s"| Maximum drawdown (%) | ${candidate.maxDrawdownPercent} | ${base.maxDrawdownPercent} | " +
          s"${candidate.maxDrawdownPercent - base.maxDrawdownPercent} pp |",
        s"| Initial balance | ${candidate.initialBalance} | ${base.initialBalance} | ${candidate.initialBalance - base.initialBalance} |",
        "",
        "Execution coverage:",
        ""
      ) ++ result.forward.coverage.map { coverage =>
        s"- ${coverage.currencyPair}: first window ${coverage.firstWindow}; execution ${coverage.firstExecution} to ${coverage.lastMark} (last mark); " +
          s"initial warm-up loss ${coverage.warmupBarsLost} bars."
      } ++ List("", s"Full frozen strategy: [window-${result.window.index}-frozen.json](window-${result.window.index}-frozen.json).", "")
    }
    val failureLines = failure.toList.flatMap { error =>
      List(s"**Failed window ${error.window.index}:** ${escape(error.errorType)}: ${escape(error.message)}", "")
    }
    (
      List(
        s"# Walk-forward evaluation: ${experiment.round.name}",
        "",
        s"**$evidenceLabel**. Existing history has influenced strategy development; these results are not independent confirmation.",
        "",
        s"Experiment: `${experiment.id}`; master seed: ${experiment.masterSeed}; status: ${status(experiment, totals.completedWindows, failure)}.",
        s"Completed ${results.size} of ${experiment.plan.windows.size} windows. Full settings and source hashes: [manifest.json](manifest.json).",
        "",
        executionConventions,
        "Independent periods are summarised by paired net differences; their drawdowns and risk ratios are not pooled into a continuous account.",
        "",
        s"Total net difference: ${totals.totalNetDifference}; median: ${display(totals.medianNetDifference)}; " +
          s"worst: ${display(totals.worstNetDifference)}.",
        s"Positive/tied/negative windows: ${totals.positiveWindows}/${totals.tiedWindows}/${totals.negativeWindows}.",
        s"Base retained: ${totals.baseRetainedWindows} (${totals.baseSelectedWindows} selected by ranking; " +
          s"${totals.noCandidatePassedWindows} because no candidate passed).",
        ""
      ) ++ failureLines ++ List(
        "| Window | Test | Decision | Frozen net | Base net | Net difference | Drawdown % (frozen / base) |",
        "|---|---|---|---:|---:|---:|---:|"
      ) ++ rows ++ List("") ++ readinessMarkdown(readiness) ++ details
    ).mkString("\n")

  private def readinessMarkdown(readiness: List[WindowReadiness]): List[String] =
    val warnings = readiness.flatMap(ready => ready.periods.filter(_.warmupBarsLost > 0).map(ready.window.index -> _))
    if (readiness.isEmpty) List("Data readiness has not completed.", "")
    else
      val coverage =
        if (warnings.isEmpty) List("No periods lose initial bars to indicator warm-up.", "")
        else
          val rows = warnings.map { case (window, period) =>
            s"| $window | ${escape(period.stage)} | ${escape(period.currencyPair)} | ${period.range} | " +
              s"${period.warmupBarsLost} | ${period.executableBars} |"
          }
          List(
            "These periods lose initial bars to indicator warm-up. Their first complete window primes the simulator before execution begins.",
            "",
            "| Window | Stage | Pair | Period | Warm-up bars lost | Executable bars |",
            "|---|---|---|---|---:|---:|"
          ) ++ rows ++ List("")
      List(
        "## Data readiness",
        "",
        "All requested periods were checked before searching. Full coverage: [preflight.json](preflight.json).",
        ""
      ) ++
        coverage

  private val executionConventions =
    "Each training fold, selection period and test starts with fresh state. Prior bars provide indicator lookback only. " +
      "The first in-period window primes the simulator; execution begins on the next bar. " +
      "State continues across source-file boundaries. Open positions are liquidated at the period end. " +
      "Candidate and base use identical execution, costs and capital. The original base and search settings remain fixed across windows."

  private def parameters(value: Parameters.GA | Parameters.SCGA): Json = value match
    case p: Parameters.GA =>
      Json.obj(
        "algorithm"            -> p.name.asJson,
        "populationSize"       -> p.populationSize.asJson,
        "maxGen"               -> p.maxGen.asJson,
        "crossoverProbability" -> p.crossoverProbability.asJson,
        "mutationProbability"  -> p.mutationProbability.asJson,
        "elitismRatio"         -> p.elitismRatio.asJson,
        "shuffle"              -> p.shuffle.asJson,
        "initialOversampling"  -> p.initialOversampling.asJson
      )
    case p: Parameters.SCGA =>
      Json.obj(
        "algorithm"                     -> p.name.asJson,
        "populationSize"                -> p.populationSize.asJson,
        "maxGen"                        -> p.maxGen.asJson,
        "crossoverProbability"          -> p.crossoverProbability.asJson,
        "mutationProbability"           -> p.mutationProbability.asJson,
        "shuffle"                       -> p.shuffle.asJson,
        "initialOversampling"           -> p.initialOversampling.asJson,
        "speciesRadius"                 -> p.speciesRadius.asJson,
        "maxSpecies"                    -> p.maxSpecies.asJson,
        "effectiveMaxSpecies"           -> p.effectiveMaxSpecies.asJson,
        "interspeciesMatingProbability" -> p.interspeciesMatingProbability.asJson
      )

  private def rangeJson(range: DateRange): Json =
    Json.obj("from" -> range.from.toString.asJson, "untilExclusive" -> range.until.toString.asJson)

  private def windowJson(window: WalkForwardWindow, seed: Long): Json = Json.obj(
    "index"         -> window.index.asJson,
    "seed"          -> seed.asJson,
    "trainingFolds" -> window.trainingFolds.map(rangeJson).asJson,
    "selection"     -> rangeJson(window.selection),
    "test"          -> rangeJson(window.test)
  )

  private def selectionJson(selection: FrozenSelection): Json = Json.obj(
    "outcome"          -> selection.outcome.toString.asJson,
    "strategy"         -> selection.strategy.asJson,
    "trainingFitness"  -> selection.trainingFitness.asJson,
    "selectionFitness" -> selection.selectionFitness.asJson
  )

  private def metrics(value: ForwardMetrics): Json = Json.obj(
    "netProfit"          -> value.netProfit.asJson,
    "costs"              -> value.costs.asJson,
    "closedTrades"       -> value.closedTrades.asJson,
    "forcedClosures"     -> value.forcedClosures.asJson,
    "profitFactor"       -> value.profitFactor.asJson,
    "maxDrawdownPercent" -> value.maxDrawdownPercent.asJson,
    "initialBalance"     -> value.initialBalance.asJson
  )

  private def forwardJson(value: ForwardResult): Json = Json.obj(
    "candidate"  -> metrics(value.candidate),
    "base"       -> metrics(value.base),
    "difference" -> Json.obj(
      "netProfit"                   -> value.netDifference.asJson,
      "costs"                       -> (value.candidate.costs - value.base.costs).asJson,
      "closedTrades"                -> (value.candidate.closedTrades - value.base.closedTrades).asJson,
      "forcedClosures"              -> (value.candidate.forcedClosures - value.base.forcedClosures).asJson,
      "maxDrawdownPercentagePoints" -> (value.candidate.maxDrawdownPercent - value.base.maxDrawdownPercent).asJson
    ),
    "coverage" -> value.coverage.map { coverage =>
      Json.obj(
        "currencyPair"   -> coverage.currencyPair.asJson,
        "firstWindow"    -> coverage.firstWindow.toString.asJson,
        "firstExecution" -> coverage.firstExecution.toString.asJson,
        "lastMark"       -> coverage.lastMark.toString.asJson,
        "warmupBarsLost" -> coverage.warmupBarsLost.asJson
      )
    }.asJson
  )

  private def status(experiment: WalkForwardExperiment, completedWindows: Int, failure: Option[Failure]): String =
    if (failure.nonEmpty) "failed" else if (completedWindows == experiment.plan.windows.size) "completed" else "running"

  private def outcomeLabel(value: SelectionOutcome): String = value match
    case SelectionOutcome.CandidateSelected => "Candidate selected"
    case SelectionOutcome.BaseSelected      => "Base selected by ranking"
    case SelectionOutcome.NoCandidatePassed => "Base retained: no candidate passed"

  private def display(value: Option[BigDecimal]): String = value.fold("n/a")(_.toString)
  private def escape(value: String): String              = value.replace("\n", " ").replace("\r", " ").replace("|", "\\|")
