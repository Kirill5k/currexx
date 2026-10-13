package currexx.backtest.optimizer.reporting

import currexx.algorithms.ValidatedPopulation
import currexx.algorithms.operators.Validator
import currexx.backtest.optimizer.{
  OptimisationRound,
  OptimisationSearchSpace,
  OptimisationTarget,
  ScoringFunction,
  SearchObjectiveConfig,
  UpgradeDecision
}
import currexx.backtest.TestStrategy
import io.circe.syntax.*

/** Text is derived from completed measurements; rendering never evaluates a candidate. */
object OptimisationReportRenderer:
  def incompleteDiagnostics(roundName: String, error: Throwable): (String, List[String]) =
    s"Incomplete diagnostics: $roundName" -> List(
      "Optimisation and upgrade decision are complete; reporting failed. Remaining rounds can continue.",
      error.toString
    )

  val progressIntro: List[String] = List(
    "Top-member fitness uses the generation's rotating search folds; values across generations are not directly comparable.",
    "Stable progress uses all search folds. First seen means the first successful search evaluation; initial population is generation 0."
  )

  def progress(snapshot: RunDiagnostics.Snapshot, generation: Int, foldCount: Int): List[String] =
    val fold = if (foldCount <= 1) "none" else (Math.floorMod(generation, foldCount) + 1).toString
    val best = snapshot.bestSeen.fold("No successful search evaluations.") { found =>
      f"Best all-fold fitness seen: ${found.fitness}%.6f; first seen generation ${found.generation}."
    }
    List(
      s"Excluded search fold for selection: $fold",
      best,
      s"Search requests: ${snapshot.searchRequests}; distinct successfully searched candidates: ${snapshot.distinctSearchCandidates}."
    )

  def sections(report: OptimisationReport): List[(String, List[String])] =
    val breaches =
      report.finalists.headOption.toList.flatMap(c => report.candidates.get(c._1).toList.flatMap(_.validation.toList.flatMap(_.violations)))
    val validationAvailable = report.finalists.headOption.exists(c => report.candidates.get(c._1).exists(_.validation.nonEmpty))
    List(
      s"Search ranking: ${report.roundName}" -> (report.corpus.describe ::: List("") ::: outcome(
        report.finalists,
        breaches,
        validationAvailable,
        report.searchObjective,
        CandidateDisplay.forTarget(report.target)
      ) ::: selectionSource(report)),
      "Search configuration"             -> configurationLines(report),
      "Parameter changes"                -> parameterChanges(report),
      "Upgrade decision"                 -> upgradeLines(report),
      "Baseline measurements"            -> baselineLines(report),
      "Baseline comparisons"             -> comparisonLines(report),
      "Leader fold diagnostics"          -> leaderLines(report),
      "Finalist provenance"              -> provenanceLines(report),
      "Search observations and workload" -> workloadLines(report)
    )

  def verdict(
      round: OptimisationRound,
      population: ValidatedPopulation[TestStrategy],
      championBreaches: List[ScoringFunction.Violation]
  ): List[String] = round.corpus.describe ::: List("") ::: outcome(
    population,
    championBreaches,
    round.corpus.validationFold.nonEmpty,
    round.searchObjective,
    CandidateDisplay.forTarget(round.target)
  )

  private def outcome(
      population: ValidatedPopulation[TestStrategy],
      breaches: List[ScoringFunction.Violation],
      validationAvailable: Boolean,
      objective: SearchObjectiveConfig,
      renderCandidate: TestStrategy => String
  ): List[String] =
    population.headOption match
      case None                                   => List("No candidates were evaluated.")
      case Some((champion, training, validation)) =>
        val searchLeader = population.maxBy(_._2.value)
        val retained     =
          if (objective == SearchObjectiveConfig.Current && training.value > 0.0)
            List(f"Validation/training fitness ratio: ${validation.value / training.value * 100}%.1f%% (diagnostic only).")
          else Nil
        val breachLines =
          if (!validationAvailable) List("Validation diagnostics unavailable: no validation measurements.")
          else if (breaches.isEmpty) List("Satisfies every constraint on validation data.")
          else s"BREACHES ${breaches.size} constraint(s) on validation data:" :: breaches.map(breach => s"  - $breach")
        List(
          f"SEARCH LEADER: search fitness=${searchLeader._2.value}%.6f; candidate=${renderCandidate(searchLeader._1)}",
          f"Leading finalist: search fitness=${training.value}%.6f; absolute validation fitness=${validation.value}%.6f",
          s"Candidate: ${renderCandidate(champion)}",
          "Ranking is diagnostic. Only the separate upgrade decision authorises a candidate for further evaluation."
        ) ::: retained ::: breachLines

  private def upgradeLines(report: OptimisationReport): List[String] =
    List(
      s"Format version: ${UpgradeDecisionRenderer.formatVersion}",
      s"Absolute scoring: ${report.scoringDescription}",
      s"Search objective: ${UpgradeDecisionRenderer.objective(report.searchObjective).noSpaces}",
      s"Upgrade policy: ${UpgradeDecisionRenderer.policy(report.upgradePolicy).noSpaces}"
    ) ::: report.upgrade.fold(List("Upgrade decision unavailable; ranking does not establish an upgrade.")) { result =>
      UpgradeDecisionRenderer.markdown(result.decision, CandidateDisplay.forTarget(report.target)) :::
        UpgradeDecisionRenderer.financialRecords(result) :::
        List(s"Recorded decision: ${UpgradeDecisionRenderer.decision(result.decision).noSpaces}")
    }

  private def configurationLines(report: OptimisationReport): List[String] =
    val fixed = report.target match
      case OptimisationTarget.Indicator => List("Rules stay fixed to the base.")
      case OptimisationTarget.Strategy  => List("Indicators stay fixed to the base.")
      case OptimisationTarget.Both      => Nil
    val fixedIndicators = Option
      .when(report.target != OptimisationTarget.Strategy)(
        s"Fixed indicator inputs: ${report.fixedIndicators.toList.sortBy(_.toString).mkString(", ")}"
      )
      .toList
    List(
      s"Optimisation target: ${report.target}",
      s"Rule parameter bounds: ${RuleBoundsRenderer.json(report.ruleBounds).noSpaces}"
    ) ::: fixedIndicators ::: fixed ::: List(s"Base candidate: ${report.baseCandidate.asJson.noSpaces}")

  private def parameterChanges(report: OptimisationReport): List[String] =
    report.finalists.toList.zipWithIndex.flatMap { case ((candidate, _, _), index) =>
      val changes = CandidateParameters.changes(report.baseCandidate, candidate)
      s"Finalist #${index + 1}:" :: (if (changes.isEmpty) List("  No parameter changes.") else changes.map("  " + _))
    }

  private def baselineLines(report: OptimisationReport): List[String] =
    List("Every seed uses this round's target and fixed inputs.") ::: report.baselines.flatMap { baseline =>
      val disposition = baseline.disposition.fold("target") {
        case OptimisationSearchSpace.SeedDisposition.Accepted             => "accepted seed"
        case OptimisationSearchSpace.SeedDisposition.TargetDuplicate      => "duplicate of target"
        case OptimisationSearchSpace.SeedDisposition.SeedDuplicate(index) =>
          val name = report.baselines.filter(_.disposition.nonEmpty).lift(index).fold(s"#${index + 1}")(_.name)
          s"duplicate of seed '$name'"
        case OptimisationSearchSpace.SeedDisposition.Incompatible => "incompatible; excluded from search and replay"
      }
      val measurement = baseline.effective.flatMap(report.candidates.get).fold("not evaluated") { candidate =>
        val validation = candidate.validation.fold("n/a")(fold => f"${fold.score}%.6f")
        f"training=${candidate.trainingFitness}%.6f; validation=$validation"
      }
      val matches = baseline.effective.toList.flatMap(candidate => report.catalogueMatches.getOrElse(candidate, Nil))
      List(s"${baselineLabel(baseline)}: $disposition; $measurement", s"  ${catalogueText(matches)}") ::: baseline.reason.toList.map(
        reason => s"  Reason: $reason"
      )
    }

  private def selectionSource(report: OptimisationReport): List[String] =
    report.finalists.headOption.filter { case (_, _, validation) => Validator.passesGate(validation) }.toList.map { case (champion, _, _) =>
      val matches = report.baselines.filter(_.effective.contains(champion))
      if (matches.exists(_.disposition.isEmpty)) "Leading finalist source: target parameters."
      else if (matches.nonEmpty)
        s"Leading finalist source: supplied seed parameters (${matches.map(baselineLabel).mkString(", ")}); " +
          "evaluated with this round's fixed inputs."
      else "Leading finalist source: searched candidate."
    }

  private def comparisonLines(report: OptimisationReport): List[String] =
    val leaders = reportCandidates(report)
    if (leaders.isEmpty) List("No searched candidate to compare.")
    else
      val target = report.baselines.find(_.disposition.isEmpty).flatMap(_.effective).flatMap(report.candidates.get)
      val seeds  = report.baselines
        .filter(_.disposition.nonEmpty)
        .sortBy(_.name)
        .flatMap { baseline =>
          baseline.effective.flatMap(report.candidates.get).map(baselineLabel(baseline) -> _)
        }
        .distinctBy(_._2.candidate)
      val trainingSeed   = seeds.sortBy { case (name, candidate) => (-candidate.trainingFitness, name) }.headOption
      val validationSeed = seeds
        .flatMap { case (name, candidate) => candidate.validation.map(fold => name -> fold.score) }
        .sortBy { case (name, score) => (-score, name) }
        .headOption
      leaders.flatMap { case (label, candidate) =>
        target.toList.flatMap(reference => foldComparisons(label, candidate, reference)) :::
          target.toList.flatMap { reference =>
            List(s"$label vs target, all-fold training fitness: ${delta(candidate.trainingFitness, reference.trainingFitness)}") :::
              (for {
                validation <- candidate.validation.toList
                baseline   <- reference.validation.toList
              } yield s"$label vs target, validation fitness: ${delta(validation.score, baseline.score)}")
          } ::: trainingSeed.toList.map { case (name, reference) =>
            s"$label vs strongest seed on training ($name): ${delta(candidate.trainingFitness, reference.trainingFitness)}"
          } ::: validationSeed.toList.flatMap { case (name, score) =>
            candidate.validation.toList.map(validation =>
              s"$label vs strongest seed on validation ($name): ${delta(validation.score, score)}"
            )
          }
      } ::: List("Fitness improvements describe these datasets; they do not establish independent out-of-sample improvement.")

  private def foldComparisons(label: String, candidate: CandidateDiagnostics, target: CandidateDiagnostics): List[String] =
    candidate.searchFolds.zip(target.searchFolds).zipWithIndex.map { case ((fold, reference), index) =>
      s"$label vs target, search fold ${index + 1}: ${foldDelta(fold, reference)}"
    } ::: (for {
      fold      <- candidate.validation.toList
      reference <- target.validation.toList
    } yield s"$label vs target, validation period: ${foldDelta(fold, reference)}")

  private def foldDelta(fold: FoldDiagnostics, target: FoldDiagnostics): String =
    f"net=${fold.netProfit - target.netProfit}%+.5f; closed=${fold.closedTrades - target.closedTrades}%+d; " +
      f"forced=${fold.forcedClosures - target.forcedClosures}%+d; costs=${fold.costs - target.costs}%+.5f; " +
      f"portfolio drawdown=${fold.maxDrawdownPercent - target.maxDrawdownPercent}%+.2f percentage points; " +
      s"constraint breaches=${target.violations.size} -> ${fold.violations.size}"

  private def delta(value: Double, baseline: Double): String =
    val relative = if (baseline > 0.0) f"${(value - baseline) / baseline * 100}%+.2f%%" else "n/a (baseline is zero)"
    f"${value - baseline}%+.6f ($relative)"

  private def leaderLines(report: OptimisationReport): List[String] =
    List(
      "Each fold is a separate simulation: state resets between folds and open positions are liquidated at each fold's end.",
      "Their net results are not a continuous multi-year backtest and should not be interpreted as BatchBacktester's continuous net."
    ) ::: reportCandidates(report).flatMap { case (label, candidate) =>
      List(s"$label:", s"Candidate: ${CandidateDisplay.forTarget(report.target)(candidate.candidate)}") :::
        candidate.searchFolds.zipWithIndex.flatMap { case (fold, index) => foldLines(s"Search fold ${index + 1}", fold) } :::
        candidate.validation.fold(List("Validation: unavailable."))(foldLines("Validation", _))
    }

  private def reportCandidates(report: OptimisationReport): List[(String, CandidateDiagnostics)] =
    val first    = report.finalists.headOption.map(_._1)
    val training = report.finalists.sortBy(c => -c._2.value).headOption.map(_._1).filterNot(first.contains)
    val best     =
      report.diagnostics.bestSeen.map(_.candidate).filterNot(candidate => first.contains(candidate) || training.contains(candidate))
    val approved = report.upgrade.toList.flatMap(_.decision match
      case UpgradeDecision.Approved(candidate, _, _) => List("Approved upgrade" -> candidate)
      case UpgradeDecision.RetainBase(_)             => Nil)
    val roles = first.toList.map("Leading finalist" -> _) ++ training.toList.map("Shortlist training leader" -> _) ++ best.toList.map {
      candidate =>
        val label =
          if (report.finalists.exists(_._1 == candidate)) "Best all-fold search candidate"
          else "Best all-fold search candidate (diagnostic only; absent from final shortlist)"
        label -> candidate
    }
    (roles ++ approved).distinctBy(_._2).flatMap { case (label, candidate) => report.candidates.get(candidate).map(label -> _) }

  private def foldLines(label: String, fold: FoldDiagnostics): List[String] =
    List(
      f"$label: score=${fold.score}%.6f; net=${fold.netProfit}%.5f; closed=${fold.closedTrades}; forced=${fold.forcedClosures}; " +
        f"costs=${fold.costs}%.5f; portfolio drawdown=${fold.maxDrawdownPercent}%.2f%%"
    ) :::
      (if (fold.violations.isEmpty) List("  No constraint breaches.") else fold.violations.map(v => s"  BREACH: $v"))

  private def provenanceLines(report: OptimisationReport): List[String] =
    List("First seen = first successful search evaluation; generation 0 includes initial oversampling.") :::
      report.finalists.toList.zipWithIndex.map { case ((candidate, _, _), index) =>
        val generation = report.diagnostics.firstSeen.get(candidate).fold("not observed")(_.toString)
        val baselines  = report.baselines.filter(_.effective.contains(candidate)).map(baselineLabel)
        s"#${index + 1}: first seen=$generation; baselines=${if (baselines.isEmpty) "none" else baselines.mkString(", ")}; " +
          catalogueText(report.catalogueMatches.getOrElse(candidate, Nil))
      }

  private def baselineLabel(baseline: BaselineReport): String =
    if (baseline.fixedInputsRestored) s"${baseline.name} (fixed inputs restored)" else baseline.name

  private def matchesText(matches: List[CatalogueMatch]): String =
    if (matches.isEmpty) "none"
    else
      matches
        .map { entry =>
          val kind = entry.kind match
            case DuplicateKind.Exact               => "exact"
            case DuplicateKind.FixedInputsRestored => "after restoring fixed inputs"
          s"${entry.name} ($kind)"
        }
        .mkString(", ")

  private def catalogueText(matches: List[CatalogueMatch]): String =
    s"catalogue strategies=${matchesText(matches.filter(_.scope == CatalogueScope.FullCandidate))}; " +
      s"parameter-only catalogue matches (different rules)=${matchesText(matches.filter(_.scope == CatalogueScope.IndicatorsOnly))}; " +
      s"parameter-only catalogue matches (different indicators)=${matchesText(matches.filter(_.scope == CatalogueScope.RulesOnly))}"

  private def workloadLines(report: OptimisationReport): List[String] =
    val snapshot = report.diagnostics
    val best     = snapshot.bestSeen.fold(List("No successful search evaluations.")) { found =>
      List(
        f"Best all-fold fitness ever searched: ${found.fitness}%.6f; first seen generation ${found.generation}.",
        s"Present in final shortlist: ${report.finalists.exists(_._1 == found.candidate)}",
        s"Candidate: ${CandidateDisplay.forTarget(report.target)(found.candidate)}"
      )
    }
    best ::: List(
      s"Search evaluation requests: ${snapshot.searchRequests}; successful: ${snapshot.successfulSearchRequests}; " +
        s"distinct successfully searched candidates: ${snapshot.distinctSearchCandidates}.",
      s"Final rescore requests: ${snapshot.rescoreRequests} (includes missing baseline evaluations).",
      s"Search cache computations: ${snapshot.computationAttempts} attempted, ${snapshot.completedComputations} completed; " +
        s"unique computed candidates: ${snapshot.uniqueComputedCandidates}.",
      s"Search cache reuses: ${snapshot.cacheReuses} (includes waiting on an in-flight computation)."
    ) ::: List(
      workload("Search + rescore", snapshot.workloads.getOrElse(RunDiagnostics.Stage.Search, RunDiagnostics.Workload())),
      workload("Validation", snapshot.workloads.getOrElse(RunDiagnostics.Stage.Validation, RunDiagnostics.Workload())),
      workload("Reporting replay", report.reportingWorkload),
      s"Optimisation duration: ${report.optimisationDuration.toMillis} ms; reporting duration: ${report.reportingDuration.toMillis} ms.",
      "Pair simulations are counted at execution, including attempts that fail; cached requests do not imply another simulation."
    )

  private def workload(label: String, work: RunDiagnostics.Workload): String =
    s"$label: candidate requests=${work.candidateRequests}; folds=${work.foldCompleted}/${work.foldAttempts} completed/attempted; " +
      s"pair simulations=${work.pairCompleted}/${work.pairAttempts} completed/attempted."
