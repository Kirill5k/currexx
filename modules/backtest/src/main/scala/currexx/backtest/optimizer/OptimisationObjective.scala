package currexx.backtest.optimizer

import currexx.algorithms.{EvaluationPhase, Fitness, memoize}
import currexx.backtest.{OrderStats, TestStrategy}
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.reporting.{CandidateDiagnostics, FoldDiagnostics, RunDiagnostics}
import currexx.core.signal.SignalDetector
import currexx.domain.market.CurrencyPair
import currexx.domain.signal.Indicator
import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

object OptimisationObjective {

  final case class Operators[F[_]](
      evaluator: FoldRotatingEvaluator[F],
      validationObjective: TestStrategy => F[Fitness],
      backtest: TestStrategy => F[List[List[OrderStats]]],
      validate: TestStrategy => F[List[OrderStats]],
      inspect: TestStrategy => F[CandidateDiagnostics],
      selectionEvidence: TestStrategy => F[SelectionEvidence]
  )

  def make[F[_]: {Async, Parallel}](
      corpus: Corpus,
      strategy: TestStrategy,
      poolSize: Int,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.Robust(),
      searchSpace: Option[OptimisationSearchSpace] = None,
      diagnostics: Option[RunDiagnostics[F]] = None,
      searchObjective: SearchObjectiveConfig = SearchObjectiveConfig.Current,
      upgradePolicy: UpgradePolicyConfig = UpgradePolicyConfig()
  ): F[Operators[F]] =
    for
      backtests <- OptimisationBacktest.make(corpus, strategy, poolSize, otherIndicators, signalDetector, diagnostics)
      canonicalise = (candidate: TestStrategy) =>
        searchSpace.fold[Either[Throwable, TestStrategy]](Right(candidate))(_.canonicalise(candidate))
      baseline  <- prepareBaseline(backtests, scoringFunction, searchObjective, strategy, canonicalise, diagnostics)
      selection <- memoize[F, TestStrategy, SelectionEvidence] { candidate =>
        for
          _        <- diagnostics.traverse_(_.foldStarted(RunDiagnostics.Stage.Validation))
          stats    <- backtests.validationWithPairs(candidate, RunDiagnostics.Stage.Validation)
          evidence <- Async[F].fromEither(SelectionEvidence.fromStats(candidate, stats, scoringFunction))
          _        <- diagnostics.traverse_(_.foldCompleted(RunDiagnostics.Stage.Validation))
        yield evidence
      }
      objective = new OptimisationObjective(
        backtests,
        scoringFunction,
        searchSpace,
        diagnostics,
        selection,
        searchObjective,
        upgradePolicy,
        baseline
      )
      initialScores = baseline.headOption.fold(Map.empty[TestStrategy, List[Double]]) { case (_, evidence) =>
        Map(evidence.candidate -> baseline.toList.sortBy(_._1).map(_._2.score.value))
      }
      evaluator <- FoldRotatingEvaluator.cachedScores[F](
        backtests.searchFoldsWithPairs(RunDiagnostics.Stage.Search).zipWithIndex.map { case (run, fold) =>
          candidate => run(candidate).flatMap(stats => Async[F].fromEither(objective.searchScore(candidate, stats, fold)))
        },
        objective.canonicalise,
        diagnostics.map(_.evaluatorObserver),
        initialScores
      )
    yield Operators(
      evaluator,
      objective.validationFitness,
      objective.backtest,
      objective.validate,
      objective.inspect,
      objective.selectionEvidence
    )

  private def prepareBaseline[F[_]: Async](
      backtests: OptimisationBacktest[F],
      scoring: ScoringFunction,
      objective: SearchObjectiveConfig,
      baseCandidate: TestStrategy,
      canonicalise: TestStrategy => Either[Throwable, TestStrategy],
      diagnostics: Option[RunDiagnostics[F]]
  ): F[Map[Int, SelectionEvidence]] = objective match
    case SearchObjectiveConfig.Current             => Async[F].pure(Map.empty)
    case SearchObjectiveConfig.BaselineRelative(_) =>
      val observer = diagnostics.map(_.evaluatorObserver)
      for
        canonical <- Async[F].fromEither(canonicalise(baseCandidate))
        _         <- observer.traverse_(_.requested(EvaluationPhase.Rescore))
        _         <- observer.traverse_(_.computationStarted(canonical))
        evidence  <- backtests.searchFoldsWithPairs(RunDiagnostics.Stage.Search).zipWithIndex.traverse { case (run, fold) =>
          for
            _        <- observer.traverse_(_.foldStarted)
            stats    <- run(canonical)
            measured <- Async[F].fromEither(SelectionEvidence.fromStats(canonical, stats, scoring))
            _        <- observer.traverse_(_.foldCompleted)
          yield fold -> measured
        }
        _ <- observer.traverse_(_.computationCompleted)
      yield evidence.toMap
}

/** Candidate-level operations share canonicalisation, scoring and explicit workload ownership. */
final class OptimisationObjective[F[_]: Async] private (
    backtests: OptimisationBacktest[F],
    scoringFunction: ScoringFunction,
    searchSpace: Option[OptimisationSearchSpace],
    diagnostics: Option[RunDiagnostics[F]],
    cachedSelection: TestStrategy => F[SelectionEvidence],
    searchObjective: SearchObjectiveConfig,
    upgradePolicy: UpgradePolicyConfig,
    baselines: Map[Int, SelectionEvidence]
) {
  import RunDiagnostics.Stage

  private def canonicalise(candidate: TestStrategy): Either[Throwable, TestStrategy] =
    searchSpace.fold[Either[Throwable, TestStrategy]](Right(candidate))(_.canonicalise(candidate))

  private def withCandidate[A](candidate: TestStrategy, stage: Stage)(run: TestStrategy => F[A]): F[A] =
    Async[F].fromEither(canonicalise(candidate)).flatMap { canonical =>
      diagnostics.traverse_(_.candidateRequested(stage)).flatMap(_ => run(canonical))
    }

  def backtest(candidate: TestStrategy): F[List[List[OrderStats]]] =
    withCandidate(candidate, Stage.Backtest)(searchResults(_, Stage.Backtest)(identity))

  def validate(candidate: TestStrategy): F[List[OrderStats]] =
    withCandidate(candidate, Stage.Validation)(validationResults(_, Stage.Validation))

  def validationFitness(candidate: TestStrategy): F[Fitness] =
    if (backtests.hasValidation) selectionEvidence(candidate).map(evidence => Fitness(evidence.score.value))
    else validate(candidate).map(stats => Fitness(scoringFunction.score(stats)))

  def selectionEvidence(candidate: TestStrategy): F[SelectionEvidence] =
    withCandidate(candidate, Stage.Validation)(cachedSelection)

  def inspect(candidate: TestStrategy): F[CandidateDiagnostics] =
    withCandidate(candidate, Stage.Reporting) { canonical =>
      for
        search <- backtests.searchFoldsWithPairs(Stage.Reporting).zipWithIndex.traverse { case (run, fold) =>
          observedFold(Stage.Reporting)(run(canonical)).flatMap { stats =>
            Async[F].fromEither(searchScore(canonical, stats, fold)).map(score => summarise(stats.map(_._2), score))
          }
        }
        validation <-
          if (backtests.hasValidation) cachedSelection(canonical).map(evidence => Some(summarise(evidence)))
          else Async[F].pure(Option.empty[FoldDiagnostics])
      yield CandidateDiagnostics(canonical, search, validation)
    }

  // Reduce each fold before starting the next one. Inspection retains summaries, not every fold's trade and equity histories.
  private def searchResults[A](candidate: TestStrategy, stage: Stage)(reduce: List[OrderStats] => A): F[List[A]] =
    backtests.searchFolds(stage).traverse(run => observedFold(stage)(run(candidate)).map(reduce))

  private def validationResults(candidate: TestStrategy, stage: Stage): F[List[OrderStats]] =
    if (backtests.hasValidation) observedFold(stage)(backtests.validation(candidate, stage))
    else Async[F].pure(Nil)

  private def observedFold[A](stage: Stage)(run: F[A]): F[A] =
    diagnostics.traverse_(_.foldStarted(stage)).flatMap(_ => run.flatTap(_ => diagnostics.traverse_(_.foldCompleted(stage))))

  private def searchScore(candidate: TestStrategy, stats: List[(CurrencyPair, OrderStats)], fold: Int): Either[Throwable, Double] =
    searchObjective match
      case SearchObjectiveConfig.Current             => Right(scoringFunction.score(stats.map(_._2)))
      case SearchObjectiveConfig.BaselineRelative(_) =>
        for
          base     <- baselines.get(fold).toRight(new IllegalArgumentException(s"Missing baseline evidence for search fold $fold"))
          evidence <- SelectionEvidence.fromStats(candidate, stats, scoringFunction)
          _        <- Either.cond(
            evidence.coverage == base.coverage,
            (),
            new IllegalArgumentException(s"Mismatched search coverage for fold $fold")
          )
        yield searchObjective.adjust(evidence.score.value, evidence.netProfit, base, upgradePolicy)

  private def summarise(evidence: SelectionEvidence): FoldDiagnostics =
    FoldDiagnostics(
      score = evidence.score.value,
      netProfit = evidence.netProfit,
      closedTrades = evidence.closedTrades.value,
      forcedClosures = evidence.forcedClosures.value,
      costs = evidence.costs.value,
      maxDrawdownPercent = evidence.maxDrawdownPercent.value,
      violations = evidence.violations
    )

  private def summarise(stats: List[OrderStats], score: Double): FoldDiagnostics = {
    val portfolio = OrderStats.combine(stats)
    FoldDiagnostics(
      score = score,
      netProfit = portfolio.totalProfit,
      closedTrades = portfolio.total,
      forcedClosures = portfolio.forcedClosureCount,
      costs = portfolio.totalCosts,
      maxDrawdownPercent = portfolio.maxDrawdownPercent,
      violations = scoringFunction.violations(stats)
    )
  }
}
