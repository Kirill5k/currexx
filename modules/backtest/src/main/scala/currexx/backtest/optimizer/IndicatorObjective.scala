package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Fitness
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.reporting.{CandidateDiagnostics, FoldDiagnostics, RunDiagnostics}
import currexx.backtest.OrderStats
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator

object IndicatorObjective {

  /** How a candidate's per-fold scores become the single number selection sorts by.
    *
    * The geometric mean, because a fold aggregate has to answer whether a candidate held up in every regime, not whether it held up on
    * balance: an arithmetic mean lets one fold at four times the target pay outright for a fold worth nothing, which is the
    * single-fitted-regime shape this arrangement exists to refuse.
    *
    * Taken over every fold with a small constant added to each, rather than over all of them raw. Until 2026-09-02 this returned exactly
    * 0.0 the moment any single fold scored zero, which scored a candidate that lost money in one fold of six identically to one that lost
    * in all six - and that is not a rare corner. `TestStrategy` records that every JMA-crossover val loses money in 2023-07..2024-06, which
    * is search folds 1 to 3, so the whole s1_v2 and s2 lineage was guaranteed a training fitness of exactly zero by construction. It
    * showed: across the eleven rounds of 2026-09-01/02, the s2, s1_v2 and s4 rounds each finished with 15 to 23 of their 25 finalists tied
    * at exactly 0.000000 on training, out of a population that was almost entirely there - a hundred generations of selection climbing a
    * constant function. The round that produced the batch's only keeper, s2_optimized_v5, scored it 0.000000 and it survived on the
    * validation step alone.
    *
    * So the cliff is now a ramp. A fold that earned nothing is scored as `deadFoldFloor` rather than as an annihilating zero, which costs a
    * candidate most of its score without costing it everything, and a candidate that earns in no fold at all still scores exactly 0.0
    * because the shift cancels. Shifting rather than filtering is what keeps the result monotonic in every fold - see `deadFoldFloor`,
    * where the filtered version's two failures are recorded. The geometric mean over all of them keeps the property the product was chosen
    * for: one huge fold still cannot pay for a weak one.
    *
    * Not always over every fold. `FoldRotatingEvaluator` withholds one per generation so that selection cannot climb a single regime, and
    * `combineExcluding` is where that happens - in the aggregation rather than in the backtest, so a rotation is free and the run's closing
    * figure can still be taken over all of them.
    */
  object FoldAggregation {

    /** What a fold worth nothing is treated as being worth, so that the mean has something to multiply by.
      *
      * The shift is what keeps the aggregate monotonic. Taking the geometric mean over only the folds that earned something, and
      * discounting it by how many did not, looked like the same idea and was not: it made `[1.8, 0.0]` score 0.225 and `[1.8, 0.001]` score
      * 0.042, so a candidate was punished five-fold for turning a dead fold into a barely-alive one. It also failed the property it was
      * supposed to keep - `[0, 2, 2, 2, 2, 2]` scored 1.157 against a clean sweep of `[1, 1, 1, 1, 1, 1]` at 1.000.
      *
      * At 0.01 a dead fold costs a candidate more than half its score at six folds (a clean sweep scores 1.000 where one dead fold scores
      * 0.458, two 0.207 and five 0.012), and a clean sweep still outranks a candidate scoring twice as well in five folds and nothing in
      * the sixth. Lower makes a dead fold closer to fatal, higher makes it cheap: at 0.05 that same five-fold candidate wins.
      *
      * Empirical, and deliberately not swept against the holdout corpus. The holdout is worth something only for as long as nothing has
      * been chosen against it, and fitting a selection rule to it spends exactly that. Pinning this number properly needs a period held
      * back from the holdout too, or a nested split inside the search folds.
      */
    private val deadFoldFloor = 0.01

    def combine(foldScores: List[Double]): Double =
      // Earning nothing anywhere is answered directly rather than left to the arithmetic, which lands on 3e-18 instead of zero once the
      // shift is taken back off - and both the zero-count in the report and the run's own `NOTHING SELECTED` test read `<= 0.0`. No cliff
      // is reintroduced by this: the shifted mean tends to zero as the scores do, so the exact answer is also the limit of the smooth one.
      if (foldScores.isEmpty || foldScores.forall(_ <= 0.0)) 0.0
      else
        val shifted = foldScores.map(score => math.max(0.0, score) + deadFoldFloor)
        math.pow(shifted.product, 1.0 / shifted.size) - deadFoldFloor

    /** Geometric mean over every fold except the one at `excludedIndex`, if any. `None` is the full fold set. */
    def combineExcluding(foldScores: List[Double], excludedIndex: Option[Int]): Double =
      excludedIndex match
        case None           => combine(foldScores)
        case Some(excluded) => combine(foldScores.zipWithIndex.filterNot(_._2 == excluded).map(_._1))
  }

  final case class Operators[F[_]](
      evaluator: FoldRotatingEvaluator[F],
      validationObjective: Indicator => F[Fitness],
      backtest: Indicator => F[List[List[OrderStats]]],
      validate: Indicator => F[List[OrderStats]],
      inspect: Indicator => F[CandidateDiagnostics]
  )

  def make[F[_]: {Async, Parallel}](
      corpus: Corpus,
      strategy: TradeStrategy,
      poolSize: Int,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.Robust(),
      searchSpace: Option[IndicatorSearchSpace] = None,
      diagnostics: Option[RunDiagnostics[F]] = None
  ): F[Operators[F]] =
    for
      backtests <- IndicatorBacktest.make(corpus, strategy, poolSize, otherIndicators, signalDetector, diagnostics)
      objective = new IndicatorObjective(backtests, scoringFunction, searchSpace, diagnostics)
      evaluator <- FoldRotatingEvaluator.cached[F](
        backtests.searchFolds(RunDiagnostics.Stage.Search),
        scoringFunction,
        objective.canonicalise,
        diagnostics.map(_.evaluatorObserver)
      )
    yield Operators(evaluator, objective.validationFitness, objective.backtest, objective.validate, objective.inspect)
}

/** Candidate-level operations share canonicalisation, scoring and explicit workload ownership. */
final class IndicatorObjective[F[_]: Async] private (
    backtests: IndicatorBacktest[F],
    scoringFunction: ScoringFunction,
    searchSpace: Option[IndicatorSearchSpace],
    diagnostics: Option[RunDiagnostics[F]]
) {
  import RunDiagnostics.Stage

  private def canonicalise(indicator: Indicator): Either[Throwable, Indicator] =
    searchSpace.fold[Either[Throwable, Indicator]](Right(indicator))(_.canonicalise(indicator))

  private def withCandidate[A](indicator: Indicator, stage: Stage)(run: Indicator => F[A]): F[A] =
    Async[F].fromEither(canonicalise(indicator)).flatMap { candidate =>
      diagnostics.traverse_(_.candidateRequested(stage)).flatMap(_ => run(candidate))
    }

  def backtest(indicator: Indicator): F[List[List[OrderStats]]] =
    withCandidate(indicator, Stage.Backtest)(searchResults(_, Stage.Backtest)(identity))

  def validate(indicator: Indicator): F[List[OrderStats]] =
    withCandidate(indicator, Stage.Validation)(validationResults(_, Stage.Validation))

  def validationFitness(indicator: Indicator): F[Fitness] =
    validate(indicator).map(stats => Fitness(scoringFunction.score(stats)))

  def inspect(indicator: Indicator): F[CandidateDiagnostics] =
    withCandidate(indicator, Stage.Reporting) { candidate =>
      for
        search     <- searchResults(candidate, Stage.Reporting)(summarise)
        validation <-
          if (backtests.hasValidation) validationResults(candidate, Stage.Reporting).map(stats => Some(summarise(stats)))
          else Async[F].pure(Option.empty[FoldDiagnostics])
      yield CandidateDiagnostics(candidate, search, validation)
    }

  // Reduce each fold before starting the next one. Inspection retains summaries, not every fold's trade and equity histories.
  private def searchResults[A](indicator: Indicator, stage: Stage)(reduce: List[OrderStats] => A): F[List[A]] =
    backtests.searchFolds(stage).traverse(run => observedFold(stage)(run(indicator)).map(reduce))

  private def validationResults(indicator: Indicator, stage: Stage): F[List[OrderStats]] =
    if (backtests.hasValidation) observedFold(stage)(backtests.validation(indicator, stage))
    else Async[F].pure(Nil)

  private def observedFold[A](stage: Stage)(run: F[A]): F[A] =
    diagnostics.traverse_(_.foldStarted(stage)).flatMap(_ => run.flatTap(_ => diagnostics.traverse_(_.foldCompleted(stage))))

  private def summarise(stats: List[OrderStats]): FoldDiagnostics = {
    val portfolio = OrderStats.combine(stats)
    FoldDiagnostics(
      score = scoringFunction.score(stats),
      netProfit = portfolio.totalProfit,
      closedTrades = portfolio.total,
      forcedClosures = portfolio.forcedClosureCount,
      costs = portfolio.totalCosts,
      maxDrawdownPercent = portfolio.maxDrawdownPercent,
      violations = scoringFunction.violations(stats)
    )
  }
}
