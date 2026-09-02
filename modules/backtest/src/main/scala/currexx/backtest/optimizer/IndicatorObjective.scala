package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.{Evaluator, Validator}
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.services.TestServicesPool
import currexx.backtest.{MarketDataProvider, OrderStats, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.signal.Indicator
import fs2.Stream

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
  }

  final case class Operators[F[_]](
      evaluator: Evaluator[F, Indicator],
      validator: Validator[F, Indicator],
      backtest: Indicator => F[List[List[OrderStats]]],
      validate: Indicator => F[List[OrderStats]]
  )

  def make[F[_]: {Async, Parallel}](
      corpus: Corpus,
      strategy: TradeStrategy,
      poolSize: Int,
      shortlistSize: Int,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.Robust()
  ): F[Operators[F]] =
    for
      folds      <- corpus.searchFolds.traverse(_.parTraverse(MarketDataProvider.read[F](_).compile.toList))
      validation <- corpus.validationFold.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      initialSettings = TestSettings.make(folds.head.head.head.currencyPair, strategy, otherIndicators)
      pool <- TestServicesPool.make[F](initialSettings, poolSize)
      // Sequential across folds and parallel within one, so that widening the corpus by a fold cannot widen how many
      // backtests contend for the pool: a fold is the same six-pair run the pool was already sized for.
      perFold  = folds.map(fold => backtestOver[F](pool, fold, strategy, otherIndicators, signalDetector))
      backtest = (indicator: Indicator) => perFold.traverse(_(indicator))
      validate = backtestOver[F](pool, validation, strategy, otherIndicators, signalDetector)
      evaluator <- Evaluator.cached[F, Indicator] { indicator =>
        backtest(indicator).map(stats => indicator -> Fitness(FoldAggregation.combine(stats.map(scoringFunction.score))))
      }
      validator <- Validator.shortlisted[F, Indicator](shortlistSize)(ind => validate(ind).map(res => Fitness(scoringFunction.score(res))))
    yield Operators(evaluator, validator, backtest, validate)

  private def backtestOver[F[_]: {Async, Parallel}](
      pool: TestServicesPool[F],
      dataSets: List[List[MarketTimeSeriesData]],
      strategy: TradeStrategy,
      otherIndicators: List[Indicator],
      signalDetector: SignalDetector
  ): Indicator => F[List[OrderStats]] =
    indicator =>
      dataSets.parTraverse { testData =>
        pool.use(TestSettings.make(testData.head.currencyPair, strategy, indicator :: otherIndicators)) { services =>
          for
            _ <- Stream
              .emits(testData)
              .through(services.processMarketData(signalDetector))
              .compile
              .drain
            orderStats <- services.getOrderStats()
          yield orderStats
        }
      }
}
