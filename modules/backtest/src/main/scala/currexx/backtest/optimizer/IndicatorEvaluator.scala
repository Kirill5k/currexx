package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.Evaluator
import currexx.backtest.services.TestServicesPool
import currexx.backtest.types.{GreaterThanOne, PositiveUnitInterval}
import currexx.backtest.types.given
import currexx.backtest.{MarketDataProvider, OrderStats, RiskRatio, TestSettings}
import currexx.core.signal.SignalDetector
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator
import eu.timepit.refined.types.numeric.{PosDouble, PosInt}
import fs2.Stream

object IndicatorEvaluator {

  /** How a run ranks candidates, together with the acceptance test that shares its thresholds.
    *
    * `score` is what selection sorts by and `violations` is what decides whether to trust the result; the two answer different questions of
    * the same thresholds. Keeping them on one object is what stops a champion from being re-checked against numbers it was never scored
    * against — the search holds only this, so there is nothing else a caller could reach for and no configuration to pass twice.
    */
  trait ScoringFunction:
    def score(stats: List[OrderStats]): Double
    def violations(stats: List[OrderStats]): List[ScoringFunction.Violation]

  object ScoringFunction {
    final case class RobustConfig(
        minClosedTrades: PosInt = 150,
        minProfitableDatasetRatio: PositiveUnitInterval = 2.0 / 3.0,
        minProfitFactor: GreaterThanOne = 1.2,
        maxDrawdownPercent: PosDouble = 15.0,
        maxCostToPreCostProfitRatio: PosDouble = 0.4,
        // Net return of the pooled portfolio, whose balance is the sum of the per-dataset balances. That makes this
        // target an average per-dataset return, so it keeps its meaning however many datasets a round is run against.
        targetNetReturn: PosDouble = 0.1,
        targetRecoveryFactor: PosDouble = 3.0,
        targetSortinoRatio: PosDouble = 2.0,
        targetExpectancyToLossRatio: PosDouble = 0.2
    )

    /** Scores candidates on cost-adjusted portfolio performance, discounted by how far each falls short of being trustworthy, and reports
      * the shortfalls of any one of them against the same thresholds.
      *
      * A quality score rates performance on four axes, each measured against a target that scales it into comparable units:
      *   - 35% net return
      *   - 30% recovery factor
      *   - 17.5% Sortino ratio
      *   - 17.5% expectancy relative to average loss
      *
      * That score is then multiplied by a discount in [0, 1]: the product of one satisfaction factor per constraint. Each factor is 1.0
      * once its threshold is met and falls off linearly below it, rather than cutting the score to zero at the threshold. Thresholds
      * implemented as cliffs left most of the population sharing an identical score of 0.0, which gives selection nothing to rank and turns
      * early generations into a random walk; a ramp preserves the gradient that lets a genetic algorithm climb towards the threshold.
      *
      * Genuinely disqualifying results — no trades, no profit, non-positive expectancy, or invalid orders (which mean the simulated order
      * book desynchronised, so none of the other numbers can be trusted) — are constraints too, carrying a factor of exactly 0.0 that
      * annihilates the product.
      */
    def robust(config: RobustConfig = RobustConfig()): ScoringFunction =
      new ScoringFunction:
        override def score(stats: List[OrderStats]): Double =
          if (stats.isEmpty) 0.0
          else {
            val portfolio = OrderStats.combine(stats)
            val discount  = constraints(portfolio, profitableRatio(stats), config).map(_.satisfaction).product
            if (discount == 0.0) 0.0 else quality(portfolio, config) * discount
          }

        /** Re-checks a result against the thresholds it was scored against, as pass or fail.
          *
          * Scoring ramps rather than gates on purpose, because gating flattens the fitness landscape and leaves selection nothing to rank.
          * That makes it a good search signal and a poor acceptance test: the winner of a run is only the best of whatever happened to be
          * tried, and a candidate breaching every threshold still scores above zero and still wins if nothing better turned up. Deciding
          * whether to trust the winner is a separate question from ranking candidates during the search, and needs asking separately — of
          * the same constraints, so that the answer cannot contradict the score.
          */
        override def violations(stats: List[OrderStats]): List[Violation] =
          if (stats.isEmpty) List(Violation("dataset count", "0", "at least 1"))
          else constraints(OrderStats.combine(stats), profitableRatio(stats), config).flatMap(_.violation)

    private def quality(portfolio: OrderStats, config: RobustConfig): Double = {
      // A metric whose denominator vanished is undefined rather than bad — no drawdown to recover from, no losing
      // month, no losing trade — so it is credited with its target instead of being scored as zero. The credit is
      // worth only as much as the sample behind it: an absence of drawdown across five trades is evidence of nothing,
      // and all three of these can vanish at once, which would otherwise hand a handful of tiny winning trades 0.65
      // of a full score for free. A thin sample is therefore discounted twice, once here on the unearned portion and
      // again by the sample-size constraint discounting the score as a whole, which is the intent.
      val confidence                       = sampleConfidence(portfolio, config)
      def credited(target: Double): Double = target * confidence

      val netReturn      = (portfolio.totalProfit / portfolio.initialBalance).toDouble
      val recoveryFactor = portfolio.recoveryFactor.fold(credited(config.targetRecoveryFactor.value))(_.toDouble)
      // Only a ratio that was measured and found to have no downside earns the credit. One that could not be measured
      // at all — every trade closed inside a single calendar month — has demonstrated nothing and earns nothing. That
      // is not reachable by trading rarely, which the sample-size constraint already covers, but by trading in a burst.
      val sortinoRatio = portfolio.sortinoRatio match
        case RiskRatio.Defined(value)   => value
        case RiskRatio.ZeroDeviation    => credited(config.targetSortinoRatio.value)
        case RiskRatio.InsufficientData => 0.0
      val expectancyToLoss =
        if (portfolio.averageLoss == 0) credited(config.targetExpectancyToLossRatio.value)
        else (portfolio.expectancy / portfolio.averageLoss).toDouble

      (0.350 * scaled(netReturn, config.targetNetReturn.value)) +
        (0.300 * scaled(recoveryFactor, config.targetRecoveryFactor.value)) +
        (0.175 * scaled(sortinoRatio, config.targetSortinoRatio.value)) +
        (0.175 * scaled(expectancyToLoss, config.targetExpectancyToLossRatio.value))
    }

    private def sampleConfidence(portfolio: OrderStats, config: RobustConfig): Double =
      rampUp(portfolio.total.toDouble, 0.0, config.minClosedTrades.value.toDouble)

    private def profitableRatio(stats: List[OrderStats]): Double =
      stats.count(_.totalProfit > 0).toDouble / stats.size

    /** A constraint that `robust` only discounts for, restated as something a result either satisfies or does not. */
    final case class Violation(constraint: String, actual: String, required: String):
      override def toString: String = s"$constraint is $actual, required $required"

    /** A threshold `robust` scores against, evaluated once and read two ways.
      *
      * `satisfaction` is the single encoding: 1.0 once the threshold is met, falling towards 0.0 below it. Scoring multiplies these
      * together; the champion check reports every one that did not reach 1.0. That equivalence is exact, because a ramp reaches 1.0 on
      * precisely the condition a pass/fail predicate would test. Writing the predicate separately would let it drift out of step with the
      * ramp, which would mean accepting a winner the search had penalised, or rejecting one it had not.
      *
      * Only the description is by-name, because only scoring is on the search's path: it reads `satisfaction` from all of these and the
      * wording of none of them. Formatting eagerly meant every evaluation rendered eight BigDecimals it then discarded, once per candidate
      * for the whole run, to describe a breach that is described at most once.
      */
    final private class Constraint(name: String, actual: => String, required: => String, val satisfaction: Double):
      def violation: Option[Violation] = Option.when(satisfaction < 1.0)(Violation(name, actual, required))

    private def constraints(portfolio: OrderStats, profitableRatio: Double, config: RobustConfig): List[Constraint] = {
      // With nothing earned before costs there is no share for them to be a fraction of, so the ratio is pinned at the
      // hard limit, which is where both readings of the constraint want it: a factor of 0.0 and a reported breach.
      val costRatio = if (portfolio.preCostProfit <= 0) BigDecimal(1) else portfolio.totalCosts / portfolio.preCostProfit
      List(
        Constraint(
          "invalid order count",
          portfolio.invalidOrderCount.toString,
          "0",
          if (portfolio.invalidOrderCount > 0) 0.0 else 1.0
        ),
        Constraint(
          "closed trades",
          portfolio.total.toString,
          s">= ${config.minClosedTrades.value}",
          sampleConfidence(portfolio, config)
        ),
        Constraint(
          "net profit",
          portfolio.totalProfit.toString,
          "> 0",
          if (portfolio.totalProfit <= 0) 0.0 else 1.0
        ),
        Constraint(
          "expectancy",
          portfolio.expectancy.toString,
          "> 0",
          if (portfolio.expectancy <= 0) 0.0 else 1.0
        ),
        Constraint(
          "profit factor",
          portfolio.profitFactor.fold("N/A")(_.toString),
          s">= ${config.minProfitFactor.value}",
          portfolio.profitFactor.fold(1.0)(pf => rampUp(pf.toDouble, 1.0, config.minProfitFactor.value))
        ),
        Constraint(
          "max drawdown",
          f"${portfolio.maxDrawdownPercent}%.2f%%",
          f"<= ${config.maxDrawdownPercent.value}%.2f%%",
          rampDown(portfolio.maxDrawdownPercent.toDouble, config.maxDrawdownPercent.value, config.maxDrawdownPercent.value * 2)
        ),
        Constraint(
          "costs as a share of gross profit",
          f"$costRatio%.3f",
          f"<= ${config.maxCostToPreCostProfitRatio.value}%.3f",
          rampDown(costRatio.toDouble, config.maxCostToPreCostProfitRatio.value, 1.0)
        ),
        Constraint(
          "profitable datasets",
          f"$profitableRatio%.3f",
          f">= ${config.minProfitableDatasetRatio.value}%.3f",
          rampUp(profitableRatio, 0.0, config.minProfitableDatasetRatio.value)
        )
      )
    }

    // Upper bound that any single scaled component approaches but never reaches. The logarithm alone is unbounded, and
    // two of the four components divide by a quantity nothing stops from approaching zero — recovery factor by max
    // drawdown, expectancy by average loss — so without a bound a candidate that got lucky on one axis outscores every
    // balanced candidate combined.
    private val maxComponentScore = 3.0

    /** Scales a metric against its target with diminishing returns past the target: proportional up to the target (target -> 1.0), then
      * saturating towards `maxComponentScore` above it, so that an outlier on a single axis cannot dominate the weighted sum.
      *
      * The saturation is asymptotic rather than a hard ceiling. A ceiling scores every candidate past it identically, which costs selection
      * its ability to rank exactly where the candidates are most worth telling apart, and leaves that axis with no gradient for the
      * optimiser to climb. `tanh` bends the same logarithmic curve — it shares its slope at the target — into one that is bounded yet still
      * strictly increasing everywhere, so a better candidate always outscores a worse one however far out it is.
      */
    private def scaled(value: Double, target: Double): Double =
      if (value <= 0.0) 0.0
      else {
        val ratio = value / target
        if (ratio <= 1.0) ratio
        else {
          val headroom = maxComponentScore - 1.0
          1.0 + headroom * math.tanh(math.log(ratio) / headroom)
        }
      }

    /** 0 at or below `floor`, 1 at or above `target`, linear in between. */
    private def rampUp(value: Double, floor: Double, target: Double): Double =
      if (value <= floor) 0.0
      else if (value >= target) 1.0
      else (value - floor) / (target - floor)

    /** 1 at or below `limit`, 0 at or above `hardLimit`, linear in between. */
    private def rampDown(value: Double, limit: Double, hardLimit: Double): Double =
      if (value <= limit) 1.0
      else if (value >= hardLimit) 0.0
      else 1.0 - ((value - limit) / (hardLimit - limit))
  }

  /** The evaluator a run searches with, together with the backtest underneath it.
    *
    * Fitness collapses a run to a single number and throws away the statistics it came from, so a finished search can say nothing about its
    * own champion beyond its score. Handing back the backtest lets the caller replay one indicator and examine the result properly.
    */
  final case class Evaluation[F[_]](
      evaluator: Evaluator[F, Indicator],
      backtest: Indicator => F[List[OrderStats]]
  )

  def make[F[_]: {Async, Parallel}](
      testFilePaths: List[String],
      strategy: TradeStrategy,
      poolSize: Int,
      otherIndicators: List[Indicator] = Nil,
      signalDetector: SignalDetector = SignalDetector.pure,
      scoringFunction: ScoringFunction = ScoringFunction.robust()
  ): F[Evaluation[F]] =
    for
      testDataSets <- testFilePaths.parTraverse(MarketDataProvider.read[F](_).compile.toList)
      initialSettings = TestSettings.make(testDataSets.head.head.currencyPair, strategy, otherIndicators)
      pool <- TestServicesPool.make[F](initialSettings, poolSize)
      backtest = (ind: Indicator) =>
        testDataSets.parTraverse { testData =>
          pool.use(TestSettings.make(testData.head.currencyPair, strategy, ind :: otherIndicators)) { services =>
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
      eval <- Evaluator.cached[F, Indicator](ind => backtest(ind).map(res => ind -> Fitness(scoringFunction.score(res))))
    yield Evaluation(eval, backtest)
}
