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

import scala.language.implicitConversions

object IndicatorEvaluator {

  type ScoringFunction = List[OrderStats] => Double

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

    /** Scores a candidate on cost-adjusted portfolio performance, discounted by how far it falls short of being trustworthy.
      *
      * A quality score rates performance on four axes, each measured against a target that scales it into comparable units:
      *   - 35% net return
      *   - 30% recovery factor
      *   - 17.5% Sortino ratio
      *   - 17.5% expectancy relative to average loss
      *
      * That score is then multiplied by a penalty in [0, 1] derived from sample size, profit factor, drawdown, cost share of gross profit,
      * and the fraction of datasets that were profitable. Each penalty is 1.0 once its threshold is met and falls off linearly below it,
      * rather than cutting the score to zero at the threshold. Thresholds implemented as cliffs left most of the population sharing an
      * identical score of 0.0, which gives selection nothing to rank and turns early generations into a random walk; a ramp preserves the
      * gradient that lets a genetic algorithm climb towards the threshold.
      *
      * Only genuinely disqualifying results score zero: no trades, no profit, non-positive expectancy, or invalid orders (which mean the
      * simulated order book desynchronised, so none of the other numbers can be trusted).
      */
    def robust(config: RobustConfig = RobustConfig()): ScoringFunction = stats =>
      if (stats.isEmpty) 0.0
      else {
        val portfolio = OrderStats.combine(stats)
        if (portfolio.invalidOrderCount > 0 || portfolio.total == 0 || portfolio.totalProfit <= 0 || portfolio.expectancy <= 0) 0.0
        else {
          val profitableRatio = stats.count(_.totalProfit > 0).toDouble / stats.size
          quality(portfolio, config) * penalty(portfolio, profitableRatio, config)
        }
      }

    private def quality(portfolio: OrderStats, config: RobustConfig): Double = {
      // A metric whose denominator vanished is undefined rather than bad — no drawdown to recover from, no losing
      // month, no losing trade — so it is credited with its target instead of being scored as zero. The credit is
      // worth only as much as the sample behind it: an absence of drawdown across five trades is evidence of nothing,
      // and all three of these can vanish at once, which would otherwise hand a handful of tiny winning trades 0.65
      // of a full score for free. A thin sample is therefore discounted twice, once here on the unearned portion and
      // again by the sample-size penalty on the score as a whole, which is the intent.
      val confidence                       = sampleConfidence(portfolio, config)
      def credited(target: Double): Double = target * confidence

      val netReturn      = (portfolio.totalProfit / portfolio.initialBalance).toDouble
      val recoveryFactor = portfolio.recoveryFactor.fold(credited(config.targetRecoveryFactor.value))(_.toDouble)
      // Only a ratio that was measured and found to have no downside earns the credit. One that could not be measured
      // at all — every trade closed inside a single calendar month — has demonstrated nothing and earns nothing. That
      // is not reachable by trading rarely, which the sample-size penalty already covers, but by trading in a burst.
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

    private def penalty(portfolio: OrderStats, profitableRatio: Double, config: RobustConfig): Double = {
      val sampleSize   = sampleConfidence(portfolio, config)
      val profitFactor = portfolio.profitFactor.fold(1.0)(pf => rampUp(pf.toDouble, 1.0, config.minProfitFactor.value))
      val drawdown = rampDown(portfolio.maxDrawdownPercent.toDouble, config.maxDrawdownPercent.value, config.maxDrawdownPercent.value * 2)
      val costs    =
        if (portfolio.preCostProfit <= 0) 0.0
        else rampDown((portfolio.totalCosts / portfolio.preCostProfit).toDouble, config.maxCostToPreCostProfitRatio.value, 1.0)
      val breadth = rampUp(profitableRatio, 0.0, config.minProfitableDatasetRatio.value)
      sampleSize * profitFactor * drawdown * costs * breadth
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

    /** A constraint that `robust` only discounts for, restated as something a result either satisfies or does not. */
    final case class Violation(constraint: String, actual: String, required: String):
      override def toString: String = s"$constraint is $actual, required $required"

    /** Re-checks a result against the thresholds it was scored against, as pass or fail.
      *
      * Scoring ramps rather than gates on purpose, because gating flattens the fitness landscape and leaves selection nothing to rank. That
      * makes it a good search signal and a poor acceptance test: the winner of a run is only the best of whatever happened to be tried, and
      * a candidate breaching every threshold still scores above zero and still wins if nothing better turned up. Deciding whether to trust
      * the winner is a separate question from ranking candidates during the search, and needs asking separately.
      */
    def violations(stats: List[OrderStats], config: RobustConfig = RobustConfig()): List[Violation] =
      if (stats.isEmpty) List(Violation("dataset count", "0", "at least 1"))
      else {
        val portfolio       = OrderStats.combine(stats)
        val profitableRatio = stats.count(_.totalProfit > 0).toDouble / stats.size
        val costRatio       = if (portfolio.preCostProfit <= 0) BigDecimal(1) else portfolio.totalCosts / portfolio.preCostProfit
        List(
          Option.when(portfolio.invalidOrderCount > 0)(
            Violation("invalid order count", portfolio.invalidOrderCount.toString, "0")
          ),
          Option.when(portfolio.total < config.minClosedTrades.value)(
            Violation("closed trades", portfolio.total.toString, s">= ${config.minClosedTrades.value}")
          ),
          Option.when(portfolio.totalProfit <= 0)(
            Violation("net profit", portfolio.totalProfit.toString, "> 0")
          ),
          Option.when(portfolio.expectancy <= 0)(
            Violation("expectancy", portfolio.expectancy.toString, "> 0")
          ),
          Option.when(portfolio.profitFactor.exists(_.toDouble < config.minProfitFactor.value))(
            Violation("profit factor", portfolio.profitFactor.fold("N/A")(_.toString), s">= ${config.minProfitFactor.value}")
          ),
          Option.when(portfolio.maxDrawdownPercent.toDouble > config.maxDrawdownPercent.value)(
            Violation("max drawdown", f"${portfolio.maxDrawdownPercent}%.2f%%", f"<= ${config.maxDrawdownPercent.value}%.2f%%")
          ),
          Option.when(costRatio.toDouble > config.maxCostToPreCostProfitRatio.value)(
            Violation("costs as a share of gross profit", f"${costRatio}%.3f", f"<= ${config.maxCostToPreCostProfitRatio.value}%.3f")
          ),
          Option.when(profitableRatio < config.minProfitableDatasetRatio.value)(
            Violation("profitable datasets", f"$profitableRatio%.3f", f">= ${config.minProfitableDatasetRatio.value}%.3f")
          )
        ).flatten
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
      eval <- Evaluator.cached[F, Indicator](ind => backtest(ind).map(res => ind -> Fitness(scoringFunction(res))))
    yield Evaluation(eval, backtest)
}
