package currexx.backtest.optimizer

import currexx.backtest.{OrderStats, RiskRatio}
import currexx.backtest.syntax.*
import currexx.backtest.types.{GreaterThanOne, PositiveUnitInterval}
import currexx.backtest.types.given
import eu.timepit.refined.types.numeric.{PosDouble, PosInt}

import java.time.{Instant, YearMonth, ZoneOffset}

/** How a run ranks candidates, together with the acceptance test that shares its thresholds.
  *
  * `score` is what selection sorts by; `violations` is what decides whether to trust the winner. Both read the same thresholds off the same
  * object, so a champion cannot be re-checked against numbers it was never scored against.
  */
trait ScoringFunction:
  def score(stats: List[OrderStats]): Double

  /** Re-checks a result against the thresholds it was scored against, as pass or fail.
    *
    * Scoring ramps rather than gates so selection has a gradient to climb, which makes it a poor acceptance test: a candidate breaching
    * every threshold still scores above zero and still wins a round that turned up nothing better.
    */
  def violations(stats: List[OrderStats]): List[ScoringFunction.Violation]

object ScoringFunction {
  final case class Violation(constraint: String, actual: String, required: String):
    override def toString: String = s"$constraint is $actual, required $required"

  /** A threshold evaluated once and read two ways: `satisfaction` is 1.0 once met and falls towards 0.0 below it, and anything short of 1.0
    * is reported as a violation. One encoding, so the ramp and the pass/fail check cannot drift apart.
    *
    * The descriptions are by-name because scoring reads only `satisfaction`, once per candidate for the whole run.
    */
  final private class Constraint(name: String, actual: => String, required: => String, val satisfaction: Double):
    def violation: Option[Violation] = Option.when(satisfaction < 1.0)(Violation(name, actual, required))

  object Robust {
    // Ceiling any single scaled component approaches but never reaches. Two of the four divide by a quantity nothing
    // stops from approaching zero — recovery factor by drawdown, expectancy by average loss — so unbounded, one lucky
    // axis outscores every balanced candidate.
    private val maxComponentScore = 3.0

    final case class Config(
        // Per pair per month, multiplied up by `tradeFloor` into the pooled total the run has to reach, so the threshold
        // keeps its meaning however long the window and however many datasets the corpus holds. Five, so a month holds
        // enough trades for the monthly return series to be something other than noise; far above it the only candidates
        // left are the ones trading often enough for costs to consume the edge, which is what
        // maxCostToPreCostProfitRatio pulls against.
        minTradesPerMonth: PosInt = 5,
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

    /** Scores candidates on cost-adjusted portfolio performance, discounted by how far each falls short of being trustworthy.
      *
      * Quality is a weighted sum against scaled targets — 35% net return, 30% recovery factor, 17.5% Sortino, 17.5% expectancy over average
      * loss — multiplied by the product of the constraint satisfactions. Each ramps rather than gates, because a cliff at the threshold
      * leaves most of the population sharing a score of 0.0 and selection nothing to rank. Only the genuinely disqualifying carry an exact
      * 0.0: no trades, no profit, non-positive expectancy, or invalid orders, which mean the simulated order book desynchronised and no
      * other number can be trusted.
      */
    def apply(config: Config = Config()): ScoringFunction =
      new ScoringFunction {
        override def score(stats: List[OrderStats]): Double =
          if (stats.isEmpty) 0.0
          else {
            val portfolio  = OrderStats.combine(stats)
            val dataMonths = monthsOfData(portfolio)
            val discount   = constraints(portfolio, dataMonths, stats.size, profitableRatio(stats), config).map(_.satisfaction).product
            if (discount == 0.0) 0.0 else quality(portfolio, dataMonths, stats.size, config) * discount
          }

        override def violations(stats: List[OrderStats]): List[Violation] =
          if (stats.isEmpty) List(Violation("dataset count", "0", "at least 1"))
          else {
            val portfolio = OrderStats.combine(stats)
            constraints(portfolio, monthsOfData(portfolio), stats.size, profitableRatio(stats), config).flatMap(_.violation)
          }
      }

    private def quality(portfolio: OrderStats, dataMonths: Int, datasetCount: Int, config: Config): Double = {
      // A vanished denominator means the metric is undefined rather than bad, so it is credited with its target — but
      // only in proportion to the sample behind it, since all three can vanish at once and would otherwise hand a
      // handful of tiny winning trades 0.65 of a full score for free.
      val confidence = sampleConfidence(portfolio, config.minTradesPerMonth, dataMonths, datasetCount)

      def credited(target: Double): Double = target * confidence

      val netReturn      = (portfolio.totalProfit / portfolio.initialBalance).toDouble
      val recoveryFactor = portfolio.recoveryFactor.fold(credited(config.targetRecoveryFactor.value))(_.toDouble)
      // Only a ratio that was measured and found to have no downside earns the credit. One that could not be measured at
      // all — every trade closed inside a single calendar month — has demonstrated nothing.
      val sortinoRatio = portfolio.sortinoRatio match
        case RiskRatio.Defined(value)   => value
        case RiskRatio.ZeroDeviation    => credited(config.targetSortinoRatio.value)
        case RiskRatio.InsufficientData => 0.0
      val expectancyToLoss =
        if (portfolio.averageLoss == 0) credited(config.targetExpectancyToLossRatio.value)
        else (portfolio.expectancy / portfolio.averageLoss).toDouble

      (0.350 * scaled(netReturn, config.targetNetReturn.value, maxScore = maxComponentScore)) +
        (0.300 * scaled(recoveryFactor, config.targetRecoveryFactor.value, maxScore = maxComponentScore)) +
        (0.175 * scaled(sortinoRatio, config.targetSortinoRatio.value, maxScore = maxComponentScore)) +
        (0.175 * scaled(expectancyToLoss, config.targetExpectancyToLossRatio.value, maxScore = maxComponentScore))
    }

    private def profitableRatio(stats: List[OrderStats]): Double =
      stats.count(_.totalProfit > 0).toDouble / stats.size

    private def constraints(
        portfolio: OrderStats,
        dataMonths: Int,
        datasetCount: Int,
        profitableRatio: Double,
        config: Config
    ): List[Constraint] = {
      // Nothing earned before costs means there is no share for them to be a fraction of, so the ratio is pinned at the
      // hard limit: a factor of 0.0 and a reported breach.
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
          tradeFloorDescription(config.minTradesPerMonth, dataMonths, datasetCount),
          sampleConfidence(portfolio, config.minTradesPerMonth, dataMonths, datasetCount)
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

  }

  object Consistent {
    private val maxComponentScore = 3.0

    final case class Config(
        // Per pair per month, multiplied up by `tradeFloor` into the pooled total the run has to reach, so the threshold
        // keeps its meaning however long the window and however many datasets the corpus holds. Five, because every
        // constraint below reads the record a period at a time and the sign of a pair-month holding two trades is close
        // to a coin flip; far above it the only candidates left are the ones trading often enough for costs to consume
        // the edge, which is what maxCostToPreCostProfitRatio pulls against.
        minTradesPerMonth: PosInt = 5,
        // Calendar months per period. One: swept over 1..6 against every trade floor, monthly wins everywhere and
        // three-month periods turn the rank correlation negative. It is the number of periods the counting statistics
        // are made of, so fewer, denser samples lose more than they gain.
        periodMonths: PosInt = 1,
        // Months of data the run has to cover before its consistency has been measured at all, counted in months rather
        // than periods so that it keeps its meaning when `periodMonths` changes.
        minMonthsCovered: PosInt = 3,
        minProfitableDatasetRatio: PositiveUnitInterval = 2.0 / 3.0,
        // More than half the pair-months have to make money: the constraint that counts losing months instead of netting
        // them off against the winners. Unvalidated — sweeping 0.3..0.65 moves the rank correlation by 0.02 — but it is
        // the direct encoding of the requirement.
        minProfitablePeriodRatio: PositiveUnitInterval = 0.55,
        // Ceiling on how much of one pair's winnings may come from its single best period, over a full year of them;
        // `singlePeriodProfitShareLimit` rescales it to the periods actually judged.
        //
        // Loose on purpose: tightening it costs predictive power (sweeping 0.25..1.0 raises the rank correlation from
        // +0.46 to +0.59), because a trend follower earning in bursts is the nature of the edge and not evidence against
        // it. Winners paying for losers is what the profitable-period ratio and the period profit factor measure; this is
        // left to catch the pathological end, where one period is most of everything a pair ever made.
        maxSinglePeriodProfitShare: PositiveUnitInterval = 0.7,
        minPeriodProfitFactor: GreaterThanOne = 1.3,
        targetPeriodProfitFactor: PosDouble = 2.0,
        minProfitFactor: GreaterThanOne = 1.2,
        maxDrawdownPercent: PosDouble = 15.0,
        maxCostToPreCostProfitRatio: PosDouble = 0.4,
        targetNetReturn: PosDouble = 0.1,
        targetRecoveryFactor: PosDouble = 3.0,
        targetSortinoRatio: PosDouble = 2.0,
        targetExpectancyToLossRatio: PosDouble = 0.2
    ) {

      /** Divides the annual return target into the per-period target, annualises the risk ratio, and anchors the concentration limit.
        * Derived rather than configured so that it cannot disagree with `periodMonths`.
        */
      val periodsPerYear: Double = 12.0 / periodMonths.value

      /** `maxSinglePeriodProfitShare` rescaled from the year it was calibrated on to the `periods` actually being judged.
        *
        * The best period's share of a pair's winnings cannot fall below an even split, so a limit fixed at the annual value is a
        * near-certain breach on a short window: over three periods the fairest possible split is already 0.333, and a pair with one winning
        * period scores exactly 1.0 with no way to do better. Interpolating between the even split and 1.0 keeps the configured value at a
        * full year of periods (0.700 at twelve monthly ones) and relaxes as periods run out: 0.754 at four, 0.782 at three, 1.000 at one,
        * where a single period is by definition all of the winnings and there is nothing left to measure.
        */
      def singlePeriodProfitShareLimit(periods: Int): Double =
        if (periods <= 1 || periodsPerYear <= 1.0) 1.0
        else {
          val evenSplit   = 1.0 / periods
          val annualSplit = 1.0 / periodsPerYear
          val slope       = (maxSinglePeriodProfitShare.value - annualSplit) / (1.0 - annualSplit)
          math.max(evenSplit, evenSplit + slope * (1.0 - evenSplit))
        }
    }

    /** One dataset's profit per period over every period it covered, including the ones no trade closed in.
      *
      * `profitByMonth` holds a key only for a month some trade closed in, so a candidate that traded in three months of a twelve-month run
      * and won all three would read as 100% profitable periods and perfectly unconcentrated. Filling from the data window is what makes it
      * 25%, and what stops a candidate flattering itself by sitting out either end.
      */
    final private case class Series(profits: List[BigDecimal]) {
      val count: Int         = profits.size
      private val gains      = profits.filter(_ > 0)
      private val grossGain  = gains.sum
      val median: BigDecimal = profits.median

      /** The best period's share of what this dataset's winning periods made, or `None` when none of them won: there was nothing to divide
        * up, so there is no concentration to measure, and a dataset that never won is already caught by net profit, expectancy, the
        * profitable-pair-month ratio and the median.
        */
      val bestShare: Option[Double] = Option.when(grossGain > 0)((gains.max / grossGain).toDouble)
    }

    /** A candidate's record read at both the granularities that can hide compensation, because neither alone catches it.
      *
      * Pooling pairs before splitting into periods lets one pair's good March pay for another's bad March, so the counting statistics are
      * taken across every pair-month separately. Concentration stays per dataset, where a share of the winnings still means "one period
      * carried this pair"; the typical-period figure stays pooled, which is what a portfolio-level typical period is asking about.
      */
    final private case class Evidence(
        perDataset: List[Series],
        pooled: Series,
        pooledBalance: BigDecimal,
        monthsCovered: Int,
        dataMonths: Int
    ) {

      /** How many datasets the pooled figures cover, which the trade floor needs so that widening the corpus cannot weaken it. */
      val datasetCount: Int = perDataset.size

      private val pairMonths: List[BigDecimal] = perDataset.flatMap(_.profits)
      private val gain: BigDecimal             = pairMonths.filter(_ > 0).sum
      private val loss: BigDecimal             = pairMonths.filter(_ < 0).map(_.abs).sum

      /** Share of pair-months that made money. Every dataset contributes every period it covered, so a pair that was flat all year drags
        * this down instead of being invisible.
        */
      val profitableRatio: Double = if (pairMonths.isEmpty) 0.0 else pairMonths.count(_ > 0).toDouble / pairMonths.size

      /** What the winning pair-months made against what the losing ones cost. `None` when none lost, the same undefined-because-good case
        * `OrderStats.profitFactor` reports.
        */
      val profitFactor: Option[BigDecimal] = Option.when(loss != 0)(gain / loss)

      /** The most concentrated dataset's share, since a candidate carried by one period in one pair is the thing being flagged. `None` when
        * no dataset had a winning period at all — those drop out rather than pinning the max at the worst possible value.
        */
      val worstBestShare: Option[Double] = perDataset.flatMap(_.bestShare).maxOption

      val medianPeriodReturn: Double =
        if (pooledBalance == 0) 0.0 else (pooled.median / pooledBalance).toDouble

      /** Sortino over the filled pooled series rather than `OrderStats.sortinoRatio`, which is built from `profitByMonth` and so measures
        * dispersion across only the months a candidate chose to trade — leaving the one axis whose job is to punish downside blind to
        * exactly the flat and skipped months every other constraint here counts. Computed the way `OrderStats` computes it, so the target
        * keeps its calibration.
        */
      def sortino(periodsPerYear: Double): RiskRatio = {
        val (_, reversed) = pooled.profits.foldLeft((pooledBalance, List.empty[Double])) { case ((balance, acc), profit) =>
          val periodReturn = if (balance == 0) 0.0 else (profit / balance).toDouble
          (balance + profit, periodReturn :: acc)
        }
        val returns = reversed.reverse
        if (returns.size < 2) RiskRatio.InsufficientData
        else {
          val mean     = returns.sum / returns.size
          val downside = math.sqrt(returns.map(r => math.pow(math.min(r, 0.0), 2)).sum / returns.size)
          RiskRatio.from(mean, downside, math.sqrt(periodsPerYear))
        }
      }
    }

    /** Scores candidates on how consistently they earn across the run rather than on what they earned over it.
      *
      * A pooled total is a sum: a candidate that lost for eight months and made it all back in the ninth reports the same net profit,
      * profit factor and drawdown as one that earned steadily, and `scaled` floors at zero, so a losing period can only dilute a positive
      * total and never count against it. That is the shape a search converges on when it is free to, and the shape that does not survive a
      * different year. So the unit of evidence is the period a dataset covered: 30% median period return, 20% pair-month profit factor, 20%
      * recovery factor, 15% Sortino over the filled series, 15% expectancy over average loss.
      *
      * Constraints then attack compensation from each direction it comes from — too few profitable pair-months, too much of one pair's
      * winnings in its best period, winning periods that do not outweigh losing ones — and a median period that loses money is
      * disqualifying outright. Everything `Robust` checks is still checked, on the pooled portfolio.
      */
    def apply(config: Config = Config()): ScoringFunction =
      new ScoringFunction {
        override def score(stats: List[OrderStats]): Double =
          if (stats.isEmpty) 0.0
          else {
            val portfolio = OrderStats.combine(stats)
            val evidence  = evidenceOf(stats, portfolio, config)
            val discount  = constraints(portfolio, evidence, profitableRatio(stats), config).map(_.satisfaction).product
            if (discount == 0.0) 0.0 else quality(portfolio, evidence, config) * discount
          }

        override def violations(stats: List[OrderStats]): List[Violation] =
          if (stats.isEmpty) List(Violation("dataset count", "0", "at least 1"))
          else {
            val portfolio = OrderStats.combine(stats)
            constraints(portfolio, evidenceOf(stats, portfolio, config), profitableRatio(stats), config).flatMap(_.violation)
          }
      }

    private def evidenceOf(stats: List[OrderStats], portfolio: OrderStats, config: Config): Evidence = {
      // Coverage is counted in months off the ungrouped series, never as periods times period length: grouping merges a
      // remainder into the last period, so multiplying back would overstate a run whose month count is not a whole
      // number of periods.
      val pooledMonths = monthlyProfits(portfolio)
      Evidence(
        perDataset = stats.map(dataset => Series(intoPeriods(monthlyProfits(dataset), config))),
        pooled = Series(intoPeriods(pooledMonths, config)),
        pooledBalance = portfolio.initialBalance,
        monthsCovered = pooledMonths.size,
        dataMonths = monthsOfData(portfolio)
      )
    }

    private def quality(portfolio: OrderStats, evidence: Evidence, config: Config): Double = {
      val confidence = sampleConfidence(portfolio, config.minTradesPerMonth, evidence.dataMonths, evidence.datasetCount)

      def credited(target: Double): Double = target * confidence

      val targetPeriodReturn = config.targetNetReturn.value / config.periodsPerYear
      val periodProfitFactor = evidence.profitFactor.fold(credited(config.targetPeriodProfitFactor.value))(_.toDouble)
      val recoveryFactor     = portfolio.recoveryFactor.fold(credited(config.targetRecoveryFactor.value))(_.toDouble)
      val sortinoRatio       = evidence.sortino(config.periodsPerYear) match
        case RiskRatio.Defined(value)   => value
        case RiskRatio.ZeroDeviation    => credited(config.targetSortinoRatio.value)
        case RiskRatio.InsufficientData => 0.0
      val expectancyToLoss =
        if (portfolio.averageLoss == 0) credited(config.targetExpectancyToLossRatio.value)
        else (portfolio.expectancy / portfolio.averageLoss).toDouble

      (0.300 * scaled(evidence.medianPeriodReturn, targetPeriodReturn, maxScore = maxComponentScore)) +
        (0.200 * scaled(periodProfitFactor, config.targetPeriodProfitFactor.value, maxScore = maxComponentScore)) +
        (0.200 * scaled(recoveryFactor, config.targetRecoveryFactor.value, maxScore = maxComponentScore)) +
        (0.150 * scaled(sortinoRatio, config.targetSortinoRatio.value, maxScore = maxComponentScore)) +
        (0.150 * scaled(expectancyToLoss, config.targetExpectancyToLossRatio.value, maxScore = maxComponentScore))
    }

    private def profitableRatio(stats: List[OrderStats]): Double =
      stats.count(_.totalProfit > 0).toDouble / stats.size

    /** Groups a monthly series into periods of `periodMonths` from the start of the window, so every dataset's periods line up.
      *
      * A short remainder is merged into the last full period rather than standing as one of its own: a stub would be scored against a full
      * period's return target and counted as a full period by anything measuring coverage — a weak period manufactured by arithmetic.
      * Merging keeps every month's profit in the evidence, which dropping the remainder would not.
      */
    private def intoPeriods(months: List[BigDecimal], config: Config): List[BigDecimal] = {
      val size   = config.periodMonths.value
      val groups = months.grouped(size).toList
      val merged =
        if (groups.sizeIs > 1 && groups.last.sizeIs < size) groups.dropRight(2) :+ (groups(groups.size - 2) ++ groups.last)
        else groups
      merged.map(_.sum)
    }

    /** What the run made in each month it is answerable for, oldest first. A month no trade closed in contributes a zero rather than being
      * absent, which is what stops the breakdown flattering a candidate that traded in a burst.
      */
    private def monthlyProfits(stats: OrderStats): List[BigDecimal] =
      coveredMonths(stats).map(month => stats.profitByMonth.getOrElse(month.toString, BigDecimal(0)))

    private def constraints(portfolio: OrderStats, evidence: Evidence, profitableRatio: Double, config: Config): List[Constraint] = {
      val costRatio  = if (portfolio.preCostProfit <= 0) BigDecimal(1) else portfolio.totalCosts / portfolio.preCostProfit
      val shareLimit = config.singlePeriodProfitShareLimit(evidence.pooled.count)
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
          tradeFloorDescription(config.minTradesPerMonth, evidence.dataMonths, evidence.datasetCount),
          sampleConfidence(portfolio, config.minTradesPerMonth, evidence.dataMonths, evidence.datasetCount)
        ),
        // The union, not the data window: this asks how much of the run there was to judge, and a month a position was
        // liquidated into is a month that earned something.
        Constraint(
          "months covered",
          s"${evidence.monthsCovered} months",
          s">= ${config.minMonthsCovered.value} months",
          rampUp(evidence.monthsCovered.toDouble, 0.0, config.minMonthsCovered.value.toDouble)
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
        // Disqualifying rather than discounted, and on the same footing as a negative expectancy: a candidate whose
        // typical period loses money has not found an edge, however well the total reads.
        Constraint(
          "median period profit",
          evidence.pooled.median.toString,
          "> 0",
          if (evidence.pooled.median <= 0) 0.0 else 1.0
        ),
        Constraint(
          "profitable pair-months",
          f"${evidence.profitableRatio}%.3f",
          f">= ${config.minProfitablePeriodRatio.value}%.3f",
          rampUp(evidence.profitableRatio, 0.0, config.minProfitablePeriodRatio.value)
        ),
        Constraint(
          "most concentrated pair's best month",
          evidence.worstBestShare.fold("N/A")(share => f"$share%.3f"),
          f"<= $shareLimit%.3f (${config.maxSinglePeriodProfitShare.value}%.3f scaled to ${evidence.pooled.count} periods)",
          // Concentration is weak evidence, not a disqualification, so the hard limit sits above every attainable share:
          // a share of 1.0 costs half the score rather than all of it. Zeroing here wiped whole folds, and through the
          // product over folds, whole runs — a pair with one winning period has no way to score better than 1.0.
          evidence.worstBestShare.fold(1.0)(share => rampDown(share, shareLimit, 2.0 - shareLimit))
        ),
        Constraint(
          "pair-month profit factor",
          evidence.profitFactor.fold("N/A")(_.toString),
          s">= ${config.minPeriodProfitFactor.value}",
          evidence.profitFactor.fold(1.0)(pf => rampUp(pf.toDouble, 1.0, config.minPeriodProfitFactor.value))
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
  }

  /** Every calendar month a run is answerable for, oldest first: the union of the months the data covered and the months a trade closed in.
    *
    * Both directions matter. The window reaches past the trades when a run sat out either end, which it has to answer for. A trade reaches
    * past the window when a position still open at the final bar is liquidated one interval later, landing in the next month for any
    * dataset ending at 23:00 on the last of the month — and profit must not be able to exist outside the record while `totalProfit` counts
    * it.
    *
    * With no window recorded this degenerates to the span of the trades, which is all a caller assembling `OrderStats` by hand can supply.
    */
  private def coveredMonths(stats: OrderStats): List[YearMonth] = {
    val bounds = stats.completedTrades.map(_.closedAt) ::: stats.dataWindow.toList.flatMap(w => List(w.from, w.to))
    bounds match
      case Nil   => Nil
      case times => monthsBetween(times.min, times.max)
  }

  /** Calendar months of market data the run was given, which is what the trade floor scales by: a run cannot be asked to trade through
    * months it was never offered. `coveredMonths` deliberately spans the month a final position was liquidated into, and billing the floor
    * for that month demands a fourth month of trades from three months of data. Falls back to the span of the trades with no window
    * recorded.
    */
  private def monthsOfData(stats: OrderStats): Int =
    stats.dataWindow.fold(coveredMonths(stats).size)(w => monthsBetween(w.from, w.to).size)

  /** The calendar months two instants touch, inclusive. Not `ChronoUnit.MONTHS.between`, which counts whole elapsed months and would read
    * 2024-08-01 to 2024-10-31T23:00 as two.
    */
  private def monthsBetween(from: Instant, to: Instant): List[YearMonth] = {
    val first = YearMonth.from(from.atZone(ZoneOffset.UTC))
    val last  = YearMonth.from(to.atZone(ZoneOffset.UTC))
    Iterator.iterate(first)(_.plusMonths(1)).takeWhile(!_.isAfter(last)).toList
  }

  /** The pooled trade count a run of this shape was expected to produce, which the sample-size ramp measures against.
    *
    * Derived from the months of data and the dataset count rather than configured as a total, because the count it is compared against is
    * pooled over both: a fixed total silently doubles the frequency it demands when a year is split into halves, and silently halves it
    * when six pairs become twelve — weakening the only sample-size guard there is at the moment the corpus was widened to strengthen it.
    */
  private def tradeFloor(minTradesPerMonth: PosInt, dataMonths: Int, datasetCount: Int): Double =
    minTradesPerMonth.value.toDouble * dataMonths * datasetCount

  private def sampleConfidence(portfolio: OrderStats, minTradesPerMonth: PosInt, dataMonths: Int, datasetCount: Int): Double =
    rampUp(portfolio.total.toDouble, 0.0, tradeFloor(minTradesPerMonth, dataMonths, datasetCount))

  private def tradeFloorDescription(minTradesPerMonth: PosInt, dataMonths: Int, datasetCount: Int): String =
    f">= ${tradeFloor(minTradesPerMonth, dataMonths, datasetCount)}%.0f " +
      f"(${minTradesPerMonth.value} per pair-month over $dataMonths months x $datasetCount pairs)"

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

  /** Scales a metric against its target: proportional up to it (target -> 1.0), then saturating asymptotically towards `maxScore`, so an
    * outlier on one axis cannot dominate the weighted sum. Asymptotic rather than a hard ceiling, which would score every candidate past it
    * identically exactly where they are most worth telling apart; `tanh` shares the log curve's slope at the target and stays strictly
    * increasing.
    */
  private def scaled(value: Double, target: Double, maxScore: Double): Double =
    if (value <= 0.0) 0.0
    else {
      val ratio = value / target
      if (ratio <= 1.0) ratio
      else {
        val headroom = maxScore - 1.0
        1.0 + headroom * math.tanh(math.log(ratio) / headroom)
      }
    }
}
