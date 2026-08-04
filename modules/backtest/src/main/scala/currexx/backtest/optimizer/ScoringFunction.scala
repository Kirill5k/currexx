package currexx.backtest.optimizer

import currexx.backtest.{OrderStats, RiskRatio}
import currexx.backtest.syntax.*
import currexx.backtest.types.{GreaterThanOne, PositiveUnitInterval}
import currexx.backtest.types.given
import eu.timepit.refined.types.numeric.{PosDouble, PosInt}

import java.time.{YearMonth, ZoneOffset}

/** How a run ranks candidates, together with the acceptance test that shares its thresholds.
  *
  * `score` is what selection sorts by and `violations` is what decides whether to trust the result; the two answer different questions of
  * the same thresholds. Keeping them on one object is what stops a champion from being re-checked against numbers it was never scored
  * against — the search holds only this, so there is nothing else a caller could reach for and no configuration to pass twice.
  */
trait ScoringFunction:
  def score(stats: List[OrderStats]): Double

  /** Re-checks a result against the thresholds it was scored against, as pass or fail.
    *
    * Scoring ramps rather than gates on purpose, because gating flattens the fitness landscape and leaves selection nothing to rank. That
    * makes it a good search signal and a poor acceptance test: the winner of a run is only the best of whatever happened to be tried, and a
    * candidate breaching every threshold still scores above zero and still wins if nothing better turned up. Deciding whether to trust the
    * winner is a separate question from ranking candidates during the search, and needs asking separately — of the same constraints, so
    * that the answer cannot contradict the score.
    */
  def violations(stats: List[OrderStats]): List[ScoringFunction.Violation]

object ScoringFunction {
  final case class Violation(constraint: String, actual: String, required: String):
    override def toString: String = s"$constraint is $actual, required $required"

  /** A threshold scoring function scores against, evaluated once and read two ways.
    *
    * `satisfaction` is the single encoding: 1.0 once the threshold is met, falling towards 0.0 below it. Scoring multiplies these together;
    * the champion check reports every one that did not reach 1.0. That equivalence is exact, because a ramp reaches 1.0 on precisely the
    * condition a pass/fail predicate would test. Writing the predicate separately would let it drift out of step with the ramp, which would
    * mean accepting a winner the search had penalised, or rejecting one it had not.
    *
    * Only the description is by-name, because only scoring is on the search's path: it reads `satisfaction` from all of these and the
    * wording of none of them. Formatting eagerly meant every evaluation rendered eight BigDecimals it then discarded, once per candidate
    * for the whole run, to describe a breach that is described at most once.
    */
  final private class Constraint(name: String, actual: => String, required: => String, val satisfaction: Double):
    def violation: Option[Violation] = Option.when(satisfaction < 1.0)(Violation(name, actual, required))

  object Robust {
    // Upper bound that any single scaled component approaches but never reaches. The logarithm alone is unbounded, and
    // two of the four components divide by a quantity nothing stops from approaching zero — recovery factor by max
    // drawdown, expectancy by average loss — so without a bound a candidate that got lucky on one axis outscores every
    // balanced candidate combined.
    private val maxComponentScore = 3.0

    final case class Config(
        // Per pair per month, multiplied up by `tradeFloor` into the pooled total the run has to reach, so that the
        // threshold keeps its meaning however long the window and however many datasets the corpus holds. It was a
        // pooled figure of ten, which over the six majors and their six-month training half meant under two per
        // pair-month, and meant something different again the moment either number changed.
        //
        // Five, so that a month holds enough trades for the monthly return series the Sortino ratio is measured over
        // to be something other than noise. Far above it the only candidates left are the ones trading often enough
        // for costs to consume the edge, which is what maxCostToPreCostProfitRatio is left to catch; the two pull
        // against each other on purpose.
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
    def apply(config: Config = Config()): ScoringFunction =
      new ScoringFunction {
        override def score(stats: List[OrderStats]): Double =
          if (stats.isEmpty) 0.0
          else {
            val portfolio = OrderStats.combine(stats)
            val months    = coveredMonths(portfolio).size
            val discount  = constraints(portfolio, months, stats.size, profitableRatio(stats), config).map(_.satisfaction).product
            if (discount == 0.0) 0.0 else quality(portfolio, months, stats.size, config) * discount
          }

        override def violations(stats: List[OrderStats]): List[Violation] =
          if (stats.isEmpty) List(Violation("dataset count", "0", "at least 1"))
          else {
            val portfolio = OrderStats.combine(stats)
            constraints(portfolio, coveredMonths(portfolio).size, stats.size, profitableRatio(stats), config).flatMap(_.violation)
          }
      }

    private def quality(portfolio: OrderStats, monthsCovered: Int, datasetCount: Int, config: Config): Double = {
      // A metric whose denominator vanished is undefined rather than bad — no drawdown to recover from, no losing
      // month, no losing trade — so it is credited with its target instead of being scored as zero. The credit is
      // worth only as much as the sample behind it: an absence of drawdown across five trades is evidence of nothing,
      // and all three of these can vanish at once, which would otherwise hand a handful of tiny winning trades 0.65
      // of a full score for free. A thin sample is therefore discounted twice, once here on the unearned portion and
      // again by the sample-size constraint discounting the score as a whole, which is the intent.
      val confidence = sampleConfidence(portfolio, config.minTradesPerMonth, monthsCovered, datasetCount)

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

      (0.350 * scaled(netReturn, config.targetNetReturn.value, maxScore = maxComponentScore)) +
        (0.300 * scaled(recoveryFactor, config.targetRecoveryFactor.value, maxScore = maxComponentScore)) +
        (0.175 * scaled(sortinoRatio, config.targetSortinoRatio.value, maxScore = maxComponentScore)) +
        (0.175 * scaled(expectancyToLoss, config.targetExpectancyToLossRatio.value, maxScore = maxComponentScore))
    }

    private def profitableRatio(stats: List[OrderStats]): Double =
      stats.count(_.totalProfit > 0).toDouble / stats.size

    private def constraints(
        portfolio: OrderStats,
        monthsCovered: Int,
        datasetCount: Int,
        profitableRatio: Double,
        config: Config
    ): List[Constraint] = {
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
          tradeFloorDescription(config.minTradesPerMonth, monthsCovered, datasetCount),
          sampleConfidence(portfolio, config.minTradesPerMonth, monthsCovered, datasetCount)
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
        // Per pair per month, multiplied up by `tradeFloor` into the pooled total the run has to reach, so that the
        // threshold keeps its meaning however long the window and however many datasets the corpus holds. It was a
        // pooled figure of ten, which over the six majors and their six-month training half meant under two per
        // pair-month, and meant something different again the moment either number changed.
        //
        // Five, because every constraint below reads the record a month at a time, and the sign of a pair-month
        // holding two trades is close to a coin flip: at that rate the profitable-pair-month ratio and the pair-month
        // profit factor are counting noise whatever thresholds they are given, and the sample-size discount that was
        // supposed to withhold judgement until there was a sample is satisfied by almost anything. Far above it the
        // only candidates left are the ones trading often enough for costs to consume the edge, which is what
        // maxCostToPreCostProfitRatio is left to catch; the two pull against each other on purpose.
        minTradesPerMonth: PosInt = 5,
        // Calendar months per period.
        //
        // One, and not because thin periods are ideal — at ~2 trades per pair-month, whether a given pair-month made
        // money is close to a coin flip. Widening it to hold more trades per period was the obvious fix and it is the
        // wrong one: swept over 1..6 months against every trade floor above, monthly wins everywhere, and three-month
        // periods turn the correlation negative. Fewer, denser samples lose more than they gain, because it is the
        // number of periods that the counting statistics are made of.
        periodMonths: PosInt = 1,
        // Months of data the run has to cover before its consistency has been measured at all, counted in months rather
        // than periods so that it keeps its meaning when `periodMonths` changes.
        minMonthsCovered: PosInt = 6,
        minProfitableDatasetRatio: PositiveUnitInterval = 2.0 / 3.0,
        // More than half the pair-months have to make money. This is the constraint that counts losing months, as
        // opposed to netting them off against the winners. Unvalidated: sweeping it over 0.3..0.65 moves the rank
        // correlation by 0.02, because none of the seventeen strategies measured exhibits the extreme pattern it
        // targets. Kept because it is the direct encoding of the requirement, not because it has been shown to pay.
        minProfitablePeriodRatio: PositiveUnitInterval = 0.55,
        // Ceiling on how much of one pair's winnings may come from its single best month.
        //
        // Loose on purpose. Tightening this costs predictive power — sweeping 0.25..1.0 raises the rank correlation
        // from +0.46 to +0.59 — because concentration and compensation are not the same thing, and only the second is
        // the defect. A trend-following strategy earns in bursts when trends happen; that its best month dwarfs its
        // median one is the nature of the edge rather than evidence against it. Refusing lumpy winners therefore
        // selects against the legitimate case. What the requirement is actually about is winners paying for losers,
        // and the profitable-month ratio and the pair-month profit factor are what measure that. This is left only to
        // catch the pathological end, where a single month is most of everything a pair ever made.
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

      /** Divides the annual return target into the per-period target, and annualises the risk ratio. Derived rather than configured so that
        * it cannot disagree with `periodMonths`.
        */
      val periodsPerYear: Double = 12.0 / periodMonths.value
    }

    /** One dataset's profit by calendar month over every month the dataset covered, including the months no trade closed in.
      *
      * Filling the gaps is what stops the breakdown from flattering a candidate that traded in a burst, and taking the months from the data
      * window rather than from the trades is what stops it flattering one that sat out either end. `profitByMonth` holds a key only for a
      * month some trade closed in, so a candidate that made three trades in three months of a twelve-month run and won all three has a
      * breakdown that is 100% profitable periods and perfectly unconcentrated. Judged over the twelve months it was offered, the same
      * candidate is 25% profitable periods. Neither the pooled sample-size constraint nor a gap-only fill covers this, because the burst
      * can be any number of trades and can sit anywhere: only the months the data covered bound it.
      */
    final private case class Series(profits: List[BigDecimal]) {
      val count: Int            = profits.size
      private val gains         = profits.filter(_ > 0)
      private val losses        = profits.filter(_ < 0)
      val grossGain: BigDecimal = gains.sum
      val grossLoss: BigDecimal = losses.map(_.abs).sum
      val profitableCount: Int  = gains.size
      val median: BigDecimal    = profits.median

      /** The best month's share of everything this dataset's winning months made. 1.0 when there was nothing to win, which is where both
        * readings of the constraint want it: a breach, and a satisfaction of 0.0.
        */
      val bestShare: Double =
        if (grossGain <= 0) 1.0 else (gains.max / grossGain).toDouble
    }

    /** A candidate's record read at both the granularities that can hide compensation, because neither alone catches it.
      *
      * Pooling six pairs before breaking the result into months lets one pair's good March pay for another's bad March, so the counting
      * statistics are taken across every pair-month separately. But concentration has to stay per pair and per year to keep its threshold
      * meaningful: a share of the winnings that means "one month carried this pair" across twelve samples means almost nothing across
      * seventy-two, where no single month could hold much of the total however lopsided the year was. So the share is measured within each
      * dataset and the worst one is taken, and the typical-month figure stays pooled, where a portfolio-level "typical month" is what it is
      * asking about.
      */
    final private case class Evidence(perDataset: List[Series], pooled: Series, pooledBalance: BigDecimal, monthsCovered: Int) {

      /** How many datasets the pooled figures were pooled over, which the trade floor needs so that widening the corpus cannot weaken it.
        */
      val datasetCount: Int = perDataset.size

      private val pairMonths: List[BigDecimal] = perDataset.flatMap(_.profits)
      private val gain: BigDecimal             = pairMonths.filter(_ > 0).sum
      private val loss: BigDecimal             = pairMonths.filter(_ < 0).map(_.abs).sum

      /** Share of pair-months that made money. Every dataset contributes every month it covered, so a pair that was flat all year drags
        * this down instead of being invisible.
        */
      val profitableRatio: Double = if (pairMonths.isEmpty) 0.0 else pairMonths.count(_ > 0).toDouble / pairMonths.size

      /** What the winning pair-months made against what the losing ones cost. `None` when none lost, the same undefined-because-good case
        * `OrderStats.profitFactor` reports.
        */
      val profitFactor: Option[BigDecimal] = Option.when(loss != 0)(gain / loss)

      /** The most concentrated dataset's share, since a candidate carried by one month in one pair is the thing being refused. */
      val worstBestShare: Double = perDataset.map(_.bestShare).maxOption.getOrElse(1.0)

      val medianPeriodReturn: Double =
        if (pooledBalance == 0) 0.0 else (pooled.median / pooledBalance).toDouble

      /** Sortino over the filled pooled series rather than `OrderStats.sortinoRatio`.
        *
        * The stored ratio comes from `profitByMonth`, which has no entry for a month nothing closed in, so it measures dispersion across
        * only the months a candidate chose to trade. Reading it here would leave the one axis whose job is to punish downside blind to
        * precisely the flat and skipped months every other constraint on this object counts, and would have it disagree with them about
        * what the run even was. Computed the same way `OrderStats` computes it, so the target keeps its calibration.
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

    /** Scores candidates on how consistently they earn across the run rather than on what they earned over it, and reports the shortfalls
      * of any one of them against the same thresholds.
      *
      * `Robust` measures one pooled portfolio, and a pooled total is a sum: a candidate that lost money in eight months of the year and
      * made it all back in the ninth reports the same net profit, profit factor and drawdown as one that earned steadily, and scores the
      * same. Worse, the arithmetic cannot express the difference — `scaled` floors at zero, so a losing period has no way to count against
      * a candidate and can only dilute a total that is still positive. That is the shape of result a genetic algorithm converges on when it
      * is free to, because a single well-fitted period is far easier to find in a fixed sample than a dozen mediocre ones, and it is
      * exactly the shape that does not survive contact with a different year.
      *
      * So the unit of evidence here is the calendar month a dataset covered, not the run:
      *   - 30% median period return, which one exceptional month cannot move
      *   - 20% pair-month profit factor, weighing what the winning pair-months made against what the losing ones cost
      *   - 20% recovery factor
      *   - 15% Sortino ratio, over the filled series so that skipped months count as the flat months they were
      *   - 15% expectancy relative to average loss
      *
      * Three constraints then attack compensation from the three directions it can come from, and a fourth withholds the whole judgement
      * until there are enough months to make it: too few pair-months profitable, too much of one pair's winnings concentrated in its best
      * month, and winning pair-months that do not outweigh losing ones by a margin. A median month that loses money is disqualifying
      * outright, on the same footing as a negative expectancy — whatever the total says, a candidate whose typical month loses money has
      * not found anything.
      *
      * Everything `Robust` checks is still checked, on the pooled portfolio, because those questions are about the run as a whole and are
      * not answered by breaking it up.
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
        monthsCovered = pooledMonths.size
      )
    }

    private def quality(portfolio: OrderStats, evidence: Evidence, config: Config): Double = {
      val confidence = sampleConfidence(portfolio, config.minTradesPerMonth, evidence.monthsCovered, evidence.datasetCount)

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

    /** Groups a monthly series into periods of `periodMonths`, from the start of the window forward so that the periods of every dataset in
      * a run line up with each other.
      *
      * A remainder shorter than a full period is merged into the last full one rather than left standing as a period of its own. A stub
      * period would be scored against the return target of a full period and counted as a full period by anything measuring coverage, so
      * thirteen months at three months to the period would report fifteen months covered and end on a period holding a third of the profit
      * a period is expected to hold — a weak period manufactured by arithmetic. Merging makes the last period slightly long instead, which
      * is the smaller distortion, and unlike dropping the remainder it keeps every month's profit in the evidence.
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
          tradeFloorDescription(config.minTradesPerMonth, evidence.monthsCovered, evidence.datasetCount),
          sampleConfidence(portfolio, config.minTradesPerMonth, evidence.monthsCovered, evidence.datasetCount)
        ),
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
          f"${evidence.worstBestShare}%.3f",
          f"<= ${config.maxSinglePeriodProfitShare.value}%.3f",
          rampDown(evidence.worstBestShare, config.maxSinglePeriodProfitShare.value, 1.0)
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

  /** Every calendar month a run is answerable for, oldest first, including the ones no trade closed in.
    *
    * That means every month the data covered and every month a trade closed in, which are not the same set in either direction. The window
    * can reach past the trades, which is the point of having it: a run that sat out the ends of its sample has to answer for those months.
    * But a trade can also close past the window, because a position still open when the data runs out is liquidated at a mark stamped one
    * interval after the final bar — so a run whose last bar is the last hour of a month realises that profit in the next one. Bounding the
    * series by the window alone would drop it from the evidence while `totalProfit` went on counting it, leaving the pooled constraints and
    * the period constraints disagreeing about what the run earned. Spanning the union is what makes it impossible for profit to exist
    * outside the record.
    *
    * With no window recorded this degenerates to the span of the trades, which is what a caller assembling `OrderStats` by hand supplies.
    * That is the weaker measurement, because it cannot see a run that sat out either end; it is here to keep those callers working, not as
    * an equivalent.
    */
  private def coveredMonths(stats: OrderStats): List[YearMonth] = {
    val bounds = stats.completedTrades.map(_.closedAt) ::: stats.dataWindow.toList.flatMap(w => List(w.from, w.to))
    bounds match
      case Nil   => Nil
      case times =>
        val first = YearMonth.from(times.min.atZone(ZoneOffset.UTC))
        val last  = YearMonth.from(times.max.atZone(ZoneOffset.UTC))
        Iterator.iterate(first)(_.plusMonths(1)).takeWhile(!_.isAfter(last)).toList
  }

  /** The pooled trade count a run of this shape was expected to produce, which is what the sample-size ramp measures against.
    *
    * Derived from both the months the run covered and the datasets it was run against, rather than configured as a total, so that it cannot
    * disagree with the shape of the corpus it is applied to. As a total it did, in both directions, because the count it is compared
    * against is pooled over every dataset and every month while a fixed floor is pooled over neither.
    *
    * A figure calibrated against a year silently doubles the frequency it demands the moment that year is split into halves, which is
    * selection pressure towards trading more often and the failure mode the cost constraint exists to catch. A figure calibrated against
    * six pairs silently halves it the moment six become twelve, which is the only sample-size guard there is weakening by exactly the
    * factor the corpus was widened by — at the one moment the corpus was widened in order to strengthen it.
    */
  private def tradeFloor(minTradesPerMonth: PosInt, monthsCovered: Int, datasetCount: Int): Double =
    minTradesPerMonth.value.toDouble * monthsCovered * datasetCount

  private def sampleConfidence(portfolio: OrderStats, minTradesPerMonth: PosInt, monthsCovered: Int, datasetCount: Int): Double =
    rampUp(portfolio.total.toDouble, 0.0, tradeFloor(minTradesPerMonth, monthsCovered, datasetCount))

  private def tradeFloorDescription(minTradesPerMonth: PosInt, monthsCovered: Int, datasetCount: Int): String =
    f">= ${tradeFloor(minTradesPerMonth, monthsCovered, datasetCount)}%.0f " +
      f"(${minTradesPerMonth.value} per pair-month over $monthsCovered months x $datasetCount pairs)"

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

  /** Scales a metric against its target with diminishing returns past the target: proportional up to the target (target -> 1.0), then
    * saturating towards `maxScore` above it, so that an outlier on a single axis cannot dominate the weighted sum.
    *
    * The saturation is asymptotic rather than a hard ceiling. A ceiling scores every candidate past it identically, which costs selection
    * its ability to rank exactly where the candidates are most worth telling apart, and leaves that axis with no gradient for the optimiser
    * to climb. `tanh` bends the same logarithmic curve — it shares its slope at the target — into one that is bounded yet still strictly
    * increasing everywhere, so a better candidate always outscores a worse one however far out it is.
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
