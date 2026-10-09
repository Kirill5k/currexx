package currexx.backtest.optimizer

import cats.syntax.traverse.*
import currexx.backtest.{DataWindow, OrderStats}
import currexx.backtest.types.{FiniteNonNegDouble, NonEmptyMap}
import currexx.domain.market.CurrencyPair
import currexx.domain.signal.Indicator
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, NonNegInt, PosBigDecimal}

import java.time.{YearMonth, ZoneOffset}

final case class SelectionCoverage(dataWindow: DataWindow, initialBalance: PosBigDecimal)

/** Compact, checked results from one fixed period. Candidate identities must already be canonical before entering the evidence cache. */
final class SelectionEvidence private (
    val candidate: Indicator,
    val coverage: NonEmptyMap[CurrencyPair, SelectionCoverage],
    val initialBalance: PosBigDecimal,
    val netProfit: BigDecimal,
    val costs: NonNegBigDecimal,
    val maxDrawdownPercent: NonNegBigDecimal,
    val monthlyProfits: NonEmptyMap[YearMonth, BigDecimal],
    val score: FiniteNonNegDouble,
    val violations: List[ScoringFunction.Violation],
    val closedTrades: NonNegInt,
    val forcedClosures: NonNegInt
) {
  def monthsCovered: Int = monthlyProfits.value.size

  private def fields =
    (
      candidate,
      coverage,
      initialBalance,
      netProfit,
      costs,
      maxDrawdownPercent,
      monthlyProfits,
      score,
      violations,
      closedTrades,
      forcedClosures
    )

  override def equals(other: Any): Boolean = other match
    case that: SelectionEvidence => (this eq that) || fields == that.fields
    case _                       => false

  override def hashCode(): Int  = fields.hashCode()
  override def toString: String = s"SelectionEvidence$fields"
}

object SelectionEvidence {

  /** Refine raw measurements and check relationships once, before evidence can enter selection or a cache. */
  def from(
      candidate: Indicator,
      coverage: Map[CurrencyPair, SelectionCoverage],
      initialBalance: BigDecimal,
      netProfit: BigDecimal,
      costs: BigDecimal,
      maxDrawdownPercent: BigDecimal,
      monthlyProfits: Map[YearMonth, BigDecimal],
      score: Double,
      violations: List[ScoringFunction.Violation],
      closedTrades: Int,
      forcedClosures: Int
  ): Either[IllegalArgumentException, SelectionEvidence] =
    for
      covered       <- NonEmptyMap.from(coverage).left.map(invalid("selection coverage"))
      capital       <- PosBigDecimal.from(initialBalance).left.map(invalid("selection capital"))
      paidCosts     <- NonNegBigDecimal.from(costs).left.map(invalid("selection costs"))
      drawdown      <- NonNegBigDecimal.from(maxDrawdownPercent).left.map(invalid("selection drawdown"))
      months        <- NonEmptyMap.from(monthlyProfits).left.map(invalid("selection months"))
      absoluteScore <- FiniteNonNegDouble.from(score).left.map(invalid("absolute selection score"))
      trades        <- NonNegInt.from(closedTrades).left.map(invalid("closed trade count"))
      closures      <- NonNegInt.from(forcedClosures).left.map(invalid("forced closure count"))
      _             <- check(coverage.values.forall(c => !c.dataWindow.to.isBefore(c.dataWindow.from)), "Reversed selection coverage")
      _             <- check(
        coverage.values.map(_.initialBalance.value).sum == initialBalance,
        "Selection coverage does not reconcile with pooled capital"
      )
      _ <- check(coveredMonths(coverage) == monthlyProfits.keySet, "Selection monthly results do not match data coverage")
      _ <- check(accountingMatches(monthlyProfits.values.sum, netProfit), "Selection monthly results do not reconcile with net profit")
      _ <- check(forcedClosures <= closedTrades, "Forced closure count exceeds closed trade count")
    yield new SelectionEvidence(
      candidate,
      covered,
      capital,
      netProfit,
      paidCosts,
      drawdown,
      months,
      absoluteScore,
      violations,
      trades,
      closures
    )

  // DECIMAL128 additions to account equity can lose digits that remain in a small net-profit sum. This checks accounting integrity only;
  // comparisons against upgrade thresholds still use the full amounts and never use a tolerance or display rounding.
  private def accountingMatches(first: BigDecimal, second: BigDecimal): Boolean =
    (first - second).abs <= BigDecimal("0.000000000000000001")

  /** Pair identifiers come from the simulated datasets, including pairs that opened no trades. */
  def fromStats(
      candidate: Indicator,
      stats: List[(CurrencyPair, OrderStats)],
      scorer: ScoringFunction
  ): Either[IllegalArgumentException, SelectionEvidence] =
    for
      _        <- check(stats.nonEmpty, "Selection data is required")
      _        <- check(stats.map(_._1).distinct.size == stats.size, "Selection data contains duplicate currency pairs")
      measured <- stats.traverse { case (pair, result) =>
        for
          window  <- result.dataWindow.toRight(new IllegalArgumentException(s"Missing selection coverage for $pair"))
          _       <- check(!window.to.isBefore(window.from), s"Reversed selection coverage for $pair")
          capital <- PosBigDecimal.from(result.initialBalance).left.map(invalid(s"selection capital for $pair"))
          _       <- check(result.invalidOrderCount == 0, s"Invalid orders in selection data for $pair: ${result.invalidOrderCount}")
          _       <- check(result.totalCosts >= 0, s"Negative selection costs for $pair")
          _       <- check(result.completedTrades.forall(_.currencyPair == pair), s"Trade pair differs from selection dataset $pair")
          months  <- monthlyProfits(result, window)
        yield (pair, SelectionCoverage(window, capital), months)
      }
      absoluteScore = scorer.score(stats.map(_._2))
      portfolio     = OrderStats.combine(stats.map(_._2))
      evidence <- from(
        candidate = candidate,
        coverage = measured.map { case (pair, window, _) => pair -> window }.toMap,
        initialBalance = portfolio.initialBalance,
        netProfit = portfolio.totalProfit,
        costs = portfolio.totalCosts,
        maxDrawdownPercent = portfolio.maxDrawdownPercent,
        monthlyProfits = measured.flatMap(_._3.toList).groupMapReduce(_._1)(_._2)(_ + _),
        score = absoluteScore,
        violations = scorer.violations(stats.map(_._2)),
        closedTrades = portfolio.total,
        forcedClosures = portfolio.forcedClosureCount
      )
    yield evidence

  private def coveredMonths(coverage: Map[CurrencyPair, SelectionCoverage]): Set[YearMonth] =
    coverage.values.flatMap { covered =>
      val first = YearMonth.from(covered.dataWindow.from.atZone(ZoneOffset.UTC))
      val last  = YearMonth.from(covered.dataWindow.to.atZone(ZoneOffset.UTC))
      Iterator.iterate(first)(_.plusMonths(1)).takeWhile(!_.isAfter(last))
    }.toSet

  private def invalid(field: String)(reason: String): IllegalArgumentException =
    new IllegalArgumentException(s"Invalid $field: $reason")

  private def monthlyProfits(stats: OrderStats, window: DataWindow): Either[IllegalArgumentException, Map[YearMonth, BigDecimal]] = {
    val curve = stats.equityCurve
    val first = YearMonth.from(window.from.atZone(ZoneOffset.UTC))
    val last  = YearMonth.from(window.to.atZone(ZoneOffset.UTC))
    // A simulator may settle the final position immediately after its last mark. That settlement belongs to the observed final month.
    val terminalTime    = window.to.plusNanos(1)
    val terminalAllowed = stats.completedTrades.exists(t => t.forcedClosure && t.closedAt == terminalTime)
    for
      _ <- check(curve.map(_.time) == curve.map(_.time).sorted, "Selection equity history is not chronological")
      _ <- check(
        curve.forall(p => !p.time.isBefore(window.from) && (!p.time.isAfter(window.to) || (terminalAllowed && p.time == terminalTime))),
        "Selection equity history extends outside its data coverage"
      )
      _ <- check(
        accountingMatches(curve.lastOption.fold(BigDecimal(0))(_.equity - stats.initialBalance), stats.totalProfit),
        "Selection equity history does not reconcile with net profit"
      )
    yield {
      val closingEquity = curve.foldLeft(Map.empty[YearMonth, BigDecimal]) { (months, point) =>
        val month = if (point.time.isAfter(window.to)) last else YearMonth.from(point.time.atZone(ZoneOffset.UTC))
        months.updated(month, point.equity)
      }
      Iterator
        .iterate(first)(_.plusMonths(1))
        .takeWhile(!_.isAfter(last))
        .foldLeft((stats.initialBalance, Map.empty[YearMonth, BigDecimal])) { case ((previous, profits), month) =>
          val equity = closingEquity.getOrElse(month, previous)
          (equity, profits.updated(month, equity - previous))
        }
        ._2
    }
  }

  private def check(valid: Boolean, message: => String): Either[IllegalArgumentException, Unit] =
    Either.cond(valid, (), new IllegalArgumentException(message))
}
