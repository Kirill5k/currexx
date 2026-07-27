package currexx.backtest

import currexx.backtest.syntax.*
import currexx.backtest.types.given
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{Currency, CurrencyPair, TradeOrder as TO}
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, PosBigDecimal}

import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.math.sqrt

final case class TransactionCosts(
    spreadPips: NonNegBigDecimal = BigDecimal("0.8"),
    slippagePipsPerSide: NonNegBigDecimal = BigDecimal("0.1"),
    commissionPerTrade: NonNegBigDecimal = BigDecimal(0)
)

final case class RiskSettings(
    initialBalance: PosBigDecimal = BigDecimal(10000),
    accountCurrency: Currency = Currency.USD,
    unitsPerLot: PosBigDecimal = BigDecimal(100000),
    transactionCosts: TransactionCosts = TransactionCosts(),
    quoteToAccountRates: Map[Currency, PosBigDecimal] = Map.empty
)

final case class CompletedTrade(
    currencyPair: CurrencyPair,
    position: TO.Position,
    openedAt: Instant,
    closedAt: Instant,
    entryPrice: BigDecimal,
    exitPrice: BigDecimal,
    volume: BigDecimal,
    grossProfit: BigDecimal,
    costs: BigDecimal,
    netProfit: BigDecimal,
    returnPct: BigDecimal = BigDecimal(0),
    // True when the position was still open at the end of the data and was liquidated at the final mark price
    // rather than by a trading rule.
    forcedClosure: Boolean = false
)

final case class EquityPoint(
    time: Instant,
    equity: BigDecimal,
    drawdown: BigDecimal,
    drawdownPercent: BigDecimal
)

final case class MarketMark(
    price: BigDecimal,
    observedAt: Instant
)

/** A risk-adjusted return ratio, or the reason there is no number to report.
  *
  * The two undefined cases mean opposite things and must not collapse into a single `None`. A vanished denominator is the best available
  * outcome — for Sortino, not one losing month — whereas too short a series means the ratio was never measured at all. A caller that
  * credits the second as generously as the first rewards a candidate for something it never demonstrated.
  */
enum RiskRatio:
  case Defined(value: Double)
  // The series had no dispersion to divide by: for Sortino, no month lost money; for Sharpe, every month returned the same.
  case ZeroDeviation
  // Fewer than two monthly returns, so there was no series to measure dispersion across.
  case InsufficientData

  def toOption: Option[Double] = this match
    case Defined(value) => Some(value)
    case _              => None

object RiskRatio:
  def from(mean: Double, deviation: Double, annualizer: Double): RiskRatio =
    if (deviation == 0) ZeroDeviation else Defined(mean / deviation * annualizer)

final case class OrderStats(
    total: Int = 0,
    buys: Int = 0,
    sells: Int = 0,
    winCount: Int = 0,
    lossCount: Int = 0,
    breakevenCount: Int = 0,
    lossTotal: Double = 0.0,
    // Every figure below is realized: positions still open when the data ran out are liquidated at the final mark
    // and counted as completed trades, so trade counts, profit and the equity curve all describe the same trades.
    totalProfit: BigDecimal = BigDecimal(0),
    preCostProfit: BigDecimal = BigDecimal(0),
    // Total trading costs in account currency (spread + two-sided slippage + commission)
    totalCosts: BigDecimal = BigDecimal(0),
    grossProfit: BigDecimal = BigDecimal(0),
    grossLoss: BigDecimal = BigDecimal(0),
    biggestWin: BigDecimal = BigDecimal(0),
    biggestLoss: BigDecimal = BigDecimal(0),
    profitByMonth: Map[String, BigDecimal] = Map.empty,
    completedTrades: List[CompletedTrade] = Nil,
    equityCurve: List[EquityPoint] = Nil,
    initialBalance: BigDecimal = BigDecimal(10000),
    maxDrawdown: BigDecimal = BigDecimal(0),
    // Largest peak-to-trough equity decline (((peak equity − lowest subsequent equity) / peak equity) × 100)
    // Lower is better. Equity is only sampled at trade close times, so this does not capture intra-trade drawdown
    // between candles, nor unrealized losses on positions that were open concurrently.
    maxDrawdownPercent: BigDecimal = BigDecimal(0),
    // Risk-adjusted performance calculated from monthly equity returns and annualized, assuming a zero risk-free
    // rate. Higher means returns were more consistent. See RiskRatio for the two ways this can be unmeasurable.
    sharpeRatio: RiskRatio = RiskRatio.InsufficientData,
    sortinoRatio: RiskRatio = RiskRatio.InsufficientData,
    maxConsecutiveWins: Int = 0,
    maxConsecutiveLosses: Int = 0,
    forcedClosureCount: Int = 0,
    invalidOrderCount: Int = 0
):
  def medianProfitByMonth: BigDecimal = profitByMonth.values.toList.median.roundTo(5)
  def meanProfitByMonth: BigDecimal   = profitByMonth.values.toList.mean.roundTo(5)
  def meanLoss: BigDecimal            = if (lossCount == 0) BigDecimal(0) else BigDecimal(lossTotal / lossCount)
  def averageWin: BigDecimal          = if (winCount == 0) BigDecimal(0) else grossProfit / winCount
  def averageLoss: BigDecimal         = if (lossCount == 0) BigDecimal(0) else grossLoss / lossCount
  def payoffRatio: Option[BigDecimal] = Option.when(averageLoss != 0)((averageWin / averageLoss).roundTo(5))
  def winRate: BigDecimal             = if (total == 0) BigDecimal(0) else (BigDecimal(winCount) / total).roundTo(5)
  // Average net profit per closed trade
  // Positive expectancy means the strategy made money per trade on average after costs.
  def expectancy: BigDecimal = if (total == 0) BigDecimal(0) else (totalProfit / total).roundTo(8)
  // Relationship between winning and losing closed trades
  // Above 1 is profitable; 1.5 means $1.50 won for every $1 lost.
  def profitFactor: Option[BigDecimal]   = Option.when(grossLoss != 0)((grossProfit / grossLoss).roundTo(5))
  def recoveryFactor: Option[BigDecimal] = Option.when(maxDrawdown != 0)((totalProfit / maxDrawdown).roundTo(5))
  def winLossRatio: BigDecimal           =
    if (lossCount == 0) BigDecimal(winCount)
    else (BigDecimal(winCount) / BigDecimal(lossCount)).roundTo(5)

  private def showRatio(value: Option[?]): String =
    value.fold("N/A")(_.toString)

  private def showRatio(value: RiskRatio): String = value match
    case RiskRatio.Defined(v)       => v.toString
    case RiskRatio.ZeroDeviation    => "zero-deviation"
    case RiskRatio.InsufficientData => "insufficient-data"

  override def toString: String =
    s"""OrderStats(
       |netProfit=$totalProfit,
       |preCostProfit=$preCostProfit,
       |closedTrades=$total,
       |forcedClosures=$forcedClosureCount,
       |winRate=$winRate,
       |expectancy=$expectancy,
       |averageWin=$averageWin,
       |averageLoss=$averageLoss,
       |payoffRatio=${showRatio(payoffRatio)},
       |profitFactor=${showRatio(profitFactor)},
       |maxDrawdown=$maxDrawdown,
       |maxDrawdownPercent=$maxDrawdownPercent,
       |recoveryFactor=${showRatio(recoveryFactor)},
       |sharpeRatio=${showRatio(sharpeRatio)},
       |sortinoRatio=${showRatio(sortinoRatio)},
       |meanProfitByMonth=$meanProfitByMonth,
       |medianProfitByMonth=$medianProfitByMonth,
       |biggestWin=$biggestWin,
       |biggestLoss=$biggestLoss,
       |meanLoss=$meanLoss,
       |buys=$buys,
       |sells=$sells,
       |wins=$winCount,
       |losses=$lossCount,
       |breakevens=$breakevenCount,
       |maxConsecutiveWins=$maxConsecutiveWins,
       |maxConsecutiveLosses=$maxConsecutiveLosses,
       |costs=$totalCosts,
       |invalidOrders=$invalidOrderCount
       |)""".stripMargin.replaceAll("\n", "")

object OrderStats {
  private[backtest] val monthFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM").withZone(ZoneOffset.UTC)

  def fromTrades(
      trades: List[CompletedTrade],
      settings: RiskSettings,
      invalidOrderCount: Int = 0
  ): OrderStats = {
    val sortedTrades         = trades.sortBy(_.closedAt)
    val (completed, curve)   = buildEquityCurve(sortedTrades, settings.initialBalance.value)
    val netProfits           = completed.map(_.netProfit)
    val wins                 = netProfits.filter(_ > 0)
    val losses               = netProfits.filter(_ < 0)
    val monthly              = completed.groupMapReduce(t => monthFormatter.format(t.closedAt))(_.netProfit)(_ + _)
    val (maxWins, maxLosses) = streaks(netProfits)
    val (sharpe, sortino)    = monthlyRiskRatios(monthly, settings.initialBalance.value)

    OrderStats(
      total = completed.size,
      buys = completed.count(_.position == TO.Position.Buy),
      sells = completed.count(_.position == TO.Position.Sell),
      winCount = wins.size,
      lossCount = losses.size,
      breakevenCount = netProfits.count(_ == 0),
      lossTotal = losses.sum.toDouble,
      totalProfit = netProfits.sum,
      preCostProfit = completed.map(_.grossProfit).sum,
      totalCosts = completed.map(_.costs).sum,
      grossProfit = wins.sum,
      grossLoss = losses.map(_.abs).sum,
      biggestWin = wins.maxOption.getOrElse(BigDecimal(0)),
      biggestLoss = losses.minOption.getOrElse(BigDecimal(0)),
      profitByMonth = monthly,
      completedTrades = completed,
      equityCurve = curve,
      initialBalance = settings.initialBalance.value,
      maxDrawdown = curve.map(_.drawdown).maxOption.getOrElse(BigDecimal(0)),
      maxDrawdownPercent = curve.map(_.drawdownPercent).maxOption.getOrElse(BigDecimal(0)),
      sharpeRatio = sharpe,
      sortinoRatio = sortino,
      maxConsecutiveWins = maxWins,
      maxConsecutiveLosses = maxLosses,
      forcedClosureCount = completed.count(_.forcedClosure),
      invalidOrderCount = invalidOrderCount
    )
  }

  /** Pools per-dataset results into a single portfolio.
    *
    * The pooled account starts with the sum of the member balances, because each dataset was simulated on its own account. Charging the
    * combined trades against a single dataset's balance would scale returns and drawdown percentages with the number of datasets, so any
    * threshold expressed as a percentage would silently change meaning whenever a dataset is added or removed.
    *
    * That sum is the only balance this can honestly use, so there is no settings parameter to pass one in with: initial balance is all
    * `fromTrades` reads out of `RiskSettings`, and any value supplied here would be discarded. The default only ever applies to an empty
    * list, which has no member balances to add up.
    */
  def combine(stats: List[OrderStats]): OrderStats =
    fromTrades(
      trades = stats.flatMap(_.completedTrades),
      settings = stats.map(_.initialBalance).sum match
        case pooled if pooled > 0 => RiskSettings(initialBalance = PosBigDecimal.unsafeFrom(pooled))
        case _                    => RiskSettings(),
      invalidOrderCount = stats.map(_.invalidOrderCount).sum
    )

  private def buildEquityCurve(
      trades: List[CompletedTrade],
      initialBalance: BigDecimal
  ): (List[CompletedTrade], List[EquityPoint]) = {
    val tradesByCloseTime         = trades.groupBy(_.closedAt).toList.sortBy(_._1)
    val (_, _, completed, points) = tradesByCloseTime.foldLeft(
      (initialBalance, initialBalance, List.empty[CompletedTrade], List.empty[EquityPoint])
    ) { case ((equity, peak, accTrades, accPoints), (closedAt, simultaneousTrades)) =>
      val enriched = simultaneousTrades.map { trade =>
        val returnPct = if (equity == 0) BigDecimal(0) else (trade.netProfit / equity * 100).roundTo(8)
        trade.copy(returnPct = returnPct)
      }
      val next     = equity + simultaneousTrades.map(_.netProfit).sum
      val nextPeak = peak.max(next)
      val drawdown = nextPeak - next
      val ddPct    = if (nextPeak == 0) BigDecimal(0) else (drawdown / nextPeak * 100).roundTo(8)
      (
        next,
        nextPeak,
        enriched.reverse ::: accTrades,
        EquityPoint(closedAt, next, drawdown, ddPct) :: accPoints
      )
    }
    (completed.reverse, points.reverse)
  }

  /** Annualized Sharpe and Sortino from the monthly profit series.
    *
    * Neither a zero denominator nor too short a series is reported as a ratio of 0.0, because 0.0 is a poor result and neither of these is:
    * a zero downside deviation means no month lost money, and a single month means nothing was measured. Scoring either as zero would push
    * an optimiser towards strategies that do have losing months. They are returned as distinct cases rather than one catch-all so that a
    * caller can credit the good outcome without also crediting the absent one.
    */
  private def monthlyRiskRatios(profitByMonth: Map[String, BigDecimal], initialBalance: BigDecimal): (RiskRatio, RiskRatio) = {
    val monthlyProfits = profitByMonth.toList.sortBy(_._1).map(_._2)

    val (_, returns) = monthlyProfits.foldLeft((initialBalance, List.empty[Double])) { case ((balance, acc), profit) =>
      val monthlyReturn = if (balance == 0) 0.0 else (profit / balance).toDouble
      (balance + profit, monthlyReturn :: acc)
    }
    val orderedReturns = returns.reverse
    if (orderedReturns.size < 2) (RiskRatio.InsufficientData, RiskRatio.InsufficientData)
    else {
      val mean       = orderedReturns.sum / orderedReturns.size
      val variance   = orderedReturns.map(r => math.pow(r - mean, 2)).sum / (orderedReturns.size - 1)
      val deviation  = sqrt(variance)
      val downside   = orderedReturns.map(r => math.pow(math.min(r, 0.0), 2)).sum / orderedReturns.size
      val downsideSd = sqrt(downside)
      val annualizer = sqrt(12.0)
      (RiskRatio.from(mean, deviation, annualizer), RiskRatio.from(mean, downsideSd, annualizer))
    }
  }

  private def streaks(profits: List[BigDecimal]): (Int, Int) = {
    val (_, _, maxWins, maxLosses) = profits.foldLeft((0, 0, 0, 0)) {
      case ((wins, _, bestWins, bestLosses), profit) if profit > 0 =>
        val nextWins = wins + 1
        (nextWins, 0, bestWins.max(nextWins), bestLosses)
      case ((_, losses, bestWins, bestLosses), profit) if profit < 0 =>
        val nextLosses = losses + 1
        (0, nextLosses, bestWins, bestLosses.max(nextLosses))
      case ((_, _, bestWins, bestLosses), _) =>
        (0, 0, bestWins, bestLosses)
    }
    (maxWins, maxLosses)
  }
}

object OrderStatsCollector {
  final private case class CollectionState(
      trades: List[CompletedTrade] = Nil,
      openPosition: Option[TradeOrderPlacement] = None,
      invalidOrderCount: Int = 0
  )

  def collect(
      orders: List[TradeOrderPlacement],
      finalMark: Option[MarketMark] = None,
      settings: RiskSettings = RiskSettings()
  ): OrderStats = {
    val state = orders.foldLeft(CollectionState()) { (state, currentOrder) =>
      val openPosition = state.openPosition.flatMap(placement => asEnter(placement).map(placement -> _))
      (openPosition, currentOrder.order) match {
        case (None, _: TO.Enter) =>
          state.copy(openPosition = Some(currentOrder))

        case (None, _: TO.Exit) =>
          state.copy(invalidOrderCount = state.invalidOrderCount + 1)

        case (Some((_, open)), enter: TO.Enter) if enter.position == open.position =>
          state.copy(invalidOrderCount = state.invalidOrderCount + 1)

        case (Some((placement, open)), enter: TO.Enter) =>
          val trade = closeTrade(open, placement.time, enter.price, currentOrder.time, settings)
          state.copy(trades = trade :: state.trades, openPosition = Some(currentOrder))

        case (Some((placement, open)), exit: TO.Exit) =>
          val trade = closeTrade(open, placement.time, exit.price, currentOrder.time, settings)
          state.copy(trades = trade :: state.trades, openPosition = None)
      }
    }

    // A position still open when the data runs out is liquidated at the final mark instead of being reported as an
    // unrealized balance. Reporting it separately left totalProfit including it while trade counts, expectancy,
    // profit factor and the monthly return series all excluded it, so no two metrics described the same trades.
    val forcedClosure = for
      placement <- state.openPosition
      open      <- asEnter(placement)
      mark      <- finalMark
      if !mark.observedAt.isBefore(placement.time)
    yield closeTrade(open, placement.time, mark.price, mark.observedAt, settings, forcedClosure = true)

    OrderStats.fromTrades(
      trades = state.trades.reverse ::: forcedClosure.toList,
      settings = settings,
      invalidOrderCount = state.invalidOrderCount
    )
  }

  private def closeTrade(
      open: TO.Enter,
      openedAt: Instant,
      exitPrice: BigDecimal,
      closedAt: Instant,
      settings: RiskSettings,
      forcedClosure: Boolean = false
  ): CompletedTrade = {
    val units      = open.volume * settings.unitsPerLot.value
    val grossQuote = priceProfit(open.position, open.price, exitPrice) * units
    val gross      = toAccountCurrency(open.currencyPair, grossQuote, exitPrice, settings)
    val costs      = transactionCosts(open.currencyPair, units, exitPrice, settings)
    CompletedTrade(
      currencyPair = open.currencyPair,
      position = open.position,
      openedAt = openedAt,
      closedAt = closedAt,
      entryPrice = open.price,
      exitPrice = exitPrice,
      volume = open.volume,
      grossProfit = gross,
      costs = costs,
      netProfit = gross - costs,
      forcedClosure = forcedClosure
    )
  }

  private def asEnter(placement: TradeOrderPlacement): Option[TO.Enter] =
    placement.order match {
      case enter: TO.Enter => Some(enter)
      case _: TO.Exit      => None
    }

  private def priceProfit(position: TO.Position, entryPrice: BigDecimal, exitPrice: BigDecimal): BigDecimal =
    position match {
      case TO.Position.Buy  => exitPrice - entryPrice
      case TO.Position.Sell => entryPrice - exitPrice
    }

  private def transactionCosts(
      currencyPair: CurrencyPair,
      units: BigDecimal,
      exitPrice: BigDecimal,
      settings: RiskSettings
  ): BigDecimal = {
    val pipSize           = if (currencyPair.quote.code == "JPY") BigDecimal("0.01") else BigDecimal("0.0001")
    val variableCostPips  = settings.transactionCosts.spreadPips.value + (settings.transactionCosts.slippagePipsPerSide.value * 2)
    val variableQuoteCost = variableCostPips * pipSize * units
    toAccountCurrency(currencyPair, variableQuoteCost, exitPrice, settings).abs + settings.transactionCosts.commissionPerTrade.value
  }

  private def toAccountCurrency(
      currencyPair: CurrencyPair,
      quoteAmount: BigDecimal,
      price: BigDecimal,
      settings: RiskSettings
  ): BigDecimal =
    if (currencyPair.quote == settings.accountCurrency) quoteAmount
    else if (currencyPair.base == settings.accountCurrency) quoteAmount / price
    else
      settings.quoteToAccountRates
        .get(currencyPair.quote)
        .map(rate => quoteAmount * rate.value)
        .getOrElse {
          throw new IllegalArgumentException(
            s"Missing ${currencyPair.quote.code}/${settings.accountCurrency.code} conversion rate for $currencyPair"
          )
        }
}
