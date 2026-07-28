package currexx.backtest

import currexx.backtest.syntax.*
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{Currency, CurrencyPair, PriceRange, TradeOrder as TO}

import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.math.sqrt

final case class TransactionCosts(
    spreadPips: BigDecimal = BigDecimal("0.8"),
    slippagePipsPerSide: BigDecimal = BigDecimal("0.1"),
    commissionPerTrade: BigDecimal = BigDecimal(0)
)

final case class RiskSettings(
    initialBalance: BigDecimal = BigDecimal(10000),
    accountCurrency: Currency = Currency.USD,
    unitsPerLot: BigDecimal = BigDecimal(100000),
    transactionCosts: TransactionCosts = TransactionCosts(),
    quoteToAccountRates: Map[Currency, BigDecimal] = Map.empty
):
  require(initialBalance > 0, "Initial balance must be positive")
  require(unitsPerLot > 0, "Units per lot must be positive")
  require(transactionCosts.spreadPips >= 0, "Spread cannot be negative")
  require(transactionCosts.slippagePipsPerSide >= 0, "Slippage cannot be negative")
  require(transactionCosts.commissionPerTrade >= 0, "Commission cannot be negative")
  require(quoteToAccountRates.values.forall(_ > 0), "Quote-to-account conversion rates must be positive")

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
    returnPct: BigDecimal = BigDecimal(0)
)

final case class OpenPositionSnapshot(
    currencyPair: CurrencyPair,
    position: TO.Position,
    openedAt: Instant,
    markedAt: Instant,
    entryPrice: BigDecimal,
    markPrice: BigDecimal,
    volume: BigDecimal,
    grossProfit: BigDecimal,
    estimatedCosts: BigDecimal,
    unrealizedProfit: BigDecimal
)

final case class EquityPoint(
    time: Instant,
    equity: BigDecimal,
    drawdown: BigDecimal,
    drawdownPercent: BigDecimal,
    realized: Boolean
)

final case class OrderStats(
    total: Int = 0,
    buys: Int = 0,
    sells: Int = 0,
    winCount: Int = 0,
    lossCount: Int = 0,
    breakevenCount: Int = 0,
    lossTotal: Double = 0.0,
    totalProfit: BigDecimal = BigDecimal(0),
    realizedProfit: BigDecimal = BigDecimal(0),
    unrealizedProfit: BigDecimal = BigDecimal(0),
    preCostProfit: BigDecimal = BigDecimal(0),
    totalCosts: BigDecimal = BigDecimal(0),
    grossProfit: BigDecimal = BigDecimal(0),
    grossLoss: BigDecimal = BigDecimal(0),
    biggestWin: BigDecimal = BigDecimal(0),
    biggestLoss: BigDecimal = BigDecimal(0),
    profitByMonth: Map[String, BigDecimal] = Map.empty,
    completedTrades: List[CompletedTrade] = Nil,
    openPositions: List[OpenPositionSnapshot] = Nil,
    equityCurve: List[EquityPoint] = Nil,
    initialBalance: BigDecimal = BigDecimal(10000),
    maxDrawdown: BigDecimal = BigDecimal(0),
    maxDrawdownPercent: BigDecimal = BigDecimal(0),
    sharpeRatio: Double = 0.0,
    sortinoRatio: Double = 0.0,
    maxConsecutiveWins: Int = 0,
    maxConsecutiveLosses: Int = 0,
    invalidOrderCount: Int = 0
):
  def medianProfitByMonth: BigDecimal = profitByMonth.values.toList.median.roundTo(5)
  def meanProfitByMonth: BigDecimal   = profitByMonth.values.toList.mean.roundTo(5)
  def meanLoss: BigDecimal            = if (lossCount == 0) BigDecimal(0) else BigDecimal(lossTotal / lossCount)
  def averageWin: BigDecimal          = if (winCount == 0) BigDecimal(0) else grossProfit / winCount
  def averageLoss: BigDecimal         = if (lossCount == 0) BigDecimal(0) else grossLoss / lossCount
  def payoffRatio: BigDecimal         = if (averageLoss == 0) averageWin else (averageWin / averageLoss).roundTo(5)
  def winRate: BigDecimal             = if (total == 0) BigDecimal(0) else (BigDecimal(winCount) / total).roundTo(5)
  def expectancy: BigDecimal          = if (total == 0) BigDecimal(0) else (realizedProfit / total).roundTo(8)
  def profitFactor: BigDecimal        = if (grossLoss == 0) grossProfit else (grossProfit / grossLoss).roundTo(5)
  def recoveryFactor: BigDecimal      = if (maxDrawdown == 0) totalProfit else (totalProfit / maxDrawdown).roundTo(5)
  def winLossRatio: BigDecimal        =
    if (lossCount == 0) BigDecimal(winCount)
    else (BigDecimal(winCount) / BigDecimal(lossCount)).roundTo(5)

  override def toString: String =
    s"""OrderStats(
       |netProfit=$totalProfit,
       |realizedProfit=$realizedProfit,
       |unrealizedProfit=$unrealizedProfit,
       |closedTrades=$total,
       |openPositions=${openPositions.size},
       |winRate=$winRate,
       |expectancy=$expectancy,
       |averageWin=$averageWin,
       |averageLoss=$averageLoss,
       |payoffRatio=$payoffRatio,
       |profitFactor=$profitFactor,
       |maxDrawdown=$maxDrawdown,
       |maxDrawdownPercent=$maxDrawdownPercent,
       |recoveryFactor=$recoveryFactor,
       |sharpeRatio=$sharpeRatio,
       |sortinoRatio=$sortinoRatio,
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
      openPositions: List[OpenPositionSnapshot],
      settings: RiskSettings,
      invalidOrderCount: Int = 0
  ): OrderStats = {
    val sortedTrades               = trades.sortBy(_.closedAt)
    val (completed, realizedCurve) = buildRealizedEquityCurve(sortedTrades, settings.initialBalance)
    val markedCurve                = appendMarkedEquity(realizedCurve, openPositions, settings.initialBalance)
    val netProfits                 = completed.map(_.netProfit)
    val wins                       = netProfits.filter(_ > 0)
    val losses                     = netProfits.filter(_ < 0)
    val monthly                    = completed.groupMapReduce(t => monthFormatter.format(t.closedAt))(_.netProfit)(_ + _)
    val (maxWins, maxLosses)       = streaks(netProfits)
    val (sharpe, sortino)          = monthlyRiskRatios(completed, settings.initialBalance)
    val realized                   = netProfits.sum
    val unrealized                 = openPositions.map(_.unrealizedProfit).sum
    val drawdown                   = markedCurve.map(_.drawdown).maxOption.getOrElse(BigDecimal(0))
    val drawdownPct                = markedCurve.map(_.drawdownPercent).maxOption.getOrElse(BigDecimal(0))

    OrderStats(
      total = completed.size,
      buys = completed.count(_.position == TO.Position.Buy),
      sells = completed.count(_.position == TO.Position.Sell),
      winCount = wins.size,
      lossCount = losses.size,
      breakevenCount = netProfits.count(_ == 0),
      lossTotal = losses.sum.toDouble,
      totalProfit = realized + unrealized,
      realizedProfit = realized,
      unrealizedProfit = unrealized,
      preCostProfit = completed.map(_.grossProfit).sum + openPositions.map(_.grossProfit).sum,
      totalCosts = completed.map(_.costs).sum + openPositions.map(_.estimatedCosts).sum,
      grossProfit = wins.sum,
      grossLoss = losses.map(_.abs).sum,
      biggestWin = wins.maxOption.getOrElse(BigDecimal(0)),
      biggestLoss = losses.minOption.getOrElse(BigDecimal(0)),
      profitByMonth = monthly,
      completedTrades = completed,
      openPositions = openPositions,
      equityCurve = markedCurve,
      initialBalance = settings.initialBalance,
      maxDrawdown = drawdown,
      maxDrawdownPercent = drawdownPct,
      sharpeRatio = sharpe,
      sortinoRatio = sortino,
      maxConsecutiveWins = maxWins,
      maxConsecutiveLosses = maxLosses,
      invalidOrderCount = invalidOrderCount
    )
  }

  def combine(stats: List[OrderStats], settings: RiskSettings = RiskSettings()): OrderStats =
    fromTrades(
      trades = stats.flatMap(_.completedTrades),
      openPositions = stats.flatMap(_.openPositions),
      settings = settings,
      invalidOrderCount = stats.map(_.invalidOrderCount).sum
    )

  private def buildRealizedEquityCurve(
      trades: List[CompletedTrade],
      initialBalance: BigDecimal
  ): (List[CompletedTrade], List[EquityPoint]) = {
    val (_, _, completed, points) = trades.foldLeft(
      (initialBalance, initialBalance, List.empty[CompletedTrade], List.empty[EquityPoint])
    ) { case ((equity, peak, accTrades, accPoints), trade) =>
      val returnPct = if (equity == 0) BigDecimal(0) else (trade.netProfit / equity * 100).roundTo(8)
      val enriched  = trade.copy(returnPct = returnPct)
      val next      = equity + trade.netProfit
      val nextPeak  = peak.max(next)
      val drawdown  = nextPeak - next
      val ddPct     = if (nextPeak == 0) BigDecimal(0) else (drawdown / nextPeak * 100).roundTo(8)
      (next, nextPeak, enriched :: accTrades, EquityPoint(trade.closedAt, next, drawdown, ddPct, realized = true) :: accPoints)
    }
    (completed.reverse, points.reverse)
  }

  private def appendMarkedEquity(
      realizedCurve: List[EquityPoint],
      openPositions: List[OpenPositionSnapshot],
      initialBalance: BigDecimal
  ): List[EquityPoint] =
    if (openPositions.isEmpty) realizedCurve
    else {
      val realizedEquity = realizedCurve.lastOption.map(_.equity).getOrElse(initialBalance)
      val previousPeak   = realizedCurve.map(_.equity).foldLeft(initialBalance)(_.max(_))
      val markedEquity   = realizedEquity + openPositions.map(_.unrealizedProfit).sum
      val peak           = previousPeak.max(markedEquity)
      val drawdown       = peak - markedEquity
      val drawdownPct    = if (peak == 0) BigDecimal(0) else (drawdown / peak * 100).roundTo(8)
      val markedAt       = openPositions.maxBy(_.markedAt.toEpochMilli).markedAt
      realizedCurve :+ EquityPoint(markedAt, markedEquity, drawdown, drawdownPct, realized = false)
    }

  private def monthlyRiskRatios(trades: List[CompletedTrade], initialBalance: BigDecimal): (Double, Double) = {
    val monthlyProfits = trades
      .groupMapReduce(t => monthFormatter.format(t.closedAt))(_.netProfit)(_ + _)
      .toList
      .sortBy(_._1)
      .map(_._2)

    val (_, returns) = monthlyProfits.foldLeft((initialBalance, List.empty[Double])) { case ((balance, acc), profit) =>
      val monthlyReturn = if (balance == 0) 0.0 else (profit / balance).toDouble
      (balance + profit, monthlyReturn :: acc)
    }
    val orderedReturns = returns.reverse
    if (orderedReturns.size < 2) (0.0, 0.0)
    else {
      val mean       = orderedReturns.sum / orderedReturns.size
      val variance   = orderedReturns.map(r => math.pow(r - mean, 2)).sum / (orderedReturns.size - 1)
      val deviation  = sqrt(variance)
      val downside   = orderedReturns.map(r => math.pow(math.min(r, 0.0), 2)).sum / orderedReturns.size
      val downsideSd = sqrt(downside)
      val annualizer = sqrt(12.0)
      (
        if (deviation == 0) 0.0 else mean / deviation * annualizer,
        if (downsideSd == 0) 0.0 else mean / downsideSd * annualizer
      )
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
      finalPrice: Option[PriceRange] = None,
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

    val markedPosition = for
      placement <- state.openPosition
      open      <- asEnter(placement)
      mark      <- finalPrice
      if !mark.time.isBefore(placement.time)
    yield markOpenPosition(open, placement.time, mark, settings)

    OrderStats.fromTrades(
      trades = state.trades.reverse,
      openPositions = markedPosition.toList,
      settings = settings,
      invalidOrderCount = state.invalidOrderCount
    )
  }

  private def closeTrade(
      open: TO.Enter,
      openedAt: Instant,
      exitPrice: BigDecimal,
      closedAt: Instant,
      settings: RiskSettings
  ): CompletedTrade = {
    val units      = open.volume * settings.unitsPerLot
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
      netProfit = gross - costs
    )
  }

  private def markOpenPosition(
      open: TO.Enter,
      openedAt: Instant,
      mark: PriceRange,
      settings: RiskSettings
  ): OpenPositionSnapshot = {
    val markPrice  = BigDecimal(mark.close)
    val units      = open.volume * settings.unitsPerLot
    val grossQuote = priceProfit(open.position, open.price, markPrice) * units
    val gross      = toAccountCurrency(open.currencyPair, grossQuote, markPrice, settings)
    val costs      = transactionCosts(open.currencyPair, units, markPrice, settings)
    OpenPositionSnapshot(
      currencyPair = open.currencyPair,
      position = open.position,
      openedAt = openedAt,
      markedAt = mark.time,
      entryPrice = open.price,
      markPrice = markPrice,
      volume = open.volume,
      grossProfit = gross,
      estimatedCosts = costs,
      unrealizedProfit = gross - costs
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
    val variableCostPips  = settings.transactionCosts.spreadPips + (settings.transactionCosts.slippagePipsPerSide * 2)
    val variableQuoteCost = variableCostPips * pipSize * units
    toAccountCurrency(currencyPair, variableQuoteCost, exitPrice, settings).abs + settings.transactionCosts.commissionPerTrade
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
        .map(quoteAmount * _)
        .getOrElse {
          throw new IllegalArgumentException(
            s"Missing ${currencyPair.quote.code}/${settings.accountCurrency.code} conversion rate for $currencyPair"
          )
        }
}
