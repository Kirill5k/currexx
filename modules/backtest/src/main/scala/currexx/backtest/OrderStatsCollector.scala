package currexx.backtest

import cats.syntax.foldable.*
import cats.syntax.traverse.*
import currexx.backtest.syntax.*
import currexx.backtest.types.given
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{Currency, CurrencyPair, TradeOrder as TO}
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, PosBigDecimal}

import java.time.Instant
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

/** The span of market data a run was given, as opposed to the span it chose to trade over.
  *
  * The two are not the same, and only the first can say whether a run sat out the months at either end. A breakdown of profit by month
  * derived from the trades alone starts at the first trade and stops at the last, so a candidate that opens nothing until March and nothing
  * after October has those five months vanish from its record rather than count against it. They are exactly the months worth counting,
  * because avoiding an unfavourable stretch of a fixed sample is one of the cheapest things for a search to fit.
  */
final case class DataWindow(from: Instant, to: Instant):
  def union(other: DataWindow): DataWindow =
    DataWindow(if (from.isBefore(other.from)) from else other.from, if (to.isAfter(other.to)) to else other.to)

/** A risk-adjusted return ratio, or the reason there is no number to report.
  *
  * The two undefined cases mean opposite things and must not collapse into a single `None`. A vanished denominator is the best available
  * outcome — for Sortino, not one losing month — whereas too short a series means the ratio was never measured at all. A caller that
  * credits the second as generously as the first rewards a candidate for something it never demonstrated.
  */
enum RiskRatio {
  case Defined(value: Double)
  // The series had no dispersion to divide by: for Sortino, no month lost money; for Sharpe, every month returned the same.
  case ZeroDeviation
  // Fewer than two monthly returns, so there was no series to measure dispersion across.
  case InsufficientData

  def toOption: Option[Double] = this match
    case Defined(value) => Some(value)
    case _              => None
}

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
    // Trade totals include positions liquidated at the final mark. Risk and calendar profit also include the
    // unrealized P&L observed while those positions were open.
    totalProfit: BigDecimal = BigDecimal(0),
    preCostProfit: BigDecimal = BigDecimal(0),
    // Total trading costs in account currency (spread + two-sided slippage + commission)
    totalCosts: BigDecimal = BigDecimal(0),
    grossProfit: BigDecimal = BigDecimal(0),
    grossLoss: BigDecimal = BigDecimal(0),
    biggestWin: BigDecimal = BigDecimal(0),
    biggestLoss: BigDecimal = BigDecimal(0),
    // Change in account equity per calendar month, including flat months in the data window.
    profitByMonth: Map[String, BigDecimal] = Map.empty,
    completedTrades: List[CompletedTrade] = Nil,
    equityCurve: List[EquityPoint] = Nil,
    initialBalance: BigDecimal = BigDecimal(10000),
    maxDrawdown: BigDecimal = BigDecimal(0),
    // Largest peak-to-trough equity decline (((peak equity − lowest subsequent equity) / peak equity) × 100)
    // Sampled at candle closes and executions; movements inside a candle are not reconstructed from OHLC extremes.
    maxDrawdownPercent: BigDecimal = BigDecimal(0),
    // Risk-adjusted performance calculated from monthly equity returns and annualized, assuming a zero risk-free
    // rate. Higher means returns were more consistent. See RiskRatio for the two ways this can be unmeasurable.
    sharpeRatio: RiskRatio = RiskRatio.InsufficientData,
    sortinoRatio: RiskRatio = RiskRatio.InsufficientData,
    maxConsecutiveWins: Int = 0,
    maxConsecutiveLosses: Int = 0,
    forcedClosureCount: Int = 0,
    invalidOrderCount: Int = 0,
    // The market data the run was given, when the caller knew it. Absent leaves anything measured per period to fall
    // back on the span of the trades, which cannot see a run that sat out either end of the sample.
    dataWindow: Option[DataWindow] = None
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

  /** Trade-close accounting for callers with no market-price history. Calendar gaps are filled, but open-position risk is unobserved. */
  def fromTrades(
      trades: List[CompletedTrade],
      settings: RiskSettings,
      invalidOrderCount: Int = 0,
      dataWindow: Option[DataWindow] = None
  ): OrderStats = {
    val stats = aggregateTrades(trades, settings.initialBalance.value, invalidOrderCount, dataWindow)
    withEquityCurve(stats, tradeCloseEquityCurve(stats.completedTrades, stats.initialBalance))
  }

  /** Trade aggregates without an assumed equity path. The caller attaches the observed or explicitly trade-close curve once. */
  private[backtest] def aggregateTrades(
      trades: List[CompletedTrade],
      initialBalance: BigDecimal,
      invalidOrderCount: Int,
      dataWindow: Option[DataWindow]
  ): OrderStats = {
    val completed            = withTradeReturns(trades, initialBalance)
    val netProfits           = completed.map(_.netProfit)
    val wins                 = netProfits.filter(_ > 0)
    val losses               = netProfits.filter(_ < 0)
    val (maxWins, maxLosses) = streaks(netProfits)

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
      completedTrades = completed,
      initialBalance = initialBalance,
      maxConsecutiveWins = maxWins,
      maxConsecutiveLosses = maxLosses,
      forcedClosureCount = completed.count(_.forcedClosure),
      invalidOrderCount = invalidOrderCount,
      dataWindow = dataWindow
    )
  }

  /** Pools trade aggregates and observed equity curves. Each dataset was simulated on its own account, so the portfolio starts with their
    * combined balances. Using one member's balance would make percentage thresholds change meaning with dataset count.
    */
  def combine(stats: List[OrderStats]): OrderStats = {
    val initialBalance = stats.map(_.initialBalance).sum match
      case pooled if pooled > 0 => pooled
      case _                    => RiskSettings().initialBalance.value
    val pooled = aggregateTrades(
      trades = stats.flatMap(_.completedTrades),
      initialBalance = initialBalance,
      invalidOrderCount = stats.map(_.invalidOrderCount).sum,
      // The pooled window has to span every member's, or a month one dataset was given and another was not would be
      // missing from the pooled record of a run that did cover it.
      dataWindow = stats.flatMap(_.dataWindow).reduceOption(_.union(_))
    )
    withEquityCurve(pooled, EquityCurve.combine(stats, pooled.initialBalance))
  }

  private[backtest] def withEquityCurve(stats: OrderStats, curve: List[EquityPoint]): OrderStats = {
    val monthly           = EquityCurve.monthlyProfits(curve, stats.initialBalance, stats.dataWindow)
    val (sharpe, sortino) = riskRatios(monthly.toList.sortBy(_._1).map(_._2), stats.initialBalance)
    stats.copy(
      equityCurve = curve,
      profitByMonth = monthly,
      maxDrawdown = curve.map(_.drawdown).maxOption.getOrElse(BigDecimal(0)),
      maxDrawdownPercent = curve.map(_.drawdownPercent).maxOption.getOrElse(BigDecimal(0)),
      sharpeRatio = sharpe,
      sortinoRatio = sortino
    )
  }

  // Per-trade returns remain relative to the realized balance immediately before simultaneous closes.
  private def withTradeReturns(
      trades: List[CompletedTrade],
      initialBalance: BigDecimal
  ): List[CompletedTrade] = {
    val tradesByCloseTime = trades.groupBy(_.closedAt).toList.sortBy(_._1)
    val (_, completed)    = tradesByCloseTime.foldLeft((initialBalance, List.empty[CompletedTrade])) {
      case ((balance, accTrades), (_, simultaneousTrades)) =>
        val enriched = simultaneousTrades.map { trade =>
          val returnPct = if (balance == 0) BigDecimal(0) else (trade.netProfit / balance * 100).roundTo(8)
          trade.copy(returnPct = returnPct)
        }
        (balance + simultaneousTrades.map(_.netProfit).sum, enriched.reverse ::: accTrades)
    }
    completed.reverse
  }

  private def tradeCloseEquityCurve(trades: List[CompletedTrade], initialBalance: BigDecimal): List[EquityPoint] = {
    val profits     = trades.groupMapReduce(_.closedAt)(_.netProfit)(_ + _).toList.sortBy(_._1)
    val (_, values) = profits.foldLeft((initialBalance, List.empty[(Instant, BigDecimal)])) { case ((equity, points), (time, profit)) =>
      val next = equity + profit
      (next, (time -> next) :: points)
    }
    EquityCurve.fromValues(values.reverse, initialBalance)
  }

  /** Annualized Sharpe and Sortino from the monthly profit series.
    *
    * Neither a zero denominator nor too short a series is reported as a ratio of 0.0, because 0.0 is a poor result and neither of these is:
    * a zero downside deviation means no month lost money, and a single month means nothing was measured. Scoring either as zero would push
    * an optimiser towards strategies that do have losing months. They are returned as distinct cases rather than one catch-all so that a
    * caller can credit the good outcome without also crediting the absent one.
    */
  def riskRatios(
      profits: List[BigDecimal],
      initialBalance: BigDecimal,
      periodsPerYear: Double = 12.0
  ): (RiskRatio, RiskRatio) = {
    val (_, returns) = profits.foldLeft((initialBalance, List.empty[Double])) { case ((balance, acc), profit) =>
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
      val annualizer = sqrt(periodsPerYear)
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

  /** Values equity at every supplied market mark and execution. Callers must supply the candle-close history for the evaluated window;
    * missing observations cannot be inferred from trades. The latest mark liquidates any remaining position.
    *
    * An empty history is valid only for an empty, unstarted run. Identical duplicate marks are harmless; conflicting prices are rejected.
    */
  def collect(
      orders: List[TradeOrderPlacement],
      marketMarks: List[MarketMark],
      settings: RiskSettings = RiskSettings(),
      dataWindow: Option[DataWindow] = None
  ): Either[IllegalArgumentException, OrderStats] =
    marketMarks match {
      case Nil if orders.isEmpty && dataWindow.isEmpty =>
        Right(OrderStats(initialBalance = settings.initialBalance.value))
      case Nil =>
        Left(
          new IllegalArgumentException(
            "Market marks are required for equity accounting; use collectTradeOnly when no price history is available"
          )
        )
      case _ =>
        for
          marks <- validateMarks(marketMarks)
          state <- collectOrders(orders, marks.lastOption, settings)
          curve <- state.openPosition match {
            case None    => markedEquityCurve(state.trades, marks, settings)
            case Some(_) =>
              Left(new IllegalArgumentException("Market marks must extend through the open position so its final equity can be measured"))
          }
        yield {
          val stats = OrderStats.aggregateTrades(state.trades, settings.initialBalance.value, state.invalidOrderCount, dataWindow)
          OrderStats.withEquityCurve(stats, curve)
        }
    }

  /** Explicit trade-close accounting. A final mark may settle an open position, but does not supply its intervening equity path. */
  def collectTradeOnly(
      orders: List[TradeOrderPlacement],
      finalMark: Option[MarketMark] = None,
      settings: RiskSettings = RiskSettings(),
      dataWindow: Option[DataWindow] = None
  ): Either[IllegalArgumentException, OrderStats] =
    collectOrders(orders, finalMark, settings).map { state =>
      OrderStats.fromTrades(state.trades, settings, state.invalidOrderCount, dataWindow)
    }

  private def validateMarks(marks: List[MarketMark]): Either[IllegalArgumentException, List[MarketMark]] =
    marks
      .sortBy(_.observedAt)
      .foldLeft[Either[IllegalArgumentException, List[MarketMark]]](Right(Nil)) { (result, mark) =>
        result.flatMap { previous =>
          previous.headOption match {
            case Some(last) if last.observedAt == mark.observedAt && last.price == mark.price =>
              Right(previous)
            case Some(last) if last.observedAt == mark.observedAt =>
              Left(new IllegalArgumentException(s"Conflicting market marks at ${mark.observedAt}: ${last.price} and ${mark.price}"))
            case _ =>
              Right(mark :: previous)
          }
        }
      }
      .map(_.reverse)

  private def collectOrders(
      orders: List[TradeOrderPlacement],
      finalMark: Option[MarketMark],
      settings: RiskSettings
  ): Either[IllegalArgumentException, CollectionState] = {
    val collected: Either[IllegalArgumentException, CollectionState] = orders.foldM(CollectionState()) { (state, currentOrder) =>
      val openPosition = state.openPosition.flatMap(placement => asEnter(placement).map(placement -> _))
      (openPosition, currentOrder.order) match {
        case (None, _: TO.Enter) =>
          Right(state.copy(openPosition = Some(currentOrder)))

        case (None, _: TO.Exit) =>
          Right(state.copy(invalidOrderCount = state.invalidOrderCount + 1))

        case (Some((_, open)), enter: TO.Enter) if enter.position == open.position =>
          Right(state.copy(invalidOrderCount = state.invalidOrderCount + 1))

        case (Some((placement, open)), enter: TO.Enter) =>
          closeTrade(open, placement.time, enter.price, currentOrder.time, settings)
            .map(trade => state.copy(trades = trade :: state.trades, openPosition = Some(currentOrder)))

        case (Some((placement, open)), exit: TO.Exit) =>
          closeTrade(open, placement.time, exit.price, currentOrder.time, settings)
            .map(trade => state.copy(trades = trade :: state.trades, openPosition = None))
      }
    }

    // A position still open when the data runs out is liquidated at the final mark instead of being reported as an
    // unrealized balance. Reporting it separately left totalProfit including it while trade counts, expectancy,
    // profit factor and the monthly return series all excluded it, so no two metrics described the same trades.
    collected.flatMap { state =>
      val closing = for
        placement <- state.openPosition
        open      <- asEnter(placement)
        mark      <- finalMark
        if !mark.observedAt.isBefore(placement.time)
      yield (placement, open, mark)

      closing
        .traverse { case (placement, open, mark) =>
          closeTrade(open, placement.time, mark.price, mark.observedAt, settings, forcedClosure = true)
        }
        .map { forcedClosure =>
          state.copy(
            trades = state.trades.reverse ::: forcedClosure.toList,
            openPosition = state.openPosition.filter(_ => forcedClosure.isEmpty)
          )
        }
    }
  }

  /** Equity is the amount left on liquidation at the current price. Reserve the full configured round-trip cost while a position is open,
    * then replace its marked P&L with the realized net result on close; costs are never deducted a second time. This keeps the existing
    * trade-cost model and terminal P&L while recognizing its cost throughout the position's life.
    */
  private def markedEquityCurve(
      trades: List[CompletedTrade],
      marks: List[MarketMark],
      settings: RiskSettings
  ): Either[IllegalArgumentException, List[EquityPoint]] = {
    val openings = trades.groupBy(_.openedAt)
    val closings = trades.groupBy(_.closedAt)
    val prices   = marks.map(mark => mark.observedAt -> mark.price).toMap
    val times    = (openings.keySet ++ closings.keySet ++ prices.keySet).toList.sorted
    times
      .foldM((settings.initialBalance.value, List.empty[CompletedTrade], List.empty[(Instant, BigDecimal)])) {
        case ((balance, positions, values), time) =>
          val opened   = openings.getOrElse(time, Nil)
          val closed   = closings.getOrElse(time, Nil)
          val active   = (positions ::: opened).filter(_.closedAt.isAfter(time))
          val realised = balance + closed.map(_.netProfit).sum
          // At executions the fill price is the available mark; at candle close the supplied close takes precedence.
          val price      = prices.get(time).orElse(opened.headOption.map(_.entryPrice)).orElse(closed.headOption.map(_.exitPrice))
          val unrealised = price
            .traverse { currentPrice =>
              active
                .traverse { trade =>
                  for
                    units      = trade.volume * settings.unitsPerLot.value
                    grossQuote = priceProfit(trade.position, trade.entryPrice, currentPrice) * units
                    gross <- toAccountCurrency(trade.currencyPair, grossQuote, currentPrice, settings)
                    costs <- transactionCosts(trade.currencyPair, units, currentPrice, settings)
                  yield gross - costs
                }
                .map(_.sum)
            }
          unrealised
            .map { profit =>
              (realised, active, (time -> (realised + profit.getOrElse(BigDecimal(0)))) :: values)
            }
      }
      .map { case (_, _, values) =>
        EquityCurve.fromValues(values.reverse, settings.initialBalance.value)
      }
  }

  private def closeTrade(
      open: TO.Enter,
      openedAt: Instant,
      exitPrice: BigDecimal,
      closedAt: Instant,
      settings: RiskSettings,
      forcedClosure: Boolean = false
  ): Either[IllegalArgumentException, CompletedTrade] =
    for
      units      = open.volume * settings.unitsPerLot.value
      grossQuote = priceProfit(open.position, open.price, exitPrice) * units
      gross <- toAccountCurrency(open.currencyPair, grossQuote, exitPrice, settings)
      costs <- transactionCosts(open.currencyPair, units, exitPrice, settings)
    yield CompletedTrade(
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
  ): Either[IllegalArgumentException, BigDecimal] = {
    val pipSize           = if (currencyPair.quote.code == "JPY") BigDecimal("0.01") else BigDecimal("0.0001")
    val variableCostPips  = settings.transactionCosts.spreadPips.value + (settings.transactionCosts.slippagePipsPerSide.value * 2)
    val variableQuoteCost = variableCostPips * pipSize * units
    toAccountCurrency(currencyPair, variableQuoteCost, exitPrice, settings)
      .map(_.abs + settings.transactionCosts.commissionPerTrade.value)
  }

  private def toAccountCurrency(
      currencyPair: CurrencyPair,
      quoteAmount: BigDecimal,
      price: BigDecimal,
      settings: RiskSettings
  ): Either[IllegalArgumentException, BigDecimal] =
    if (currencyPair.quote == settings.accountCurrency) Right(quoteAmount)
    else if (currencyPair.base == settings.accountCurrency) {
      Either.cond(
        price > 0,
        quoteAmount / price,
        new IllegalArgumentException(
          s"Cannot convert ${currencyPair.quote.code}/${settings.accountCurrency.code} at non-positive price $price"
        )
      )
    } else
      settings.quoteToAccountRates
        .get(currencyPair.quote)
        .toRight(
          new IllegalArgumentException(
            s"Missing ${currencyPair.quote.code}/${settings.accountCurrency.code} conversion rate for $currencyPair"
          )
        )
        .map(rate => quoteAmount * rate.value)
}
