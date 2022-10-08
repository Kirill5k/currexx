package currexx.core.common.action

import cats.data.NonEmptyList
import currexx.core.market.MarketState
import currexx.core.signal.Signal
import currexx.core.monitor.MonitorId
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, Indicator, Interval, MarketTimeSeriesData}
import currexx.domain.monitor.Limits
import currexx.domain.user.UserId

import scala.concurrent.duration.FiniteDuration

enum Action:
  case RescheduleAllMonitors
  case ScheduleMonitor(uid: UserId, mid: MonitorId, period: FiniteDuration)
  case FetchMarketData(uid: UserId, cps: NonEmptyList[CurrencyPair], interval: Interval)
  case AssertProfit(uid: UserId, cps: NonEmptyList[CurrencyPair], limits: Limits)
  case ProcessMarketData(uid: UserId, data: MarketTimeSeriesData)
  case ProcessSignals(uid: UserId, currencyPair: CurrencyPair, signals: List[Signal])
  case ProcessMarketStateUpdate(state: MarketState, triggers: List[Indicator])
  case ProcessTradeOrderPlacement(order: TradeOrderPlacement)
  case CloseOpenOrders(uid: UserId, currencyPair: CurrencyPair)
  case CloseAllOpenOrders(uid: UserId)
