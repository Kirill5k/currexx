package currexx.core.common.action

import currexx.core.market.MarketState
import currexx.core.signal.Signal
import currexx.core.monitor.MonitorId
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, Indicator, MarketTimeSeriesData}
import currexx.domain.user.UserId

import scala.concurrent.duration.FiniteDuration

enum Action:
  case RescheduleAllMonitors
  case QueryMonitor(uid: UserId, mid: MonitorId)
  case ScheduleMonitor(uid: UserId, mid: MonitorId, period: FiniteDuration)
  case ProcessMarketData(uid: UserId, data: MarketTimeSeriesData)
  case ProcessSignals(uid: UserId, currencyPair: CurrencyPair, signals: List[Signal])
  case ProcessMarketStateUpdate(state: MarketState, triggers: List[Indicator])
  case ProcessTradeOrderPlacement(order: TradeOrderPlacement)
  case CloseOpenOrders(uid: UserId, currencyPair: CurrencyPair)
  case CloseAllOpenOrders(uid: UserId)
