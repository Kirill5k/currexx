package currexx.core.common.action

import currexx.core.market.MarketState
import currexx.core.signal.Signal
import currexx.core.monitor.MonitorId
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, MarketTimeSeriesData}
import currexx.domain.market.v2.Indicator
import currexx.domain.user.UserId

import scala.concurrent.duration.FiniteDuration

enum Action:
  case RescheduleAllMonitors
  case QueryMonitor(uid: UserId, mid: MonitorId)
  case ScheduleMonitor(uid: UserId, mid: MonitorId, period: FiniteDuration)
  case ProcessMarketData(uid: UserId, data: MarketTimeSeriesData)
  case ProcessSignal(signal: Signal)
  case ProcessMarketStateUpdate(state: MarketState, trigger: Indicator)
  case ProcessTradeOrderPlacement(order: TradeOrderPlacement)
  case CloseOpenOrders(uid: UserId, currencyPair: CurrencyPair)
  case CloseAllOpenOrders(uid: UserId)
