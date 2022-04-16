package currexx.core.common.action

import currexx.core.signal.Signal
import currexx.core.monitor.MonitorId
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.user.UserId

import scala.concurrent.duration.FiniteDuration

enum Action:
  case RescheduleAllMonitors
  case QueryMonitor(uid: UserId, mid: MonitorId)
  case ScheduleMonitor(uid: UserId, mid: MonitorId, period: FiniteDuration)
  case ProcessMarketData(uid: UserId, data: MarketTimeSeriesData)
  case SignalSubmitted(signal: Signal)
