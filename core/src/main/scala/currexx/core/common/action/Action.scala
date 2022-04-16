package currexx.core.common.action

import currexx.core.signal.Signal
import currexx.core.monitor.MonitorId
import currexx.domain.user.UserId

enum Action:
  case QueryMonitor(uid: UserId, mid: MonitorId)
  case SignalSubmitted(signal: Signal)
